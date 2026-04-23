//----------------------------------------------------------------------------//
// Bloom.pas — ZDS-Booster bloom post-process                                 //
//                                                                            //
// Pipeline (runs before FXAA so glow edges get antialiased too):             //
//   1. Copy backbuffer into a full-res scene texture                         //
//   2. Bright-pass + downsample to a half-res texture (keeps pixels whose    //
//      luma exceeds BloomThreshold, scaled by excess)                        //
//   3. Horizontal 9-tap Gaussian into a ping-pong texture                    //
//   4. Vertical   9-tap Gaussian back                                        //
//   5. Additive composite half-res blur onto the default framebuffer         //
//                                                                            //
// All FBOs internal — doesn't conflict with engine's own FBO usage because   //
// state is restored with glPushAttrib before ApplyBloom is called.           //
//----------------------------------------------------------------------------//
unit Bloom;

interface

uses Windows, OpenGL, SysUtils, Math, Variables, EngineUtils;

procedure InitBloom(W, H: Integer);
procedure ShutdownBloom;
procedure ResizeBloom(W, H: Integer);
// Call after scene is rendered, BEFORE FXAA / SwapBuffers.
procedure ApplyBloom;

implementation

const
  BLOOM_LOG = 'DGLEngine_Log.txt';

  BLOOM_VS: AnsiString =
    '#version 120'#10 +
    'varying vec2 vUV;'#10 +
    'void main() {'#10 +
    '  vUV = gl_Vertex.xy * 0.5 + 0.5;'#10 +
    '  gl_Position = vec4(gl_Vertex.xy, 0.0, 1.0);'#10 +
    '}'#10;

  // Bright-pass + downsample. Samples 4 texels (2x2 box), averages, then
  // subtracts the threshold and scales. Output is already half-resolution.
  BLOOM_BRIGHT_FS: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2 uInvSrc;'#10 +
    'uniform float uThreshold;'#10 +
    'varying vec2 vUV;'#10 +
    'void main() {'#10 +
    '  vec3 c0 = texture2D(uTex, vUV + vec2(-0.5,-0.5) * uInvSrc).rgb;'#10 +
    '  vec3 c1 = texture2D(uTex, vUV + vec2( 0.5,-0.5) * uInvSrc).rgb;'#10 +
    '  vec3 c2 = texture2D(uTex, vUV + vec2(-0.5, 0.5) * uInvSrc).rgb;'#10 +
    '  vec3 c3 = texture2D(uTex, vUV + vec2( 0.5, 0.5) * uInvSrc).rgb;'#10 +
    '  vec3 c  = 0.25 * (c0 + c1 + c2 + c3);'#10 +
    '  float luma = dot(c, vec3(0.299, 0.587, 0.114));'#10 +
    '  float w = max(0.0, luma - uThreshold) / max(luma, 0.0001);'#10 +
    '  gl_FragColor = vec4(c * w, 1.0);'#10 +
    '}'#10;

  // 9-tap Gaussian blur. uDir = (1/W, 0) for H pass, (0, 1/H) for V pass.
  BLOOM_BLUR_FS: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2 uDir;'#10 +
    'varying vec2 vUV;'#10 +
    'const float W0 = 0.2270270270;'#10 +
    'const float W1 = 0.1945945946;'#10 +
    'const float W2 = 0.1216216216;'#10 +
    'const float W3 = 0.0540540541;'#10 +
    'const float W4 = 0.0162162162;'#10 +
    'void main() {'#10 +
    '  vec3 acc = texture2D(uTex, vUV).rgb * W0;'#10 +
    '  acc += texture2D(uTex, vUV + uDir * 1.0).rgb * W1;'#10 +
    '  acc += texture2D(uTex, vUV - uDir * 1.0).rgb * W1;'#10 +
    '  acc += texture2D(uTex, vUV + uDir * 2.0).rgb * W2;'#10 +
    '  acc += texture2D(uTex, vUV - uDir * 2.0).rgb * W2;'#10 +
    '  acc += texture2D(uTex, vUV + uDir * 3.0).rgb * W3;'#10 +
    '  acc += texture2D(uTex, vUV - uDir * 3.0).rgb * W3;'#10 +
    '  acc += texture2D(uTex, vUV + uDir * 4.0).rgb * W4;'#10 +
    '  acc += texture2D(uTex, vUV - uDir * 4.0).rgb * W4;'#10 +
    '  gl_FragColor = vec4(acc, 1.0);'#10 +
    '}'#10;

  // Additive composite — straight pass-through; blending is done via glBlendFunc.
  BLOOM_COMBINE_FS: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform float uStrength;'#10 +
    'varying vec2 vUV;'#10 +
    'void main() {'#10 +
    '  vec3 c = texture2D(uTex, vUV).rgb * uStrength;'#10 +
    '  gl_FragColor = vec4(c, 1.0);'#10 +
    '}'#10;

var
  SceneTex: GLuint;
  BloomFBO1, BloomFBO2: GLuint;
  BloomTex1, BloomTex2: GLuint;
  ProgBright, ProgBlur, ProgCombine: GLuint;
  uBright_Tex, uBright_InvSrc, uBright_Threshold: GLint;
  uBlur_Tex, uBlur_Dir: GLint;
  uCombine_Tex, uCombine_Strength: GLint;
  FullW, FullH: Integer;
  HalfW, HalfH: Integer;

function CompileShader(ShaderType: GLenum; const Source: AnsiString;
                       const Tag: string): GLuint;
var
  Src: PAnsiChar;
  Status, LogLen: GLint;
  LogBuf: AnsiString;
begin
  Result := glCreateShader(ShaderType);
  Src := PAnsiChar(Source);
  glShaderSource(Result, 1, @Src, nil);
  glCompileShader(Result);
  Status := 0;
  glGetShaderiv(Result, GL_COMPILE_STATUS, @Status);
  if Status = 0 then
  begin
    LogLen := 0;
    glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @LogLen);
    if LogLen > 0 then
    begin
      SetLength(LogBuf, LogLen);
      glGetShaderInfoLog(Result, LogLen, nil, PAnsiChar(LogBuf));
      AddToLogFile(BLOOM_LOG, 'Bloom shader compile failed (' + Tag + '): ' + string(LogBuf));
    end;
    glDeleteShader(Result);
    Result := 0;
  end;
end;

function LinkProgram(VS, FS: GLuint; const Tag: string): GLuint;
var
  Status, LogLen: GLint;
  LogBuf: AnsiString;
begin
  Result := glCreateProgram;
  glAttachShader(Result, VS);
  glAttachShader(Result, FS);
  glLinkProgram(Result);
  Status := 0;
  glGetProgramiv(Result, GL_LINK_STATUS, @Status);
  if Status = 0 then
  begin
    LogLen := 0;
    glGetProgramiv(Result, GL_INFO_LOG_LENGTH, @LogLen);
    if LogLen > 0 then
    begin
      SetLength(LogBuf, LogLen);
      glGetProgramInfoLog(Result, LogLen, nil, PAnsiChar(LogBuf));
      AddToLogFile(BLOOM_LOG, 'Bloom program link failed (' + Tag + '): ' + string(LogBuf));
    end;
    glDeleteProgram(Result);
    Result := 0;
  end;
end;

function MakeTex(W, H: Integer): GLuint;
begin
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

function MakeFBO(Tex: GLuint): GLuint;
var
  Status: GLenum;
begin
  glGenFramebuffers(1, @Result);
  glBindFramebuffer(GL_FRAMEBUFFER, Result);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Tex, 0);
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if Status <> GL_FRAMEBUFFER_COMPLETE then
  begin
    AddToLogFile(BLOOM_LOG, 'Bloom FBO incomplete, status=$' + IntToHex(Status, 4));
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glDeleteFramebuffers(1, @Result);
    Result := 0;
    Exit;
  end;
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure CreateResources(W, H: Integer);
begin
  FullW := W;
  FullH := H;
  HalfW := Max(1, W div 2);
  HalfH := Max(1, H div 2);

  SceneTex  := MakeTex(FullW, FullH);
  BloomTex1 := MakeTex(HalfW, HalfH);
  BloomTex2 := MakeTex(HalfW, HalfH);

  BloomFBO1 := MakeFBO(BloomTex1);
  BloomFBO2 := MakeFBO(BloomTex2);
end;

procedure DestroyResources;
begin
  if BloomFBO1 <> 0 then begin glDeleteFramebuffers(1, @BloomFBO1); BloomFBO1 := 0; end;
  if BloomFBO2 <> 0 then begin glDeleteFramebuffers(1, @BloomFBO2); BloomFBO2 := 0; end;
  if SceneTex  <> 0 then begin glDeleteTextures(1, @SceneTex);  SceneTex  := 0; end;
  if BloomTex1 <> 0 then begin glDeleteTextures(1, @BloomTex1); BloomTex1 := 0; end;
  if BloomTex2 <> 0 then begin glDeleteTextures(1, @BloomTex2); BloomTex2 := 0; end;
end;

procedure InitBloom(W, H: Integer);
var
  VS, FSB, FSBl, FSC: GLuint;
begin
  BloomActive := False;
  if not InitBloomEnable then Exit;
  if not UsingModernContext then
  begin
    AddToLogFile(BLOOM_LOG, 'Bloom: legacy context, skipping.');
    Exit;
  end;
  if not (Assigned(glCreateShader) and Assigned(glGenFramebuffers)) then
  begin
    AddToLogFile(BLOOM_LOG, 'Bloom: required GL entry points missing.');
    Exit;
  end;
  if (W <= 0) or (H <= 0) then Exit;

  VS  := CompileShader(GL_VERTEX_SHADER,   BLOOM_VS,          'vertex');
  FSB := CompileShader(GL_FRAGMENT_SHADER, BLOOM_BRIGHT_FS,   'bright');
  FSBl:= CompileShader(GL_FRAGMENT_SHADER, BLOOM_BLUR_FS,     'blur');
  FSC := CompileShader(GL_FRAGMENT_SHADER, BLOOM_COMBINE_FS,  'combine');
  if (VS = 0) or (FSB = 0) or (FSBl = 0) or (FSC = 0) then
  begin
    if VS  <> 0 then glDeleteShader(VS);
    if FSB <> 0 then glDeleteShader(FSB);
    if FSBl<> 0 then glDeleteShader(FSBl);
    if FSC <> 0 then glDeleteShader(FSC);
    Exit;
  end;

  ProgBright  := LinkProgram(VS, FSB,  'bright');
  ProgBlur    := LinkProgram(VS, FSBl, 'blur');
  ProgCombine := LinkProgram(VS, FSC,  'combine');
  glDeleteShader(VS);
  glDeleteShader(FSB);
  glDeleteShader(FSBl);
  glDeleteShader(FSC);

  if (ProgBright = 0) or (ProgBlur = 0) or (ProgCombine = 0) then
  begin
    if ProgBright  <> 0 then glDeleteProgram(ProgBright);
    if ProgBlur    <> 0 then glDeleteProgram(ProgBlur);
    if ProgCombine <> 0 then glDeleteProgram(ProgCombine);
    ProgBright := 0; ProgBlur := 0; ProgCombine := 0;
    Exit;
  end;

  uBright_Tex       := glGetUniformLocation(ProgBright,  'uTex');
  uBright_InvSrc    := glGetUniformLocation(ProgBright,  'uInvSrc');
  uBright_Threshold := glGetUniformLocation(ProgBright,  'uThreshold');
  uBlur_Tex         := glGetUniformLocation(ProgBlur,    'uTex');
  uBlur_Dir         := glGetUniformLocation(ProgBlur,    'uDir');
  uCombine_Tex      := glGetUniformLocation(ProgCombine, 'uTex');
  uCombine_Strength := glGetUniformLocation(ProgCombine, 'uStrength');

  CreateResources(W, H);
  if (BloomFBO1 = 0) or (BloomFBO2 = 0) then
  begin
    DestroyResources;
    glDeleteProgram(ProgBright);
    glDeleteProgram(ProgBlur);
    glDeleteProgram(ProgCombine);
    ProgBright := 0; ProgBlur := 0; ProgCombine := 0;
    Exit;
  end;

  BloomActive := True;
  AddToLogFile(BLOOM_LOG, 'Bloom: enabled at ' + IntToStr(W) + 'x' + IntToStr(H));
end;

procedure ShutdownBloom;
begin
  DestroyResources;
  if ProgBright  <> 0 then begin glDeleteProgram(ProgBright);  ProgBright  := 0; end;
  if ProgBlur    <> 0 then begin glDeleteProgram(ProgBlur);    ProgBlur    := 0; end;
  if ProgCombine <> 0 then begin glDeleteProgram(ProgCombine); ProgCombine := 0; end;
  BloomActive := False;
end;

procedure ResizeBloom(W, H: Integer);
begin
  if not BloomActive then Exit;
  if (W <= 0) or (H <= 0) then Exit;
  if (W = FullW) and (H = FullH) then Exit;
  DestroyResources;
  CreateResources(W, H);
  if (BloomFBO1 = 0) or (BloomFBO2 = 0) then BloomActive := False;
end;

procedure DrawFullScreenQuad;
begin
  glBegin(GL_TRIANGLE_STRIP);
    glVertex2f(-1.0, -1.0);
    glVertex2f( 1.0, -1.0);
    glVertex2f(-1.0,  1.0);
    glVertex2f( 1.0,  1.0);
  glEnd;
end;

procedure ApplyBloom;
begin
  if not BloomActive then Exit;
  if not InitBloomEnable then Exit;

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);

  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_FOG);
  glDisable(GL_BLEND);
  glDepthMask(GL_FALSE);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_TEXTURE_2D);

  glMatrixMode(GL_PROJECTION); glPushMatrix; glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);  glPushMatrix; glLoadIdentity;

  // 1) Snapshot backbuffer into SceneTex.
  glBindTexture(GL_TEXTURE_2D, SceneTex);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, FullW, FullH);

  // 2) Bright-pass + downsample into BloomTex1 (via BloomFBO1).
  glBindFramebuffer(GL_FRAMEBUFFER, BloomFBO1);
  glViewport(0, 0, HalfW, HalfH);
  glUseProgram(ProgBright);
  glBindTexture(GL_TEXTURE_2D, SceneTex);
  glUniform1i(uBright_Tex, 0);
  glUniform2f(uBright_InvSrc, 1.0 / FullW, 1.0 / FullH);
  glUniform1f(uBright_Threshold, InitBloomThreshold);
  DrawFullScreenQuad;

  // 3) Horizontal blur: BloomTex1 -> BloomFBO2.
  glBindFramebuffer(GL_FRAMEBUFFER, BloomFBO2);
  glViewport(0, 0, HalfW, HalfH);
  glUseProgram(ProgBlur);
  glBindTexture(GL_TEXTURE_2D, BloomTex1);
  glUniform1i(uBlur_Tex, 0);
  glUniform2f(uBlur_Dir, 1.0 / HalfW, 0.0);
  DrawFullScreenQuad;

  // 4) Vertical blur: BloomTex2 -> BloomFBO1.
  glBindFramebuffer(GL_FRAMEBUFFER, BloomFBO1);
  glViewport(0, 0, HalfW, HalfH);
  glBindTexture(GL_TEXTURE_2D, BloomTex2);
  glUniform1i(uBlur_Tex, 0);
  glUniform2f(uBlur_Dir, 0.0, 1.0 / HalfH);
  DrawFullScreenQuad;

  // 5) Additive composite BloomTex1 -> default framebuffer.
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, FullW, FullH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE);
  glUseProgram(ProgCombine);
  glBindTexture(GL_TEXTURE_2D, BloomTex1);
  glUniform1i(uCombine_Tex, 0);
  glUniform1f(uCombine_Strength, InitBloomStrength);
  DrawFullScreenQuad;

  glPopMatrix;
  glMatrixMode(GL_PROJECTION); glPopMatrix;
  glMatrixMode(GL_MODELVIEW);

  glUseProgram(0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  glPopClientAttrib;
  glPopAttrib;
end;

end.
