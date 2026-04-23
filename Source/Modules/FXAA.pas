//----------------------------------------------------------------------------//
// FXAA.pas — ZDS-Booster post-process antialiasing                           //
//                                                                            //
// Present-time approach: the engine renders to the default framebuffer as    //
// always. Right before SwapBuffers we copy the backbuffer into a texture     //
// and redraw it full-screen through the FXAA fragment shader. This avoids    //
// fighting the engine's own FBO passes (shadow maps, render-to-texture)     //
// which unbind to 0 mid-frame and used to cause flicker with the earlier    //
// wrap-the-whole-frame design.                                               //
//                                                                            //
// Compact FXAA 3.11 (Lottes) community variant. GLSL 1.20.                  //
//----------------------------------------------------------------------------//
unit FXAA;

interface

uses Windows, OpenGL, SysUtils, Variables, EngineUtils;

procedure InitFXAA(W, H: Integer);
procedure ShutdownFXAA;
procedure ResizeFXAA(W, H: Integer);
// Begin is kept as a no-op for historical symmetry. EndFXAAFrame is where
// the real work happens — call it immediately before SwapBuffers.
procedure BeginFXAAFrame;
procedure EndFXAAFrame;

implementation

const
  FXAA_LOG = 'DGLEngine_Log.txt';

  FXAA_VS: AnsiString =
    '#version 120'#10 +
    'varying vec2 vUV;'#10 +
    'void main() {'#10 +
    '  vUV = gl_Vertex.xy * 0.5 + 0.5;'#10 +
    '  gl_Position = vec4(gl_Vertex.xy, 0.0, 1.0);'#10 +
    '}'#10;

  // Edge-aware FXAA: early-exit on low local luma contrast so texture detail
  // and HUD text pass through unchanged. Only pixels on real geometry edges
  // get averaged. EDGE_THRESHOLD tuned to match NVIDIA's "quality" preset.
  FXAA_FS: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2 uInvRes;'#10 +
    'varying vec2 vUV;'#10 +
    '#define EDGE_THRESHOLD     (1.0/8.0)'#10 +
    '#define EDGE_THRESHOLD_MIN (1.0/24.0)'#10 +
    '#define FXAA_REDUCE_MIN    (1.0/128.0)'#10 +
    '#define FXAA_REDUCE_MUL    (1.0/8.0)'#10 +
    '#define FXAA_SPAN_MAX      4.0'#10 +
    'void main() {'#10 +
    '  vec3 rgbM  = texture2D(uTex, vUV).rgb;'#10 +
    '  vec3 rgbNW = texture2D(uTex, vUV + vec2(-1.0,-1.0) * uInvRes).rgb;'#10 +
    '  vec3 rgbNE = texture2D(uTex, vUV + vec2( 1.0,-1.0) * uInvRes).rgb;'#10 +
    '  vec3 rgbSW = texture2D(uTex, vUV + vec2(-1.0, 1.0) * uInvRes).rgb;'#10 +
    '  vec3 rgbSE = texture2D(uTex, vUV + vec2( 1.0, 1.0) * uInvRes).rgb;'#10 +
    '  vec3 luma = vec3(0.299, 0.587, 0.114);'#10 +
    '  float lM  = dot(rgbM,  luma);'#10 +
    '  float lNW = dot(rgbNW, luma);'#10 +
    '  float lNE = dot(rgbNE, luma);'#10 +
    '  float lSW = dot(rgbSW, luma);'#10 +
    '  float lSE = dot(rgbSE, luma);'#10 +
    '  float lMin = min(lM, min(min(lNW, lNE), min(lSW, lSE)));'#10 +
    '  float lMax = max(lM, max(max(lNW, lNE), max(lSW, lSE)));'#10 +
    '  float range = lMax - lMin;'#10 +
    '  if (range < max(EDGE_THRESHOLD_MIN, lMax * EDGE_THRESHOLD)) {'#10 +
    '    gl_FragColor = vec4(rgbM, 1.0);'#10 +
    '    return;'#10 +
    '  }'#10 +
    '  vec2 dir;'#10 +
    '  dir.x = -((lNW + lNE) - (lSW + lSE));'#10 +
    '  dir.y =  ((lNW + lSW) - (lNE + lSE));'#10 +
    '  float dirReduce = max((lNW + lNE + lSW + lSE) * (0.25 * FXAA_REDUCE_MUL), FXAA_REDUCE_MIN);'#10 +
    '  float rcpDirMin = 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);'#10 +
    '  dir = clamp(dir * rcpDirMin, vec2(-FXAA_SPAN_MAX), vec2(FXAA_SPAN_MAX)) * uInvRes;'#10 +
    '  vec3 rgbA = 0.5 * (texture2D(uTex, vUV + dir * (1.0/3.0 - 0.5)).rgb +'#10 +
    '                     texture2D(uTex, vUV + dir * (2.0/3.0 - 0.5)).rgb);'#10 +
    '  vec3 rgbB = rgbA * 0.5 + 0.25 * (texture2D(uTex, vUV + dir * -0.5).rgb +'#10 +
    '                                    texture2D(uTex, vUV + dir *  0.5).rgb);'#10 +
    '  float lB = dot(rgbB, luma);'#10 +
    '  if ((lB < lMin) || (lB > lMax))'#10 +
    '    gl_FragColor = vec4(rgbA, 1.0);'#10 +
    '  else'#10 +
    '    gl_FragColor = vec4(rgbB, 1.0);'#10 +
    '}'#10;

var
  ColorTex: GLuint;
  Prog: GLuint;
  uTex, uInvRes: GLint;
  FBWidth, FBHeight: Integer;

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
      AddToLogFile(FXAA_LOG, 'FXAA shader compile failed (' + Tag + '): ' + string(LogBuf));
    end
    else
      AddToLogFile(FXAA_LOG, 'FXAA shader compile failed (' + Tag + '), no info log.');
    glDeleteShader(Result);
    Result := 0;
  end;
end;

function LinkProgram(VS, FS: GLuint): GLuint;
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
      AddToLogFile(FXAA_LOG, 'FXAA program link failed: ' + string(LogBuf));
    end;
    glDeleteProgram(Result);
    Result := 0;
  end;
end;

procedure CreateColorTex(W, H: Integer);
begin
  glGenTextures(1, @ColorTex);
  glBindTexture(GL_TEXTURE_2D, ColorTex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glBindTexture(GL_TEXTURE_2D, 0);
  FBWidth  := W;
  FBHeight := H;
end;

procedure DestroyColorTex;
begin
  if ColorTex <> 0 then
  begin
    glDeleteTextures(1, @ColorTex);
    ColorTex := 0;
  end;
end;

procedure InitFXAA(W, H: Integer);
var
  VS, FS: GLuint;
begin
  FXAAActive := False;
  if not InitFXAAEnable then Exit;
  if not UsingModernContext then
  begin
    AddToLogFile(FXAA_LOG, 'FXAA: legacy context, skipping postprocess.');
    Exit;
  end;
  if not Assigned(glCreateShader) then
  begin
    AddToLogFile(FXAA_LOG, 'FXAA: shader entry points missing.');
    Exit;
  end;
  if (W <= 0) or (H <= 0) then Exit;

  VS := CompileShader(GL_VERTEX_SHADER,   FXAA_VS, 'vertex');
  FS := CompileShader(GL_FRAGMENT_SHADER, FXAA_FS, 'fragment');
  if (VS = 0) or (FS = 0) then
  begin
    if VS <> 0 then glDeleteShader(VS);
    if FS <> 0 then glDeleteShader(FS);
    Exit;
  end;
  Prog := LinkProgram(VS, FS);
  glDeleteShader(VS);
  glDeleteShader(FS);
  if Prog = 0 then Exit;

  uTex    := glGetUniformLocation(Prog, 'uTex');
  uInvRes := glGetUniformLocation(Prog, 'uInvRes');

  CreateColorTex(W, H);

  FXAAActive := True;
  AddToLogFile(FXAA_LOG, 'FXAA: enabled at ' + IntToStr(W) + 'x' + IntToStr(H));
end;

procedure ShutdownFXAA;
begin
  if Prog <> 0 then begin glDeleteProgram(Prog); Prog := 0; end;
  DestroyColorTex;
  FXAAActive := False;
end;

procedure ResizeFXAA(W, H: Integer);
begin
  if not FXAAActive then Exit;
  if (W <= 0) or (H <= 0) then Exit;
  if (W = FBWidth) and (H = FBHeight) then Exit;
  DestroyColorTex;
  CreateColorTex(W, H);
end;

procedure BeginFXAAFrame;
begin
  // No-op in present-time mode.
end;

procedure EndFXAAFrame;
begin
  if not FXAAActive then Exit;
  // Respect a live user toggle — menu can flip InitFXAAEnable at runtime.
  if not InitFXAAEnable then Exit;
  if Prog = 0 then Exit;
  if ColorTex = 0 then Exit;

  // Snapshot the fully-rendered backbuffer (engine + 2D HUD + everything) into
  // our texture. Driver resolves MSAA during the copy so the two techniques
  // compose cleanly.
  glBindTexture(GL_TEXTURE_2D, ColorTex);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, FBWidth, FBHeight);

  // Full state isolation — engine might have any combination of matrices,
  // blending, lighting, client arrays active.
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);

  glViewport(0, 0, FBWidth, FBHeight);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_FOG);
  glDepthMask(GL_FALSE);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, ColorTex);

  glUseProgram(Prog);
  glUniform1i(uTex, 0);
  glUniform2f(uInvRes, 1.0 / FBWidth, 1.0 / FBHeight);

  glMatrixMode(GL_PROJECTION); glPushMatrix; glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);  glPushMatrix; glLoadIdentity;

  glBegin(GL_TRIANGLE_STRIP);
    glVertex2f(-1.0, -1.0);
    glVertex2f( 1.0, -1.0);
    glVertex2f(-1.0,  1.0);
    glVertex2f( 1.0,  1.0);
  glEnd;

  glPopMatrix;
  glMatrixMode(GL_PROJECTION); glPopMatrix;
  glMatrixMode(GL_MODELVIEW);

  glUseProgram(0);
  glBindTexture(GL_TEXTURE_2D, 0);

  glPopClientAttrib;
  glPopAttrib;
end;

end.
