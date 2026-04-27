//----------------------------------------------------------------------------//
// PostFX.pas — ZDS-Booster unified post-processing pipeline                  //
//                                                                            //
// Replaces the older Bloom + FXAA modules with a single render-graph that    //
// runs in a documented order at present time (right before SwapBuffers).     //
//                                                                            //
// Frame data flow:                                                           //
//                                                                            //
//   [scene + HUD on default FB]                                              //
//        |                                                                   //
//        v   PassSnapshot                                                    //
//   SceneTex (full-res, RGBA8)                                               //
//        |                                                                   //
//        v   PassBloom (HDR knee bright-pass -> mip downsample/upsample)     //
//   BloomMip[0] = sum of Gaussian-pyramid contributions (full-res / 2)       //
//        |                                                                   //
//        v   PassTonemap (ACES Filmic, exposure, bloom composite)            //
//   PingTex                                                                  //
//        |                                                                   //
//        v   PassCAS    (AMD FidelityFX Contrast Adaptive Sharpening)        //
//   PongTex                                                                  //
//        |                                                                   //
//        v   PassChromAb (lens chromatic aberration)                         //
//   PingTex                                                                  //
//        |                                                                   //
//        v   PassFinishing (vignette + film grain + ordered dither)          //
//   PongTex                                                                  //
//        |                                                                   //
//        v   PassFXAA (writes to default FB; replaces backbuffer)            //
//   default FB                                                               //
//        |                                                                   //
//        v   SwapBuffers                                                     //
//                                                                            //
// Each pass can be skipped via a boolean toggle in Variables.pas; toggles    //
// are read every frame so CheatMenu.pas can flip them live without restart.  //
//                                                                            //
// State isolation: glPushAttrib(GL_ALL_ATTRIB_BITS) at the very start, full  //
// glPopAttrib at the end. Matrix stacks pushed and popped likewise. Engine   //
// won't notice we ran.                                                       //
//                                                                            //
// GLSL: targets #version 120 so it works in compatibility profile contexts  //
// without requiring uniform buffer objects, gl_Layer, etc.                   //
//                                                                            //
// Dependencies: Variables.pas (toggles), EngineUtils.pas (AddToLogFile).     //
// Bloom.pas and FXAA.pas are kept untouched — EngineCore.pas chooses to     //
// call ApplyPostFX instead and leaves the old units as dormant fallback.    //
//----------------------------------------------------------------------------//
unit PostFX;

interface

uses Windows, OpenGL, SysUtils, Math, Variables, EngineUtils, Bloom, FXAA;

procedure InitPostFX(W, H: Integer);
procedure ShutdownPostFX;
procedure ResizePostFX(W, H: Integer);
// Call after the scene + HUD have rendered, immediately before SwapBuffers.
procedure ApplyPostFX;
// Diagnostic dump to DGLEngine_Log.txt — call when debugging.
procedure DiagnosePostFX;

// Frame-boundary hooks for 2D-mask reconstruction.
//   * BeginPostFXFrame  — called once at the start of the frame to invalidate
//                         the previous Pre2D snapshot.
//   * MarkBefore2D      — called from DrawFunc2D.Begin2D; on the first call
//                         per frame it copies the current backbuffer into
//                         Pre2DTex (= "scene before any 2D HUD was drawn").
// The merge step in the final present pass uses the diff between Pre2DTex
// and SceneTex to detect 2D pixels and bypass the post-process for them,
// keeping the HUD/text crisp while the 3D world stays bloomed/tonemapped.
procedure BeginPostFXFrame;
procedure MarkBefore2D;

implementation

const
  POSTFX_LOG = 'DGLEngine_Log.txt';

  // Six mip levels gives a Gaussian-pyramid spread comparable to the
  // Call-of-Duty/Frostbite physical bloom presentations: small dots stay
  // local, large emissives spread out generously.
  MAX_BLOOM_MIPS = 6;

  // Smallest mip we'll try to allocate. Below this the tent filter degenerates.
  MIN_MIP_SIZE = 4;

//----------------------------------------------------------------------------//
//                                  SHADERS                                   //
//----------------------------------------------------------------------------//

  // Single vertex shader shared by every fragment pass. Input is the NDC
  // quad emitted by DrawFullscreenQuad (-1..+1). UV is computed by mapping
  // the position into 0..1.
  VS_FULLSCREEN: AnsiString =
    '#version 120'#10 +
    'varying vec2 vUV;'#10 +
    'void main() {'#10 +
    '  vUV = gl_Vertex.xy * 0.5 + 0.5;'#10 +
    '  gl_Position = vec4(gl_Vertex.xy, 0.0, 1.0);'#10 +
    '}'#10;

  // Bright-pass with HDR-style soft knee curve and built-in 1/2 downsample.
  //
  // The knee curve is the Frostbite/COD formulation:
  //     soft = clamp(brightness - threshold + knee, 0, 2*knee)
  //     soft = soft^2 / (4*knee + 1e-4)
  //     contribution = max(soft, brightness - threshold)
  // It produces a gradual transition around `threshold` instead of a hard cut,
  // so even pixels at ~0.7 luma can contribute weakly when threshold=1.0,
  // giving a much softer and more visible result than the old hard-threshold.
  //
  // Output is half-resolution because the fragment also samples a 2x2 box.
  FS_BRIGHT: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2  uInvSrc;'#10 +
    'uniform float uThreshold;'#10 +
    'uniform float uKnee;'#10 +
    'varying vec2  vUV;'#10 +
    'vec3 prefilter(vec3 c) {'#10 +
    '  float br = max(c.r, max(c.g, c.b));'#10 +
    '  float rq = clamp(br - uThreshold + uKnee, 0.0, 2.0 * uKnee);'#10 +
    '  rq = rq * rq * (1.0 / (4.0 * uKnee + 1e-4));'#10 +
    '  float w = max(rq, br - uThreshold) / max(br, 1e-4);'#10 +
    '  return c * w;'#10 +
    '}'#10 +
    'void main() {'#10 +
    '  vec3 c0 = texture2D(uTex, vUV + vec2(-0.5,-0.5) * uInvSrc).rgb;'#10 +
    '  vec3 c1 = texture2D(uTex, vUV + vec2( 0.5,-0.5) * uInvSrc).rgb;'#10 +
    '  vec3 c2 = texture2D(uTex, vUV + vec2(-0.5, 0.5) * uInvSrc).rgb;'#10 +
    '  vec3 c3 = texture2D(uTex, vUV + vec2( 0.5, 0.5) * uInvSrc).rgb;'#10 +
    '  vec3 avg = 0.25 * (c0 + c1 + c2 + c3);'#10 +
    '  gl_FragColor = vec4(prefilter(avg), 1.0);'#10 +
    '}'#10;

  // 13-tap "dual filter" downsample (Marius Bjørge, ARM/SIGGRAPH 2015).
  // Five 2x2 box samples; centre weighted 0.5, four corners 0.125 each.
  // Halves resolution while attenuating fireflies thanks to per-tap karis
  // weighting by inverse luma is intentionally NOT applied here — bright
  // pixels are exactly what we want to keep. (Karis weight is appropriate
  // when starting from raw HDR; we already pre-filtered through the knee.)
  FS_DOWNSAMPLE: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2  uInvSrc;'#10 +
    'varying vec2  vUV;'#10 +
    'void main() {'#10 +
    '  vec3 c00 = texture2D(uTex, vUV + uInvSrc * vec2(-2.0,-2.0)).rgb;'#10 +
    '  vec3 c01 = texture2D(uTex, vUV + uInvSrc * vec2( 0.0,-2.0)).rgb;'#10 +
    '  vec3 c02 = texture2D(uTex, vUV + uInvSrc * vec2( 2.0,-2.0)).rgb;'#10 +
    '  vec3 c10 = texture2D(uTex, vUV + uInvSrc * vec2(-1.0,-1.0)).rgb;'#10 +
    '  vec3 c11 = texture2D(uTex, vUV + uInvSrc * vec2( 1.0,-1.0)).rgb;'#10 +
    '  vec3 c20 = texture2D(uTex, vUV + uInvSrc * vec2(-2.0, 0.0)).rgb;'#10 +
    '  vec3 c21 = texture2D(uTex, vUV).rgb;'#10 +
    '  vec3 c22 = texture2D(uTex, vUV + uInvSrc * vec2( 2.0, 0.0)).rgb;'#10 +
    '  vec3 c30 = texture2D(uTex, vUV + uInvSrc * vec2(-1.0, 1.0)).rgb;'#10 +
    '  vec3 c31 = texture2D(uTex, vUV + uInvSrc * vec2( 1.0, 1.0)).rgb;'#10 +
    '  vec3 c40 = texture2D(uTex, vUV + uInvSrc * vec2(-2.0, 2.0)).rgb;'#10 +
    '  vec3 c41 = texture2D(uTex, vUV + uInvSrc * vec2( 0.0, 2.0)).rgb;'#10 +
    '  vec3 c42 = texture2D(uTex, vUV + uInvSrc * vec2( 2.0, 2.0)).rgb;'#10 +
    '  vec3 acc = c21 * 0.125;'#10 +
    '  acc += (c10 + c11 + c30 + c31) * (0.125);'#10 +
    '  acc += (c00 + c02 + c40 + c42) * (0.03125);'#10 +
    '  acc += (c01 + c20 + c22 + c41) * (0.0625);'#10 +
    '  gl_FragColor = vec4(acc, 1.0);'#10 +
    '}'#10;

  // 3x3 tent upsample. Reads from a smaller mip; output has the size of the
  // larger destination. Used additively over the destination thanks to
  // glBlendFunc(GL_ONE, GL_ONE) set by the caller.
  FS_UPSAMPLE: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2  uInvSrc;'#10 +
    'uniform float uRadius;'#10 +
    'varying vec2  vUV;'#10 +
    'void main() {'#10 +
    '  vec2 d = uInvSrc * uRadius;'#10 +
    '  vec3 acc = vec3(0.0);'#10 +
    '  acc += texture2D(uTex, vUV + vec2(-d.x,-d.y)).rgb * 1.0;'#10 +
    '  acc += texture2D(uTex, vUV + vec2( 0.0,-d.y)).rgb * 2.0;'#10 +
    '  acc += texture2D(uTex, vUV + vec2( d.x,-d.y)).rgb * 1.0;'#10 +
    '  acc += texture2D(uTex, vUV + vec2(-d.x, 0.0)).rgb * 2.0;'#10 +
    '  acc += texture2D(uTex, vUV                 ).rgb * 4.0;'#10 +
    '  acc += texture2D(uTex, vUV + vec2( d.x, 0.0)).rgb * 2.0;'#10 +
    '  acc += texture2D(uTex, vUV + vec2(-d.x, d.y)).rgb * 1.0;'#10 +
    '  acc += texture2D(uTex, vUV + vec2( 0.0, d.y)).rgb * 2.0;'#10 +
    '  acc += texture2D(uTex, vUV + vec2( d.x, d.y)).rgb * 1.0;'#10 +
    '  gl_FragColor = vec4(acc / 16.0, 1.0);'#10 +
    '}'#10;

  // Scene composition + tone mapping. This single pass handles:
  //   * SSAO darkening (multiplied into scene before bloom)
  //   * Exponential depth-based volumetric fog with height falloff
  //   * Bloom additive composite (with intensity gain)
  //   * sRGB->linear roundtrip with ACES Filmic curve
  //   * Manual exposure gain
  //
  // ACES Filmic curve (Stephen Hill / Krzysztof Narkowicz fit). Acts on
  // linear-light input. The framebuffer we sample from is sRGB-encoded
  // because the engine renders straight to default LDR backbuffer, so we
  // approximate gamma 2.2 as a back-conversion for the tonemap math.
  //
  // Each effect has a *Enable scalar uniform (0/1) so the menu can flip
  // it without a re-link.
  FS_TONEMAP: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uScene;'#10 +
    'uniform sampler2D uBloom;'#10 +
    'uniform sampler2D uSSAO;'#10 +
    'uniform sampler2D uDepth;'#10 +
    'uniform float uExposure;'#10 +
    'uniform float uBloomIntensity;'#10 +
    'uniform float uBloomEnable;'#10 +
    'uniform float uTonemapEnable;'#10 +
    'uniform float uSSAOEnable;'#10 +
    'uniform float uSSAOIntensity;'#10 +
    'uniform float uFogEnable;'#10 +
    'uniform float uFogDensity;'#10 +
    'uniform float uFogStart;'#10 +
    'uniform vec3  uFogColor;'#10 +
    'uniform float uZNear;'#10 +
    'uniform float uZFar;'#10 +
    'varying vec2  vUV;'#10 +
    'float linearDepth(float d) {'#10 +
    '  float ndc = d * 2.0 - 1.0;'#10 +
    '  return (2.0 * uZNear * uZFar) / (uZFar + uZNear - ndc * (uZFar - uZNear));'#10 +
    '}'#10 +
    'vec3 ACES(vec3 x) {'#10 +
    '  const float a = 2.51;'#10 +
    '  const float b = 0.03;'#10 +
    '  const float c = 2.43;'#10 +
    '  const float d = 0.59;'#10 +
    '  const float e = 0.14;'#10 +
    '  return clamp((x*(a*x+b)) / (x*(c*x+d)+e), 0.0, 1.0);'#10 +
    '}'#10 +
    'void main() {'#10 +
    '  vec3 scene = texture2D(uScene, vUV).rgb;'#10 +
    '  vec3 bloom = texture2D(uBloom, vUV).rgb;'#10 +
    '  float ao = mix(1.0, texture2D(uSSAO, vUV).r, uSSAOEnable);'#10 +
    '  scene *= mix(1.0, ao, uSSAOIntensity);'#10 +
    '  if (uFogEnable > 0.5) {'#10 +
    '    float d = texture2D(uDepth, vUV).r;'#10 +
    '    float lz = linearDepth(d);'#10 +
    '    float fogF = 1.0 - exp(-uFogDensity * max(0.0, lz - uFogStart));'#10 +
    '    fogF = clamp(fogF, 0.0, 1.0);'#10 +
    '    scene = mix(scene, uFogColor, fogF);'#10 +
    '  }'#10 +
    '  vec3 c = scene + bloom * uBloomIntensity * uBloomEnable;'#10 +
    '  vec3 lin = pow(c, vec3(2.2));'#10 +
    '  lin *= uExposure;'#10 +
    '  vec3 mapped = ACES(lin);'#10 +
    '  vec3 outc = pow(mapped, vec3(1.0/2.2));'#10 +
    '  gl_FragColor = vec4(mix(c, outc, uTonemapEnable), 1.0);'#10 +
    '}'#10;

  // SSAO — simplified Volumetric Obscurance variant. Doesn't need explicit
  // normals; uses pixel-space sample distribution rotated per-fragment by
  // a hash-derived angle, with range falloff to suppress haloing.
  //
  // Renders at half-resolution for performance. Since the AO output is
  // multiplied into the scene during tonemap, downsampled blur is implicit
  // (linear sampling at full-res reads a smoothed AO value).
  //
  // The classic gotcha: depth comparison must be in *linear* space, not
  // NDC, otherwise the radius behaves wildly with distance.
  FS_SSAO: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uDepth;'#10 +
    'uniform vec2  uInvRes;'#10 +    // 1/full-res (samples are in full-res space)
    'uniform float uRadius;'#10 +
    'uniform float uIntensity;'#10 +
    'uniform float uBias;'#10 +
    'uniform float uZNear;'#10 +
    'uniform float uZFar;'#10 +
    'varying vec2  vUV;'#10 +
    'float linearDepth(float d) {'#10 +
    '  float ndc = d * 2.0 - 1.0;'#10 +
    '  return (2.0 * uZNear * uZFar) / (uZFar + uZNear - ndc * (uZFar - uZNear));'#10 +
    '}'#10 +
    'float hash(vec2 p) {'#10 +
    '  return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);'#10 +
    '}'#10 +
    'void main() {'#10 +
    '  float dC = texture2D(uDepth, vUV).r;'#10 +
    '  if (dC > 0.999) { gl_FragColor = vec4(1.0); return; }'#10 +
    '  float zC = linearDepth(dC);'#10 +
    '  if (zC > 0.95 * uZFar) { gl_FragColor = vec4(1.0); return; }'#10 +
    '  float rot = hash(vUV) * 6.2831853;'#10 +
    '  float cs = cos(rot), sn = sin(rot);'#10 +
    // Pixel-space radius scales inversely with depth so the apparent
    // world-space sample radius stays roughly constant.
    '  vec2 baseR = vec2(uRadius * 100.0 / max(zC, 0.5)) * uInvRes;'#10 +
    '  float occ = 0.0;'#10 +
    '  vec2 offsets[8];'#10 +
    '  offsets[0] = vec2( 1.000,  0.000);'#10 +
    '  offsets[1] = vec2( 0.707, -0.707);'#10 +
    '  offsets[2] = vec2( 0.000, -1.000);'#10 +
    '  offsets[3] = vec2(-0.707, -0.707);'#10 +
    '  offsets[4] = vec2(-1.000,  0.000);'#10 +
    '  offsets[5] = vec2(-0.707,  0.707);'#10 +
    '  offsets[6] = vec2( 0.000,  1.000);'#10 +
    '  offsets[7] = vec2( 0.707,  0.707);'#10 +
    '  for (int i = 0; i < 8; i++) {'#10 +
    '    vec2 o = offsets[i];'#10 +
    '    vec2 r = vec2(o.x*cs - o.y*sn, o.x*sn + o.y*cs);'#10 +
    '    float ringScale = 0.25 + 0.75 * float(i) / 7.0;'#10 +
    '    vec2 sUv = vUV + r * baseR * ringScale;'#10 +
    '    float zS = linearDepth(texture2D(uDepth, sUv).r);'#10 +
    '    float dz = zC - zS;'#10 +
    '    float rangeCheck = smoothstep(0.0, 1.0, uRadius / max(abs(dz), 1e-3));'#10 +
    '    occ += step(uBias, dz) * rangeCheck;'#10 +
    '  }'#10 +
    '  occ /= 8.0;'#10 +
    '  float ao = clamp(1.0 - occ * uIntensity, 0.0, 1.0);'#10 +
    '  gl_FragColor = vec4(ao, ao, ao, 1.0);'#10 +
    '}'#10;

  // DOF — single-pass bokeh-ish circle sampler with circle-of-confusion
  // computed from depth. CoC = 0 inside [focus-range/2, focus+range/2],
  // grows linearly to 1.0 at infinity. Sample radius scales with CoC.
  //
  // 8-tap circular pattern; combined with 1 centre tap = 9 fetches max
  // when DOF is fully active. Foreground-defocus is included (CoC handles
  // both sides of focus zone via abs(linearDepth - focusDist)).
  //
  // Trades quality for cost — for genuine cinema-grade DOF you'd want
  // a separable Karis half-disk plus alpha-channel CoC propagation.
  FS_DOF: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform sampler2D uDepth;'#10 +
    'uniform vec2  uInvRes;'#10 +
    'uniform float uFocus;'#10 +
    'uniform float uRange;'#10 +
    'uniform float uAperture;'#10 +
    'uniform float uZNear;'#10 +
    'uniform float uZFar;'#10 +
    'varying vec2  vUV;'#10 +
    'float linearDepth(float d) {'#10 +
    '  float ndc = d * 2.0 - 1.0;'#10 +
    '  return (2.0 * uZNear * uZFar) / (uZFar + uZNear - ndc * (uZFar - uZNear));'#10 +
    '}'#10 +
    'void main() {'#10 +
    '  float lz = linearDepth(texture2D(uDepth, vUV).r);'#10 +
    '  float dist = abs(lz - uFocus);'#10 +
    '  float coc = clamp(max(0.0, dist - uRange * 0.5) / max(uRange, 0.001), 0.0, 1.0);'#10 +
    '  vec3 acc = texture2D(uTex, vUV).rgb;'#10 +
    '  float radius = coc * uAperture;'#10 +
    '  if (radius > 0.5) {'#10 +
    '    vec2 dirs[8];'#10 +
    '    dirs[0] = vec2( 1.000,  0.000);'#10 +
    '    dirs[1] = vec2( 0.707,  0.707);'#10 +
    '    dirs[2] = vec2( 0.000,  1.000);'#10 +
    '    dirs[3] = vec2(-0.707,  0.707);'#10 +
    '    dirs[4] = vec2(-1.000,  0.000);'#10 +
    '    dirs[5] = vec2(-0.707, -0.707);'#10 +
    '    dirs[6] = vec2( 0.000, -1.000);'#10 +
    '    dirs[7] = vec2( 0.707, -0.707);'#10 +
    '    for (int i = 0; i < 8; i++) {'#10 +
    '      acc += texture2D(uTex, vUV + dirs[i] * radius * uInvRes).rgb;'#10 +
    '    }'#10 +
    '    acc /= 9.0;'#10 +
    '  }'#10 +
    '  gl_FragColor = vec4(acc, 1.0);'#10 +
    '}'#10;

  // AMD FidelityFX CAS (Contrast Adaptive Sharpening), simplified.
  // The original CAS uses a 9-tap diamond and a contrast-sensitive weight;
  // we use a 5-tap cross which is a reasonable fast variant. Sharpness
  // (uSharpness) is in [0, 1]; ~0.3 is a good default.
  FS_CAS: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2  uInvRes;'#10 +
    'uniform float uSharpness;'#10 +
    'varying vec2  vUV;'#10 +
    'void main() {'#10 +
    '  vec3 a = texture2D(uTex, vUV + vec2(-1.0, 0.0) * uInvRes).rgb;'#10 +
    '  vec3 b = texture2D(uTex, vUV + vec2( 0.0,-1.0) * uInvRes).rgb;'#10 +
    '  vec3 c = texture2D(uTex, vUV).rgb;'#10 +
    '  vec3 d = texture2D(uTex, vUV + vec2( 1.0, 0.0) * uInvRes).rgb;'#10 +
    '  vec3 e = texture2D(uTex, vUV + vec2( 0.0, 1.0) * uInvRes).rgb;'#10 +
    '  vec3 mn = min(c, min(min(a, b), min(d, e)));'#10 +
    '  vec3 mx = max(c, max(max(a, b), max(d, e)));'#10 +
    '  vec3 amp_rcp = 1.0 / max(mx, vec3(1e-4));'#10 +
    '  vec3 amp = clamp(min(mn, vec3(1.0) - mx) * amp_rcp, 0.0, 1.0);'#10 +
    '  amp = sqrt(amp);'#10 +
    '  float w = -uSharpness * 0.125;'#10 +
    '  vec3 ww = vec3(w);'#10 +
    '  vec3 rcpW = 1.0 / (1.0 + 4.0 * (ww * amp));'#10 +
    '  vec3 outc = ((a + b + d + e) * (ww * amp) + c) * rcpW;'#10 +
    '  gl_FragColor = vec4(clamp(outc, 0.0, 1.0), 1.0);'#10 +
    '}'#10;

  // Chromatic aberration: separate R/G/B fetches with displacement that
  // grows radially from screen centre. uAmount is in pixels at the edge of
  // the screen (so 1.5 means ~1.5 pixels of separation at the corner).
  FS_CA: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2  uInvRes;'#10 +
    'uniform float uAmount;'#10 +
    'varying vec2  vUV;'#10 +
    'void main() {'#10 +
    '  vec2 p = vUV - 0.5;'#10 +
    '  float r2 = dot(p, p);'#10 +
    '  vec2 dir = p * r2 * uAmount;'#10 +
    '  vec2 d = dir * uInvRes;'#10 +
    '  float r = texture2D(uTex, vUV - d).r;'#10 +
    '  float g = texture2D(uTex, vUV    ).g;'#10 +
    '  float b = texture2D(uTex, vUV + d).b;'#10 +
    '  gl_FragColor = vec4(r, g, b, 1.0);'#10 +
    '}'#10;

  // Finishing pass — vignette, film grain, ordered dither anti-banding.
  // The dither uses a Bayer-style 4x4 matrix scaled to 1/255 LSB so it
  // effectively breaks up the smooth gradients that come from tonemap+bloom.
  FS_FINISHING: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform vec2  uRes;'#10 +
    'uniform float uVignette;'#10 +
    'uniform float uGrain;'#10 +
    'uniform float uDither;'#10 +
    'uniform float uTime;'#10 +
    'varying vec2  vUV;'#10 +
    'float hash(vec2 p) {'#10 +
    '  p = fract(p * vec2(123.34, 456.21));'#10 +
    '  p += dot(p, p + 78.233);'#10 +
    '  return fract(p.x * p.y);'#10 +
    '}'#10 +
    'float bayer4(vec2 p) {'#10 +
    '  int x = int(mod(p.x, 4.0));'#10 +
    '  int y = int(mod(p.y, 4.0));'#10 +
    '  int idx = x + y * 4;'#10 +
    '  if (idx == 0)  return 0.0/16.0;'#10 +
    '  if (idx == 1)  return 8.0/16.0;'#10 +
    '  if (idx == 2)  return 2.0/16.0;'#10 +
    '  if (idx == 3)  return 10.0/16.0;'#10 +
    '  if (idx == 4)  return 12.0/16.0;'#10 +
    '  if (idx == 5)  return 4.0/16.0;'#10 +
    '  if (idx == 6)  return 14.0/16.0;'#10 +
    '  if (idx == 7)  return 6.0/16.0;'#10 +
    '  if (idx == 8)  return 3.0/16.0;'#10 +
    '  if (idx == 9)  return 11.0/16.0;'#10 +
    '  if (idx == 10) return 1.0/16.0;'#10 +
    '  if (idx == 11) return 9.0/16.0;'#10 +
    '  if (idx == 12) return 15.0/16.0;'#10 +
    '  if (idx == 13) return 7.0/16.0;'#10 +
    '  if (idx == 14) return 13.0/16.0;'#10 +
    '  return 5.0/16.0;'#10 +
    '}'#10 +
    'void main() {'#10 +
    '  vec3 c = texture2D(uTex, vUV).rgb;'#10 +
    '  vec2 p = vUV - 0.5;'#10 +
    '  float v = 1.0 - dot(p, p) * uVignette * 2.0;'#10 +
    '  c *= clamp(v, 0.0, 1.0);'#10 +
    '  float n = hash(vUV + vec2(uTime, uTime * 1.7));'#10 +
    '  c += (n - 0.5) * uGrain;'#10 +
    '  vec2 px = vUV * uRes;'#10 +
    '  float dith = (bayer4(px) - 0.5) * uDither / 255.0;'#10 +
    '  c += vec3(dith);'#10 +
    '  gl_FragColor = vec4(c, 1.0);'#10 +
    '}'#10;

  // Trivial pass-through with optional 2D-mask merge. When uPre2DValid > 0.5
  // the shader checks the per-pixel difference between the original
  // backbuffer (uScene) and the pre-2D snapshot (uPre2D); if they differ
  // by more than uMaskThresh the pixel is considered "drawn by 2D" (HUD,
  // crosshair, gauges) and the original colour is preserved instead of the
  // post-processed one.
  //
  // Threshold of 1/255 ≈ 0.004 is the smallest detectable LDR difference;
  // we use 0.01 to absorb dithering / small driver-side colour noise.
  FS_COPY: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform sampler2D uScene;'#10 +
    'uniform sampler2D uPre2D;'#10 +
    'uniform float uPre2DValid;'#10 +
    'uniform float uMaskThresh;'#10 +
    'varying vec2 vUV;'#10 +
    'void main() {'#10 +
    '  if (uPre2DValid > 0.5) {'#10 +
    '    vec3 sc = texture2D(uScene, vUV).rgb;'#10 +
    '    vec3 p2 = texture2D(uPre2D, vUV).rgb;'#10 +
    '    vec3 d  = abs(sc - p2);'#10 +
    '    if (max(d.r, max(d.g, d.b)) > uMaskThresh) {'#10 +
    '      gl_FragColor = vec4(sc, 1.0);'#10 +
    '      return;'#10 +
    '    }'#10 +
    '  }'#10 +
    '  gl_FragColor = texture2D(uTex, vUV);'#10 +
    '}'#10;

  // FXAA 3.11 community variant (compact, edge-aware) extended with the
  // same 2D-mask merge as FS_COPY. 2D pixels short-circuit out of the AA
  // pass entirely so HUD text never gets re-aliased through the FXAA edge
  // detector. The early-out also saves a handful of texture fetches per HUD
  // pixel which is a minor performance win for HUD-heavy scenes.
  FS_FXAA: AnsiString =
    '#version 120'#10 +
    'uniform sampler2D uTex;'#10 +
    'uniform sampler2D uScene;'#10 +
    'uniform sampler2D uPre2D;'#10 +
    'uniform float uPre2DValid;'#10 +
    'uniform float uMaskThresh;'#10 +
    'uniform vec2 uInvRes;'#10 +
    'varying vec2 vUV;'#10 +
    '#define EDGE_THRESHOLD     (1.0/8.0)'#10 +
    '#define EDGE_THRESHOLD_MIN (1.0/24.0)'#10 +
    '#define FXAA_REDUCE_MIN    (1.0/128.0)'#10 +
    '#define FXAA_REDUCE_MUL    (1.0/8.0)'#10 +
    '#define FXAA_SPAN_MAX      4.0'#10 +
    'void main() {'#10 +
    '  if (uPre2DValid > 0.5) {'#10 +
    '    vec3 sc = texture2D(uScene, vUV).rgb;'#10 +
    '    vec3 p2 = texture2D(uPre2D, vUV).rgb;'#10 +
    '    vec3 dd = abs(sc - p2);'#10 +
    '    if (max(dd.r, max(dd.g, dd.b)) > uMaskThresh) {'#10 +
    '      gl_FragColor = vec4(sc, 1.0);'#10 +
    '      return;'#10 +
    '    }'#10 +
    '  }'#10 +
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

//----------------------------------------------------------------------------//
//                            STATE / RESOURCES                               //
//----------------------------------------------------------------------------//

type
  TBloomMip = record
    Tex, FBO: GLuint;
    W, H: Integer;
  end;

var
  // Per-frame state.
  FullW, FullH: Integer;
  PostFXReady: Boolean;
  FrameCounter: Cardinal;

  // Scene snapshot (full-resolution copy of default backbuffer).
  SceneTex: GLuint;

  // Pre-2D snapshot. Captured by MarkBefore2D() the first time DrawFunc2D's
  // Begin2D fires inside a frame, i.e. the moment the engine has finished
  // its 3D pass and is about to start the 2D HUD. The diff between this
  // and SceneTex (taken at the very end of the frame) gives us a reliable
  // 2D-pixel mask without needing a stencil buffer.
  Pre2DTex: GLuint;
  Pre2DCaptured: Boolean;  // true between MarkBefore2D and the next BeginPostFXFrame

  // Two full-resolution ping-pong FBOs for the colour-only pass chain.
  PingTex, PongTex: GLuint;
  PingFBO, PongFBO: GLuint;

  // Bloom pyramid.
  BloomMips: array[0 .. MAX_BLOOM_MIPS - 1] of TBloomMip;
  NumBloomMips: Integer;

  // Depth-based extras (Phase 2).
  DepthTex: GLuint;             // full-res depth-component texture (snap of default FB depth)
  SSAOTex: GLuint;              // half-res AO factor (R channel)
  SSAOFBO: GLuint;
  HalfW, HalfH: Integer;        // half-resolution for SSAO

  // Programs.
  ProgBright, ProgDownsample, ProgUpsample: GLuint;
  ProgTonemap, ProgCAS, ProgCA, ProgFinishing, ProgFXAA, ProgCopy: GLuint;
  ProgSSAO, ProgDOF: GLuint;

  // Cached uniform locations.
  uBright_Tex, uBright_InvSrc, uBright_Threshold, uBright_Knee: GLint;
  uDS_Tex, uDS_InvSrc: GLint;
  uUS_Tex, uUS_InvSrc, uUS_Radius: GLint;
  uTM_Scene, uTM_Bloom, uTM_SSAO, uTM_Depth: GLint;
  uTM_Exposure, uTM_BloomI, uTM_BloomEnable, uTM_TonemapEnable: GLint;
  uTM_SSAOEnable, uTM_SSAOIntensity: GLint;
  uTM_FogEnable, uTM_FogDensity, uTM_FogStart, uTM_FogColor: GLint;
  uTM_ZNear, uTM_ZFar: GLint;
  uCAS_Tex, uCAS_InvRes, uCAS_Sharpness: GLint;
  uCA_Tex, uCA_InvRes, uCA_Amount: GLint;
  uFin_Tex, uFin_Res, uFin_Vignette, uFin_Grain, uFin_Dither, uFin_Time: GLint;
  uFXAA_Tex, uFXAA_InvRes: GLint;
  uFXAA_Scene, uFXAA_Pre2D, uFXAA_Pre2DValid, uFXAA_MaskThresh: GLint;
  uCopy_Tex: GLint;
  uCopy_Scene, uCopy_Pre2D, uCopy_Pre2DValid, uCopy_MaskThresh: GLint;
  uSSAO_Depth, uSSAO_InvRes, uSSAO_Radius, uSSAO_Intensity, uSSAO_Bias,
    uSSAO_ZNear, uSSAO_ZFar: GLint;
  uDOF_Tex, uDOF_Depth, uDOF_InvRes, uDOF_Focus, uDOF_Range, uDOF_Aperture,
    uDOF_ZNear, uDOF_ZFar: GLint;

  // Pipeline state used during ApplyPostFX. Track which ping-pong tex holds
  // the latest result so optional passes can be skipped.
  CurReadTex: GLuint;
  CurWriteTex: GLuint;
  CurWriteFBO: GLuint;

//----------------------------------------------------------------------------//
//                                HELPERS                                     //
//----------------------------------------------------------------------------//

function CompileShader(ShaderType: GLenum; const Source: AnsiString;
                       const Tag: string): GLuint;
var
  Src: PAnsiChar;
  Status, LogLen: GLint;
  LogBuf: AnsiString;
begin
  Result := glCreateShader(ShaderType);
  if Result = 0 then
  begin
    AddToLogFile(POSTFX_LOG, 'PostFX: glCreateShader returned 0 for ' + Tag);
    Exit;
  end;
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
      AddToLogFile(POSTFX_LOG,
        'PostFX: shader compile failed (' + Tag + '): ' + string(LogBuf));
    end
    else
      AddToLogFile(POSTFX_LOG,
        'PostFX: shader compile failed (' + Tag + '), no info log.');
    glDeleteShader(Result);
    Result := 0;
  end;
end;

function LinkProgram2(VS, FS: GLuint; const Tag: string): GLuint;
var
  Status, LogLen: GLint;
  LogBuf: AnsiString;
begin
  Result := glCreateProgram;
  if Result = 0 then
  begin
    AddToLogFile(POSTFX_LOG, 'PostFX: glCreateProgram returned 0 for ' + Tag);
    Exit;
  end;
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
      AddToLogFile(POSTFX_LOG,
        'PostFX: program link failed (' + Tag + '): ' + string(LogBuf));
    end;
    glDeleteProgram(Result);
    Result := 0;
  end;
end;

function MakeColorTex(W, H: Integer): GLuint;
begin
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, W, H, 0,
               GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

// Depth texture suitable for glCopyTexSubImage2D(GL_DEPTH_COMPONENT) and
// for direct texture2D() sampling in shaders (texture compare mode off).
function MakeDepthTex(W, H: Integer): GLuint;
begin
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT24, W, H, 0,
               GL_DEPTH_COMPONENT, GL_UNSIGNED_INT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  // Disable texture compare so texture2D() returns the raw depth value.
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_NONE);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

function MakeFBO(Tex: GLuint; const Tag: string): GLuint;
var
  Status: GLenum;
begin
  glGenFramebuffers(1, @Result);
  glBindFramebuffer(GL_FRAMEBUFFER, Result);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                         GL_TEXTURE_2D, Tex, 0);
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if Status <> GL_FRAMEBUFFER_COMPLETE then
  begin
    AddToLogFile(POSTFX_LOG,
      'PostFX: FBO incomplete (' + Tag + ') status=$' + IntToHex(Status, 4));
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glDeleteFramebuffers(1, @Result);
    Result := 0;
    Exit;
  end;
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure DrawFullscreenQuad;
begin
  glBegin(GL_TRIANGLE_STRIP);
    glVertex2f(-1.0, -1.0);
    glVertex2f( 1.0, -1.0);
    glVertex2f(-1.0,  1.0);
    glVertex2f( 1.0,  1.0);
  glEnd;
end;

//----------------------------------------------------------------------------//
//                       PROGRAM / RESOURCE LIFECYCLE                         //
//----------------------------------------------------------------------------//

function CompileAndLink(const VSSrc, FSSrc: AnsiString;
                        const Tag: string): GLuint;
var
  VS, FS: GLuint;
begin
  Result := 0;
  VS := CompileShader(GL_VERTEX_SHADER,   VSSrc, Tag + '.vs');
  if VS = 0 then Exit;
  FS := CompileShader(GL_FRAGMENT_SHADER, FSSrc, Tag + '.fs');
  if FS = 0 then
  begin
    glDeleteShader(VS);
    Exit;
  end;
  Result := LinkProgram2(VS, FS, Tag);
  glDeleteShader(VS);
  glDeleteShader(FS);
end;

function CompilePrograms: Boolean;
begin
  Result := False;
  ProgBright     := CompileAndLink(VS_FULLSCREEN, FS_BRIGHT,     'bright');
  ProgDownsample := CompileAndLink(VS_FULLSCREEN, FS_DOWNSAMPLE, 'downsample');
  ProgUpsample   := CompileAndLink(VS_FULLSCREEN, FS_UPSAMPLE,   'upsample');
  ProgTonemap    := CompileAndLink(VS_FULLSCREEN, FS_TONEMAP,    'tonemap');
  ProgCAS        := CompileAndLink(VS_FULLSCREEN, FS_CAS,        'cas');
  ProgCA         := CompileAndLink(VS_FULLSCREEN, FS_CA,         'chromab');
  ProgFinishing  := CompileAndLink(VS_FULLSCREEN, FS_FINISHING,  'finishing');
  ProgFXAA       := CompileAndLink(VS_FULLSCREEN, FS_FXAA,       'fxaa');
  ProgCopy       := CompileAndLink(VS_FULLSCREEN, FS_COPY,       'copy');
  ProgSSAO       := CompileAndLink(VS_FULLSCREEN, FS_SSAO,       'ssao');
  ProgDOF        := CompileAndLink(VS_FULLSCREEN, FS_DOF,        'dof');

  if (ProgBright = 0) or (ProgDownsample = 0) or (ProgUpsample = 0) or
     (ProgTonemap = 0) or (ProgCAS = 0) or (ProgCA = 0) or
     (ProgFinishing = 0) or (ProgFXAA = 0) or (ProgCopy = 0) or
     (ProgSSAO = 0) or (ProgDOF = 0) then
    Exit;

  uBright_Tex       := glGetUniformLocation(ProgBright,    'uTex');
  uBright_InvSrc    := glGetUniformLocation(ProgBright,    'uInvSrc');
  uBright_Threshold := glGetUniformLocation(ProgBright,    'uThreshold');
  uBright_Knee      := glGetUniformLocation(ProgBright,    'uKnee');

  uDS_Tex    := glGetUniformLocation(ProgDownsample, 'uTex');
  uDS_InvSrc := glGetUniformLocation(ProgDownsample, 'uInvSrc');

  uUS_Tex    := glGetUniformLocation(ProgUpsample,   'uTex');
  uUS_InvSrc := glGetUniformLocation(ProgUpsample,   'uInvSrc');
  uUS_Radius := glGetUniformLocation(ProgUpsample,   'uRadius');

  uTM_Scene         := glGetUniformLocation(ProgTonemap, 'uScene');
  uTM_Bloom         := glGetUniformLocation(ProgTonemap, 'uBloom');
  uTM_SSAO          := glGetUniformLocation(ProgTonemap, 'uSSAO');
  uTM_Depth         := glGetUniformLocation(ProgTonemap, 'uDepth');
  uTM_Exposure      := glGetUniformLocation(ProgTonemap, 'uExposure');
  uTM_BloomI        := glGetUniformLocation(ProgTonemap, 'uBloomIntensity');
  uTM_BloomEnable   := glGetUniformLocation(ProgTonemap, 'uBloomEnable');
  uTM_TonemapEnable := glGetUniformLocation(ProgTonemap, 'uTonemapEnable');
  uTM_SSAOEnable    := glGetUniformLocation(ProgTonemap, 'uSSAOEnable');
  uTM_SSAOIntensity := glGetUniformLocation(ProgTonemap, 'uSSAOIntensity');
  uTM_FogEnable     := glGetUniformLocation(ProgTonemap, 'uFogEnable');
  uTM_FogDensity    := glGetUniformLocation(ProgTonemap, 'uFogDensity');
  uTM_FogStart      := glGetUniformLocation(ProgTonemap, 'uFogStart');
  uTM_FogColor      := glGetUniformLocation(ProgTonemap, 'uFogColor');
  uTM_ZNear         := glGetUniformLocation(ProgTonemap, 'uZNear');
  uTM_ZFar          := glGetUniformLocation(ProgTonemap, 'uZFar');

  uCAS_Tex       := glGetUniformLocation(ProgCAS, 'uTex');
  uCAS_InvRes    := glGetUniformLocation(ProgCAS, 'uInvRes');
  uCAS_Sharpness := glGetUniformLocation(ProgCAS, 'uSharpness');

  uCA_Tex    := glGetUniformLocation(ProgCA, 'uTex');
  uCA_InvRes := glGetUniformLocation(ProgCA, 'uInvRes');
  uCA_Amount := glGetUniformLocation(ProgCA, 'uAmount');

  uFin_Tex      := glGetUniformLocation(ProgFinishing, 'uTex');
  uFin_Res      := glGetUniformLocation(ProgFinishing, 'uRes');
  uFin_Vignette := glGetUniformLocation(ProgFinishing, 'uVignette');
  uFin_Grain    := glGetUniformLocation(ProgFinishing, 'uGrain');
  uFin_Dither   := glGetUniformLocation(ProgFinishing, 'uDither');
  uFin_Time     := glGetUniformLocation(ProgFinishing, 'uTime');

  uFXAA_Tex        := glGetUniformLocation(ProgFXAA, 'uTex');
  uFXAA_InvRes     := glGetUniformLocation(ProgFXAA, 'uInvRes');
  uFXAA_Scene      := glGetUniformLocation(ProgFXAA, 'uScene');
  uFXAA_Pre2D      := glGetUniformLocation(ProgFXAA, 'uPre2D');
  uFXAA_Pre2DValid := glGetUniformLocation(ProgFXAA, 'uPre2DValid');
  uFXAA_MaskThresh := glGetUniformLocation(ProgFXAA, 'uMaskThresh');

  uCopy_Tex        := glGetUniformLocation(ProgCopy, 'uTex');
  uCopy_Scene      := glGetUniformLocation(ProgCopy, 'uScene');
  uCopy_Pre2D      := glGetUniformLocation(ProgCopy, 'uPre2D');
  uCopy_Pre2DValid := glGetUniformLocation(ProgCopy, 'uPre2DValid');
  uCopy_MaskThresh := glGetUniformLocation(ProgCopy, 'uMaskThresh');

  uSSAO_Depth     := glGetUniformLocation(ProgSSAO, 'uDepth');
  uSSAO_InvRes    := glGetUniformLocation(ProgSSAO, 'uInvRes');
  uSSAO_Radius    := glGetUniformLocation(ProgSSAO, 'uRadius');
  uSSAO_Intensity := glGetUniformLocation(ProgSSAO, 'uIntensity');
  uSSAO_Bias      := glGetUniformLocation(ProgSSAO, 'uBias');
  uSSAO_ZNear     := glGetUniformLocation(ProgSSAO, 'uZNear');
  uSSAO_ZFar      := glGetUniformLocation(ProgSSAO, 'uZFar');

  uDOF_Tex      := glGetUniformLocation(ProgDOF, 'uTex');
  uDOF_Depth    := glGetUniformLocation(ProgDOF, 'uDepth');
  uDOF_InvRes   := glGetUniformLocation(ProgDOF, 'uInvRes');
  uDOF_Focus    := glGetUniformLocation(ProgDOF, 'uFocus');
  uDOF_Range    := glGetUniformLocation(ProgDOF, 'uRange');
  uDOF_Aperture := glGetUniformLocation(ProgDOF, 'uAperture');
  uDOF_ZNear    := glGetUniformLocation(ProgDOF, 'uZNear');
  uDOF_ZFar     := glGetUniformLocation(ProgDOF, 'uZFar');

  Result := True;
end;

procedure DestroyPrograms;
begin
  if ProgBright     <> 0 then begin glDeleteProgram(ProgBright);     ProgBright     := 0; end;
  if ProgDownsample <> 0 then begin glDeleteProgram(ProgDownsample); ProgDownsample := 0; end;
  if ProgUpsample   <> 0 then begin glDeleteProgram(ProgUpsample);   ProgUpsample   := 0; end;
  if ProgTonemap    <> 0 then begin glDeleteProgram(ProgTonemap);    ProgTonemap    := 0; end;
  if ProgCAS        <> 0 then begin glDeleteProgram(ProgCAS);        ProgCAS        := 0; end;
  if ProgCA         <> 0 then begin glDeleteProgram(ProgCA);         ProgCA         := 0; end;
  if ProgFinishing  <> 0 then begin glDeleteProgram(ProgFinishing);  ProgFinishing  := 0; end;
  if ProgFXAA       <> 0 then begin glDeleteProgram(ProgFXAA);       ProgFXAA       := 0; end;
  if ProgCopy       <> 0 then begin glDeleteProgram(ProgCopy);       ProgCopy       := 0; end;
  if ProgSSAO       <> 0 then begin glDeleteProgram(ProgSSAO);       ProgSSAO       := 0; end;
  if ProgDOF        <> 0 then begin glDeleteProgram(ProgDOF);        ProgDOF        := 0; end;
end;

procedure CreateBloomMips(W, H: Integer);
var
  i, mw, mh: Integer;
begin
  mw := W div 2;
  mh := H div 2;
  NumBloomMips := 0;
  for i := 0 to MAX_BLOOM_MIPS - 1 do
  begin
    if (mw < MIN_MIP_SIZE) or (mh < MIN_MIP_SIZE) then Break;
    BloomMips[i].W := mw;
    BloomMips[i].H := mh;
    BloomMips[i].Tex := MakeColorTex(mw, mh);
    BloomMips[i].FBO := MakeFBO(BloomMips[i].Tex, 'bloom_mip_' + IntToStr(i));
    if BloomMips[i].FBO = 0 then
    begin
      // Failed; fall back to whatever mips we already created.
      glDeleteTextures(1, @BloomMips[i].Tex);
      BloomMips[i].Tex := 0;
      Break;
    end;
    Inc(NumBloomMips);
    mw := Max(MIN_MIP_SIZE, mw div 2);
    mh := Max(MIN_MIP_SIZE, mh div 2);
  end;
end;

procedure DestroyBloomMips;
var
  i: Integer;
begin
  for i := 0 to MAX_BLOOM_MIPS - 1 do
  begin
    if BloomMips[i].FBO <> 0 then begin glDeleteFramebuffers(1, @BloomMips[i].FBO); BloomMips[i].FBO := 0; end;
    if BloomMips[i].Tex <> 0 then begin glDeleteTextures(1, @BloomMips[i].Tex);     BloomMips[i].Tex := 0; end;
    BloomMips[i].W := 0;
    BloomMips[i].H := 0;
  end;
  NumBloomMips := 0;
end;

procedure CreateResources(W, H: Integer);
begin
  FullW := W;
  FullH := H;
  HalfW := Max(2, W div 2);
  HalfH := Max(2, H div 2);

  SceneTex := MakeColorTex(W, H);
  Pre2DTex := MakeColorTex(W, H);
  PingTex  := MakeColorTex(W, H);
  PongTex  := MakeColorTex(W, H);
  PingFBO  := MakeFBO(PingTex, 'ping');
  PongFBO  := MakeFBO(PongTex, 'pong');
  CreateBloomMips(W, H);

  // Depth-aware extras. If FBO creation fails (e.g. driver refuses depth
  // format), DepthCaptureActive stays false and depth-based passes will
  // be skipped at runtime — the colour pipeline keeps working.
  DepthCaptureActive := False;
  if InitDepthCaptureEnable then
  begin
    DepthTex := MakeDepthTex(W, H);
    SSAOTex  := MakeColorTex(HalfW, HalfH);
    SSAOFBO  := MakeFBO(SSAOTex, 'ssao');
    if (DepthTex <> 0) and (SSAOFBO <> 0) then
      DepthCaptureActive := True
    else
    begin
      AddToLogFile(POSTFX_LOG, 'PostFX: depth/SSAO resources unavailable, depth-based effects disabled');
      if DepthTex <> 0 then begin glDeleteTextures(1, @DepthTex); DepthTex := 0; end;
      if SSAOFBO <> 0 then begin glDeleteFramebuffers(1, @SSAOFBO); SSAOFBO := 0; end;
      if SSAOTex <> 0 then begin glDeleteTextures(1, @SSAOTex); SSAOTex := 0; end;
    end;
  end;
end;

procedure DestroyResources;
begin
  if PingFBO  <> 0 then begin glDeleteFramebuffers(1, @PingFBO); PingFBO := 0; end;
  if PongFBO  <> 0 then begin glDeleteFramebuffers(1, @PongFBO); PongFBO := 0; end;
  if SceneTex <> 0 then begin glDeleteTextures(1, @SceneTex);    SceneTex := 0; end;
  if Pre2DTex <> 0 then begin glDeleteTextures(1, @Pre2DTex);    Pre2DTex := 0; end;
  if PingTex  <> 0 then begin glDeleteTextures(1, @PingTex);     PingTex := 0; end;
  if PongTex  <> 0 then begin glDeleteTextures(1, @PongTex);     PongTex := 0; end;
  if SSAOFBO  <> 0 then begin glDeleteFramebuffers(1, @SSAOFBO); SSAOFBO := 0; end;
  if SSAOTex  <> 0 then begin glDeleteTextures(1, @SSAOTex);     SSAOTex := 0; end;
  if DepthTex <> 0 then begin glDeleteTextures(1, @DepthTex);    DepthTex := 0; end;
  DepthCaptureActive := False;
  Pre2DCaptured := False;
  DestroyBloomMips;
end;

//----------------------------------------------------------------------------//
//                                 INIT                                       //
//----------------------------------------------------------------------------//

function HavePostFXEntryPoints: Boolean;
begin
  Result := Assigned(glCreateShader)     and
            Assigned(glShaderSource)     and
            Assigned(glCompileShader)    and
            Assigned(glCreateProgram)    and
            Assigned(glAttachShader)     and
            Assigned(glLinkProgram)      and
            Assigned(glUseProgram)       and
            Assigned(glGetUniformLocation) and
            Assigned(glUniform1i)        and
            Assigned(glUniform1f)        and
            Assigned(glUniform2f)        and
            Assigned(glActiveTexture)    and
            Assigned(glGenFramebuffers)  and
            Assigned(glBindFramebuffer)  and
            Assigned(glFramebufferTexture2D) and
            Assigned(glCheckFramebufferStatus) and
            Assigned(glDeleteFramebuffers);
end;

procedure InitPostFX(W, H: Integer);
begin
  PostFXReady := False;
  if (W <= 0) or (H <= 0) then Exit;

  AddToLogFile(POSTFX_LOG,
    'PostFX: init request ' + IntToStr(W) + 'x' + IntToStr(H));

  if not HavePostFXEntryPoints then
  begin
    AddToLogFile(POSTFX_LOG,
      'PostFX: required GL entry points missing — disabled.');
    Exit;
  end;

  if not CompilePrograms then
  begin
    AddToLogFile(POSTFX_LOG,
      'PostFX: shader compilation failed — disabled.');
    DestroyPrograms;
    Exit;
  end;

  CreateResources(W, H);
  if (PingFBO = 0) or (PongFBO = 0) or (SceneTex = 0) or (NumBloomMips < 2) then
  begin
    AddToLogFile(POSTFX_LOG,
      'PostFX: resource creation failed (NumBloomMips=' +
      IntToStr(NumBloomMips) + ') — disabled.');
    DestroyResources;
    DestroyPrograms;
    Exit;
  end;

  PostFXReady := True;
  // Register 2D-mask hooks now that everything is up. DrawFunc2D and
  // EngineCore call through these procedure variables and tolerate them
  // being nil, so registering only when PostFX is ready keeps fallback
  // paths simple (legacy Bloom/FXAA, modern context unavailable, etc.).
  PostFX_OnBegin2D    := MarkBefore2D;
  PostFX_OnBeginFrame := BeginPostFXFrame;

  if UsingModernContext then
    AddToLogFile(POSTFX_LOG,
      'PostFX: enabled at ' + IntToStr(W) + 'x' + IntToStr(H) +
      ', bloom_mips=' + IntToStr(NumBloomMips) + ', context=modern')
  else
    AddToLogFile(POSTFX_LOG,
      'PostFX: enabled at ' + IntToStr(W) + 'x' + IntToStr(H) +
      ', bloom_mips=' + IntToStr(NumBloomMips) + ', context=legacy');
end;

procedure ShutdownPostFX;
begin
  // Unhook callbacks before tearing down so a late draw can't reach into
  // freed GL resources.
  PostFX_OnBegin2D    := nil;
  PostFX_OnBeginFrame := nil;
  DestroyResources;
  DestroyPrograms;
  PostFXReady := False;
end;

procedure ResizePostFX(W, H: Integer);
begin
  if not PostFXReady then Exit;
  if (W <= 0) or (H <= 0) then Exit;
  if (W = FullW) and (H = FullH) then Exit;
  DestroyResources;
  CreateResources(W, H);
  if (PingFBO = 0) or (PongFBO = 0) or (NumBloomMips < 2) then
  begin
    PostFXReady := False;
    AddToLogFile(POSTFX_LOG, 'PostFX: resize failed — disabled.');
  end;
end;

//----------------------------------------------------------------------------//
//                              RENDER GRAPH                                  //
//----------------------------------------------------------------------------//

procedure SaveGLState;
begin
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
end;

procedure RestoreGLState;
begin
  glPopMatrix;
  glMatrixMode(GL_PROJECTION); glPopMatrix;
  glMatrixMode(GL_MODELVIEW);

  glUseProgram(0);
  // Unbind every texture unit we might have touched so the engine doesn't
  // see stale bindings on its next draw.
  glActiveTexture(GL_TEXTURE3); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE2); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE1); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE0); glBindTexture(GL_TEXTURE_2D, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  glPopClientAttrib;
  glPopAttrib;
end;

procedure SwapPingPong;
var
  TmpTex: GLuint;
  TmpFBO: GLuint;
begin
  // After a pass that wrote to CurWriteFBO/CurWriteTex, that texture now
  // holds the latest output. Make it the new read source and swap targets.
  CurReadTex := CurWriteTex;
  if CurWriteFBO = PingFBO then
  begin
    TmpTex := PongTex; TmpFBO := PongFBO;
  end
  else
  begin
    TmpTex := PingTex; TmpFBO := PingFBO;
  end;
  CurWriteTex := TmpTex;
  CurWriteFBO := TmpFBO;
end;

procedure PassSnapshotScene;
begin
  // Copy default backbuffer (which contains scene + 2D HUD + booster UI)
  // into SceneTex. Driver resolves MSAA during the copy.
  glBindTexture(GL_TEXTURE_2D, SceneTex);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, FullW, FullH);
end;

procedure PassSnapshotDepth;
begin
  // Copy default FB depth into DepthTex. With MSAA the driver may give us
  // single-sample resolved depth or refuse — we don't validate per-pixel,
  // we simply read it back and let SSAO/DOF/Fog tolerate noise.
  if (not DepthCaptureActive) or (DepthTex = 0) then Exit;
  glBindTexture(GL_TEXTURE_2D, DepthTex);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, FullW, FullH);
end;

procedure PassSSAO;
begin
  if not InitSSAOEnable then Exit;
  if not DepthCaptureActive then Exit;
  if SSAOFBO = 0 then Exit;

  glBindFramebuffer(GL_FRAMEBUFFER, SSAOFBO);
  glViewport(0, 0, HalfW, HalfH);
  glDisable(GL_BLEND);
  glUseProgram(ProgSSAO);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, DepthTex);
  glUniform1i(uSSAO_Depth, 0);
  glUniform2f(uSSAO_InvRes, 1.0 / FullW, 1.0 / FullH);
  glUniform1f(uSSAO_Radius,    InitSSAORadius);
  glUniform1f(uSSAO_Intensity, InitSSAOIntensity);
  glUniform1f(uSSAO_Bias,      InitSSAOBias);
  glUniform1f(uSSAO_ZNear,     InitZNear);
  glUniform1f(uSSAO_ZFar,      InitZFar);
  DrawFullscreenQuad;
end;

procedure PassBloom;
var
  i: Integer;
  Threshold, Knee: Single;
begin
  if NumBloomMips < 2 then Exit;

  Threshold := InitBloomThreshold;
  Knee      := InitBloomKnee;
  if Threshold < 0.0 then Threshold := 0.0;
  if Knee < 1e-3 then Knee := 1e-3;

  // 1) Bright-pass: SceneTex (full-res) -> BloomMips[0] (1/2)
  glBindFramebuffer(GL_FRAMEBUFFER, BloomMips[0].FBO);
  glViewport(0, 0, BloomMips[0].W, BloomMips[0].H);
  glDisable(GL_BLEND);
  glUseProgram(ProgBright);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, SceneTex);
  glUniform1i(uBright_Tex, 0);
  glUniform2f(uBright_InvSrc, 1.0 / FullW, 1.0 / FullH);
  glUniform1f(uBright_Threshold, Threshold);
  glUniform1f(uBright_Knee, Knee);
  DrawFullscreenQuad;

  // 2) Progressive downsample: BloomMips[i-1] -> BloomMips[i]
  glUseProgram(ProgDownsample);
  for i := 1 to NumBloomMips - 1 do
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, BloomMips[i].FBO);
    glViewport(0, 0, BloomMips[i].W, BloomMips[i].H);
    glBindTexture(GL_TEXTURE_2D, BloomMips[i - 1].Tex);
    glUniform1i(uDS_Tex, 0);
    glUniform2f(uDS_InvSrc,
                1.0 / BloomMips[i - 1].W, 1.0 / BloomMips[i - 1].H);
    DrawFullscreenQuad;
  end;

  // 3) Progressive upsample with additive blend.
  //    BloomMips[i+1] tent-upsampled additively into BloomMips[i].
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE);
  glUseProgram(ProgUpsample);
  for i := NumBloomMips - 2 downto 0 do
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, BloomMips[i].FBO);
    glViewport(0, 0, BloomMips[i].W, BloomMips[i].H);
    glBindTexture(GL_TEXTURE_2D, BloomMips[i + 1].Tex);
    glUniform1i(uUS_Tex, 0);
    glUniform2f(uUS_InvSrc,
                1.0 / BloomMips[i + 1].W, 1.0 / BloomMips[i + 1].H);
    glUniform1f(uUS_Radius, InitBloomRadius);
    DrawFullscreenQuad;
  end;
  glDisable(GL_BLEND);
end;

procedure PassTonemap;
var
  BloomEn, TonemapEn, SSAOEn, FogEn: Single;
begin
  // Composition + tonemap pass. Reads four textures:
  //   * uScene  (TEX0) = full-res scene snapshot
  //   * uBloom  (TEX1) = bloom mip[0] (or scene as dummy if disabled)
  //   * uSSAO   (TEX2) = half-res AO factor (or scene as dummy if disabled)
  //   * uDepth  (TEX3) = depth texture (or scene as dummy if disabled)
  // Writes to PingFBO. Subsequent passes pick up CurReadTex=PingTex.
  glBindFramebuffer(GL_FRAMEBUFFER, PingFBO);
  glViewport(0, 0, FullW, FullH);
  glDisable(GL_BLEND);

  glUseProgram(ProgTonemap);

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, SceneTex);
  glUniform1i(uTM_Scene, 0);

  glActiveTexture(GL_TEXTURE1);
  if (NumBloomMips >= 2) and InitBloomEnable then
    glBindTexture(GL_TEXTURE_2D, BloomMips[0].Tex)
  else
    glBindTexture(GL_TEXTURE_2D, SceneTex);
  glUniform1i(uTM_Bloom, 1);

  glActiveTexture(GL_TEXTURE2);
  if DepthCaptureActive and InitSSAOEnable and (SSAOTex <> 0) then
    glBindTexture(GL_TEXTURE_2D, SSAOTex)
  else
    glBindTexture(GL_TEXTURE_2D, SceneTex);
  glUniform1i(uTM_SSAO, 2);

  glActiveTexture(GL_TEXTURE3);
  if DepthCaptureActive and (DepthTex <> 0) then
    glBindTexture(GL_TEXTURE_2D, DepthTex)
  else
    glBindTexture(GL_TEXTURE_2D, SceneTex);
  glUniform1i(uTM_Depth, 3);

  glUniform1f(uTM_Exposure, InitExposure);
  glUniform1f(uTM_BloomI,   InitBloomIntensity);

  if InitBloomEnable and (NumBloomMips >= 2) then BloomEn := 1.0 else BloomEn := 0.0;
  if InitTonemapEnable                         then TonemapEn := 1.0 else TonemapEn := 0.0;
  if InitSSAOEnable and DepthCaptureActive    then SSAOEn := 1.0 else SSAOEn := 0.0;
  if InitFogEnable and DepthCaptureActive     then FogEn := 1.0 else FogEn := 0.0;

  glUniform1f(uTM_BloomEnable,   BloomEn);
  glUniform1f(uTM_TonemapEnable, TonemapEn);
  glUniform1f(uTM_SSAOEnable,    SSAOEn);
  glUniform1f(uTM_SSAOIntensity, InitSSAOIntensity);
  glUniform1f(uTM_FogEnable,     FogEn);
  glUniform1f(uTM_FogDensity,    InitFogDensity);
  glUniform1f(uTM_FogStart,      InitFogStart);
  glUniform3f(uTM_FogColor,      InitFogColorR, InitFogColorG, InitFogColorB);
  glUniform1f(uTM_ZNear,         InitZNear);
  glUniform1f(uTM_ZFar,          InitZFar);

  DrawFullscreenQuad;

  // Reset the active texture unit; later passes only use TEX0.
  glActiveTexture(GL_TEXTURE3); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE2); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE1); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE0);

  // After tonemap: PingTex is the latest, PongFBO is the next write target.
  CurReadTex  := PingTex;
  CurWriteTex := PongTex;
  CurWriteFBO := PongFBO;
end;

procedure PassDOF;
begin
  if not InitDOFEnable then Exit;
  if not DepthCaptureActive then Exit;
  if InitDOFAperture <= 0.001 then Exit;

  glBindFramebuffer(GL_FRAMEBUFFER, CurWriteFBO);
  glViewport(0, 0, FullW, FullH);
  glDisable(GL_BLEND);
  glUseProgram(ProgDOF);

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, CurReadTex);
  glUniform1i(uDOF_Tex, 0);

  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, DepthTex);
  glUniform1i(uDOF_Depth, 1);

  glUniform2f(uDOF_InvRes,   1.0 / FullW, 1.0 / FullH);
  glUniform1f(uDOF_Focus,    InitDOFFocusDistance);
  glUniform1f(uDOF_Range,    InitDOFFocusRange);
  glUniform1f(uDOF_Aperture, InitDOFAperture);
  glUniform1f(uDOF_ZNear,    InitZNear);
  glUniform1f(uDOF_ZFar,     InitZFar);
  DrawFullscreenQuad;

  glActiveTexture(GL_TEXTURE1); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE0);
  SwapPingPong;
end;

procedure PassCAS;
begin
  if not InitSharpenEnable then Exit;
  if InitSharpenAmount <= 0.001 then Exit;

  glBindFramebuffer(GL_FRAMEBUFFER, CurWriteFBO);
  glViewport(0, 0, FullW, FullH);
  glUseProgram(ProgCAS);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, CurReadTex);
  glUniform1i(uCAS_Tex, 0);
  glUniform2f(uCAS_InvRes, 1.0 / FullW, 1.0 / FullH);
  glUniform1f(uCAS_Sharpness, InitSharpenAmount);
  DrawFullscreenQuad;
  SwapPingPong;
end;

procedure PassChromAb;
begin
  if not InitChromAbEnable then Exit;
  if InitChromAbAmount <= 0.001 then Exit;

  glBindFramebuffer(GL_FRAMEBUFFER, CurWriteFBO);
  glViewport(0, 0, FullW, FullH);
  glUseProgram(ProgCA);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, CurReadTex);
  glUniform1i(uCA_Tex, 0);
  glUniform2f(uCA_InvRes, 1.0 / FullW, 1.0 / FullH);
  glUniform1f(uCA_Amount, InitChromAbAmount);
  DrawFullscreenQuad;
  SwapPingPong;
end;

procedure PassFinishing;
var
  Vig, Grain, Dither: Single;
  TimeF: Single;
begin
  if InitVignetteEnable then Vig := InitVignetteAmount else Vig := 0.0;
  if InitGrainEnable    then Grain := InitGrainAmount  else Grain := 0.0;
  if InitDitherEnable   then Dither := 1.0 else Dither := 0.0;

  // Always run if at least one finishing effect is active to take advantage
  // of the same pass; if all three are off, skip entirely.
  if (Vig <= 0.001) and (Grain <= 0.001) and (Dither <= 0.001) then Exit;

  TimeF := (FrameCounter mod 65536) * 0.0001;

  glBindFramebuffer(GL_FRAMEBUFFER, CurWriteFBO);
  glViewport(0, 0, FullW, FullH);
  glUseProgram(ProgFinishing);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, CurReadTex);
  glUniform1i(uFin_Tex, 0);
  glUniform2f(uFin_Res, FullW, FullH);
  glUniform1f(uFin_Vignette, Vig);
  glUniform1f(uFin_Grain, Grain);
  glUniform1f(uFin_Dither, Dither);
  glUniform1f(uFin_Time, TimeF);
  DrawFullscreenQuad;
  SwapPingPong;
end;

procedure PassPresentFXAAOrCopy;
var
  Pre2DValid: Single;
begin
  // Final pass: writes CurReadTex back to the default framebuffer.
  // The shader also receives uScene + uPre2D so it can detect 2D pixels
  // (HUD, gauges, custom on-screen text drawn through DrawFunc2D) and
  // bypass the post-process for them, leaving them pixel-perfect crisp.
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glViewport(0, 0, FullW, FullH);
  glDisable(GL_BLEND);

  if Pre2DCaptured and (Pre2DTex <> 0) then Pre2DValid := 1.0 else Pre2DValid := 0.0;

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, CurReadTex);

  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, SceneTex);

  glActiveTexture(GL_TEXTURE2);
  if Pre2DCaptured and (Pre2DTex <> 0) then
    glBindTexture(GL_TEXTURE_2D, Pre2DTex)
  else
    glBindTexture(GL_TEXTURE_2D, SceneTex);  // dummy, weight is 0

  if InitFXAAEnable then
  begin
    glUseProgram(ProgFXAA);
    glUniform1i(uFXAA_Tex,        0);
    glUniform1i(uFXAA_Scene,      1);
    glUniform1i(uFXAA_Pre2D,      2);
    glUniform1f(uFXAA_Pre2DValid, Pre2DValid);
    glUniform1f(uFXAA_MaskThresh, 0.01);
    glUniform2f(uFXAA_InvRes,     1.0 / FullW, 1.0 / FullH);
  end
  else
  begin
    glUseProgram(ProgCopy);
    glUniform1i(uCopy_Tex,        0);
    glUniform1i(uCopy_Scene,      1);
    glUniform1i(uCopy_Pre2D,      2);
    glUniform1f(uCopy_Pre2DValid, Pre2DValid);
    glUniform1f(uCopy_MaskThresh, 0.01);
  end;

  DrawFullscreenQuad;

  // Clean up extra texture bindings (TEX0 reset is part of RestoreGLState).
  glActiveTexture(GL_TEXTURE2); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE1); glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE0);
end;

// === 2D-mask hooks ===
//
// BeginPostFXFrame is called once per frame from EngineMainDraw, before any
// drawing happens. It clears the per-frame Pre2D snapshot flag.
//
// MarkBefore2D is called from DrawFunc2D.Begin2D. The first time per frame
// it runs we copy the current backbuffer into Pre2DTex — that's our "scene
// without any 2D HUD" reference. Subsequent calls in the same frame are
// guarded by Pre2DCaptured and become no-ops.
procedure BeginPostFXFrame;
begin
  Pre2DCaptured := False;
end;

procedure MarkBefore2D;
begin
  if Pre2DCaptured then Exit;
  if not PostFXReady then Exit;
  if not InitPostFXEnable then Exit;
  if Pre2DTex = 0 then Exit;

  // Snapshot needs to honour the current GL state cleanly. The engine will
  // be calling us with whatever 2D-prep state Begin2D set up; we do a
  // minimal save / copy / restore around the texture bind.
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Pre2DTex);
  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, FullW, FullH);
  glBindTexture(GL_TEXTURE_2D, 0);
  Pre2DCaptured := True;
end;

procedure ApplyPostFX;
begin
  // If our pipeline isn't ready, OR the user explicitly disabled PostFX,
  // fall back to the legacy Bloom + FXAA modules. Each respects its own
  // Init*Enable flag, so disabling PostFX still leaves the user with the
  // simple bloom + AA they had before — never a worse picture than v1.x.
  if (not PostFXReady) or (not InitPostFXEnable) then
  begin
    ApplyBloom;
    EndFXAAFrame;
    Exit;
  end;

  Inc(FrameCounter);

  SaveGLState;
  try
    PassSnapshotScene;
    PassSnapshotDepth;
    if InitSSAOEnable then PassSSAO;
    if InitBloomEnable then PassBloom;
    PassTonemap;
    PassDOF;
    PassCAS;
    PassChromAb;
    PassFinishing;
    PassPresentFXAAOrCopy;
  finally
    RestoreGLState;
  end;
end;

function YN(B: Boolean): string;
begin
  if B then Result := 'True' else Result := 'False';
end;

procedure DiagnosePostFX;
var
  i: Integer;
begin
  AddToLogFile(POSTFX_LOG, '--- PostFX diagnostics ---');
  AddToLogFile(POSTFX_LOG, 'PostFXReady       = ' + YN(PostFXReady));
  AddToLogFile(POSTFX_LOG, 'UsingModernContext= ' + YN(UsingModernContext));
  AddToLogFile(POSTFX_LOG, 'FullW x FullH     = ' + IntToStr(FullW) + 'x' + IntToStr(FullH));
  AddToLogFile(POSTFX_LOG, 'NumBloomMips      = ' + IntToStr(NumBloomMips));
  for i := 0 to NumBloomMips - 1 do
    AddToLogFile(POSTFX_LOG,
      '  mip[' + IntToStr(i) + '] = ' +
      IntToStr(BloomMips[i].W) + 'x' + IntToStr(BloomMips[i].H));
  AddToLogFile(POSTFX_LOG, 'InitPostFXEnable  = ' + YN(InitPostFXEnable));
  AddToLogFile(POSTFX_LOG, 'InitBloomEnable   = ' + YN(InitBloomEnable));
  AddToLogFile(POSTFX_LOG, 'InitTonemapEnable = ' + YN(InitTonemapEnable));
  AddToLogFile(POSTFX_LOG, 'InitFXAAEnable    = ' + YN(InitFXAAEnable));
  AddToLogFile(POSTFX_LOG, 'InitSharpenEnable = ' + YN(InitSharpenEnable));
  AddToLogFile(POSTFX_LOG, 'InitVignetteEnable= ' + YN(InitVignetteEnable));
  AddToLogFile(POSTFX_LOG, 'InitGrainEnable   = ' + YN(InitGrainEnable));
  AddToLogFile(POSTFX_LOG, 'InitDitherEnable  = ' + YN(InitDitherEnable));
  AddToLogFile(POSTFX_LOG, 'InitChromAbEnable = ' + YN(InitChromAbEnable));
  AddToLogFile(POSTFX_LOG, 'InitBloomThreshold= ' + FloatToStr(InitBloomThreshold));
  AddToLogFile(POSTFX_LOG, 'InitBloomIntensity= ' + FloatToStr(InitBloomIntensity));
  AddToLogFile(POSTFX_LOG, 'InitBloomKnee     = ' + FloatToStr(InitBloomKnee));
  AddToLogFile(POSTFX_LOG, 'InitBloomRadius   = ' + FloatToStr(InitBloomRadius));
  AddToLogFile(POSTFX_LOG, 'InitExposure      = ' + FloatToStr(InitExposure));
  AddToLogFile(POSTFX_LOG, 'DepthCaptureActive= ' + YN(DepthCaptureActive));
  AddToLogFile(POSTFX_LOG, 'InitSSAOEnable    = ' + YN(InitSSAOEnable));
  AddToLogFile(POSTFX_LOG, 'InitFogEnable     = ' + YN(InitFogEnable));
  AddToLogFile(POSTFX_LOG, 'InitDOFEnable     = ' + YN(InitDOFEnable));
  AddToLogFile(POSTFX_LOG, 'HalfW x HalfH     = ' + IntToStr(HalfW) + 'x' + IntToStr(HalfH));
  AddToLogFile(POSTFX_LOG, '--- end PostFX ---');
end;

initialization
  PostFXReady   := False;
  FrameCounter  := 0;
  NumBloomMips  := 0;
  Pre2DCaptured := False;

end.
