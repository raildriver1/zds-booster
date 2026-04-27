//----------------------------------------------------------------------------//
//DRON's OpenGl Engine V 1.0 http://dronprogs.mirgames.ru                     //
//                                                                            //
// Variables.pas V 1.0, 15.01.2006                                            //
//                                                                            //
// Simply all engine shared variables and types.                              //
//                                                                            //
// Copyright (C) 2005-2006 Korotkov Andrew aka DRON                           //
//                                                                            //
//This program is free software; you can redistribute it and/or               //
//modify it under the terms of the GNU General Public License                 //
//as published by the Free Software Foundation; either version 2              //
//of the License, or any later version.                                       //
//                                                                            //
//This program is distributed in the hope that it will be useful,             //
//but WITHOUT ANY WARRANTY; without even the implied warranty of              //
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               //
//GNU General Public License (http://www.gnu.org/copyleft/gpl.html)           //
//for more details.                                                           //
//----------------------------------------------------------------------------//
unit Variables;

interface
uses Windows,IniFile,OpenGL;

type

TVertex=record X, Y, Z: single; end;

TVertex4D=record X, Y, Z, W: single; end;

TTextureInfo = record
Index : Cardinal;
Width,Height : integer;
Detail, FileType : byte;
end;

TEngTimer = record
Active : boolean;
Interval,Tick : integer;
OnTimer : procedure;
end;

TBox = record
X,Y,W,H : cardinal;
end;

TFontHeader = record
version : byte;
Width,Height : integer;
Buks : array [0..223] of TBox;
end;

TDGLFont = record
Load : boolean;
Width,Height : integer;
Buks : array [0..223] of TBox;
Texture : GLUInt;
end;

TPlugin = record
Name : string;
Loaded : boolean;
Handle: THandle;
Init : procedure (DGLE_Handle : THandle; EngWinHandle : HWND; ProcessInterval : byte); stdcall;
Free : procedure; stdcall;
DrawPre : procedure; stdcall;
DrawPost : procedure; stdcall;
Process : procedure; stdcall;
OnMessage : procedure (var Msg : tagMSG); stdcall;
end;

const
  FPS_TIMER = 1;
  FPS_INTERVAL = 1000;
  PROCESS_TIMER = 2;

  VERSION : shortstring = 'v1.1 RC2,30.04.2008,21:12';

  ENGINE_LABEL = 'DGLEngine Version 1.1 RC2';

  EngineLog = 'DGLEngine_Log.txt';

var
  WND_TITLE : PAnsiChar = ENGINE_LABEL;
  IniFileName : string = 'Settings.ini';
  PROCESS_INTERVAL : byte = 20;
  h_Wnd  : HWND;
  h_DC   : HDC;
  h_RC   : HGLRC;
  ExtFPS : Integer;
  OpenGLInitialized : boolean;
  StartQuitingEngine : boolean = true;
  IsWriteLog : boolean = true;
  UseSettingsIni : boolean = true;

  MultyTexActive : boolean = false;
  DrawToPanel : boolean = FALSE;
  ShowLogo : boolean = TRUE;
  INITStencil : boolean = FALSE;
  InitFullscreen : boolean =  true;
  _TextureCompression : boolean = false;
  _TextureFiltering : boolean = true;
  _AllowAutoPause : boolean = true;
  _UseVBO         : boolean = true;
  _UseFBO         : boolean = true;
  _SceneDontUseMat: boolean = false;
  _frustumcalculated: boolean =false;
  //_NormCubemap : cardinal = 0;  
  MipMapping : boolean = false;
  InitResX : cardinal = 2000;
  InitResY : cardinal = 600;
  InitPDepth : integer = 32;
  InitVsync : boolean = false;
  InitFrequency : integer = 60;
  InitZNear : single = 0.5;
  InitZfar : single = 2200.0;
  initAngle : single = 45.0;
  InitZBuffer : byte = 24;

  // ZDS-Booster: modern GL / quality settings
  InitMSAASamples : integer = 4;        // 0 = off, 2/4/8/16 = MSAA level requested at context creation
  InitAnisoLevel  : integer = 16;       // 0 = off, else anisotropic filtering level requested
  InitFXAAEnable  : boolean = true;     // post-process FXAA antialiasing
  InitBloomEnable : boolean = true;     // post-process bloom
  InitBloomStrength : single = 0.85;    // legacy: bloom additive gain (Bloom.pas)
  InitBloomThreshold : single = 0.75;   // luma threshold to contribute to bloom
  InitLODBias     : single = -0.5;      // negative = sharper mips at distance
  ActualGLMajor   : integer = 0;        // filled after context creation
  ActualGLMinor   : integer = 0;
  ActualMSAASamples : integer = 0;      // 0 if MSAA not obtained
  ActualMaxAniso  : single  = 1.0;      // 1.0 means no anisotropy
  UsingModernContext : boolean = false; // true if wglCreateContextAttribsARB succeeded
  FXAAActive       : boolean = false;   // true after successful FXAA init
  BloomActive      : boolean = false;   // true after successful bloom init

  // ZDS-Booster: unified PostFX pipeline (PostFX.pas).
  // Replaces the older Bloom.pas + FXAA.pas chain with a single render-graph
  // covering bloom -> tonemap -> sharpen -> chromAb -> finishing -> FXAA.
  // Each toggle is read every frame so CheatMenu can flip it live.
  InitPostFXEnable    : boolean = true;     // master switch for the whole PostFX chain
  PostFXActive        : boolean = false;    // true after successful PostFX init

  // HDR-style bloom (PostFX) — much wider dynamic range than legacy Bloom.pas.
  // The pyramid spreads light over ~40-50px at 1080p with these defaults.
  InitBloomKnee       : single  = 0.5;      // soft-knee width below threshold
  InitBloomRadius     : single  = 1.0;      // upsample tent radius (pixels at sample mip)
  InitBloomIntensity  : single  = 0.4;      // 0..2 — how strongly bloom is mixed in tonemap

  // ACES Filmic tonemapping + manual exposure
  InitTonemapEnable   : boolean = true;     // ACES Filmic curve + sRGB roundtrip
  InitExposure        : single  = 1.0;      // EV gain before tonemap; 1.0 = neutral

  // Finishing pass (PostFX): vignette + film grain + ordered dither
  InitVignetteEnable  : boolean = true;
  InitVignetteAmount  : single  = 0.35;     // 0..1 — corner falloff strength
  InitGrainEnable     : boolean = true;
  InitGrainAmount     : single  = 0.04;     // 0..0.2 — luminance noise amplitude
  InitDitherEnable    : boolean = true;     // ordered dither breaks 8-bit banding

  // CAS sharpening (PostFX)
  InitSharpenEnable   : boolean = true;
  InitSharpenAmount   : single  = 0.30;     // 0..1 — over-sharpening starts ~0.6

  // Chromatic aberration (PostFX) — off by default; sim-style accents only.
  InitChromAbEnable   : boolean = false;
  InitChromAbAmount   : single  = 1.5;      // pixels of separation at the corner

  // System time override: when on, every frame we overwrite the simulator's
  // AbsoluteTime double (heap, pointer @ Launcher.exe+$34AEF0) with the
  // current Windows local time. Off by default — the sim's own clock keeps
  // running unless the user explicitly opts in via the WORLD-tab toggle.
  InitSystemTimeEnable : boolean = false;

  // 2D-mask callbacks. Set by PostFX.pas in InitPostFX so that low-level
  // modules (DrawFunc2D) can notify PostFX about frame boundaries and the
  // first 2D draw call without DrawFunc2D having to "use" PostFX directly.
  // Both stay nil while PostFX is uninitialised — callers always check.
  PostFX_OnBegin2D       : procedure = nil;
  PostFX_OnBeginFrame    : procedure = nil;

  // Depth-based effects (PostFX, Phase 2). The depth texture is captured
  // from the default FB after the engine + HUD have rendered. SSAO/Fog/DOF
  // all read it. Disable this if the captured depth is invalid (e.g. MSAA
  // resolve fails on a particular driver).
  InitDepthCaptureEnable : boolean = true;
  DepthCaptureActive     : boolean = false;  // set true if init succeeded

  // SSAO — screen-space ambient occlusion. Multiplies AO factor into the
  // scene during the tonemap composite. Half-resolution for performance.
  InitSSAOEnable      : boolean = true;
  InitSSAORadius      : single  = 0.6;      // sample radius in linear-depth units
  InitSSAOIntensity   : single  = 1.0;      // 0..2 — how strongly AO darkens
  InitSSAOBias        : single  = 0.025;    // small bias to avoid self-occlusion

  // Volumetric fog — exponential depth-based fog with optional sun shaft.
  // The simulator already has its own fog, so this is OFF by default.
  // Turn on for atmospheric / cinematic look.
  InitFogEnable       : boolean = false;
  InitFogDensity      : single  = 0.0008;   // exp falloff per linear-depth unit
  InitFogStart        : single  = 50.0;     // distance where fog begins
  InitFogColorR       : single  = 0.65;
  InitFogColorG       : single  = 0.70;
  InitFogColorB       : single  = 0.78;

  // DOF — depth of field. Off by default (best for cinematic shots / replays).
  // FocusDistance is in linear depth units; FocusRange is the in-focus zone.
  InitDOFEnable       : boolean = false;
  InitDOFFocusDistance: single  = 6.0;
  InitDOFFocusRange   : single  = 3.0;
  InitDOFAperture     : single  = 6.0;      // max blur radius in pixels at infinity

  WinX : integer = 0;
  WinY : integer = 0;
  CurW,CurH : integer;

  MenuFreecamBaseSpeed: Single = 1.0;
  MenuFreecamFastSpeed: Single = 2.0;
  MenuFreecamTurnSpeed: Single = 1.5;

  stepforward: Single = 1.5;
  maxvisibledistance: Single = 1600;

  newsky : boolean = false;

  Config_Freecam: Boolean = False;     // ���������� ���������� ��������� �������
  Config_MainCamera: Boolean = False;
  Config_MaxDistance: Boolean = False;
  Config_NewSky: Boolean = False;

  MenuIsChangingSettings: Boolean = False;

  TexturesInfo : array of TTextureInfo;

  EngTimers : array of TEngTimer;

  DGLFonts : array of TDGLFont;

  Plugins : array of TPlugin;

  glDraw, ProcessGame, LoadTextures, DestroyAll: procedure;

implementation
end.
