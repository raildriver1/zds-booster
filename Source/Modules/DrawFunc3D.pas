//----------------------------------------------------------------------------//
//DRON's OpenGl Engine V 1.0 http://dronprogs.mirgames.ru                     //
//                                                                            //
// GLDrawFunc3D.pas V 1.1, 13.04.2008                                         //
//                                                                            //
// This module provides all basic 3D graphics routines.                       //
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
unit DrawFunc3D;
interface
uses OpenGL, Variables, Windows, TFrustumClass, EngineUtils, SysUtils, DMD_MultyMesh,
     Textures, DPC_Packages, Classes, KlubData, KlubProcessor, Math, TlHelp32, MMSystem, IniFiles, Sound, BilServer,
     BoosterLocomotiveRegistry, LocomotiveHookRegistry, BoosterLocomotiveIdentity, BoosterWorld,
     BoosterConfigText, BoosterMemory, BoosterExtended;


type TVertex3D = record X,Y,Z : single; Color, Alpha : integer; TexX, TexY : single; end;

     TCamera = record
          Eye    : TVertex;
          Center : TVertex;
          end;

     TObj3DInfo = record
          Texture : cardinal;
          Color : Array [1..4] of GLFloat;
          Projecting : boolean;
          end;

      TSceneMesh = record
          Active      : boolean;
          Name        : string[128];
          Pos         : Tvertex;
          Scale       : single;
          Material    : TMaterial;
          Texture     : cardinal;
          DoBump      : boolean;
          BumpTexture : cardinal;
          SpecTexture : cardinal;
          MeshFrame   : cardinal;
          MeshSmooth  : boolean;
          Mesh        : cardinal;
          end;

      TMeshGeometry = record
          VerticesCount, FacesCount : cardinal;
          Vertices, Normals : array of TVertex;
          Tangents : array of TTangent;
          Faces : array of array[0..2] of cardinal;
          TextureVertices : array of TVertex;
          TextureFaces : array of array[0..2] of cardinal;
          end;
      PMeshGeometry = ^TMeshGeometry;

procedure BeginObj3D; stdcall;
procedure EndObj3D; stdcall;
procedure Position3D(X,Y,Z : single); stdcall;
procedure Position2D(X,Y : integer); stdcall;
procedure SetTexture(Texture : gluint); stdcall;
procedure Color3D(Color:integer; Alpha : byte; Diffuse : boolean; MaterialShininess : single); stdcall;
procedure AdductingMatrix3Dto2D; stdcall;
procedure ReturnStandartMatrix3D; stdcall;
procedure DrawAxes(Length : single = 1.0); stdcall;
procedure RotateX(Angle : single); stdcall;
procedure RotateY(Angle : single); stdcall;
procedure RotateZ(Angle : single); stdcall;
procedure Scale3D(Scale : single); stdcall;
procedure DrawPlane(Width,Height : single); stdcall;
procedure DrawSphere(Radius : single); stdcall;
procedure DrawLine(X,Y,Z,X1,Y1,Z1 : single; LineWidth : real = 1.0; Smooth : boolean = true); stdcall;
procedure DrawPoint(X,Y,Z : single); stdcall;
function  LoadModel(Filename : string; ScaleType : byte; NormalInv : boolean) : integer; stdcall;
procedure FreeModel(ModelIdent : integer); stdcall;
procedure DrawModel(ModelIdent, Frame : integer; Smooth : boolean); stdcall;
procedure GetModelGeometry(ModelIdent, Frame : integer; GeometryData : PMeshGeometry); stdcall;
procedure EnableSphereMapping; stdcall;
procedure DisableSphereMapping; stdcall;
procedure SetLight(ID : integer; X,Y,Z : single; LightColor : integer; Radius : single; Visualize : boolean; Scale : single); stdcall;
procedure DrawEllipse(Width,Height,Depth : single); stdcall;
function  CreateTextureToRenderIn(TextureWidth,TextureHeight : integer):GlUint; stdcall;
procedure StartRenderToTexture(Texture : GlUint); stdcall;
procedure EndRenderToTexture; stdcall;
procedure DrawSprite(Width,Height : single; FramesXCount, FramesYCount, FrameNumber: integer);stdcall;
procedure DrawCylinder(Radius,Height : single); stdcall;
procedure DrawPolygon3D(points : array of TVertex3D); stdcall;
function  ModelFramesCount(Modelident : integer):Integer; stdcall;
procedure DeactiveLight(ID : integer); stdcall;
function  ModelBoundingBox(Modelident,Frame : integer):TVertex; stdcall;
function  ModelTrianglesCount(Modelident,Frame : integer) : Cardinal; stdcall;
procedure DrawSprite_BillBoard(Width,Height : single; FramesXCount, FramesYCount, FrameNumber: integer);stdcall;
procedure ActivateMultitexturingLayer(Layer : Cardinal); stdcall;
procedure DeactiveMultytexturing; stdcall;
procedure SetMultytexturingLayerOffset(Layer : cardinal; X,Y : single); stdcall;
procedure ClearZBuffer; stdcall;
procedure DrawCube(Width,Height,Depth : single); stdcall;
procedure SetFog(Color : Integer; Fog_Start, Fog_End : single); stdcall;
procedure DeactiveFog; stdcall;
procedure SetCamera(Camera : TCamera); stdcall;
procedure CalculateFrustum; stdcall;
function  IsPointInFrustum(X,Y,Z : single) : boolean; stdcall;
function  IsSphereInFrustum(X,Y,Z,Radius : single) : boolean; stdcall;
function  IsBoxInFrustum(X,Y,Z,W,H,D : single) : boolean; stdcall;
procedure ZBuffer(Active : boolean); stdcall;
procedure ResetMatrix; stdcall;
procedure DrawTextureToTexture(TexSource,TexTarget : GluInt; X,Y : integer); stdcall;
procedure SetMultytexturingLayerTexCoordMulti(Layer : cardinal; X,Y : single); stdcall;
procedure DrawTextureToTextureTransparentColor(TexSource,TexTarget : GluInt; X,Y : integer; Color : Cardinal); stdcall;
function  ModelMaterial(Modelident : integer) : TMaterial; stdcall;
procedure CullFace(Mode : cardinal); stdcall;
procedure ModelsBump(Active : boolean); stdcall;
procedure RenderTexToTexFBO(ToTexture,TexSource,TexTarget : GluInt; X,Y : integer); stdcall;

function  LoadScene(FileName, MeshPath, TexPath : string):cardinal; stdcall;
procedure FreeScene(Ident : cardinal); stdcall;
procedure DrawScene(Ident : cardinal); stdcall;
function  SceneBoundingBox(Ident : cardinal):TVertex; stdcall;
function  CollideBoxWithScene(Ident : cardinal; BoxPos, BoxSize : Tvertex):boolean; stdcall;
function  SceneGetLastCollideObjectIndex:integer; stdcall;
function  SceneObjCount( Ident : cardinal ) : cardinal; stdcall;
function  GetSceneObjectIdent( SceneIdent : cardinal; ObjName : string ) : integer; stdcall;
procedure SceneSetObjActive( SceneIdent, ObjIdent : cardinal; Active : boolean ); stdcall;
procedure SceneSetObj( SceneIdent, ObjIdent : cardinal; SceneMesh : TSceneMesh ); stdcall;
function  SceneGetObj( SceneIdent, ObjIdent : cardinal ) : TSceneMesh; stdcall;

function ApplyKPD3Patch: Boolean;
function ApplyBLOKPatch: Boolean;
function HandleBlockKeyboardClick(mouseX, mouseY: Integer): Boolean;
function CheckKPD3FilesExist(locType: Integer; locNum: string): Boolean;
procedure InitKPD3Models;
function CheckBLOKFilesExist(locType: Integer; locNum: string): Boolean;

// Идентификация текущего локомотива/составности — используется в CheatMenu
// для per-loco конфигов кастомных текстов.
function GetLocomotiveFolder(locType: Integer): string;
function GetLocomotiveTypeFromMemory: Integer;
function GetLocNum: string;
function GetSignalSequenceRuntime(AlsAddr: Cardinal): string;
procedure InitializeTrafficLightSystemRuntime(const ARouteName: string);
function GetRouteName: string;
function GetFloatDigit(Position: Integer): string;
function GetMegaRealismSunrise: Integer;
function GetMegaRealismSunset: Integer;

var
  BoosterSunriseDawnTextureID, BoosterSunsetTwilightTextureID: Cardinal;
  BoosterDaySnowTextureID, BoosterSunsetSnowTextureID: Cardinal;
  BoosterSunsetTwilightSnowTextureID, BoosterNightSnowTextureID: Cardinal;
  BoosterSunriseDawnSnowTextureID, BoosterSunriseSnowTextureID: Cardinal;
  BoosterTexturesLoaded: Boolean;
  SkyDrawLoggedOnce: Boolean;
  SkyPatchApplied: Boolean;
  SevenSegmentFont: Integer;
  FloatValueAddr: Cardinal;
  LastTrackDirection: Integer = -1;
  _dlgMode: Cardinal = $00400000 + $349888;
  KLUBUFont: Integer;
Config_NewSky: Boolean;
  Config_MegaRealism: Boolean;
  Config_MainCamera: Boolean;   // Включена ли настройка основной камеры
  _dlgAls: Cardinal = $00400000 + $8C07ECC;
  s1, s2: TStringList;
  TrafficSystemInitialized: Boolean;
  LastSignalUpdate: Cardinal;
  SignalUpdateInterval: Cardinal;
  CachedSignalSequence: string;



procedure WriteHookAddress; stdcall;
procedure WriteHookAddressCHS8; stdcall;
procedure WriteHookAddressED4M; stdcall;
function GetCurrentHour: Integer; stdcall;  // если еще нет
function IsNightTime: Boolean;
function GetCurrentSeason: string;
procedure ProcessFreecam; stdcall;
procedure LoadSettingsAndCustomModels; stdcall;
procedure ProcessDayNightSystem; stdcall;
procedure InitializeDayNightSystem; stdcall;

var
  DayNightInitialized: Boolean = False;
  HasDayNightFolders: Boolean = False;
  DayCabTextureID, DayPultTextureID, Day254TextureID, DayKlubTextureID: Cardinal;
  NightCabTextureID, NightPultTextureID, Night254TextureID, NightKlubTextureID: Cardinal;
  CurrentTimeMode: Integer = -1;
  CurrentIsNight: Boolean = False;
  LastNewSkyState: Boolean = False;
  NewSkyInitialized: Boolean = False;
  LastTimeCheck1: Cardinal = 0;
  TimeCheckInterval1: Cardinal = 7000;
  LastStepForwardCheck: Cardinal = 0;
  StepForwardCheckInterval: Cardinal = 1000;
  LastMainCameraState: Boolean = False;
  MainCameraInitialized: Boolean = False;
  OriginalStepForwardValue: Single = 0.1;
  _dlgCommandBlock: Cardinal = $00400000 + $34988C;
  _dlgPathNumber: Cardinal = $00400000 + $4F8D958;
  _dlgPathDirection: Cardinal = $00400000 + $349890;
  _dlgVk: Cardinal = $00400000 + $4F8D940;
  _dlgRb: Cardinal = $00400000 + $349914;
  _dlgRbs: Cardinal = $00400000 + $349910;
  statek137: Boolean = False;
  als_en_state: Boolean = False;

// Runtime state freecam'а — был объявлен в implementation, но нужен
// из RA3Patches.pas для решения когда писать camera anchor. Объявление
// фактического var-а остаётся в implementation; здесь только external
// reference, чтобы не ломать существующие места.
var
  FreecamEnabled: Boolean;
  FreecamCollision: Boolean;
  FreecamWalkMode: Boolean;
  // 0 = WASD, 1 = Стрелочки (стрелки)
  FreecamControlMode: Integer;
  // Кнопка временного отключения коллизии: 0 = Откл, 1 = Shift, 2 = Ctrl
  FreecamCollisionDisableKey: Integer;



var
  ConfigLoaded: Boolean;
procedure ApplyMaxVisibleDistance; stdcall;
procedure ProcessStepForwardConfig; stdcall;
procedure DrawSky(x, y, z: Single); stdcall;
procedure DrawText3D(FontID: Integer; Text: string);
procedure PatchDrawSkyCall; stdcall;
procedure RestoreOriginalSkyCall; stdcall;
procedure ProcessAllModules; stdcall;

// Функции для синхронизации с меню
procedure SyncConfigFromMenu(Freecam, MainCamera, MaxDistance, NewSky: Boolean); stdcall;
function GetConfigFreecam: Boolean; stdcall;
function GetConfigMainCamera: Boolean; stdcall;
function GetConfigMaxDistance: Boolean; stdcall;
function GetConfigNewSky: Boolean; stdcall;

// Mega Realism sync
procedure SyncMegaRealismConfig(MegaRealism: Boolean; CityIndex: Integer); stdcall;

procedure HookKLUB(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;

procedure DrawKLUB(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall;

procedure DrawSkorostemer(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single;
  AngPrivod: Single
); stdcall;

procedure HookSkorostemerViaKLUB(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;

procedure HookSkorostemerCHS7(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;

procedure DrawKPD3VL85(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;

procedure DrawBLOCK(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;

procedure DrawSpeedometer3D;

procedure DrawKlubBilVData(
  ModelParam2: Cardinal;
  ModelParam1: Cardinal;
  AngZ: Single;
  Z: Single;
  Y: Single;
  X: Single
); stdcall; export;

procedure DrawKPD3(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single;
  AngPrivod: Single
); stdcall; export;

exports
  HookKLUB, DrawSky, DrawSkorostemer, HookSkorostemerViaKLUB, DrawKLUB, HookSkorostemerCHS7, DrawKPD3, DrawKPD3VL85, DrawBLOCK, DrawKlubBilVData;
procedure FreeEng;
procedure InitEng;

function GetAlsEnState: Boolean;
function GetAlsEnVisibleSignalCount: Integer;

function  GETLIGHT(ID: integer) : integer;
procedure _glTexCoord2f(X,Y : GLFloat; Layer : integer = -1); stdcall;
procedure _glTexCoord3f(X,Y,Z : GLFloat; Layer : integer = -1);

const
  CustomModelPath: PChar = 'Data\loc\1.dmd';
  TIME_STRUCT_PTR    = $09008034;  // [Launcher.exe+34B5F0] - указатель на время
  ABSOLUTE_TIME_PTR  = $0538D920;  // [Launcher.exe+34AEF0] - абсолютное время
  RASSET_HOUR_PTR    = $00749768;  // [Launcher.exe+34B190] - час рассвета
  ZAKAT_HOUR_PTR     = $0074976C;  // [Launcher.exe+34B824] - час заката
  LIGHTING_CHECK_PTR = $090043A4;  // [Launcher.exe+34AEA4] - проверка освещения
  
  // Адреса ресурсов
  TEXTURES_PTR       = $09110D60;  // [Launcher.exe+34B8AC] - текстуры
  MODELS_PTR         = $09110D70;  // [Launcher.exe+34B56C] - модели
  
  // Альфа-константы
  BASE_ALPHA         = 240;        // F0
  FULL_ALPHA         = 255;        // FF
  MINUTE_MULTIPLIER  = 4;
  
  // Константы вращения неба
  SKY_ROTATION_MUL   = 15.0;
  SKY_ROTATION_DIV   = 3600.0;
  SKY_ROTATION_SUB   = 180.0;
  
var
LightsOn : array [0..20] of boolean;
QuadraticObject : PGLUQuadricObj;
SphereDL : glUint;
RenderTTWidth,RenderTTHeight : integer;
In2DWeAre : boolean = false;
CurTexture : cardinal = 0;

SettingsLoaded: Boolean = False;
LocNum: string = '';
LocomotiveType: Integer = 822;
CustomModelLoaded: Boolean = False;
CustomModelID: Integer = 0;
CustomTextureID: Cardinal = 0;

SystemInitialized: Boolean = False;
  
// Оптимизация времени
LastTimeCheck: Cardinal = 0;
TimeCheckInterval: Cardinal = 300; // раз в 100мс

LastFreecamConfigState: Boolean = False;  // Предыдущее состояние из конфига
FreecamConfigStateInitialized: Boolean = False;  // Флаг инициализации

StationsList: TStringList;
StationsLoaded: Boolean = False;
CurrentStationName: string = '';
NextStationName: string = '';

LastCollide : integer = -1;

Obj3DInfo : array of TObj3DInfo;
Obj3DInfoCount : Integer = 0;
InBlock : boolean = false;

CantRenderInFBO : boolean;
fbo_frame : cardinal = 0;
fbo_depth : cardinal = 0;
fbo_w, fbo_h, fbo_z : cardinal;
fbo2 : cardinal;

implementation
uses Advanced3D, DrawFunc2D, RA3, CheatMenu, BoosterKPD3Runtime, BoosterBLOKRuntime,
     BoosterPrimitives, BoosterKPD3Display, BoosterKLUB, BoosterTrafficRuntime,
     BoosterStationRuntime, BoosterCHS7, BoosterCHS8, BoosterED4M, BoosterVL85;

type TAMesh = record
Ident : cardinal;
Mesh : TGLMultyMesh;
end;



TScene = record
 uid : cardinal;
 Models : array of TSceneMesh;
end;

// ===== ДОБАВИТЬ В СЕКЦИЮ VAR ПЕРЕД IMPLEMENTATION =====
var
  LastStationUpdate: Cardinal = 0;
  StationUpdateInterval: Cardinal = 1000; // Обновлять каждую секунду
  // Переменные для выдвигающегося интерфейса
  KeyboardPanelExpanded: Boolean = False;
  KeyboardPanelAnimation: Single = 0.0;
  KeyboardPanelTargetWidth: Single = 0.0;
  LastMousePos: TPoint;

  _dlgPressureTm: Cardinal = $00400000 + $8D10738;
  LastFloatValue: Single = -999.0; // Для отслеживания изменений
  FloatAsInt: Integer;
  FloatStr: string;

    // Переменные конфигурации
  Config_SAUT: Boolean = True;   // Отображение элементов скорости и лимитов (14, 15, 16)
  Config_BGSD: Boolean = True;   // Отображение основных данных (0-13)
  Config_STUPEN: Boolean = True;  // ← ДОБАВИТЬ ЭТУ СТРОКУ

  Config_Freecam: Boolean = True;      // Включен ли фрикам
  Config_MaxDistance: Boolean = True;  // Включена ли настройка дистанции
  Config_MegaRealismCityIndex: Integer = 2; // Индекс города (2 = Москва)

  // ===== НОВЫЕ ПЕРЕМЕННЫЕ ДЛЯ ФРИКАМА =====
  Config_BaseSpeed: Single = 0.0;        // Базовая скорость фрикама
  Config_FastSpeed: Single = 2.2;        // Быстрая скорость фрикама (Shift)
  Config_TurnSpeed: Single = 1.5;        // Скорость поворота камеры
  Config_MaxVisibleDistance: Integer = 1600; // Максимальная дистанция видимости

  Config_StepForward: Single;              // Значение stepForward

  // World sky state is declared in the public compatibility section above.

  SkorSharedTextureID: Cardinal = 0;

const
  PANEL_BASE_WIDTH = 80;      // Базовая ширина панели
  PANEL_EXPANDED_WIDTH = 300; // Расширенная ширина
  PANEL_HEIGHT = 200;         // Высота панели
  PANEL_MARGIN = 10;          // Отступ от края экрана
  ANIMATION_SPEED = 8.0;      // Скорость анимации

  // === MEGA REALISM: sunrise/sunset data from dateandtime.info ===
  // For the 15th day of each month, 2026 (local timezone)
  MEGA_REALISM_CITY_COUNT = 21;

  // Sunrise hour per city per month [cityIndex, month 1..12]
  // Source: dateandtime.info, 15th of each month, 2026
  MRSunriseH: array[0..MEGA_REALISM_CITY_COUNT-1, 1..12] of Byte = (
    (8, 7, 6, 5, 4, 4, 4, 5, 6, 7, 8, 8),     // Kaliningrad
    (9, 8, 7, 5, 4, 3, 4, 5, 6, 7, 8, 9),     // Sankt-Peterburg
    (8, 7, 6, 5, 4, 3, 4, 5, 6, 7, 8, 8),     // Moskva
    (8, 7, 6, 5, 4, 4, 4, 5, 5, 6, 7, 8),     // Voronezh
    (7, 7, 6, 5, 4, 3, 4, 4, 5, 6, 7, 7),     // Volgograd
    (8, 7, 6, 5, 4, 4, 4, 5, 5, 6, 7, 8),     // Rostov-na-Donu
    (8, 7, 6, 4, 3, 3, 3, 4, 5, 6, 7, 8),     // Nizhniy Novgorod
    (8, 7, 5, 4, 3, 2, 3, 4, 5, 6, 7, 8),     // Kazan
    (8, 7, 6, 5, 4, 4, 4, 5, 6, 7, 8, 8),     // Samara
    (9, 8, 7, 6, 5, 4, 4, 5, 6, 7, 8, 9),     // Ufa
    (9, 8, 7, 5, 4, 4, 4, 5, 6, 7, 8, 9),     // Chelyabinsk
    (9, 8, 7, 5, 4, 4, 4, 5, 6, 7, 8, 9),     // Ekaterinburg
    (9, 8, 7, 6, 4, 4, 4, 5, 6, 7, 9, 9),     // Perm
    (9, 8, 7, 6, 4, 4, 4, 5, 6, 7, 8, 9),     // Omsk
    (9, 8, 7, 6, 5, 4, 5, 6, 7, 7, 9, 9),     // Novosibirsk
    (9, 8, 7, 5, 4, 4, 4, 5, 6, 7, 8, 9),     // Krasnoyarsk
    (9, 8, 7, 6, 5, 4, 4, 5, 6, 7, 8, 9),     // Irkutsk
    (9, 8, 7, 5, 4, 4, 4, 5, 6, 7, 8, 9),     // Chita
    (8, 8, 7, 6, 5, 5, 5, 6, 6, 7, 8, 8),     // Blagoveshchensk
    (8, 8, 7, 6, 5, 4, 5, 5, 6, 7, 8, 8),     // Khabarovsk
    (8, 8, 7, 6, 5, 5, 5, 6, 6, 7, 8, 8)      // Vladivostok
  );

  // Sunrise minute per city per month
  MRSunriseM: array[0..MEGA_REALISM_CITY_COUNT-1, 1..12] of Byte = (
    (51, 59, 52, 35, 31, 0, 21, 13, 10, 7, 8, 54),     // Kaliningrad
    (45, 36, 15, 41, 20, 35, 3, 13, 26, 37, 56, 55),    // Sankt-Peterburg
    (49, 53, 44, 24, 17, 44, 6, 1, 1, 0, 4, 52),        // Moskva
    (22, 37, 37, 26, 30, 4, 22, 8, 58, 47, 41, 23),     // Voronezh
    (49, 9, 15, 11, 20, 58, 15, 55, 38, 22, 10, 47),    // Volgograd
    (3, 25, 33, 32, 44, 24, 40, 17, 58, 39, 24, 0),     // Rostov-na-Donu
    (26, 29, 19, 57, 48, 14, 36, 33, 35, 35, 41, 31),   // Nizhniy Novgorod
    (3, 7, 58, 38, 31, 58, 19, 15, 15, 14, 18, 7),      // Kazan
    (45, 57, 53, 39, 40, 11, 31, 19, 13, 5, 3, 47),     // Samara
    (30, 37, 30, 13, 9, 38, 59, 51, 48, 45, 46, 33),    // Ufa
    (10, 17, 9, 50, 45, 13, 34, 28, 26, 23, 26, 13),    // Chelyabinsk
    (23, 25, 13, 49, 39, 3, 26, 25, 28, 30, 37, 28),    // Ekaterinburg
    (48, 46, 30, 3, 50, 11, 35, 38, 44, 49, 1, 54),     // Perm
    (22, 28, 21, 3, 58, 26, 47, 41, 38, 35, 38, 24),    // Omsk
    (44, 50, 43, 24, 20, 48, 9, 2, 0, 57, 0, 46),       // Novosibirsk
    (9, 13, 3, 42, 35, 1, 23, 19, 19, 19, 24, 13),      // Krasnoyarsk
    (5, 18, 17, 5, 7, 40, 59, 45, 36, 27, 23, 5),       // Irkutsk
    (14, 21, 14, 56, 52, 21, 41, 34, 31, 28, 30, 16),   // Chita
    (56, 16, 22, 17, 26, 4, 20, 0, 44, 28, 16, 54),     // Blagoveshchensk
    (46, 7, 13, 9, 19, 57, 13, 52, 36, 19, 6, 44),      // Khabarovsk
    (41, 10, 24, 30, 49, 32, 46, 17, 51, 25, 4, 36)     // Vladivostok
  );

  // Sunset hour per city per month
  MRSunsetH: array[0..MEGA_REALISM_CITY_COUNT-1, 1..12] of Byte = (
    (16, 17, 18, 19, 20, 21, 21, 20, 18, 17, 16, 16),  // Kaliningrad
    (16, 17, 19, 20, 21, 22, 22, 20, 19, 17, 16, 15),  // Sankt-Peterburg
    (16, 17, 18, 19, 20, 21, 21, 20, 18, 17, 16, 15),  // Moskva
    (16, 17, 18, 19, 20, 20, 20, 19, 18, 17, 16, 16),  // Voronezh
    (16, 17, 18, 18, 19, 20, 20, 19, 18, 17, 16, 16),  // Volgograd
    (16, 17, 18, 19, 19, 20, 20, 19, 18, 17, 16, 16),  // Rostov-na-Donu
    (15, 17, 18, 19, 20, 20, 20, 19, 18, 17, 15, 15),  // Nizhniy Novgorod
    (15, 16, 17, 18, 19, 20, 20, 19, 18, 16, 15, 15),  // Kazan
    (16, 17, 18, 19, 20, 21, 20, 20, 18, 17, 16, 16),  // Samara
    (17, 18, 19, 20, 21, 21, 21, 20, 19, 18, 17, 16),  // Ufa
    (16, 17, 18, 19, 20, 21, 21, 20, 19, 17, 16, 16),  // Chelyabinsk
    (16, 17, 18, 20, 21, 21, 21, 20, 19, 17, 16, 16),  // Ekaterinburg
    (17, 18, 19, 20, 21, 22, 22, 21, 19, 18, 16, 16),  // Perm
    (17, 18, 19, 20, 21, 21, 21, 20, 19, 18, 17, 16),  // Omsk
    (17, 18, 19, 20, 21, 22, 21, 21, 19, 18, 17, 16),  // Novosibirsk
    (16, 17, 18, 19, 20, 21, 21, 20, 19, 17, 16, 16),  // Krasnoyarsk
    (17, 18, 19, 20, 20, 21, 21, 20, 19, 18, 17, 16),  // Irkutsk
    (17, 18, 19, 20, 20, 21, 21, 20, 19, 18, 16, 16),  // Chita
    (17, 18, 19, 19, 20, 21, 21, 20, 19, 18, 17, 17),  // Blagoveshchensk
    (17, 18, 19, 19, 20, 21, 20, 20, 19, 18, 17, 17),  // Khabarovsk
    (18, 18, 19, 19, 20, 20, 20, 20, 19, 18, 17, 17)   // Vladivostok
  );

  // Sunset minute per city per month
  MRSunsetM: array[0..MEGA_REALISM_CITY_COUNT-1, 1..12] of Byte = (
    (42, 44, 41, 40, 37, 16, 6, 11, 55, 40, 36, 11),   // Kaliningrad
    (30, 49, 0, 15, 29, 23, 6, 52, 21, 51, 30, 52),     // Sankt-Peterburg
    (28, 33, 32, 34, 34, 15, 4, 6, 48, 30, 23, 56),     // Moskva
    (42, 37, 27, 19, 8, 43, 35, 47, 38, 30, 33, 13),    // Voronezh
    (33, 22, 6, 52, 36, 6, 0, 17, 15, 13, 22, 6),       // Volgograd
    (57, 44, 26, 9, 50, 18, 13, 33, 34, 34, 46, 31),    // Rostov-na-Donu
    (59, 6, 6, 10, 11, 54, 43, 43, 23, 3, 55, 26),      // Nizhniy Novgorod
    (42, 47, 46, 48, 48, 29, 19, 20, 2, 44, 37, 10),    // Kazan
    (51, 49, 42, 38, 31, 8, 59, 7, 56, 44, 44, 21),     // Samara
    (20, 22, 19, 19, 15, 55, 45, 49, 34, 18, 14, 49),   // Ufa
    (56, 59, 57, 58, 55, 36, 25, 29, 12, 56, 50, 24),   // Chelyabinsk
    (50, 58, 59, 5, 8, 52, 40, 38, 17, 56, 46, 16),     // Ekaterinburg
    (0, 12, 17, 26, 32, 19, 6, 0, 35, 11, 57, 25),      // Perm
    (9, 12, 9, 9, 7, 47, 37, 41, 24, 8, 3, 38),         // Omsk
    (31, 33, 31, 31, 29, 9, 59, 3, 46, 30, 25, 59),     // Novosibirsk
    (45, 51, 51, 54, 54, 36, 25, 26, 7, 49, 41, 13),    // Krasnoyarsk
    (19, 15, 6, 0, 50, 26, 18, 28, 19, 9, 11, 49),      // Irkutsk
    (3, 5, 2, 2, 58, 38, 29, 33, 17, 2, 57, 32),        // Chita
    (38, 28, 12, 59, 42, 13, 7, 24, 22, 20, 29, 12),    // Blagoveshchensk
    (31, 20, 4, 49, 32, 2, 57, 15, 13, 11, 21, 4),      // Khabarovsk
    (2, 42, 18, 54, 28, 53, 50, 16, 23, 30, 49, 38)     // Vladivostok
  );

  // Тип для хранения информации о станции
type
  TStationInfo = record
    name: string;
    piket: Integer;
    distance: Integer;
  end;

var
  RenderedTex : GlUint;
  MultyCoordOffset : array [1..5] of array [0..3] of GLfloat;

  Meshs : array of TAMesh;
  MeshsCount : cardinal = 0;
  OverAllMeshUsed :cardinal = 0;

  Scenes : array of TScene;
  ScenesCount : cardinal = 0;
  ScenesOverall : cardinal = 0;

  mat_shininess : GLfloat = 0.0;

  LastStationCheck: Cardinal = 0;
  StationCheckInterval: Cardinal = 2000; // каждые 2 секунды
  CachedCurrentStation: string = '';
  CachedNextStation: string = '';


  KeyDebounceTime: array[0..11] of Cardinal;
  DebounceInterval: Cardinal = 50; // 50мс задержка

  light_ambient : array [0..3] of GLfloat = ( 0.0, 0.0, 0.0, 1.0 );
  light_diffuse : array [0..3] of GLfloat = ( 1.0, 1.0, 1.0, 1.0 );
  light_specular : array [0..3] of GLfloat = ( 1.0, 1.0, 1.0, 1.0 );
  mat_specular : array [0..3] of GLfloat = ( 0.0, 0.0, 0.0, 1.0 );

  CachedYellowBlockID: Word = 0;
  CachedGreenBlockID: Word = 0;
  LightBlockIDsCached: Boolean = False;

  // Переменные светофоров экспортируются из interface для BoosterTrafficRuntime.
  HookAddressWritten: Boolean = False;

  // Глобальные переменные для обработки команд
  LastCommand: string = '';
  CommandBuffer: string = '';
  PointerAddress: Cardinal = $900421C;
  WindowOpenAddress: Cardinal = $00400000 + $4F8D915;
  _dlgPressureAltPtr: Cardinal = $00400000 + $8D10D78;

  // Массивы для отслеживания состояний клавиш
  PreviousKeyStates: array[0..11] of Byte;
  KeyStatesInitialized: Boolean = False;

  LastMaxDistanceState: Boolean = False;
  MaxDistanceInitialized: Boolean = False;
  OriginalMaxDistancePatched: Boolean = False;
  
  // Для StepForward  
  
  OriginalMaxDistanceValue: Integer = 800; // Сохраняем оригинальное значение

  blokhookklub: Boolean = False;

  // Флаги для обработки команд  
  statek10: Boolean = False;  // <- ДОБАВИТЬ ЭТУ СТРОКУ
  SavedCommand: string = '';
  CommandCompleted: Boolean = False;
  EnterPressed: Boolean = False;

  // Переменные для моделей
  MyModelID: integer = 0;
  UsavppDisplayModelID: integer = 0;
  KlubBilIndPModelID: Integer = 0;  // для klub-bil-ind_p.dmd (||)
  KlubBilIndBModelID: Integer = 0;  // для klub-bil-ind_b.dmd (|, l, -)
  MyTextureID: cardinal = 0;
  UsavppDisplayTextureID: cardinal = 0;
  strelka: integer = 0;

  LastKeyboardCheck: Cardinal = 0;
  KeyboardCheckInterval: Cardinal = 1; // ~60 FPS = каждые 16мс

  // Переменные переменных
  en_chastota: string = 'x';
  MemoryWritten: Boolean = False;


  YellowBlockRotX: Single = 0.0;
  YellowBlockRotY: Single = 0.0;
  YellowBlockRotZ: Single = 0.0;
  YellowBlockPosX: Single = -0.086499996;
  YellowBlockPosY: Single = 0.0;
  YellowBlockPosZ: Single = 0.223;
  YellowBlockScale: Single = 0.88999999;
  YellowBlockParamsLoaded: Boolean = False;
  // Переменные для фрикамы. FreecamEnabled теперь объявлен в interface
  // (нужен из RA3Patches), здесь оставляем только остальные.
  FreecamInitialized: Boolean = False;

  // Переменные для параметров стрелки
  ArrowAngle: Single = 150.0;
  ArrowRotation: Single = 90.0;
  ArrowScale: Single = 1.61;
  ArrowX: Single = 0.895;
  ArrowY: Single = 7.45;
  ArrowZ: Single = 3.64;
  ArrowRotateX: Single = -40.4;
  ArrowRotateY: Single = 0.0;
  ArrowRotateZ: Single = 0.0;
  ArrowCurrentSpeed: Single = 0.0;
  ArrowKoef: Single = 1.63;
  ArrowParamsLoaded: Boolean = False;

  // ===== ПЕРЕМЕННЫЕ ДЛЯ РЕАЛ-ТАЙМ ОБНОВЛЕНИЯ =====
  LastArrowParamsCheck: Cardinal = 0;
  ArrowParamsCheckInterval: Cardinal = 500;
  LastBoosterConfigCheck: Cardinal = 0;
  BoosterConfigCheckInterval: Cardinal = 500; // Реже обновляем настройки отображения
  
  // Адреса камеры
  ADDR_LOOKYAW: Cardinal = $9004398;
  ADDR_LOOKPITCH: Cardinal = $900439C;
  ADDR_X: Cardinal = $9008028;
  ADDR_Y: Cardinal = $900802C;
  ADDR_Z: Cardinal = $9008030;
  FREEMODE_SWITCH_ADDR: Cardinal = $7499E8;
  
  // Параметры движения
  BASE_SPEED: Single = 0.5;
  FAST_SPEED: Single = 2.2;
  TURN_SPEED: Single = 1.5;
  
  // Сохраненные начальные значения
  InitialYaw, InitialPitch: Single;
  InitialX, InitialY, InitialZ: Single;
  
  // Переменные для обработки клавиш
  LastFreecamToggle: Cardinal = 0;
  LastFreecamDisable: Cardinal = 0;
  FreecamKeyDelay: Cardinal = 200; // 200мс задержка между нажатиями
  MaxDistanceWritten: Boolean = False;  // ← ДОБАВИТЬ ЭТУ СТРОКУ
  
  // Коллизия DMD и Free Walk (FreecamCollision/FreecamWalkMode в interface)
  WalkGravityVel: Single = 0.0;       // вертикальная скорость для гравитации
  WALK_GRAVITY: Single = 0.03;        // ускорение гравитации за кадр
  WALK_EYE_HEIGHT: Single = 1.7;      // высота глаз над землёй
  WALK_PLAYER_RADIUS: Single = 0.3;   // радиус "тела" для коллизии
  WALK_PLAYER_HEIGHT: Single = 1.8;   // высота "тела" для коллизии

  // NOP патч
  OriginalBytes: array[0..4] of Byte;
  NopBytes: array[0..4] of Byte = ($90, $90, $90, $90, $90);

  // Переменные для системы день/ночи
  pisec1TextureID: Cardinal;
  pisec2TextureID: Cardinal;
  skorPrivod1TextureID: Cardinal;
  skorPrivod2TextureID: Cardinal;
  strelkaSkorTextureID: Cardinal;
  strelkaSkorTimeTextureID: Cardinal;

  bilpom_x: Single;
  bilpom_y: Single;
  bilpom_z: Single;
  bilpom_AngZ: Single;
  bilpom_AngX: Single;

procedure SyncConfigFromMenu(Freecam, MainCamera, MaxDistance, NewSky: Boolean); stdcall;
begin
  
  // ИСПРАВЛЯЕМ: используем правильные переменные
  Config_Freecam := Freecam;
  Config_MainCamera := MainCamera;
  Config_MaxDistance := MaxDistance;
  Config_NewSky := NewSky;
end;

function GetConfigFreecam: Boolean; stdcall;
begin
  Result := Config_Freecam;
end;

function GetConfigMainCamera: Boolean; stdcall;
begin
  Result := Config_MainCamera;
end;

function GetConfigMaxDistance: Boolean; stdcall;
begin
  Result := Config_MaxDistance;
end;

function GetConfigNewSky: Boolean; stdcall;
begin
  Result := Config_NewSky;
end;

procedure SyncMegaRealismConfig(MegaRealism: Boolean; CityIndex: Integer); stdcall;
begin
  Config_MegaRealism := MegaRealism;
  if (CityIndex >= 0) and (CityIndex < MEGA_REALISM_CITY_COUNT) then
    Config_MegaRealismCityIndex := CityIndex
  else
    Config_MegaRealismCityIndex := 2; // Москва
end;

// Вспомогательная: кол-во дней в месяце
function MRDaysInMonth(mo, yr: Integer): Integer;
begin
  case mo of
    1, 3, 5, 7, 8, 10, 12: Result := 31;
    4, 6, 9, 11: Result := 30;
    2: if (yr mod 4 = 0) and ((yr mod 100 <> 0) or (yr mod 400 = 0))
         then Result := 29 else Result := 28;
  else
    Result := 30;
  end;
end;

// Возвращает время восхода в минутах от полуночи с интерполяцией по дню.
// Данные хранятся для 15-го числа каждого месяца.
// Линейно интерполируем между ближайшими 15-ми числами.
function GetMegaRealismSunrise: Integer;
var
  st: TSystemTime;
  ci, mo, day, yr: Integer;
  moA, moB: Integer;
  valA, valB: Integer;
  span, offset, dimA: Integer;
begin
  GetLocalTime(st);
  ci := Config_MegaRealismCityIndex;
  mo := st.wMonth; day := st.wDay; yr := st.wYear;
  if (ci < 0) or (ci >= MEGA_REALISM_CITY_COUNT) then ci := 2;
  if (mo < 1) or (mo > 12) then mo := 1;
  if (day < 1) or (day > 31) then day := 15;

  // Точное попадание на 15-е — без интерполяции
  if day = 15 then
  begin
    Result := Integer(MRSunriseH[ci, mo]) * 60 + Integer(MRSunriseM[ci, mo]);
    Exit;
  end;

  if day < 15 then
  begin
    moA := mo - 1; if moA < 1 then moA := 12;
    moB := mo;
    dimA := MRDaysInMonth(moA, yr);
    span := (dimA - 15) + 15;
    offset := (dimA - 15) + day;
  end
  else
  begin
    moA := mo;
    moB := mo + 1; if moB > 12 then moB := 1;
    dimA := MRDaysInMonth(moA, yr);
    span := (dimA - 15) + 15;
    offset := day - 15;
  end;

  valA := Integer(MRSunriseH[ci, moA]) * 60 + Integer(MRSunriseM[ci, moA]);
  valB := Integer(MRSunriseH[ci, moB]) * 60 + Integer(MRSunriseM[ci, moB]);

  if span <= 0 then span := 30;
  Result := valA + Round((valB - valA) * offset / span);
end;

// Возвращает время заката в минутах от полуночи с интерполяцией по дню.
function GetMegaRealismSunset: Integer;
var
  st: TSystemTime;
  ci, mo, day, yr: Integer;
  moA, moB: Integer;
  valA, valB: Integer;
  span, offset, dimA: Integer;
begin
  GetLocalTime(st);
  ci := Config_MegaRealismCityIndex;
  mo := st.wMonth; day := st.wDay; yr := st.wYear;
  if (ci < 0) or (ci >= MEGA_REALISM_CITY_COUNT) then ci := 2;
  if (mo < 1) or (mo > 12) then mo := 1;
  if (day < 1) or (day > 31) then day := 15;

  if day = 15 then
  begin
    Result := Integer(MRSunsetH[ci, mo]) * 60 + Integer(MRSunsetM[ci, mo]);
    Exit;
  end;

  if day < 15 then
  begin
    moA := mo - 1; if moA < 1 then moA := 12;
    moB := mo;
    dimA := MRDaysInMonth(moA, yr);
    span := (dimA - 15) + 15;
    offset := (dimA - 15) + day;
  end
  else
  begin
    moA := mo;
    moB := mo + 1; if moB > 12 then moB := 1;
    dimA := MRDaysInMonth(moA, yr);
    span := (dimA - 15) + 15;
    offset := day - 15;
  end;

  valA := Integer(MRSunsetH[ci, moA]) * 60 + Integer(MRSunsetM[ci, moA]);
  valB := Integer(MRSunsetH[ci, moB]) * 60 + Integer(MRSunsetM[ci, moB]);

  if span <= 0 then span := 30;
  Result := valA + Round((valB - valA) * offset / span);
end;

function GetCurrentHour: Integer;
begin
  try
    Result := PInteger(Pointer($00400000 + $8C08034))^;
    // Ограничиваем значение от 0 до 23
    if Result < 0 then Result := 0;
    if Result > 23 then Result := Result mod 24;
  except
    Result := 12; // По умолчанию день
  end;
end;

function GetRouteName: string;
var
  addr: Cardinal;
  buffer: array[0..255] of Char;
  bytesRead: Cardinal;
  fullString: string;
  slashPos: Integer;
  processHandle: THandle;
  i: Integer;
begin
  Result := '';
  
  try
    // Получаем хендл текущего процесса
    processHandle := GetCurrentProcess();
    
    // Адрес строки маршрута
    addr := $400000 + $8DD46D7;
    
    // Читаем строку из памяти
    FillChar(buffer, SizeOf(buffer), 0);
    if ReadProcessMemory(processHandle, Pointer(addr), @buffer, 256, bytesRead) then
    begin
      // Находим конец строки (первый нулевой символ)
      i := 0;
      while (i < 256) and (buffer[i] <> #0) do
        Inc(i);
      
      // Преобразуем в строку
      SetString(fullString, buffer, i);
      
      // Извлекаем название до первого обратного слеша
      slashPos := Pos('\', fullString);
      if slashPos > 0 then
        Result := Copy(fullString, 1, slashPos - 1)
      else
        Result := fullString;
        
      //AddToLogFile(EngineLog, 'Название маршрута: "' + Result + '"');
    end
    else
    begin
      //AddToLogFile(EngineLog, 'Не удалось прочитать название маршрута из памяти');
    end;

  except
    on E: Exception do
    begin
      //AddToLogFile(EngineLog, 'Ошибка при получении названия маршрута: ' + E.Message);
      Result := '';
    end;
  end;
end;




// ===== ИЗМЕНИТЬ LoadBoosterSkyTextures =====

procedure DrawSkyLayer(textureID: Cardinal; alpha: Byte; modelID: Word);
begin
  DrawWorldSkyLayer(textureID, alpha, modelID);
end;

procedure DrawSky(x, y, z: Single);
begin
  DrawWorldSkyRuntime(x, y, z);
end;


procedure PatchDrawSkyCall;
begin
  ApplyWorldSkyHook;
end;


procedure RestoreOriginalSkyCall;
begin
  RestoreWorldSkyHook;
end;


procedure ApplyMaxVisibleDistance; stdcall;
var
  PatchValue: Single;
  PatchAddress: PSingle;
  InstructionAddress: PByte;
  OldProtect: DWORD;
  StateChanged: Boolean;
  OriginalValue: Integer;
begin

  // ВАЖНО: купол снимаем ВСЕГДА, до проверки Config_MaxDistance.
  // Без этого патча даже при выключенном MaxVisibleDistance юзер видит
  // купол 1500м, а наш ApplyMaxVisibleDistance просто тогда не запускался.
  ApplyWorldDomePatch;

  // ===== ВСЕГДА УСТАНАВЛИВАЕМ 800 ПО УМОЛЧАНИЮ =====
  try
    PInteger(Pointer($91D4DD0))^ := 800;
    PSingle(Pointer($00791DD8))^ := 800.0;
  except
  end;
  
  // ПРОВЕРЯЕМ ФЛАГ ВКЛЮЧЕНИЯ СИСТЕМЫ
  if not Config_MaxDistance then
  begin
    AddToLogFile(EngineLog, 'MaxVisibleDistance отключена, остается 800');
    Exit;
  end;

  // Проверяем изменилось ли состояние модуля
  StateChanged := not MaxDistanceInitialized or (Config_MaxDistance <> LastMaxDistanceState);
  
  if StateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния MaxVisibleDistance: %s -> %s',
      [BoolToStr(LastMaxDistanceState, True), BoolToStr(Config_MaxDistance, True)]));
  end;

  if Config_MaxDistance then
  begin
    // ===== МОДУЛЬ ВКЛЮЧЕН - ПРИМЕНЯЕМ ПАТЧ =====
    if StateChanged then
      AddToLogFile(EngineLog, 'Применяем патч MaxVisibleDistance');

    try
      // Сохраняем оригинальное значение при первом включении
      if not OriginalMaxDistancePatched then
      begin
        try
          OriginalValue := PInteger(Pointer($91D4DD0))^;
          AddToLogFile(EngineLog, 'Сохранено оригинальное значение MaxDistance: ' + IntToStr(OriginalValue));
          OriginalMaxDistancePatched := True;
        except
          AddToLogFile(EngineLog, 'Не удалось прочитать оригинальное значение, используется 800');
        end;
      end;

      // Применяем наше значение
      PInteger(Pointer($91D4DD0))^ := Trunc(maxvisibledistance);

      // Преобразование и запись float по адресу 00791DD8
      PatchValue := Trunc(maxvisibledistance);
      PatchAddress := Pointer($00791DD8);
      PatchAddress^ := PatchValue;

      // Патчим ПЕРВУЮ инструкцию культинга — fcomp [0x4942CC] по $492258.
      // Перенаправляем чтение float'а с оригинальной константы 700.0 на наш
      // буфер $00791DD8 куда записан maxvisibledistance.
      InstructionAddress := Pointer($492258);

      if VirtualProtect(InstructionAddress, 6, PAGE_EXECUTE_READWRITE, @OldProtect) then
      begin
        InstructionAddress^ := $D8;
        Inc(InstructionAddress);
        InstructionAddress^ := $1D;
        Inc(InstructionAddress);
        PDWORD(InstructionAddress)^ := $00791DD8;

        VirtualProtect(Pointer($492258), 6, OldProtect, @OldProtect);
      end;

      // Патчим ВТОРУЮ инструкцию культинга — fcomp [0x4942CC] по $492076.
      // Без этого патча оставался «купол» 700м: один из двух fcomp всё равно
      // сравнивал дистанцию с оригинальным 700.0 и резал рендер.
      // Дизасм игры:  fld [ebp-0x18]; fcomp [0x4942cc]; ja skip
      // Дублируем тот же D8 1D D8 1D 79 00, что и для $492258.
      InstructionAddress := Pointer($492076);

      if VirtualProtect(InstructionAddress, 6, PAGE_EXECUTE_READWRITE, @OldProtect) then
      begin
        InstructionAddress^ := $D8;
        Inc(InstructionAddress);
        InstructionAddress^ := $1D;
        Inc(InstructionAddress);
        PDWORD(InstructionAddress)^ := $00791DD8;

        VirtualProtect(Pointer($492076), 6, OldProtect, @OldProtect);
      end;

      // Купол уже сняли в ApplyDomePatch выше — здесь повторять не надо.
      if StateChanged then
        AddToLogFile(EngineLog, 'MaxVisibleDistance установлена: ' + IntToStr(Config_MaxVisibleDistance));
        
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'Ошибка установки MaxVisibleDistance: ' + E.Message);
    end;
  end
  else
  begin
    // ===== МОДУЛЬ ВЫКЛЮЧЕН - ВОССТАНАВЛИВАЕМ ОРИГИНАЛ =====
    if StateChanged then
    begin
      AddToLogFile(EngineLog, 'Восстанавливаем оригинальные значения MaxVisibleDistance: 800');
      
      try
        // Восстанавливаем стандартное значение 800
        PInteger(Pointer($91D4DD0))^ := 800;

        // Восстанавливаем float
        PatchValue := 800.0;
        PatchAddress := Pointer($00791DD8);
        PatchAddress^ := PatchValue;

        // Восстанавливаем оригинальный адрес в инструкциях.
        // Оригинал: fcomp dword ptr [0x4942CC] = 700.0f.
        // (В предыдущей версии тут была опечатка $00942CC без ведущего 4 —
        //  она писала битый адрес и игра бы крашилась при выключении.)
        InstructionAddress := Pointer($492258);

        if VirtualProtect(InstructionAddress, 6, PAGE_EXECUTE_READWRITE, @OldProtect) then
        begin
          InstructionAddress^ := $D8;
          Inc(InstructionAddress);
          InstructionAddress^ := $1D;
          Inc(InstructionAddress);
          PDWORD(InstructionAddress)^ := $004942CC; // оригинальная константа 700.0f

          VirtualProtect(Pointer($492258), 6, OldProtect, @OldProtect);
        end;

        // И вторая fcomp-инструкция — её мы тоже теперь патчим, так что
        // при выключении должна вернуться к оригинальному [0x4942CC].
        InstructionAddress := Pointer($492076);

        if VirtualProtect(InstructionAddress, 6, PAGE_EXECUTE_READWRITE, @OldProtect) then
        begin
          InstructionAddress^ := $D8;
          Inc(InstructionAddress);
          InstructionAddress^ := $1D;
          Inc(InstructionAddress);
          PDWORD(InstructionAddress)^ := $004942CC;

          VirtualProtect(Pointer($492076), 6, OldProtect, @OldProtect);
        end;

        AddToLogFile(EngineLog, 'MaxVisibleDistance отключена, восстановлено: 800');
        
      except
        on E: Exception do
          AddToLogFile(EngineLog, 'Ошибка восстановления MaxVisibleDistance: ' + E.Message);
      end;
    end;
  end;
  
  // Обновляем состояние
  LastMaxDistanceState := Config_MaxDistance;
  MaxDistanceInitialized := True;
end;

procedure LoadConfigAtStartup;
var
  F: TextFile;
  Line, Key, Value: string;
  ColonPos: Integer;
  OldDecimalSep: Char;
begin
  // Устанавливаем значения по умолчанию
  Config_Freecam := False;
  Config_MainCamera := True;
  Config_MaxDistance := True;
  Config_NewSky := True;

  MenuFreecamBaseSpeed := 0.5;
  MenuFreecamFastSpeed := 2.0;
  MenuFreecamTurnSpeed := 1.5;
  stepforward := 0.1;
  maxvisibledistance := 1200;

  if not FileExists('zdbooster.cfg') then
  begin
    AddToLogFile(EngineLog, 'zdbooster.cfg не найден, используем значения по умолчанию');
    Exit;
  end;

  // Принудительно парсим с точкой — на русской локали StrToFloat('0.5') падает,
  // все слайдеры тихо сбрасывались в дефолт при каждом старте.
  OldDecimalSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    AddToLogFile(EngineLog, '=== ЗАГРУЗКА КОНФИГА ПРИ СТАРТЕ ===');
    AssignFile(F, 'zdbooster.cfg');
    Reset(F);
    
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Line := Trim(Line);
      ColonPos := Pos(':', Line);
      if ColonPos > 0 then
      begin
        Key := Trim(Copy(Line, 1, ColonPos - 1));
        Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));
        
        // Состояния модулей
        if Key = 'freecam' then
        begin
          Config_Freecam := (Value = '1');
          AddToLogFile(EngineLog, 'Загружен freecam: ' + BoolToStr(Config_Freecam, True));
        end
        else if Key = 'main_camera' then
          Config_MainCamera := (Value = '1')
        else if Key = 'max_distance' then
          Config_MaxDistance := (Value = '1')
        else if Key = 'newsky' then
        begin
          Config_NewSky := (Value = '1');
          newsky := Config_NewSky;
        end
        
        // Значения слайдеров
        else if Key = 'basespeed' then
        begin
          try
            MenuFreecamBaseSpeed := StrToFloat(StringReplace(Value, ',', '.', [rfReplaceAll]));
            AddToLogFile(EngineLog, 'Загружен basespeed: ' + FloatToStr(MenuFreecamBaseSpeed));
          except
            MenuFreecamBaseSpeed := 0.5;
          end;
        end
        else if Key = 'fastspeed' then
        begin
          try
            MenuFreecamFastSpeed := StrToFloat(StringReplace(Value, ',', '.', [rfReplaceAll]));
            AddToLogFile(EngineLog, 'Загружен fastspeed: ' + FloatToStr(MenuFreecamFastSpeed));
          except
            MenuFreecamFastSpeed := 2.0;
          end;
        end
        else if Key = 'turnspeed' then
        begin
          try
            MenuFreecamTurnSpeed := StrToFloat(StringReplace(Value, ',', '.', [rfReplaceAll]));
          except
            MenuFreecamTurnSpeed := 1.5;
          end;
        end
        else if Key = 'stepforward' then
        begin
          try
            stepforward := StrToFloat(StringReplace(Value, ',', '.', [rfReplaceAll]));
          except
            stepforward := 0.1;
          end;
        end
        else if Key = 'maxvisibledistance' then
        begin
          try
            maxvisibledistance := StrToInt(Value);
          except
            maxvisibledistance := 1200;
          end;
        end;
      end;
    end;
    CloseFile(F);
    
    AddToLogFile(EngineLog, Format('Конфиг загружен: freecam=%s, basespeed=%.2f, fastspeed=%.2f',
      [BoolToStr(Config_Freecam, True), MenuFreecamBaseSpeed, MenuFreecamFastSpeed]));

  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Ошибка загрузки конфига: ' + E.Message);
      try CloseFile(F); except end;
    end;
  end;
  DecimalSeparator := OldDecimalSep;
end;

var
  LastProcessTime: Cardinal = 0;
  ModuleIndex: Integer = 0;
  ConfigLoadedAtStartup: Boolean = False;  // ДОБАВИТЬ ЭТУ ПЕРЕМЕННУЮ

procedure ProcessAllModules;
var
  currentTime: Cardinal;
const
  MODULE_COUNT = 3;
begin
  currentTime := timeGetTime;
  
  // === ЗАГРУЖАЕМ КОНФИГ ТОЛЬКО ОДИН РАЗ ПРИ ПЕРВОМ ВЫЗОВЕ ===
  if not ConfigLoadedAtStartup then
  begin
    LoadConfigAtStartup;
    ConfigLoadedAtStartup := True;
    AddToLogFile(EngineLog, 'Конфиг загружен при старте системы');
  end;
  
  LastProcessTime := currentTime;

  // Обрабатываем модули по очереди (round-robin)
  // ProcessFreecam убран — вызывается каждый кадр из EngineMainDraw
  case ModuleIndex of
    0: ProcessStepForwardConfig;
    1: ApplyMaxVisibleDistance;
    2: ProcessWorldSkyPatch;
  end;
  
  ModuleIndex := (ModuleIndex + 1) mod MODULE_COUNT;
end;

//procedure DRAWTOOLS3D____DRAWKLUBLS_SINGLE_SINGLE_SINGLE_SINGLE_SINGLE_SHORTINT_SHORTINT
//(x, y, z, AngZ, AngX : Single, Sig, Pit : Integer)
//begin
//
//  BeginObj3D();
//  Position3D(AngX, AngZ, z);
//  RotateZ(y);
//  RotateX(-x);
//  SetTexture();
//  SetModel
//
//end.;

function GetLocomotiveFolder(locType: Integer): string;
begin
  Result := LocomotiveFolder(locType);
end;

function GetLocomotiveTypeFromSettings: Integer;
begin
  Result := ReadLocomotiveTypeFromSettings;
end;

function GetLocomotiveTypeFromMemory: Integer;
begin
  Result := ReadLocomotiveTypeFromMemory;
end;

procedure LoadSettingsAndCustomModels;
var
  f: TextFile;
  line, paramName, paramValue: string;
  equalPos: Integer;
  directoryPath, locFolder: string;
  klubFlag: Byte;
  currentLocType: Integer;
  
  // Пути к файлам
  kolparaModelPath, kolparaTexturePath: string;
  klubModelPath, klubTexturePath, klubTexture2Path: string;
  klubBilVPssPath: string;  // Новый элемент
  pantoPodstavkaPath, pantoSkiPath, pantoLever2BotPath, pantoLever2TopPath: string;
  pantoLever1BotPath, pantoLever1TopPath, pantoTexturePath: string;
  
  // Пути к новым моделям
  skorPrivod1Path, skorPrivod2Path: string;
  pisec1Path, pisec2Path: string;
  strelkaSkorPath, strelkaSkorTimePath: string;

  skorModelPath, skorTexturePath: string;
  SkorModelID: Integer;
  SkorTextureID: Cardinal;
  skorModelOffset, skorTextureOffset: Cardinal;

  // Пути к LS моделям
  lsPath, lsBPath, lsKPath, lsKzhPath, lsZPath, lsZhPath: string;
  lsTexturePath: string;
  
  // Пути к KLUB_LS моделям
  klubLsPath, klubLsBPath, klubLsKPath, klubLsKzhPath, klubLsZPath, klubLsZhPath: string;
  
  // ID загруженных ресурсов
  KolparaModelID, KolparaTextureID: Integer;
  KlubModelID, KlubTextureID, KlubTexture2ID: Cardinal;
  KlubBilVPssID: Integer;  // ID для нового элемента
  PantoPodstavkaID, PantoSkiID, PantoLever2BotID, PantoLever2TopID: Integer;
  PantoLever1BotID, PantoLever1TopID, PantoTextureID: Integer;
  
  // ID новых моделей
  SkorPrivod1ID, SkorPrivod2ID: Integer;
  Pisec1ID, Pisec2ID: Integer;
  StrelkaSkorID, StrelkaSkorTimeID: Integer;
  
  // ID для LS моделей
  LsID, LsBID, LsKID, LsKzhID, LsZID, LsZhID: Integer;
  LsTextureID: Integer;
  
  // ID для KLUB_LS моделей
  KlubLsID, KlubLsBID, KlubLsKID, KlubLsKzhID, KlubLsZID, KlubLsZhID: Integer;
  
  // Адреса памяти
  baseStructAddr: Cardinal;
  modelAddr, textureAddr: Pointer;
  OldProtect: DWORD;
  
  // Оффсеты для разных локомотивов
  klubBilOffset: Cardinal;
  klubBilVPssOffset: Cardinal;  // Оффсет для нового элемента
  pantoPodstavkaOffset, pantoSkiOffset, pantoLever2BotOffset: Cardinal;
  pantoLever2TopOffset, pantoLever1BotOffset, pantoLever1TopOffset: Cardinal;
  
  // Оффсеты для новых моделей
  skorPrivod1Offset, skorPrivod2Offset: Cardinal;
  pisec1Offset, pisec2Offset: Cardinal;
  strelkaSkorOffset, strelkaSkorTimeOffset: Cardinal;
  
  // Оффсеты для LS моделей
  lsOffset, lsBOffset, lsKOffset, lsKzhOffset, lsZOffset, lsZhOffset: Cardinal;
  
  // KLUB_LS оффсеты
  klubLsOffset, klubLsBOffset, klubLsKOffset, klubLsKzhOffset, klubLsZOffset, klubLsZhOffset: Cardinal;
  
  // Оффсеты для текстур
  kolparaTextureOffset, klubBilTextureOffset, klubBil2TextureOffset: Cardinal;
  pantoTextureOffset, lsTextureOffset: Cardinal;
  
  hasCustomLoc: Boolean;
  
begin
  
  if SettingsLoaded then 
  begin
    Exit;
  end;

  // Читаем настройки
  LocNum := '068';
  LocomotiveType := 822;
  AddToLogFile(EngineLog, 'Установлены значения по умолчанию: LocNum=' + LocNum + ', LocomotiveType=' + IntToStr(LocomotiveType));

  if FileExists('settings.ini') then
  begin
    AddToLogFile(EngineLog, 'Найден файл settings.ini, читаем настройки...');
    try
      AssignFile(f, 'settings.ini');
      Reset(f);
      while not Eof(f) do
      begin
        ReadLn(f, line);
        line := Trim(line);
        if (line = '') or (line[1] = '#') or (line[1] = ';') then Continue;

        equalPos := Pos('=', line);
        if equalPos > 0 then
        begin
          paramName := LowerCase(Trim(Copy(line, 1, equalPos - 1)));
          paramValue := Trim(Copy(line, equalPos + 1, Length(line)));

          if paramName = 'locnum' then
          begin
            LocNum := paramValue;
            AddToLogFile(EngineLog, 'Прочитан LocNum: ' + LocNum);
          end
          else if paramName = 'locomotivetype' then
          begin
            LocomotiveType := StrToInt(paramValue);
            AddToLogFile(EngineLog, 'Прочитан LocomotiveType: ' + IntToStr(LocomotiveType));
          end;
        end;
      end;
      CloseFile(f);
      AddToLogFile(EngineLog, 'settings.ini успешно прочитан');
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'Ошибка чтения settings.ini: ' + E.Message);
        try CloseFile(f); except end;
      end;
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'Файл settings.ini не найден, используем значения по умолчанию');
  end;

  // Загружаем индикаторы
  AddToLogFile(EngineLog, 'Загружаем индикаторные модели...');
  if KlubBilIndPModelID = 0 then
  begin
    KlubBilIndPModelID := LoadModel('booster\klub-bil-ind_p.dmd', 0, False);
    if KlubBilIndPModelID > 0 then
      AddToLogFile(EngineLog, 'Загружен klub-bil-ind_p.dmd, ID: ' + IntToStr(KlubBilIndPModelID))
    else
      AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить klub-bil-ind_p.dmd');
  end;
  if KlubBilIndBModelID = 0 then
  begin
    KlubBilIndBModelID := LoadModel('booster\klub-bil-ind_b.dmd', 0, False);
    if KlubBilIndBModelID > 0 then
      AddToLogFile(EngineLog, 'Загружен klub-bil-ind_b.dmd, ID: ' + IntToStr(KlubBilIndBModelID))
    else
      AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить klub-bil-ind_b.dmd');
  end;
  
  // Определяем тип локомотива и оффсеты
  currentLocType := GetLocomotiveTypeFromMemory;
  AddToLogFile(EngineLog, 'Тип локомотива из памяти: ' + IntToStr(currentLocType));
  
  locFolder := GetLocomotiveFolder(LocomotiveType);
  AddToLogFile(EngineLog, 'Папка локомотива: ' + locFolder);
  
  directoryPath := 'data\' + locFolder + '\' + LocNum + '\';
  AddToLogFile(EngineLog, 'Путь к директории: ' + directoryPath);

  // Проверяем существование кастомной папки
  hasCustomLoc := DirectoryExists(directoryPath + 'loc');
  AddToLogFile(EngineLog, 'Проверка папки loc: ' + directoryPath + 'loc');
  AddToLogFile(EngineLog, 'Папка loc существует: ' + BoolToStr(hasCustomLoc, True));
  
  if not hasCustomLoc then
  begin
    AddToLogFile(EngineLog, 'Кастомная папка loc не найдена, завершаем загрузку');
    SettingsLoaded := True;
    Exit;
  end;
  
  // Вычисляем оффсеты в зависимости от типа локомотива
  AddToLogFile(EngineLog, 'Вычисляем оффсеты для типа локомотива: ' + IntToStr(currentLocType));

pisec1Offset := 10;         // ← Десятичное 6
pisec2Offset := 12;         // ← Десятичное 7
skorPrivod1Offset := 14;    // ← Десятичное 8
skorPrivod2Offset := 16;    // ← Десятичное 9
strelkaSkorOffset := 6;   // ← Десятичное 10
strelkaSkorTimeOffset := 8; // ← Десятичное 12

  case currentLocType of
    23152: begin // 2ЭС5К
      AddToLogFile(EngineLog, 'Настройка оффсетов для 2ЭС5К');
      // Основные оффсеты
      klubBilOffset := $5A;
      klubBilVPssOffset := $00;  // TODO: установить правильный оффсет
      
      // Пантографы
      pantoPodstavkaOffset := $38;
      pantoSkiOffset := $3A;
      pantoLever2BotOffset := $3C;
      pantoLever2TopOffset := $3E;
      pantoLever1BotOffset := $40;
      pantoLever1TopOffset := $42;

      // Оффсеты текстур
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $6A;
      klubBil2TextureOffset := $6A;
      pantoTextureOffset := $3C;
      lsTextureOffset := $36;
      
      AddToLogFile(EngineLog, Format('2ЭС5К оффсеты: klubBil=$%X, klubBilVPss=$%X, kolparaTexture=$%X', 
        [klubBilOffset, klubBilVPssOffset, kolparaTextureOffset]));
    end;

    31714: begin // ЭП1М
      AddToLogFile(EngineLog, '=== НАСТРОЙКА ОФФСЕТОВ ДЛЯ ЭП1М ===');
      
      // Основные оффсеты
      klubBilOffset := $5A;
      klubBilVPssOffset := $00;  // TODO: установить правильный оффсет
      
      // Пантографы
      pantoPodstavkaOffset := $38;
      pantoSkiOffset := $3A;
      pantoLever2BotOffset := $3C;
      pantoLever2TopOffset := $3E;
      pantoLever1BotOffset := $40;
      pantoLever1TopOffset := $42;

      // Оффсеты текстур
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $6A;
      klubBil2TextureOffset := $6A;
      pantoTextureOffset := $3C;
      lsTextureOffset := $36;
      
      AddToLogFile(EngineLog, Format('ЭП1М оффсеты установлены: klubBil=$%X, klubBilVPss=$%X, pantoPodstavka=$%X', 
        [klubBilOffset, klubBilVPssOffset, pantoPodstavkaOffset]));
    end;

    201318: begin // ТЭМ18ДМ
      AddToLogFile(EngineLog, 'Настройка оффсетов для ТЭМ18ДМ');
      // Основные оффсеты
      klubBilOffset := $5A;
      klubBilVPssOffset := $6;  // TODO: установить правильный оффсет

      // Пантографы
      pantoPodstavkaOffset := $38;
      pantoSkiOffset := $3A;
      pantoLever2BotOffset := $3C;
      pantoLever2TopOffset := $3E;
      pantoLever1BotOffset := $40;
      pantoLever1TopOffset := $42;

      // Оффсеты текстур
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $6A;
      klubBil2TextureOffset := $6A;
      pantoTextureOffset := $3C;
      lsTextureOffset := $36;
      
      AddToLogFile(EngineLog, Format('ТЭМ18ДМ оффсеты: klubBil=$%X, klubBilVPss=$%X', 
        [klubBilOffset, klubBilVPssOffset]));
    end;

    811, 882: begin // ВЛ11М
      AddToLogFile(EngineLog, 'Настройка оффсетов для ВЛ11М (тип: ' + IntToStr(currentLocType) + ')');
      // Основные оффсеты
      klubBilOffset := $04;
      klubBilVPssOffset := $6;  // TODO: установить правильный оффсет

      // Пантографы для ВЛ11М: 0BA046C0-0BA04698 = $28
      pantoPodstavkaOffset := $28;
      pantoSkiOffset := $2A;
      pantoLever2BotOffset := $2C;
      pantoLever2TopOffset := $2E;
      pantoLever1BotOffset := $30;
      pantoLever1TopOffset := $32;

      // LS оффсеты
      lsOffset := $12;
      lsBOffset := $14;
      lsKOffset := $16;
      lsKzhOffset := $18;
      lsZOffset := $1C;
      lsZhOffset := $1A;

      // KLUB_LS оффсеты
      klubLsOffset := $12;
      klubLsBOffset := $14;
      klubLsKOffset := $16;
      klubLsKzhOffset := $18;
      klubLsZOffset := $1C;
      klubLsZhOffset := $1A;

      // Оффсеты текстур
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $34;
      klubBil2TextureOffset := $00;
      pantoTextureOffset := $3C;
      lsTextureOffset := $36;
      
      AddToLogFile(EngineLog, Format('ВЛ11М оффсеты: klubBil=$%X, pantoPodstavka=$%X, ls=$%X', 
        [klubBilOffset, pantoPodstavkaOffset, lsOffset]));
    end;

    880, 885: begin // Остальные ВЛ локомотивы
      AddToLogFile(EngineLog, 'Настройка оффсетов для ВЛ локомотива (тип: ' + IntToStr(currentLocType) + ')');
      // Основные оффсеты
      klubBilOffset := $04;
      klubBilVPssOffset := $6;  // TODO: установить правильный оффсет
      
      // Пантографы
      pantoPodstavkaOffset := $38;
      pantoSkiOffset := $3A;
      pantoLever2BotOffset := $3C;
      pantoLever2TopOffset := $3E;
      pantoLever1BotOffset := $40;
      pantoLever1TopOffset := $42;

      // LS оффсеты
      lsOffset := $12;
      lsBOffset := $14;
      lsKOffset := $16;
      lsKzhOffset := $18;
      lsZOffset := $1C;
      lsZhOffset := $1A;
      
      // KLUB_LS оффсеты
      klubLsOffset := $12;
      klubLsBOffset := $14;
      klubLsKOffset := $16;
      klubLsKzhOffset := $18;
      klubLsZOffset := $1C;
      klubLsZhOffset := $1A;

      // Оффсеты текстур
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $34;
      klubBil2TextureOffset := $00;
      pantoTextureOffset := $3C;

      skorModelOffset := $04;      // из (*off_74B56C + 4)
      skorTextureOffset := $32;    // из (*off_74B8AC + 50) = $32

      if currentLocType = 880 then
      begin
        lsTextureOffset := $36;
        AddToLogFile(EngineLog, 'ВЛ80Т: lsTextureOffset = $36');
      end
      else
      begin
        lsTextureOffset := $36;
        AddToLogFile(EngineLog, 'ВЛ85: lsTextureOffset = $38');
      end;
      
      AddToLogFile(EngineLog, Format('ВЛ оффсеты: klubBil=$%X, ls=$%X, lsTexture=$%X', 
        [klubBilOffset, lsOffset, lsTextureOffset]));
    end;

    else begin // Остальные локомотивы (ЧС2К и др)
      AddToLogFile(EngineLog, 'Настройка оффсетов для прочих локомотивов (тип: ' + IntToStr(currentLocType) + ')');
      // Основные оффсеты
      klubBilOffset := $04;
      klubBilVPssOffset := $6;  // TODO: установить правильный оффсет
      
      // Пантографы
      pantoPodstavkaOffset := $28;
      pantoSkiOffset := $2A;
      pantoLever2BotOffset := $2C;
      pantoLever2TopOffset := $2E;
      pantoLever1BotOffset := $30;
      pantoLever1TopOffset := $32;

      // LS оффсеты
      lsOffset := $12;
      lsBOffset := $14;
      lsKOffset := $16;
      lsKzhOffset := $18;
      lsZOffset := $1C;
      lsZhOffset := $1A;
      
      // KLUB_LS оффсеты
      klubLsOffset := $12;
      klubLsBOffset := $14;
      klubLsKOffset := $16;
      klubLsKzhOffset := $18;
      klubLsZOffset := $1C;
      klubLsZhOffset := $1A;

//      if currentLocType = 812 then
//      begin
//        
//      end;
//

      // Оффсеты текстур
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $34;
      klubBil2TextureOffset := $00;
      pantoTextureOffset := $3C;
      lsTextureOffset := $36;
      
      AddToLogFile(EngineLog, Format('Общие оффсеты: klubBil=$%X, pantoPodstavka=$%X, ls=$%X', 
        [klubBilOffset, pantoPodstavkaOffset, lsOffset]));
    end;
  end;
  
  // Читаем klubFlag
  try
    klubFlag := PByte(Pointer($7498A8))^;
    AddToLogFile(EngineLog, 'Прочитан klubFlag: ' + IntToStr(klubFlag));
  except
    on E: Exception do
    begin
      klubFlag := 1;
      AddToLogFile(EngineLog, 'Ошибка чтения klubFlag: ' + E.Message + ', установлен по умолчанию: 1');
    end;
  end;
  
  // Читаем базовый адрес структуры
  try
    baseStructAddr := PCardinal(Pointer($00400000 + $8D10D70))^;
    AddToLogFile(EngineLog, 'Базовый адрес структуры: $' + IntToHex(baseStructAddr, 8));
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'КРИТИЧЕСКАЯ ОШИБКА: Не удалось прочитать базовый адрес структуры: ' + E.Message);
      Exit;
    end;
  end;
  
  // ===== ЗАГРУЖАЕМ КОЛПАРА =====
  AddToLogFile(EngineLog, '=== ЗАГРУЖАЕМ КОЛПАРА ===');
  kolparaModelPath := directoryPath + 'loc\kolpara.dmd';
  kolparaTexturePath := directoryPath + 'loc\kolpara.bmp';
  
  AddToLogFile(EngineLog, 'Проверяем электрический колпара...');
  AddToLogFile(EngineLog, 'Model: ' + kolparaModelPath);
  AddToLogFile(EngineLog, 'Texture: ' + kolparaTexturePath);

  if FileExists(kolparaModelPath) and FileExists(kolparaTexturePath) then
  begin
    AddToLogFile(EngineLog, 'Найдены файлы колпара, загружаем...');
    try
      KolparaModelID := LoadModel(kolparaModelPath, 0, False);
      if KolparaModelID > 0 then
        AddToLogFile(EngineLog, 'Колпара модель загружена, ID: ' + IntToStr(KolparaModelID))
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить модель колпара');
        
      KolparaTextureID := LoadTextureFromFile(kolparaTexturePath, 0, -1);
      if KolparaTextureID > 0 then
        AddToLogFile(EngineLog, 'Колпара текстура загружена, ID: ' + IntToStr(KolparaTextureID))
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить текстуру колпара');
      
      if (KolparaModelID > 0) and (KolparaTextureID > 0) then
      begin
//        // Колпара модель
//        if (currentLocType = 23152) or (currentLocType = 31714) then
//        begin
//          modelAddr := Pointer(baseStructAddr + $68); // $34 * 2 для 2ЭС5К/ЭП1М
//          AddToLogFile(EngineLog, 'Используем адрес модели для 2ЭС5К/ЭП1М: $' + IntToHex(Cardinal(modelAddr), 8));
//        end
//        else
        begin
          modelAddr := Pointer(baseStructAddr + $34); // оригинальный
          AddToLogFile(EngineLog, 'Используем стандартный адрес модели: $' + IntToHex(Cardinal(modelAddr), 8));
        end;
          
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KolparaModelID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Колпара модель записана в память по адресу: $' + IntToHex(Cardinal(modelAddr), 8));
        end
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи модели колпара');
        
        // Колпара текстура
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + kolparaTextureOffset);
        AddToLogFile(EngineLog, 'Адрес текстуры колпара: $' + IntToHex(Cardinal(textureAddr), 8) + ' (offset: $' + IntToHex(kolparaTextureOffset, 2) + ')');
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(KolparaTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Колпара текстура записана в память');
        end
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи текстуры колпара');
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке колпара: ' + E.Message);
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'Файлы колпара не найдены, пропускаем');
  end;

// ===== ЗАГРУЖАЕМ _SKOR МОДЕЛИ ДЛЯ ВЛ80Т/ВЛ85 =====
if (currentLocType = 880) or (currentLocType = 885) then
begin
  AddToLogFile(EngineLog, '=== ЗАГРУЗКА _SKOR ДЛЯ ВЛ80Т/ВЛ85 ===');
  
  // Пути к _skor файлам
  skorModelPath := directoryPath + 'loc\_skor.dmd';
  skorTexturePath := directoryPath + 'loc\_skor.bmp';
  
  AddToLogFile(EngineLog, 'SkorModel: ' + skorModelPath);
  AddToLogFile(EngineLog, 'SkorTexture: ' + skorTexturePath);
  
  // Загружаем _skor модель
  if FileExists(skorModelPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем _skor.dmd...');
    try
      SkorModelID := LoadModel(skorModelPath, 0, False);
      if SkorModelID > 0 then
      begin
        AddToLogFile(EngineLog, '_skor.dmd загружен, ID: ' + IntToStr(SkorModelID));
        
        // Записываем модель в память по оффсету $04
        modelAddr := Pointer(baseStructAddr + skorModelOffset);
        AddToLogFile(EngineLog, 'Адрес _skor модели: $' + IntToHex(Cardinal(modelAddr), 8) + ' (offset: $' + IntToHex(skorModelOffset, 2) + ')');
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(SkorModelID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, '_skor модель записана в память');
        end;
      end
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить _skor.dmd');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке _skor модели: ' + E.Message);
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, '_skor.dmd не найден');
  end;
  
  // Загружаем _skor текстуру
  if FileExists(skorTexturePath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем _skor.bmp...');
    try
      SkorTextureID := LoadTextureFromFile(skorTexturePath, 0, -1);
      if SkorTextureID > 0 then
      begin
        AddToLogFile(EngineLog, '_skor.bmp загружена, ID: ' + IntToStr(SkorTextureID));
        
        // Записываем текстуру в память по оффсету $32 (50)
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + skorTextureOffset);
        AddToLogFile(EngineLog, 'Адрес _skor текстуры: $' + IntToHex(Cardinal(textureAddr), 8) + ' (offset: $' + IntToHex(skorTextureOffset, 2) + ')');
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(SkorTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, '_skor текстура записана в память');
        end;
      end
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить _skor.bmp');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке _skor текстуры: ' + E.Message);
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, '_skor.bmp не найдена');
  end;
end;

  // ===== ЗАГРУЖАЕМ КЛУБ =====
  if klubFlag = 1 then
  begin
    AddToLogFile(EngineLog, Format('=== ЗАГРУЗКА КЛУБА ДЛЯ ЛОКОМОТИВА %d ===', [currentLocType]));
    
    // Основной клуб
    klubModelPath := directoryPath + 'loc\klub_bil_v.dmd';
    klubTexturePath := directoryPath + 'loc\klub_bil.bmp';
    klubTexture2Path := directoryPath + 'loc\klub_bil2.bmp';
    
    // Новый элемент klub_bil_v_pss.dmd
    klubBilVPssPath := directoryPath + 'loc\klub_bil_v_pss.dmd';
    
    AddToLogFile(EngineLog, 'Проверяем основные файлы клуба...');
    AddToLogFile(EngineLog, 'Model: ' + klubModelPath);
    AddToLogFile(EngineLog, 'Texture: ' + klubTexturePath);
    AddToLogFile(EngineLog, 'Texture2: ' + klubTexture2Path);
    AddToLogFile(EngineLog, 'PSS Model: ' + klubBilVPssPath);
    
    // Если стандартных файлов нет, пробуем альтернативные
    if not (FileExists(klubModelPath) and FileExists(klubTexturePath)) then
    begin
      AddToLogFile(EngineLog, 'Стандартные файлы не найдены, переключаемся на альтернативные');
      klubModelPath := directoryPath + 'loc\klub_bil_v2.dmd';
      klubTexturePath := directoryPath + 'loc\klub_bil2.bmp';
      AddToLogFile(EngineLog, 'Alt Model: ' + klubModelPath);
      AddToLogFile(EngineLog, 'Alt Texture: ' + klubTexturePath);
    end;

    // Загружаем основной клуб
    if FileExists(klubModelPath) and FileExists(klubTexturePath) then
    begin
      AddToLogFile(EngineLog, 'Найдены файлы основного клуба, загружаем...');
      try
        KlubModelID := LoadModel(klubModelPath, 0, False);
        if KlubModelID > 0 then
          AddToLogFile(EngineLog, 'Клуб модель загружена, ID: ' + IntToStr(KlubModelID))
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить модель клуба');
          
        KlubTextureID := LoadTextureFromFile(klubTexturePath, 0, -1);
        if KlubTextureID > 0 then
          AddToLogFile(EngineLog, 'Клуб текстура загружена, ID: ' + IntToStr(KlubTextureID))
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить текстуру клуба');
        
        if (KlubModelID > 0) and (KlubTextureID > 0) then
        begin
          AddToLogFile(EngineLog, Format('Основной клуб загружен: Model=%d, Texture=%d', [KlubModelID, KlubTextureID]));
          
          // Клуб модель
          modelAddr := Pointer(baseStructAddr + klubBilOffset);
          AddToLogFile(EngineLog, 'Адрес модели клуба: $' + IntToHex(Cardinal(modelAddr), 8) + ' (offset: $' + IntToHex(klubBilOffset, 2) + ')');
          if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
          begin
            PWord(modelAddr)^ := Word(KlubModelID);
            VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
            AddToLogFile(EngineLog, 'Клуб модель записана в память');
          end
          else
            AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи модели клуба');
          
          // Клуб текстура
          textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + klubBilTextureOffset);
          AddToLogFile(EngineLog, 'Адрес текстуры клуба: $' + IntToHex(Cardinal(textureAddr), 8) + ' (offset: $' + IntToHex(klubBilTextureOffset, 2) + ')');
          if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
          begin
            PWord(textureAddr)^ := Word(KlubTextureID);
            VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
            AddToLogFile(EngineLog, 'Клуб текстура записана в память');
          end
          else
            AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи текстуры клуба');
        end;
      except
        on E: Exception do
          AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке основного клуба: ' + E.Message);
      end;
    end
    else
    begin
      AddToLogFile(EngineLog, 'Файлы основного клуба не найдены');
    end;
    
    // Загружаем klub_bil_v_pss.dmd
    if FileExists(klubBilVPssPath) and (klubBilVPssOffset <> $00) then
    begin
      AddToLogFile(EngineLog, 'Найден klub_bil_v_pss.dmd, загружаем...');
      try
        KlubBilVPssID := LoadModel(klubBilVPssPath, 0, False);
        if KlubBilVPssID > 0 then
        begin
          AddToLogFile(EngineLog, Format('klub_bil_v_pss загружен: ID=%d', [KlubBilVPssID]));
          
          modelAddr := Pointer(baseStructAddr + klubBilVPssOffset);
          AddToLogFile(EngineLog, 'Адрес PSS модели: $' + IntToHex(Cardinal(modelAddr), 8) + ' (offset: $' + IntToHex(klubBilVPssOffset, 2) + ')');
          if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
          begin
            PWord(modelAddr)^ := Word(KlubBilVPssID);
            VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
            AddToLogFile(EngineLog, Format('klub_bil_v_pss записан по адресу $%X', [Cardinal(modelAddr)]));
          end
          else
            AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи PSS модели');
        end
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить klub_bil_v_pss.dmd');
      except
        on E: Exception do
          AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке klub_bil_v_pss: ' + E.Message);
      end;
    end
    else
    begin
      if not FileExists(klubBilVPssPath) then
        AddToLogFile(EngineLog, 'klub_bil_v_pss.dmd не найден')
      else if klubBilVPssOffset = $00 then
        AddToLogFile(EngineLog, 'Оффсет для klub_bil_v_pss не установлен для данного типа локомотива');
    end;
    
    // Вторая текстура клуба
    if FileExists(klubTexture2Path) then
    begin
      AddToLogFile(EngineLog, 'Найдена вторая текстура клуба, загружаем...');
      try
        KlubTexture2ID := LoadTextureFromFile(klubTexture2Path, 0, -1);
        if KlubTexture2ID > 0 then
        begin
          AddToLogFile(EngineLog, 'Вторая текстура клуба загружена, ID: ' + IntToStr(KlubTexture2ID));
          textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + klubBil2TextureOffset);
          AddToLogFile(EngineLog, 'Адрес второй текстуры: $' + IntToHex(Cardinal(textureAddr), 8) + ' (offset: $' + IntToHex(klubBil2TextureOffset, 2) + ')');
          if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
          begin
            PWord(textureAddr)^ := Word(KlubTexture2ID);
            VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
            AddToLogFile(EngineLog, 'Вторая текстура клуба записана в память');
          end
          else
            AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи второй текстуры клуба');
        end
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить вторую текстуру клуба');
      except
        on E: Exception do
          AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке второй текстуры клуба: ' + E.Message);
      end;
    end
    else
    begin
      AddToLogFile(EngineLog, 'Вторая текстура клуба не найдена');
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, Format('klubFlag = %d, загрузка клуба пропущена', [klubFlag]));
  end;
  
  // ===== ЗАГРУЖАЕМ ПАНТОГРАФЫ =====
  AddToLogFile(EngineLog, '=== ЗАГРУЖАЕМ ПАНТОГРАФЫ ===');
  pantoPodstavkaPath := directoryPath + 'loc\tp_podstavka.dmd';
  pantoSkiPath := directoryPath + 'loc\tp_ski.dmd';
  pantoLever2BotPath := directoryPath + 'loc\l13u_lever1bot.dmd';
  pantoLever2TopPath := directoryPath + 'loc\l13u_lever1top.dmd';
  pantoLever1BotPath := directoryPath + 'loc\l13u_lever2bot.dmd';
  pantoLever1TopPath := directoryPath + 'loc\l13u_lever2top.dmd';
  pantoTexturePath := directoryPath + 'loc\2sls1.bmp';

  AddToLogFile(EngineLog, 'Пути к файлам пантографа:');
  AddToLogFile(EngineLog, 'Podstavka: ' + pantoPodstavkaPath);
  AddToLogFile(EngineLog, 'Ski: ' + pantoSkiPath);
  AddToLogFile(EngineLog, 'Lever2Bot: ' + pantoLever2BotPath);
  AddToLogFile(EngineLog, 'Lever2Top: ' + pantoLever2TopPath);
  AddToLogFile(EngineLog, 'Lever1Bot: ' + pantoLever1BotPath);
  AddToLogFile(EngineLog, 'Lever1Top: ' + pantoLever1TopPath);
  AddToLogFile(EngineLog, 'Texture: ' + pantoTexturePath);

if not FileExists(pantoSkiPath) then
begin
  pantoSkiPath := directoryPath + 'loc\l13u_ski.dmd';
  AddToLogFile(EngineLog, 'tp_ski не найден, используем fallback: ' + pantoSkiPath);
end
else if not FileExists(directoryPath + 'loc\l13u_ski.dmd') then
begin
  // tp_ski.dmd существует, это нормально
  AddToLogFile(EngineLog, 'Используем tp_ski.dmd');
end;
  // Fallback на tp_ файлы если l13u_ не найдены
  if not FileExists(pantoLever2BotPath) then
  begin
    pantoLever2BotPath := directoryPath + 'loc\tp_lever1bot.dmd';
    AddToLogFile(EngineLog, 'l13u_lever1bot не найден, используем fallback: ' + pantoLever2BotPath);
  end;
  if not FileExists(pantoLever2TopPath) then
  begin
    pantoLever2TopPath := directoryPath + 'loc\tp_lever1top.dmd';
    AddToLogFile(EngineLog, 'l13u_lever1top не найден, используем fallback: ' + pantoLever2TopPath);
  end;
  if not FileExists(pantoLever1BotPath) then
  begin
    pantoLever1BotPath := directoryPath + 'loc\tp_lever2bot.dmd';
    AddToLogFile(EngineLog, 'l13u_lever2bot не найден, используем fallback: ' + pantoLever1BotPath);
  end;
  if not FileExists(pantoLever1TopPath) then
  begin
    pantoLever1TopPath := directoryPath + 'loc\tp_lever2top.dmd';
    AddToLogFile(EngineLog, 'l13u_lever2top не найден, используем fallback: ' + pantoLever1TopPath);
  end;
  
  // Загружаем текстуру пантографа
  if FileExists(pantoTexturePath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем текстуру пантографа...');
    try
      PantoTextureID := LoadTextureFromFile(pantoTexturePath, 0, -1);
      if PantoTextureID > 0 then
      begin
        AddToLogFile(EngineLog, 'Текстура пантографа загружена, ID: ' + IntToStr(PantoTextureID));
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + pantoTextureOffset);
        AddToLogFile(EngineLog, 'Адрес текстуры пантографа: $' + IntToHex(Cardinal(textureAddr), 8) + ' (offset: $' + IntToHex(pantoTextureOffset, 2) + ')');
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(PantoTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Текстура пантографа записана в память');
        end
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи текстуры пантографа');
      end
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить текстуру пантографа');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке текстуры пантографа: ' + E.Message);
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'Текстура пантографа не найдена');
  end;
  
  // Загружаем все части пантографа
//  if FileExists(pantoPodstavkaPath) then
//  begin
//    AddToLogFile(EngineLog, 'Загружаем подставку пантографа...');
//    try
//      PantoPodstavkaID := LoadModel(pantoPodstavkaPath, 0, False);
//      if PantoPodstavkaID > 0 then
//      begin
//        AddToLogFile(EngineLog, 'Подставка пантографа загружена, ID: ' + IntToStr(PantoPodstavkaID));
//        modelAddr := Pointer(baseStructAddr + pantoPodstavkaOffset);
//        AddToLogFile(EngineLog, 'Адрес подставки: $' + IntToHex(Cardinal(modelAddr), 8) + ' (offset: $' + IntToHex(pantoPodstavkaOffset, 2) + ')');
//        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
//        begin
//          PWord(modelAddr)^ := Word(PantoPodstavkaID);
//          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
//          AddToLogFile(EngineLog, 'Подставка пантографа записана в память');
//        end
//        else
//          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось записать подставку пантографа в память');
//      end
//      else
//        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить подставку пантографа');
//    except
//      on E: Exception do
//        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке подставки пантографа: ' + E.Message);
//    end;
//  end
//  else
//  begin
//    AddToLogFile(EngineLog, 'Подставка пантографа не найдена');
//  end;
  
  if FileExists(pantoSkiPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем лыжу пантографа...');
    try
      PantoSkiID := LoadModel(pantoSkiPath, 0, False);
      if PantoSkiID > 0 then
      begin
        AddToLogFile(EngineLog, 'Лыжа пантографа загружена, ID: ' + IntToStr(PantoSkiID));
        modelAddr := Pointer(baseStructAddr + pantoSkiOffset);
        AddToLogFile(EngineLog, 'Адрес лыжи: $' + IntToHex(Cardinal(modelAddr), 8) + ' (offset: $' + IntToHex(pantoSkiOffset, 2) + ')');
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoSkiID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Лыжа пантографа записана в память');
        end
        else
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось записать лыжу пантографа в память');
      end
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить лыжу пантографа');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке лыжи пантографа: ' + E.Message);
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'Лыжа пантографа не найдена');
  end;
  
  if FileExists(pantoLever2BotPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем lever2bot пантографа...');
    try
      PantoLever2BotID := LoadModel(pantoLever2BotPath, 0, False);
      if PantoLever2BotID > 0 then
      begin
        AddToLogFile(EngineLog, 'Lever2bot пантографа загружен, ID: ' + IntToStr(PantoLever2BotID));
        modelAddr := Pointer(baseStructAddr + pantoLever2BotOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever2BotID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Lever2bot пантографа записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке lever2bot: ' + E.Message);
    end;
  end;
  
  if FileExists(pantoLever2TopPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем lever2top пантографа...');
    try
      PantoLever2TopID := LoadModel(pantoLever2TopPath, 0, False);
      if PantoLever2TopID > 0 then
      begin
        AddToLogFile(EngineLog, 'Lever2top пантографа загружен, ID: ' + IntToStr(PantoLever2TopID));
        modelAddr := Pointer(baseStructAddr + pantoLever2TopOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever2TopID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Lever2top пантографа записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке lever2top: ' + E.Message);
    end;
  end;
  
  if FileExists(pantoLever1BotPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем lever1bot пантографа...');
    try
      PantoLever1BotID := LoadModel(pantoLever1BotPath, 0, False);
      if PantoLever1BotID > 0 then
      begin
        AddToLogFile(EngineLog, 'Lever1bot пантографа загружен, ID: ' + IntToStr(PantoLever1BotID));
        modelAddr := Pointer(baseStructAddr + pantoLever1BotOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever1BotID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Lever1bot пантографа записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке lever1bot: ' + E.Message);
    end;
  end;
  
  if FileExists(pantoLever1TopPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем lever1top пантографа...');
    try
      PantoLever1TopID := LoadModel(pantoLever1TopPath, 0, False);
      if PantoLever1TopID > 0 then
      begin
        AddToLogFile(EngineLog, 'Lever1top пантографа загружен, ID: ' + IntToStr(PantoLever1TopID));
        modelAddr := Pointer(baseStructAddr + pantoLever1TopOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever1TopID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Lever1top пантографа записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке lever1top: ' + E.Message);
    end;
  end;
  
  // ===== ЗАГРУЖАЕМ НОВЫЕ МОДЕЛИ =====
  AddToLogFile(EngineLog, '=== ЗАГРУЖАЕМ НОВЫЕ МОДЕЛИ ===');
// ===== ЗАГРУЗКА И ПРИМЕНЕНИЕ _skor.bmp ДЛЯ ВСЕХ МОДЕЛЕЙ ОДНОВРЕМЕННО =====

// ===== ЗАГРУЖАЕМ МОДЕЛИ (как обычно) =====
skorPrivod1Path := directoryPath + 'loc\skor_privod1.dmd';
skorPrivod2Path := directoryPath + 'loc\skor_privod2.dmd';
pisec1Path := directoryPath + 'loc\pisec1.dmd';
pisec2Path := directoryPath + 'loc\pisec2.dmd';
strelkaSkorPath := directoryPath + 'loc\strelka_skor.dmd';
strelkaSkorTimePath := directoryPath + 'loc\strelka_skor_time.dmd';

// Загружаем skor_privod1.dmd
if FileExists(skorPrivod1Path) then
begin
  AddToLogFile(EngineLog, 'Загружаем skor_privod1.dmd...');
  try
    SkorPrivod1ID := LoadModel(skorPrivod1Path, 0, False);
    if SkorPrivod1ID > 0 then
    begin
      AddToLogFile(EngineLog, 'skor_privod1.dmd загружен, ID: ' + IntToStr(SkorPrivod1ID));
      modelAddr := Pointer(baseStructAddr + skorPrivod1Offset);
      if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PWord(modelAddr)^ := Word(SkorPrivod1ID);
        VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'skor_privod1 записан в память');
      end;
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке skor_privod1: ' + E.Message);
  end;
end;

// Загружаем skor_privod2.dmd
if FileExists(skorPrivod2Path) then
begin
  AddToLogFile(EngineLog, 'Загружаем skor_privod2.dmd...');
  try
    SkorPrivod2ID := LoadModel(skorPrivod2Path, 0, False);
    if SkorPrivod2ID > 0 then
    begin
      AddToLogFile(EngineLog, 'skor_privod2.dmd загружен, ID: ' + IntToStr(SkorPrivod2ID));
      modelAddr := Pointer(baseStructAddr + skorPrivod2Offset);
      if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PWord(modelAddr)^ := Word(SkorPrivod2ID);
        VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'skor_privod2 записан в память');
      end;
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке skor_privod2: ' + E.Message);
  end;
end;

// Загружаем pisec1.dmd
if FileExists(pisec1Path) then
begin
  AddToLogFile(EngineLog, 'Загружаем pisec1.dmd...');
  try
    Pisec1ID := LoadModel(pisec1Path, 0, False);
    if Pisec1ID > 0 then
    begin
      AddToLogFile(EngineLog, 'pisec1.dmd загружен, ID: ' + IntToStr(Pisec1ID));
      modelAddr := Pointer(baseStructAddr + pisec1Offset);
      if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PWord(modelAddr)^ := Word(Pisec1ID);
        VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'pisec1 записан в память');
      end;
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке pisec1: ' + E.Message);
  end;
end;

// Загружаем pisec2.dmd
if FileExists(pisec2Path) then
begin
  AddToLogFile(EngineLog, 'Загружаем pisec2.dmd...');
  try
    Pisec2ID := LoadModel(pisec2Path, 0, False);
    if Pisec2ID > 0 then
    begin
      AddToLogFile(EngineLog, 'pisec2.dmd загружен, ID: ' + IntToStr(Pisec2ID));
      modelAddr := Pointer(baseStructAddr + pisec2Offset);
      if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PWord(modelAddr)^ := Word(Pisec2ID);
        VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'pisec2 записан в память');
      end;
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке pisec2: ' + E.Message);
  end;
end;

// Загружаем strelka_skor.dmd
if FileExists(strelkaSkorPath) then
begin
  AddToLogFile(EngineLog, 'Загружаем strelka_skor.dmd...');
  try
    StrelkaSkorID := LoadModel(strelkaSkorPath, 0, False);
    if StrelkaSkorID > 0 then
    begin
      AddToLogFile(EngineLog, 'strelka_skor.dmd загружена, ID: ' + IntToStr(StrelkaSkorID));
      modelAddr := Pointer(baseStructAddr + strelkaSkorOffset);
      if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PWord(modelAddr)^ := Word(StrelkaSkorID);
        VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'strelka_skor записана в память');
      end;
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке strelka_skor: ' + E.Message);
  end;
end;

// Загружаем strelka_skor_time.dmd
if FileExists(strelkaSkorTimePath) then
begin
  AddToLogFile(EngineLog, 'Загружаем strelka_skor_time.dmd...');
  try
    StrelkaSkorTimeID := LoadModel(strelkaSkorTimePath, 0, False);
    if StrelkaSkorTimeID > 0 then
    begin
      AddToLogFile(EngineLog, 'strelka_skor_time.dmd загружена, ID: ' + IntToStr(StrelkaSkorTimeID));
      modelAddr := Pointer(baseStructAddr + strelkaSkorTimeOffset);
      if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PWord(modelAddr)^ := Word(StrelkaSkorTimeID);
        VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'strelka_skor_time записана в память');
      end;
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке strelka_skor_time: ' + E.Message);
  end;
end;

// ===== ЗАГРУЖАЕМ И ПРИМЕНЯЕМ _skor.bmp ДЛЯ ВСЕХ МОДЕЛЕЙ СРАЗУ =====
skorTexturePath := directoryPath + 'loc\_skor.bmp';

if FileExists(skorTexturePath) then
begin
  AddToLogFile(EngineLog, '=== ЗАГРУЖАЕМ _skor.bmp ДЛЯ ВСЕХ МОДЕЛЕЙ ===');
  AddToLogFile(EngineLog, 'Путь к текстуре: ' + skorTexturePath);
  
  try
    SkorSharedTextureID := LoadTextureFromFile(skorTexturePath, 0, -1);
    if SkorSharedTextureID > 0 then
    begin
      AddToLogFile(EngineLog, '_skor.bmp загружена, ID: ' + IntToStr(SkorSharedTextureID));
      
      // ЗАПИСЫВАЕМ ПО ОФФСЕТУ $32 (50 в десятичном) - КАК В ДЕКОМПИЛЯЦИИ
      textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + $32);
      AddToLogFile(EngineLog, 'Адрес для записи текстуры: $' + IntToHex(Cardinal(textureAddr), 8) + ' (оффсет $32)');
      
      if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PWord(textureAddr)^ := Word(SkorSharedTextureID);
        VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
        
        AddToLogFile(EngineLog, '*** _skor.bmp ПРИМЕНЕНА КО ВСЕМ МОДЕЛЯМ ***');
        AddToLogFile(EngineLog, 'Текстура ID ' + IntToStr(SkorSharedTextureID) + ' записана по адресу $' + IntToHex(Cardinal(textureAddr), 8));
        AddToLogFile(EngineLog, 'Модели которые получили текстуру:');
        if SkorPrivod1ID > 0 then AddToLogFile(EngineLog, '- skor_privod1.dmd (ID: ' + IntToStr(SkorPrivod1ID) + ')');
        if SkorPrivod2ID > 0 then AddToLogFile(EngineLog, '- skor_privod2.dmd (ID: ' + IntToStr(SkorPrivod2ID) + ')');
        if Pisec1ID > 0 then AddToLogFile(EngineLog, '- pisec1.dmd (ID: ' + IntToStr(Pisec1ID) + ')');
        if Pisec2ID > 0 then AddToLogFile(EngineLog, '- pisec2.dmd (ID: ' + IntToStr(Pisec2ID) + ')');
        if StrelkaSkorID > 0 then AddToLogFile(EngineLog, '- strelka_skor.dmd (ID: ' + IntToStr(StrelkaSkorID) + ')');
        if StrelkaSkorTimeID > 0 then AddToLogFile(EngineLog, '- strelka_skor_time.dmd (ID: ' + IntToStr(StrelkaSkorTimeID) + ')');
        
      end
      else
      begin
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось получить доступ к памяти для записи текстуры');
      end;
    end
    else
    begin
      AddToLogFile(EngineLog, 'ОШИБКА: LoadTextureFromFile вернул 0 для _skor.bmp');
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'КРИТИЧЕСКАЯ ОШИБКА загрузки _skor.bmp: ' + E.Message);
  end;
end
else
begin
  AddToLogFile(EngineLog, 'ФАЙЛ НЕ НАЙДЕН: ' + skorTexturePath);
end;

AddToLogFile(EngineLog, '=== ОБРАБОТКА _skor.bmp ЗАВЕРШЕНА ===');

// ===== ДОБАВИТЬ В VAR СЕКЦИЮ =====
{
var
  SkorSharedTextureID: Cardinal = 0;  // Общая текстура для всех моделей
}

// ===== ОБЪЯСНЕНИЕ =====
{
Из декомпилированного кода видно, что все элементы спидометра используют
один и тот же слот текстуры: *off_74B8AC + 50 (что равно оффсету $32).

Поэтому достаточно:
1. Загрузить все .dmd модели в свои слоты
2. Загрузить _skor.bmp один раз  
3. Записать ID текстуры по оффсету $32
4. Все модели автоматически получат эту текстуру
}

  AddToLogFile(EngineLog, 'ID новых моделей:');
  AddToLogFile(EngineLog, 'SkorPrivod1: ' + IntToStr(SkorPrivod1ID));
  AddToLogFile(EngineLog, 'SkorPrivod2: ' + IntToStr(SkorPrivod2ID));
  AddToLogFile(EngineLog, 'Pisec1: ' + IntToStr(Pisec1ID));
  AddToLogFile(EngineLog, 'Pisec2: ' + IntToStr(Pisec2ID));
  AddToLogFile(EngineLog, 'StrelkaSkor: ' + IntToStr(StrelkaSkorID));
  AddToLogFile(EngineLog, 'StrelkaSkorTime: ' + IntToStr(StrelkaSkorTimeID));

  // ===== ЗАГРУЖАЕМ LS МОДЕЛИ =====
  lsPath := directoryPath + 'loc\ls.dmd';
  lsBPath := directoryPath + 'loc\ls_b.dmd';
  lsKPath := directoryPath + 'loc\ls_k.dmd';
  lsKzhPath := directoryPath + 'loc\ls_kzh.dmd';
  lsZPath := directoryPath + 'loc\ls_z.dmd';
  lsZhPath := directoryPath + 'loc\ls_zh.dmd';
  lsTexturePath := directoryPath + 'loc\ls.bmp';

  AddToLogFile(EngineLog, '=== ЗАГРУЗКА LS МОДЕЛЕЙ ===');
  AddToLogFile(EngineLog, 'Пути к LS моделям:');
  AddToLogFile(EngineLog, 'LS: ' + lsPath);
  AddToLogFile(EngineLog, 'LS_B: ' + lsBPath);
  AddToLogFile(EngineLog, 'LS_K: ' + lsKPath);
  AddToLogFile(EngineLog, 'LS_KZH: ' + lsKzhPath);
  AddToLogFile(EngineLog, 'LS_Z: ' + lsZPath);
  AddToLogFile(EngineLog, 'LS_ZH: ' + lsZhPath);
  AddToLogFile(EngineLog, 'LS Texture: ' + lsTexturePath);
  
  // Загружаем текстуру LS
  if FileExists(lsTexturePath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем LS текстуру...');
    try
      LsTextureID := LoadTextureFromFile(lsTexturePath, 0, -1);
      if LsTextureID > 0 then
      begin
        AddToLogFile(EngineLog, 'LS текстура загружена, ID: ' + IntToStr(LsTextureID));
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + lsTextureOffset);
        AddToLogFile(EngineLog, 'Адрес LS текстуры: $' + IntToHex(Cardinal(textureAddr), 8) + ' (offset: $' + IntToHex(lsTextureOffset, 2) + ')');
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(LsTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'LS текстура записана в память');
        end;
      end
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить LS текстуру');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке LS текстуры: ' + E.Message);
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'LS текстура не найдена');
  end;
  
  // Загружаем ls.dmd
  if FileExists(lsPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем ls.dmd...');
    try
      LsID := LoadModel(lsPath, 0, False);
      if LsID > 0 then
      begin
        AddToLogFile(EngineLog, 'ls.dmd загружен, ID: ' + IntToStr(LsID));
        modelAddr := Pointer(baseStructAddr + lsOffset);
        AddToLogFile(EngineLog, 'Адрес LS: $' + IntToHex(Cardinal(modelAddr), 8) + ' (offset: $' + IntToHex(lsOffset, 2) + ')');
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'ls.dmd записан в память');
        end;
      end
      else
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить ls.dmd');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ls.dmd: ' + E.Message);
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'ls.dmd не найден');
  end;
  
  // Аналогично для остальных LS моделей...
  if FileExists(lsBPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем ls_b.dmd...');
    try
      LsBID := LoadModel(lsBPath, 0, False);
      if LsBID > 0 then
      begin
        AddToLogFile(EngineLog, 'ls_b.dmd загружен, ID: ' + IntToStr(LsBID));
        modelAddr := Pointer(baseStructAddr + lsBOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsBID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'ls_b.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ls_b: ' + E.Message);
    end;
  end;
  
  if FileExists(lsKPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем ls_k.dmd...');
    try
      LsKID := LoadModel(lsKPath, 0, False);
      if LsKID > 0 then
      begin
        AddToLogFile(EngineLog, 'ls_k.dmd загружен, ID: ' + IntToStr(LsKID));
        modelAddr := Pointer(baseStructAddr + lsKOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsKID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'ls_k.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ls_k: ' + E.Message);
    end;
  end;
  
  if FileExists(lsKzhPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем ls_kzh.dmd...');
    try
      LsKzhID := LoadModel(lsKzhPath, 0, False);
      if LsKzhID > 0 then
      begin
        AddToLogFile(EngineLog, 'ls_kzh.dmd загружен, ID: ' + IntToStr(LsKzhID));
        modelAddr := Pointer(baseStructAddr + lsKzhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsKzhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'ls_kzh.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ls_kzh: ' + E.Message);
    end;
  end;
  
  if FileExists(lsZPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем ls_z.dmd...');
    try
      LsZID := LoadModel(lsZPath, 0, False);
      if LsZID > 0 then
      begin
        AddToLogFile(EngineLog, 'ls_z.dmd загружен, ID: ' + IntToStr(LsZID));
        modelAddr := Pointer(baseStructAddr + lsZOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsZID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'ls_z.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ls_z: ' + E.Message);
    end;
  end;
  
  if FileExists(lsZhPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем ls_zh.dmd...');
    try
      LsZhID := LoadModel(lsZhPath, 0, False);
      if LsZhID > 0 then
      begin
        AddToLogFile(EngineLog, 'ls_zh.dmd загружен, ID: ' + IntToStr(LsZhID));
        modelAddr := Pointer(baseStructAddr + lsZhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsZhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'ls_zh.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ls_zh: ' + E.Message);
    end;
  end;
  
  // ===== ЗАГРУЖАЕМ KLUB_LS МОДЕЛИ =====
  AddToLogFile(EngineLog, '=== ЗАГРУЗКА KLUB_LS МОДЕЛЕЙ ===');
  klubLsPath := directoryPath + 'loc\klub_ls.dmd';
  klubLsBPath := directoryPath + 'loc\klub_ls_b.dmd';
  klubLsKPath := directoryPath + 'loc\klub_ls_k.dmd';
  klubLsKzhPath := directoryPath + 'loc\klub_ls_kzh.dmd';
  klubLsZPath := directoryPath + 'loc\klub_ls_z.dmd';
  klubLsZhPath := directoryPath + 'loc\klub_ls_zh.dmd';
  
  AddToLogFile(EngineLog, 'Пути к KLUB_LS моделям:');
  AddToLogFile(EngineLog, 'KLUB_LS: ' + klubLsPath);
  AddToLogFile(EngineLog, 'KLUB_LS_B: ' + klubLsBPath);
  AddToLogFile(EngineLog, 'KLUB_LS_K: ' + klubLsKPath);
  AddToLogFile(EngineLog, 'KLUB_LS_KZH: ' + klubLsKzhPath);
  AddToLogFile(EngineLog, 'KLUB_LS_Z: ' + klubLsZPath);
  AddToLogFile(EngineLog, 'KLUB_LS_ZH: ' + klubLsZhPath);
  
  // Загружаем klub_ls.dmd
  if FileExists(klubLsPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем klub_ls.dmd...');
    try
      KlubLsID := LoadModel(klubLsPath, 0, False);
      if KlubLsID > 0 then
      begin
        AddToLogFile(EngineLog, 'klub_ls.dmd загружен, ID: ' + IntToStr(KlubLsID));
        modelAddr := Pointer(baseStructAddr + klubLsOffset);
        AddToLogFile(EngineLog, 'Адрес KLUB_LS: $' + IntToHex(Cardinal(modelAddr), 8) + ' (offset: $' + IntToHex(klubLsOffset, 2) + ')');
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'klub_ls.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке klub_ls: ' + E.Message);
    end;
  end;
  
  // Аналогично для остальных KLUB_LS моделей...
  if FileExists(klubLsBPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем klub_ls_b.dmd...');
    try
      KlubLsBID := LoadModel(klubLsBPath, 0, False);
      if KlubLsBID > 0 then
      begin
        AddToLogFile(EngineLog, 'klub_ls_b.dmd загружен, ID: ' + IntToStr(KlubLsBID));
        modelAddr := Pointer(baseStructAddr + klubLsBOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsBID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'klub_ls_b.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке klub_ls_b: ' + E.Message);
    end;
  end;
  
  if FileExists(klubLsKPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем klub_ls_k.dmd...');
    try
      KlubLsKID := LoadModel(klubLsKPath, 0, False);
      if KlubLsKID > 0 then
      begin
        AddToLogFile(EngineLog, 'klub_ls_k.dmd загружен, ID: ' + IntToStr(KlubLsKID));
        modelAddr := Pointer(baseStructAddr + klubLsKOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsKID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'klub_ls_k.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке klub_ls_k: ' + E.Message);
    end;
  end;

  if FileExists(klubLsKzhPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем klub_ls_kzh.dmd...');
    try
      KlubLsKzhID := LoadModel(klubLsKzhPath, 0, False);
      if KlubLsKzhID > 0 then
      begin
        AddToLogFile(EngineLog, 'klub_ls_kzh.dmd загружен, ID: ' + IntToStr(KlubLsKzhID));
        modelAddr := Pointer(baseStructAddr + klubLsKzhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsKzhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'klub_ls_kzh.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке klub_ls_kzh: ' + E.Message);
    end;
  end;
  
  if FileExists(klubLsZPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем klub_ls_z.dmd...');
    try
      KlubLsZID := LoadModel(klubLsZPath, 0, False);
      if KlubLsZID > 0 then
      begin
        AddToLogFile(EngineLog, 'klub_ls_z.dmd загружен, ID: ' + IntToStr(KlubLsZID));
        modelAddr := Pointer(baseStructAddr + klubLsZOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsZID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'klub_ls_z.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке klub_ls_z: ' + E.Message);
    end;
  end;
  
  if FileExists(klubLsZhPath) then
  begin
    AddToLogFile(EngineLog, 'Загружаем klub_ls_zh.dmd...');
    try
      KlubLsZhID := LoadModel(klubLsZhPath, 0, False);
      if KlubLsZhID > 0 then
      begin
        AddToLogFile(EngineLog, 'klub_ls_zh.dmd загружен, ID: ' + IntToStr(KlubLsZhID));
        modelAddr := Pointer(baseStructAddr + klubLsZhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsZhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'klub_ls_zh.dmd записан в память');
        end;
      end;
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке klub_ls_zh: ' + E.Message);
    end;
  end;

  SettingsLoaded := True;
  AddToLogFile(EngineLog, '=== ЗАГРУЗКА НАСТРОЕК И МОДЕЛЕЙ ЗАВЕРШЕНА УСПЕШНО ===');
  AddToLogFile(EngineLog, 'Финальное состояние:');
  AddToLogFile(EngineLog, '- LocNum: ' + LocNum);
  AddToLogFile(EngineLog, '- LocomotiveType: ' + IntToStr(LocomotiveType));
  AddToLogFile(EngineLog, '- currentLocType: ' + IntToStr(currentLocType));
  AddToLogFile(EngineLog, '- locFolder: ' + locFolder);
  AddToLogFile(EngineLog, '- directoryPath: ' + directoryPath);
  AddToLogFile(EngineLog, '- baseStructAddr: $' + IntToHex(baseStructAddr, 8));
  AddToLogFile(EngineLog, '- klubFlag: ' + IntToStr(klubFlag));
end;

function GetLocoPatchOffset(locType: Integer): Cardinal;
begin
  Result := LocoPatchOffset(locType);
end;

function IsKeyPressed(VKey: Integer): Boolean;
begin
  Result := (GetAsyncKeyState(VKey) and $8000) <> 0;
end;

// Функция для ограничения pitch
function ClampPitch(pitch: Single): Single;
begin
  if pitch > 90.0 then
    Result := 90.0
  else if pitch < -90.0 then
    Result := -90.0
  else
    Result := pitch;
end;

// Функция для чтения Single из памяти



// Изменить функцию ApplyNopPatch
function ApplyNopPatch: Boolean;
var
  PatchAddress: Cardinal;
  OldProtect: DWORD;
  i: Integer;
  CurrentLocType: Integer;
begin
  Result := False;
  
  try
    // Читаем тип локомотива из памяти вместо переменной
    CurrentLocType := GetLocomotiveTypeFromMemory;
    PatchAddress := GetLocoPatchOffset(CurrentLocType);

    AddToLogFile(EngineLog, IntToStr(PatchAddress));

    if VirtualProtect(Pointer(PatchAddress), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      // Сохраняем оригинальные байты
      for i := 0 to 4 do
        OriginalBytes[i] := PByte(PatchAddress + i)^;
      
      // Применяем NOP патч
      for i := 0 to 4 do
        PByte(PatchAddress + i)^ := NopBytes[i];
      
      VirtualProtect(Pointer(PatchAddress), 5, OldProtect, OldProtect);
      Result := True;
      
      AddToLogFile(EngineLog, Format('NOP patch applied for locomotive type %d at address %s',
        [CurrentLocType, IntToHex(PatchAddress, 8)]));
    end;
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error applying NOP patch: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Изменить функцию RestoreOriginalBytes
function RestoreOriginalBytes: Boolean;
var
  PatchAddress: Cardinal;
  OldProtect: DWORD;
  i: Integer;
  CurrentLocType: Integer;
begin
  Result := False;
  
  try
    // Читаем тип локомотива из памяти вместо переменной
    CurrentLocType := GetLocomotiveTypeFromMemory;
    PatchAddress := GetLocoPatchOffset(CurrentLocType);

    if VirtualProtect(Pointer(PatchAddress), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      // Восстанавливаем оригинальные байты
      for i := 0 to 4 do
        PByte(PatchAddress + i)^ := OriginalBytes[i];
      
      VirtualProtect(Pointer(PatchAddress), 5, OldProtect, OldProtect);
      // Сброс углов камеры
      WriteMemorySingle(ADDR_LOOKYAW, 0.0);
      WriteMemorySingle(ADDR_LOOKPITCH, 0.0);

      Result := True;
      
      AddToLogFile(EngineLog, Format('Original bytes restored for locomotive type %d at address %s', 
        [CurrentLocType, IntToHex(PatchAddress, 8)]));
    end;
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error restoring original bytes: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Также можно изменить функцию InitializeFreecam для консистентности
procedure InitializeFreecam;
var
  CurrentLocType: Integer;
begin
  if FreecamInitialized then Exit;
  
  try
    // Читаем тип локомотива из памяти
    CurrentLocType := GetLocomotiveTypeFromMemory;
    
    // Читаем начальные значения
    InitialYaw := ReadMemorySingle(ADDR_LOOKYAW);
    InitialPitch := ReadMemorySingle(ADDR_LOOKPITCH);
    InitialX := ReadMemorySingle(ADDR_X);
    InitialY := ReadMemorySingle(ADDR_Y);
    InitialZ := ReadMemorySingle(ADDR_Z);
    
    FreecamInitialized := True;
    
    AddToLogFile(EngineLog, Format('Freecam initialized for locomotive type %d - Yaw: %.2f, Pitch: %.2f, X: %.2f, Y: %.2f, Z: %.2f',
      [CurrentLocType, InitialYaw, InitialPitch, InitialX, InitialY, InitialZ]));
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'Freecam initialization failed: ' + E.Message);
  end;
end;

// Удобная обёртка для работы с обычными строками
procedure DrawText3D(FontID: Integer; Text: string);
var
  TextPtr: PChar;
begin
  TextPtr := PChar(Text);
  Write3D(FontID, TextPtr);
end;

procedure WriteHookAddress; stdcall;
var
  HookAddr: Cardinal;
  CallAddress: Cardinal;
  CallAddress2: Cardinal;
  NewOffset: Integer;
  NewOffset2: Integer;
  OldProtect: DWORD;
  i: Integer;
  ConditionValue: Byte;

  // Адреса для замены PUSH инструкций
  SpeedXAddr: Cardinal;
  AllowedSpeedAddr: Cardinal;
  ShuntingSpeedAddr: Cardinal;
  TrainSpeedAddr: Cardinal;
  TimeAddr: Cardinal;
  NumberAccelAddr: Cardinal;
  ReverseAddr: Cardinal;
  AdditionalAddr: Cardinal;
  RadiusAddr: Cardinal;

  // Новые значения для замены (в формате little-endian)
  NewSpeedXValue: array[0..3] of Byte;
  NewAllowedSpeedValue: array[0..3] of Byte;
  NewShuntingSpeedValue: array[0..3] of Byte;
  NewTrainSpeedValue: array[0..3] of Byte;
  NewTimeValue: array[0..3] of Byte;
  NewNumberAccelValue: array[0..3] of Byte;
  NewReverseValue: array[0..3] of Byte;
  NewAdditionalValue: array[0..3] of Byte;

  function SafeVirtualProtect(Address: Pointer; Size: Cardinal; NewProtect: DWORD; var OldProtect: DWORD): Boolean;
  var
    AttemptStart: Cardinal;
    Attempts: Integer;
  begin
    AttemptStart := GetTickCount;
    Result := False;
    Attempts := 0;
    
    repeat
      try
        Result := VirtualProtect(Address, Size, NewProtect, OldProtect);
        if Result then Break;
        
        Inc(Attempts);

        if (Attempts > 10) or ((GetTickCount - AttemptStart) > 1000) then
          Break;
          
      except
        Inc(Attempts);
        if Attempts > 5 then Break;
      end;
    until False;
  end;

begin

  try
    // Читаем условие из адреса 007498A8
    try
      ConditionValue := PByte(Pointer($007498A8))^;
    except
      ConditionValue := 0; // По умолчанию если не можем прочитать
    end;

    // Всегда патчим HookKLUB
    try
      HookAddr := Cardinal(@HookKLUB);
      CallAddress := $00400000 + $277938;
      NewOffset := Integer(HookAddr) - Integer(CallAddress + 5);
      
      if SafeVirtualProtect(Pointer(CallAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        try
          PInteger(CallAddress + 1)^ := NewOffset;
          SafeVirtualProtect(Pointer(CallAddress + 1), 4, OldProtect, OldProtect);
        except
          // Игнорируем ошибки записи
        end;
      end;
    except
      // Игнорируем ошибки
    end;
    
    // Дополнительно патчим HookSkorostemerCHS7 если условие не выполнено
    if ConditionValue <> 1 then
    begin
      try
        HookAddr := Cardinal(@HookSkorostemerCHS7);
        CallAddress2 := $00400000 + $27795A;
        NewOffset2 := Integer(HookAddr) - Integer(CallAddress2 + 5);
        
        if SafeVirtualProtect(Pointer(CallAddress2 + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            PInteger(CallAddress2 + 1)^ := NewOffset2;
            SafeVirtualProtect(Pointer(CallAddress2 + 1), 4, OldProtect, OldProtect);
          except
            // Игнорируем ошибки записи
          end;
        end;
      except
        // Игнорируем ошибки
      end;
    end;
    
  except
    on E: Exception do
    begin
      // Игнорируем ошибки
    end;
  end;
end;






procedure WriteHookAddressCHS8; stdcall;
begin
  WriteHookAddressCHS8Runtime;
end;

procedure WriteHookAddressED4M; stdcall;
begin
  WriteHookAddressED4MRuntime;
end;



{System------------------------------------------------------------------------}
function vertex(x,y,z : single) : TVertex; inline;
begin
result.X:=x;
result.Y:=y;
result.Z:=z;
end;
{------------------------------------------------------------------}
function GETLIGHT(ID: integer) : integer;
begin
 result:=GL_LIGHT0+ID;
end;
{------------------------------------------------------------------}
procedure _glTexCoord2f(X,Y : GLFloat; Layer : integer = -1); stdcall;
var i : cardinal;
begin

 if MultyTexActive then
 begin
  if Layer=-1 then
  begin
   glMultiTexCoord2fARB(GL_TEXTURE0_ARB,X,Y);
  for i:=1 to 5 do
   glMultiTexCoord2fARB(GL_TEXTURE0_ARB+i, (X+MultyCoordOffset[i][0])*MultyCoordOffset[i][2],
    (Y+MultyCoordOffset[i][1])*MultyCoordOffset[i][3]);
  end else glMultiTexCoord2fARB(GL_TEXTURE0_ARB+Layer,X,Y);
 end else glTexCoord2f(x,y);

end;
{------------------------------------------------------------------}
procedure _glTexCoord3f(X,Y,Z : GLFloat; Layer : integer = -1);
var i : cardinal;
begin

 if MultyTexActive then
 begin
  if Layer=-1 then
  begin
   glMultiTexCoord3fARB(GL_TEXTURE0_ARB,X,Y,Z);
  for i:=1 to 5 do
   glMultiTexCoord3fARB(GL_TEXTURE0_ARB+i, (X+MultyCoordOffset[i][0])*MultyCoordOffset[i][2],
    (Y+MultyCoordOffset[i][1])*MultyCoordOffset[i][3],0);
  end else glMultiTexCoord3fARB(GL_TEXTURE0_ARB+Layer,X,Y,Z);
 end else glTexCoord3f(x,y,z);

end;
{------------------------------------------------------------------}
procedure CullFace(Mode : cardinal); stdcall;
begin
  case mode of
  0:glDisable(GL_CULL_FACE);
  1:
  begin
  glEnable(GL_CULL_FACE);
  glCullFace(GL_FRONT);
  end;
  2:
  begin
  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);
  end;
  end;
end;
{------------------------------------------------------------------}
procedure InitFBO(Texw, texh, ZBuf : cardinal);
var comp : integer;
begin
 if GL_EXT_framebuffer_object and
 (
 ((fbo_w<>Texw) or (fbo_h<>Texh)) or (fbo_z<>ZBuf) or ((fbo_frame=0) or (fbo_depth=0))
 )then
  begin

  fbo_w := Texw;
  fbo_h := Texh;

  if fbo_frame <> 0 then glDeleteFramebuffersEXT(1, @fbo_frame);
  if fbo_depth <> 0 then glDeleteRenderbuffersEXT(1, @fbo_depth);

  glGenFramebuffersEXT(1, @fbo_frame);
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo_frame);

  glGenRenderbuffersEXT(1, @fbo_depth);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, fbo_depth);

  case ZBuf of
  16:comp:=GL_DEPTH_COMPONENT16_ARB;
  24:comp:=GL_DEPTH_COMPONENT24_ARB;
  32:comp:=GL_DEPTH_COMPONENT32_ARB;
  else
  begin
    AddToLogFile(EngineLog,'ZBuffer depth is incirrect');
    comp:=GL_DEPTH_COMPONENT24_ARB;
  end;
  end;

  glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, comp, Texw, Texh);
 	glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, fbo_depth);
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);

  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  end;
end;
{------------------------------------------------------------------}
procedure InitEng;

   procedure CreateSphere(CX, CY, CZ, Radius : glFloat; N : Integer);  // N = precision
    var I, J : Integer;
        theta1,theta2,theta3 : glFloat;
        X, Y, Z, px, py, pz : glFloat;
    begin
      SphereDL :=glGenLists(1);
      glNewList(SphereDL, GL_COMPILE);

        if Radius < 0 then Radius :=-Radius;
        if n < 0 then n := -n;
        if (n < 4) OR (Radius <= 0) then
        begin
          glBegin(GL_POINTS);
            glVertex3f(CX, CY, CZ);
          glEnd();
          exit;
        end;

        for J :=0 to N DIV 2 -1 do
        begin
          theta1 := J*2*PI/N - PI/2;
          theta2 := (J+1)*2*PI/n - PI/2;
          glBegin(GL_QUAD_STRIP);
            For I :=0 to N do
            begin
              theta3 := i*2*PI/N;
              x := cos(theta2) * cos(theta3);
              y := sin(theta2);
              z := cos(theta2) * sin(theta3);
              px := CX + Radius*x;
              py := CY + Radius*y;
              pz := CZ + Radius*z;

              glNormal3f(X, Y, Z);
              _glTexCoord2f(1-I/n, 2*(J+1)/n);
              glVertex3f(px,py,pz);

              X := cos(theta1) * cos(theta3);
              Y := sin(theta1);
              Z := cos(theta1) * sin(theta3);
              px := CX + Radius*X;
              py := CY + Radius*Y;
              pz := CZ + Radius*Z;

              glNormal3f(X, Y, Z);
              _glTexCoord2f(1-i/n, 2*j/n);
              glVertex3f(px,py,pz);
            end;
          glEnd();
        end;
      glEndList();
    end;

var i : integer;
begin

if GL_ARB_multitexture then
begin

 for i:=1 to 5 do
 begin
  glActiveTextureARB(GL_TEXTURE0_ARB+i);
  glBindTexture(GL_TEXTURE_2D, 0);

  MultyCoordOffset[i][0]:=0.0;
  MultyCoordOffset[i][1]:=0.0;
  MultyCoordOffset[i][2]:=1.0;
  MultyCoordOffset[i][3]:=1.0;
 end;

 glActiveTextureARB(GL_TEXTURE0_ARB);
 glEnable(GL_TEXTURE_2D);
 glBindTexture(GL_TEXTURE_2D, 0);

end;

for i:=0 to 20 do
 LightsOn[i]:=False;

CreateSphere(0, 0, 0, 1, 48);

QuadraticObject := gluNewQuadric;
gluQuadricNormals(QuadraticObject, GLU_SMOOTH);

//_NormCubemap:=GenerateNormalisationCubeMap;

fbo_frame:=0;
fbo_depth:=0;
fbo_w:=0;
fbo_h:=0;
fbo_z:=0;

if GL_EXT_framebuffer_object then
glGenFramebuffersEXT(1, @fbo2);

end;
{------------------------------------------------------------------}
procedure FreeEng;
var 
  i: integer;
  Count: cardinal;
  TotalFreed: Integer;
  StartTime: Cardinal;
  OperationTimeout: Boolean;
begin
  StartTime := GetTickCount;
  TotalFreed := 0;
  OperationTimeout := False;

  try
    // Проверяем тайм-аут на каждом этапе
    if (GetTickCount - StartTime) > 5000 then
    begin
      OperationTimeout := True;
      Exit;
    end;

    // Освобождаем QuadraticObject
    try
      if QuadraticObject <> nil then
      begin
        gluDeleteQuadric(QuadraticObject);
        QuadraticObject := nil;
      end;
    except
      // Игнорируем ошибки OpenGL
    end;

    // Освобождаем SphereDL
    try
      if SphereDL <> 0 then
      begin
        glDeleteLists(SphereDL, 1);
        SphereDL := 0;
      end;
    except
      // Игнорируем ошибки OpenGL
    end;

    // Проверяем тайм-аут
    if (GetTickCount - StartTime) > 5000 then
    begin
      OperationTimeout := True;
      Exit;
    end;

    // Освобождаем FBO
    try
      if GL_EXT_framebuffer_object then
      begin
        if fbo2 <> 0 then
        begin
          glDeleteFramebuffersEXT(1, @fbo2);
          fbo2 := 0;
        end;
        if fbo_frame <> 0 then
        begin
          glDeleteFramebuffersEXT(1, @fbo_frame);
          fbo_frame := 0;
        end;
        if fbo_depth <> 0 then
        begin
          glDeleteRenderbuffersEXT(1, @fbo_depth);
          fbo_depth := 0;
        end;
      end;
    except
      // Игнорируем ошибки OpenGL
    end;

    // Очищаем массивы
    Obj3DInfo := nil;
    Obj3DInfoCount := 0;

    // Проверяем тайм-аут
    if (GetTickCount - StartTime) > 5000 then
    begin
      OperationTimeout := True;
      Exit;
    end;

    // Освобождаем AVI текстуры
    Count := 0;
    try
      if AvisCount <> 0 then
      begin
        for i := 0 to AvisCount - 1 do
        begin
          if Avis[i].loaded then
          begin
            try
              FreeAVITexture(i);
              Inc(Count);
            except
              // Игнорируем ошибки при освобождении AVI
            end;
          end;
          
          // Проверяем тайм-аут внутри цикла
          if (GetTickCount - StartTime) > 5000 then
          begin
            OperationTimeout := True;
            Break;
          end;
        end;
        TotalFreed := TotalFreed + Count;
      end;
      Avis := nil;
      AvisCount := 0;
    except
      // Игнорируем ошибки массива AVI
    end;

    // Проверяем тайм-аут
    if OperationTimeout or ((GetTickCount - StartTime) > 5000) then
      Exit;

    // Освобождаем 3D шрифты
    Count := 0;
    try
      if FontsCount > 0 then
      begin
        for i := 0 to FontsCount - 1 do
        begin
          try
            if Fonts3D[i].List <> 0 then
            begin
              glDeleteLists(Fonts3D[i].List, 256);
              Inc(Count);
            end;
          except
            // Игнорируем ошибки OpenGL
          end;
          
          // Проверяем тайм-аут
          if (GetTickCount - StartTime) > 5000 then
          begin
            OperationTimeout := True;
            Break;
          end;
        end;
        TotalFreed := TotalFreed + Count;
      end;
      Fonts3D := nil;
      FontsCount := 0;
    except
      // Игнорируем ошибки массива шрифтов
    end;

    // Проверяем тайм-аут
    if OperationTimeout or ((GetTickCount - StartTime) > 5000) then
      Exit;

    // Освобождаем сцены
    Count := 0;
    try
      if ScenesCount > 0 then
      begin
        for i := 0 to ScenesCount - 1 do
        begin
          try
            FreeScene(Scenes[i].uid);
            Inc(Count);
          except
            // Игнорируем ошибки при освобождении сцен
          end;
          
          // Проверяем тайм-аут
          if (GetTickCount - StartTime) > 5000 then
          begin
            OperationTimeout := True;
            Break;
          end;
        end;
        TotalFreed := TotalFreed + Count;
      end;
      Scenes := nil;
      ScenesCount := 0;
    except
      // Игнорируем ошибки массива сцен
    end;

    // Проверяем тайм-аут
    if OperationTimeout or ((GetTickCount - StartTime) > 5000) then
      Exit;

    // Освобождаем шейдеры
    Count := 0;
    try
      if ShadersCount > 0 then
      begin
        for i := 0 to ShadersCount - 1 do
        begin
          try
            if Shaders[i].glident <> 0 then
            begin
              glDeleteProgramsARB(1, @Shaders[i].glident);
              Inc(Count);
            end;
          except
            // Игнорируем ошибки OpenGL
          end;
          
          // Проверяем тайм-аут
          if (GetTickCount - StartTime) > 5000 then
          begin
            OperationTimeout := True;
            Break;
          end;
        end;
        TotalFreed := TotalFreed + Count;
      end;
      Shaders := nil;
      ShadersCount := 0;
    except
      // Игнорируем ошибки массива шейдеров
    end;

    // Проверяем тайм-аут
    if OperationTimeout or ((GetTickCount - StartTime) > 5000) then
      Exit;

    // Освобождаем меши
    Count := 0;
    try
      if MeshsCount > 0 then
      begin
        for i := 0 to MeshsCount - 1 do
        begin
          try
            if Assigned(Meshs[i].Mesh) then
            begin
              Meshs[i].Mesh.Free;
              Meshs[i].Mesh := nil;
              Inc(Count);
            end;
          except
            // Игнорируем ошибки при освобождении мешей
          end;
          
          // Проверяем тайм-аут
          if (GetTickCount - StartTime) > 5000 then
          begin
            OperationTimeout := True;
            Break;
          end;
        end;
        TotalFreed := TotalFreed + Count;
      end;
      Meshs := nil;
      MeshsCount := 0;
    except
      // Игнорируем ошибки массива мешей
    end;

    // Проверяем тайм-аут
    if OperationTimeout or ((GetTickCount - StartTime) > 5000) then
      Exit;

    // Освобождаем лого
    try
      if ShowLogo and (Logo <> 0) then
      begin
        FreeTexture(Logo);
        Logo := 0;
      end;
    except
      // Игнорируем ошибки с лого
    end;

    // Освобождаем текстуры
    Count := 0;
    try
      if length(TexturesInfo) > 0 then
      begin
        for i := 0 to length(TexturesInfo) - 1 do
        begin
          try
            if TexturesInfo[i].Index <> 0 then
            begin
              glDeleteTextures(1, @TexturesInfo[i].Index);
              Inc(Count);
            end;
          except
            // Игнорируем ошибки OpenGL
          end;
          
          // Проверяем тайм-аут
          if (GetTickCount - StartTime) > 5000 then
          begin
            OperationTimeout := True;
            Break;
          end;
        end;
        TotalFreed := TotalFreed + Count;
      end;
      TexturesInfo := nil;
    except
      // Игнорируем ошибки массива текстур
    end;

    // Проверяем тайм-аут
    if OperationTimeout or ((GetTickCount - StartTime) > 5000) then
      Exit;

    // Освобождаем DGL шрифты
    Count := 0;
    try
      if length(DGLFonts) > 0 then
      begin
        for i := 0 to length(DGLFonts) - 1 do
        begin
          if DGLFonts[i].Load then
          begin
            try
              if DGLFonts[i].Texture <> 0 then
              begin
                glDeleteTextures(1, @DGLFonts[i].Texture);
                Inc(Count);
              end;
            except
              // Игнорируем ошибки OpenGL
            end;
          end;
          
          // Проверяем тайм-аут
          if (GetTickCount - StartTime) > 5000 then
          begin
            OperationTimeout := True;
            Break;
          end;
        end;
        TotalFreed := TotalFreed + Count;
      end;
      DGLFonts := nil;
    except
      // Игнорируем ошибки массива DGL шрифтов
    end;

    // Освобождаем StringList'ы из трафик системы
    BoosterTrafficRuntime.FreeTrafficRuntime;
    BoosterStationRuntime.FreeStationRuntime;
    try
      if Assigned(s1) then
      begin
        s1.Free;
        s1 := nil;
      end;
    except
      // Игнорируем ошибки
    end;

    try
      if Assigned(s2) then
      begin
        s2.Free;
        s2 := nil;
      end;
    except
      // Игнорируем ошибки
    end;

    // ОДНА запись в лог вместо множественных
    try
      if TotalFreed > 0 then
        //AddToLogFile(EngineLog, Format('Engine cleanup completed: %d resources freed in %d ms',
          //[TotalFreed, GetTickCount - StartTime]))
      else if not OperationTimeout then
        //AddToLogFile(EngineLog, 'Engine cleanup completed.');

      if OperationTimeout then
        //AddToLogFile(EngineLog, 'Engine cleanup timeout exceeded, forced shutdown.');
    except
      // Если даже запись в лог не работает, ничего не делаем
    end;

  except
    on E: Exception do
    begin
      // Последняя попытка записать об ошибке
      try
        //AddToLogFile(EngineLog, 'Critical error during engine cleanup: ' + E.Message);
      except
        // Если и это не работает, просто выходим
      end;
    end;
  end;
end;

{Scenes -----------------------------------------------------------------------}
procedure SceneSetObjActive( SceneIdent, ObjIdent : cardinal; Active : boolean ); stdcall;
var i : integer;
begin
if ScenesCount>0 then
 for i := 0 to ScenesCount - 1 do
  if SceneIdent=Scenes[i].uid then
  begin
    Scenes[i].Models[ObjIdent].Active:=Active;
    Exit;
  end;
end;
{------------------------------------------------------------------}
procedure SceneSetObj( SceneIdent, ObjIdent : cardinal; SceneMesh : TSceneMesh ); stdcall;
var i : integer;
begin
if ScenesCount>0 then
 for i := 0 to ScenesCount - 1 do
  if SceneIdent=Scenes[i].uid then
  begin
    Scenes[i].Models[ObjIdent]:= SceneMesh;
    Exit;
  end;
end;
{------------------------------------------------------------------}
function SceneGetObj( SceneIdent, ObjIdent : cardinal ) : TSceneMesh; stdcall;
var i : integer;
begin
if ScenesCount>0 then
 for i := 0 to ScenesCount - 1 do
  if SceneIdent=Scenes[i].uid then
  begin
    result:=Scenes[i].Models[ObjIdent];
    Exit;
  end;
end;
{------------------------------------------------------------------}
function GetSceneObjectIdent( SceneIdent : cardinal; ObjName : string ) : integer; stdcall;
var i,j : integer;
begin
result:=-1;
if ScenesCount>0 then
 for i := 0 to ScenesCount - 1 do
  if SceneIdent=Scenes[i].uid then
  begin
    result:=-1;
    if length(Scenes[i].Models)>0 then
    for j := 0 to length(Scenes[i].Models) - 1 do
     if ObjName = Scenes[i].Models[j].Name then
     begin
       result:=j;
       Exit;
     end;
    Exit;
  end;
end;
{------------------------------------------------------------------}
function SceneGetLastCollideObjectIndex:integer; stdcall;
begin
  result:=LastCollide;
end;
{------------------------------------------------------------------}
function SceneObjCount( Ident : cardinal ) : cardinal; stdcall;
var i : integer;
begin
result:=0;
if ScenesCount>0 then
 for i := 0 to ScenesCount - 1 do
  if Ident=Scenes[i].uid then
  begin
    result:=length(Scenes[i].Models);
    Exit;
  end;
end;
{------------------------------------------------------------------}
function CollideBoxWithScene(Ident : cardinal; BoxPos, BoxSize : Tvertex):boolean; stdcall;

  type
  TPhysicsBoundBox = record
  Min, Max: TVertex;
  end;

  function BoundBoxIntersect(const BoundBox, WithBoundBox: TPhysicsBoundBox): boolean;
  begin                      
   Result :=
    ((BoundBox.Max.x >= WithBoundBox.Min.x) and
    (BoundBox.Min.x <= WithBoundBox.Max.x)) and
    ((BoundBox.Max.y >= WithBoundBox.Min.y) and
    (BoundBox.Min.y <= WithBoundBox.Max.y)) and
    ((BoundBox.Max.z >= WithBoundBox.Min.z) and
    (BoundBox.Min.z <= WithBoundBox.Max.z));
  end;

  function PhysicsBoundBox (minx, miny, minz, maxx, maxy, maxz : single):TPhysicsBoundBox; inline;
  begin
   result.Min.X:=minx;
   result.Min.Y:=miny;
   result.Min.Z:=minz;
   result.Max.X:=maxx;
   result.Max.Y:=maxy;
   result.Max.Z:=maxz;
  end;

  var
  i,j : integer;
  mbound : Tvertex;
  cur,cur2 : TPhysicsBoundBox;
begin
 result := false;

 cur := PhysicsBoundBox(BoxPos.X-BoxSize.X/2,BoxPos.Y-BoxSize.Y/2,BoxPos.Z-BoxSize.Z/2,
                        BoxPos.X+BoxSize.X/2,BoxPos.Y+BoxSize.Y/2,BoxPos.Z+BoxSize.Z/2);

 LastCollide:=-1;

 if ScenesCount>0 then
 for j := 0 to ScenesCount - 1 do
 if Ident=Scenes[j].uid then
 begin
 if length(Scenes[j].Models)>0 then
  for i := 0 to length(Scenes[j].Models) - 1 do
  if Scenes[j].Models[i].Active then
  begin

   mbound:=ModelBoundingBox(Scenes[j].models[i].Mesh,0);

   with Scenes[j].models[i] do
   cur2 := PhysicsBoundBox(
   Pos.X-(mbound.X*Scale)/2,Pos.Y-(mbound.Y*Scale)/2,Pos.Z-(mbound.Z*Scale)/2,
   Pos.X+(mbound.X*Scale)/2,Pos.Y+(mbound.Y*Scale)/2,Pos.Z+(mbound.Z*Scale)/2
   );

   if BoundBoxIntersect(cur,cur2) then
   begin
     result:=true;
     LastCollide:=i;
     Exit;
   end;

  end;

 Exit;
 end;
end;
{------------------------------------------------------------------}
function LoadScene(FileName, MeshPath, TexPath : string):cardinal; stdcall;
var
F : textfile;
s : string;
MeshFromPak, TxtFromPak : boolean;

procedure ReadEntity;
var
t : TSceneMesh;
i : integer;
a,b,c : single;
found : boolean;
begin
 Readln(f,S);
 Readln(f,S);
 Readln(f,S);
 Readln(f,S);
 t.Name:=s;
 if not MeshFromPak then
 t.Mesh:=LoadModel(MeshPath+'\'+s+'.dmd',0,false)
 else
 begin
  ExtractFromPackage(MeshPath,s+'.dmd','temp.dmd');
  t.Mesh:=LoadModel('temp.dmd',0,false);
  DeleteFile('temp.dmd');
 end;
 Readln(f,S);
 Readln(f,a,b,c);
 t.Pos.X:=a;
 t.Pos.Y:=b;
 t.Pos.Z:=c;
 Readln(f,S);
 Readln(f,t.Scale);
 t.Material:=ModelMaterial(t.Mesh);

 if t.Material.TexFileName<>'None' then
 begin

 found:=false;

 if length(Scenes[ScenesCount-1].Models)-1>0 then
  for i := 0 to length(Scenes[ScenesCount-1].Models)-1 do
   if t.Material.TexFileName=Scenes[ScenesCount-1].Models[i].Material.TexFileName then
   begin
     t.Texture:=Scenes[ScenesCount-1].Models[i].Texture;
     found:=true;
     break;
   end;

  if not found then
    if not TxtFromPak then
    t.Texture:=LoadTextureFromFile(TexPath+'\'+t.Material.TexFileName,0,-1)
    else
    t.Texture:=LoadTextureFromPackage(TexPath,t.Material.TexFileName,0,-1);

 end else t.Texture:=0;

 if t.Material.NormalMapFileName<>'None' then
 begin

 found:=false;

 if length(Scenes[ScenesCount-1].Models)-1>0 then
  for i := 0 to length(Scenes[ScenesCount-1].Models)-1 do
   if t.Material.NormalMapFileName=Scenes[ScenesCount-1].Models[i].Material.NormalMapFileName then
   begin
     t.BumpTexture:=Scenes[ScenesCount-1].Models[i].BumpTexture;
     found:=true;
     break;
   end;

  if not found then
    if not TxtFromPak then
    t.BumpTexture:=LoadTextureFromFile(TexPath+'\'+t.Material.NormalMapFileName,0,-1)
    else
    t.BumpTexture:=LoadTextureFromPackage(TexPath,t.Material.NormalMapFileName,0,-1);

 end else t.BumpTexture:=0;

 if t.Material.SpecularMapFileName<>'None' then
 begin

 found:=false;

 if length(Scenes[ScenesCount-1].Models)-1>0 then
  for i := 0 to length(Scenes[ScenesCount-1].Models)-1 do
   if t.Material.SpecularMapFileName=Scenes[ScenesCount-1].Models[i].Material.SpecularMapFileName then
   begin
     t.SpecTexture:=Scenes[ScenesCount-1].Models[i].SpecTexture;
     found:=true;
     break;
   end;

  if not found then
    if not TxtFromPak then
    t.SpecTexture:=LoadTextureFromFile(TexPath+'\'+t.Material.SpecularMapFileName,0,-1)
    else
    t.SpecTexture:=LoadTextureFromPackage(TexPath,t.Material.SpecularMapFileName,0,-1);

 end else t.SpecTexture:=0;

 t.Active:=true;
 t.DoBump:=false;
 t.MeshSmooth:=true;
 t.MeshFrame:=0;

 SetLength(Scenes[ScenesCount-1].Models,length(Scenes[ScenesCount-1].Models)+1);
 Scenes[ScenesCount-1].Models[length(Scenes[ScenesCount-1].Models)-1]:=t;

end;

begin
if fileexists(FileName) then
begin
    TxtFromPak  := copy(Uppercase(TexPath), length(TexPath)-3, 4) = '.DPC';
    MeshFromPak := copy(Uppercase(MeshPath), length(MeshPath)-3, 4) = '.DPC';

    SetLength(Scenes,ScenesCount+1);
    ScenesCount:=ScenesCount+1;
    ScenesOverall:=ScenesOverall+1;
    Scenes[ScenesCount-1].uid:=ScenesOverall;
    SetLength(Scenes[ScenesCount-1].Models,0);
   AssignFile(f,FileName);
   Reset(f);
   while not Eof(f) do begin
     Readln(f,S);
     if S = 'Entity()' then ReadEntity;
   end;
   CloseFile(F);
   result:=ScenesOverall;
end else
begin
AddToLogFile(EngineLog,'"'+FileName+'" scene file not found.');
result:=0;
end;
end;
{------------------------------------------------------------------}
procedure FreeScene(Ident : cardinal); stdcall;
var i,j : integer;
t : TScene;
begin
 if ScenesCount>0 then
  for i := 0 to ScenesCount - 1 do
  if Ident=Scenes[i].uid then
  begin

    if length(Scenes[i].Models)>0 then
    for j := 0 to length(Scenes[i].Models) - 1 do
    begin
      FreeModel(Scenes[i].Models[j].Mesh);
      if Scenes[i].Models[j].Texture<>0 then
      FreeTexture(Scenes[i].Models[j].Texture);
      if Scenes[i].Models[j].BumpTexture<>0 then
      FreeTexture(Scenes[i].Models[j].BumpTexture);
      if Scenes[i].Models[j].SpecTexture<>0 then
      FreeTexture(Scenes[i].Models[j].SpecTexture);
    end;

    if ScenesCount>1 then
    begin

      t.uid:=Scenes[ScenesCount-1].uid;
      SetLength(t.Models,length(Scenes[ScenesCount-1].Models));
      if length(Scenes[ScenesCount-1].Models)>0 then
       for j := 0 to length(t.Models) - 1 do
        t.Models[j]:=Scenes[ScenesCount-1].Models[j];

      Scenes[i].uid:=t.uid;
       SetLength(Scenes[i].Models,length(t.Models));
      if length(t.Models)>0 then
       for j := 0 to length(t.Models) - 1 do
        Scenes[i].Models[j]:=t.Models[j];

    end;

    ScenesCount:=ScenesCount-1;
    SetLength(Scenes,ScenesCount);

    Exit;
  end;
end;
{------------------------------------------------------------------}
procedure DrawScene(Ident : cardinal);stdcall;
var i,j : integer;
t : Tvertex; alpha : byte; atest : boolean;
begin
if ScenesCount>0 then
for j := 0 to ScenesCount - 1 do
if Ident=Scenes[j].uid then
begin
if not _SceneDontUseMat then
BeginObj3D;
 if length(Scenes[j].Models)>0 then
  for i := 0 to length(Scenes[j].Models) - 1 do
   begin
   t:=ModelBoundingBox(Scenes[j].Models[i].Mesh,0);
   if (not _frustumcalculated or IsBoxInFrustum(Scenes[j].Models[i].Pos.X,Scenes[j].Models[i].Pos.Y,Scenes[j].Models[i].Pos.Z,
      t.X*Scenes[j].Models[i].Scale,t.y*Scenes[j].Models[i].Scale,t.z*Scenes[j].Models[i].Scale)) and Scenes[j].Models[i].Active
   then
    begin
     glPushMatrix();

      glTranslatef(Scenes[j].Models[i].Pos.X,Scenes[j].Models[i].Pos.Y,Scenes[j].Models[i].Pos.Z);
      glScalef(Scenes[j].Models[i].Scale,Scenes[j].Models[i].Scale,Scenes[j].Models[i].Scale);

      atest:=false;

      if not _SceneDontUseMat then
      begin

      if not Scenes[j].Models[i].DoBump or (Scenes[j].Models[i].DoBump and(Scenes[j].Models[i].BumpTexture=0)) then
      glBindTexture(GL_TEXTURE_2D,Scenes[j].Models[i].Texture)
      else
      begin
        ActivateMultitexturingLayer(0);
        glBindTexture(GL_TEXTURE_2D,Scenes[j].Models[i].Texture);
        ActivateMultitexturingLayer(1);
        glBindTexture(GL_TEXTURE_2D,Scenes[j].Models[i].BumpTexture);
        ActivateMultitexturingLayer(2);
        glBindTexture(GL_TEXTURE_2D,Scenes[j].Models[i].SpecTexture);
        ActivateMultitexturingLayer(0);
        glEnable(GL_VERTEX_PROGRAM_ARB);
        glEnable(GL_FRAGMENT_PROGRAM_ARB);
        bump_active:=true;
      end;

      if Scenes[j].Models[i].Material.alpha>252 then alpha:=255 else
      if (Scenes[j].Models[i].Material.alpha<255) and (Scenes[j].Models[i].Material.alpha>220) then
      begin
      atest:=true;
      alpha:=255;
      glEnable(GL_ALPHA_TEST);
      glAlphaFunc(GL_GREATER, 0.3);
      end else alpha:=Scenes[j].Models[i].Material.alpha;

      Color3D(RGB(Scenes[j].Models[i].Material.diffuse[0],Scenes[j].Models[i].Material.diffuse[1],Scenes[j].Models[i].Material.diffuse[2]),
      alpha,false,Scenes[j].Models[i].Material.glossiness);

      end;

      DrawModel(Scenes[j].Models[i].Mesh,Scenes[j].Models[i].MeshFrame,Scenes[j].Models[i].MeshSmooth);

      if not _SceneDontUseMat then
      begin

      if atest then glDisable(GL_ALPHA_TEST);
      if Scenes[j].Models[i].DoBump and (Scenes[j].Models[i].BumpTexture<>0) then
      begin
        ActivateMultitexturingLayer(0);
        glBindTexture(GL_TEXTURE_2D,0);
        ActivateMultitexturingLayer(1);
        glBindTexture(GL_TEXTURE_2D,0);
        ActivateMultitexturingLayer(2);
        glBindTexture(GL_TEXTURE_2D,0);
        ActivateMultitexturingLayer(0);
        glDisable(GL_VERTEX_PROGRAM_ARB);
        glDisable(GL_FRAGMENT_PROGRAM_ARB); 
        bump_active:=false;
      end;

      end;

     glPopMatrix();
    end;
   end;
if not _SceneDontUseMat then
EndObj3D;
Exit;
end;
end;
{------------------------------------------------------------------}
function SceneBoundingBox(Ident : cardinal):TVertex; stdcall;
var i, j : integer;
min, max, mbound : Tvertex;
begin

result.X:=0;
result.Y:=0;
result.Z:=0;

if ScenesCount>0 then
for j := 0 to ScenesCount - 1 do
if Ident=Scenes[j].uid then
begin

min:=result;
max:=result;

 if length(Scenes[j].Models)>0 then
  for i := 0 to length(Scenes[j].Models) - 1 do
  with Scenes[j].Models[i] do
  begin

   mbound:=ModelBoundingBox(Mesh,0);

   if max.X<mbound.X+Pos.X then max.X:=mbound.X+Pos.X;
   if max.Y<mbound.Y+Pos.Y then max.Y:=mbound.Y+Pos.Y;
   if max.Z<mbound.Z+Pos.Z then max.Z:=mbound.Z+Pos.Z;

   if min.X>mbound.X+Pos.X then min.X:=mbound.X+Pos.X;
   if min.Y>mbound.Y+Pos.Y then min.Y:=mbound.Y+Pos.Y;
   if min.Z>mbound.Z+Pos.Z then min.Z:=mbound.Z+Pos.Z;

  end;

  result.X:=max.X-min.X;
  result.Y:=max.Y-min.Y;
  result.Z:=max.Z-min.Z;
Exit;
end;
end;
{3D Multy Mesh (models)--------------------------------------------------------}
procedure ModelsBump(Active : boolean); stdcall;
begin
bump_active:=Active;
end;
{------------------------------------------------------------------}
function ModelMaterial(Modelident : integer) : TMaterial; stdcall;
var i : integer; s : TMaterial;
begin
if MeshsCount<>0 then
for i:=0 to MeshsCount-1 do
 if Meshs[i].Ident=ModelIdent then
 begin
  if Meshs[i].Mesh.MaterialPresented then
  result:=Meshs[i].Mesh.Material else
  begin
  s.diffuse[0]:=255;
  s.diffuse[1]:=255;
  s.diffuse[2]:=255;
  s.glossiness:=10;
  s.alpha:=255;
  s.TexFileName:='None';
  s.NormalMapFileName:='None';
  s.SpecularMapFileName:='None';
  result:=s;
  end;
  Exit;
 end;
end;
{------------------------------------------------------------------}
procedure  GetModelGeometry(ModelIdent, Frame : integer; GeometryData : PMeshGeometry); stdcall;
var i,j : integer;
begin
if MeshsCount<>0 then
for i:=0 to MeshsCount-1 do
 if Meshs[i].Ident=ModelIdent then
 with GeometryData^ do
 begin

   VerticesCount:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).VertexCount;
   FacesCount:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).FacesCount;

   SetLength(Vertices,VerticesCount);
   SetLength(Normals,VerticesCount);
   SetLength(TextureVertices,VerticesCount);
   SetLength(Tangents,length(TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Tangent));

   for j := 0 to VerticesCount - 1 do
    begin

    Vertices[j]:=vertex(TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Vertices[j].x,TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Vertices[j].y,TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Vertices[j].z);

    case Meshs[i].Mesh.ScaleType of
    1:
    begin
      Vertices[j].X:=Vertices[j].X*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtent;
      Vertices[j].Y:=Vertices[j].Y*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtent;
      Vertices[j].Z:=Vertices[j].Z*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtent;
    end;
    2:
    begin
      Vertices[j].X:=Vertices[j].X*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtentX;
      Vertices[j].Y:=Vertices[j].Y*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtentY;
      Vertices[j].Z:=Vertices[j].Z*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtentZ;
    end;
    3:
    begin
      Vertices[j].X:=Vertices[j].X*Meshs[i].Mesh.fAllScale;
      Vertices[j].Y:=Vertices[j].Y*Meshs[i].Mesh.fAllScale;
      Vertices[j].Z:=Vertices[j].Z*Meshs[i].Mesh.fAllScale;
    end;
    end;

     Normals[j]:=vertex(TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).SmoothNormals[j].x,TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).SmoothNormals[j].y,TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).SmoothNormals[j].z);
     TextureVertices[j]:=vertex(Meshs[i].Mesh.TexVertices[j].x,Meshs[i].Mesh.TexVertices[j].y,Meshs[i].Mesh.TexVertices[j].z);

     if length(Tangents)>0 then
     Tangents[j]:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Tangent[j];

    end;

   SetLength(Faces,FacesCount);
   if Meshs[i].Mesh.TexturePresent then
   SetLength(TextureFaces,FacesCount) else
   SetLength(TextureFaces,0);

   for j := 0 to VerticesCount - 1 do
    begin
     Faces[j][0]:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Faces[j][0];
     Faces[j][1]:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Faces[j][1];
     Faces[j][2]:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Faces[j][2];
     if Meshs[i].Mesh.TexturePresent then
     begin
     TextureFaces[j][0]:=Meshs[i].Mesh.TexFaces[j][0];
     TextureFaces[j][1]:=Meshs[i].Mesh.TexFaces[j][1];
     TextureFaces[j][2]:=Meshs[i].Mesh.TexFaces[j][2];
     end;
    end;

  Exit;
 end;
end;
{------------------------------------------------------------------}
function ModelTrianglesCount(Modelident,Frame : integer) : Cardinal; stdcall;
var i : integer;
begin
result:=0;
if MeshsCount<>0 then
for i:=0 to MeshsCount-1 do
 if Meshs[i].Ident=ModelIdent then
 begin
  result:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).FacesCount;
  Exit;
 end;
end;
{------------------------------------------------------------------}
function ModelBoundingBox(Modelident,Frame : integer):TVertex; stdcall;
var i : integer;
begin
if MeshsCount<>0 then
for i:=0 to MeshsCount-1 do
 if Meshs[i].Ident=ModelIdent then
 begin

    if Frame>Meshs[i].Mesh.Meshes.Count-1 then Frame:=0;
    case Meshs[i].Mesh.ScaleType of
    1:begin
    result.x:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Width*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtent;
    result.y:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Height*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtent;
    result.z:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Depth*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtent;
    end;
    2:begin
    result.x:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Width*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtentX;
    result.y:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Height*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtentY;
    result.z:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Depth*TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).fExtentZ;
    end;
    3:begin
    result.x:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Width*Meshs[i].Mesh.fAllScale;
    result.y:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Height*Meshs[i].Mesh.fAllScale;
    result.z:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Depth*Meshs[i].Mesh.fAllScale;
    end;
    else
    begin
    result.x:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Width;
    result.y:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Height;
    result.z:=TGLMesh(Meshs[i].Mesh.Meshes.Items[Frame]).Depth;
    end;
    end;//case

 Exit;
 end;

end;
{------------------------------------------------------------------}
function ModelFramesCount(Modelident : integer):Integer; stdcall;
var i : integer;
begin
result:=0;
if MeshsCount<>0 then
for i:=0 to MeshsCount-1 do
 if Meshs[i].Ident=ModelIdent then
 begin
 result:=Meshs[i].Mesh.Meshes.Count;
 Exit;
 end;
end;
{------------------------------------------------------------------}
function LoadModel(Filename : string; ScaleType : byte; NormalInv : boolean) : integer; stdcall;
begin
 if fileexists(Filename) then
 begin
 inc(OverAllMeshUsed);

 SetLength(Meshs,MeshsCount+1);
 result:=OverAllMeshUsed;

 try
 Meshs[MeshsCount].Ident:=OverAllMeshUsed;
 Meshs[MeshsCount].Mesh:=TGLMultyMesh.Create;
 Meshs[MeshsCount].Mesh.LoadFromFile(Filename,NormalInv);
 Meshs[MeshsCount].Mesh.ScaleType := ScaleType;
 Meshs[MeshsCount].Mesh.fSmooth:= false;
 inc(MeshsCount);
 except
 //AddToLogFile(EngineLog,'Model "'+ Filename +'" not loaded. May be file corrupted or wrong file.');
 result:=0;
 Exit;
 end;
 //AddToLogFile(EngineLog,'Model "'+ Filename +'" loaded successfully.');
 end else
 begin
 //AddToLogFile(EngineLog,'Model file "'+ Filename +'" not found!');
 //MessageBox(0, PChar('Model file "'+ Filename +'" not found!'), PChar('Draw3D Unit'), MB_OK or MB_ICONERROR);
 result:=0;
 end;
end;
{------------------------------------------------------------------}
procedure DrawModel(ModelIdent, Frame : integer; Smooth : boolean); stdcall;
var i : cardinal;
begin
if MeshsCount<>0 then
for i:=0 to MeshsCount-1 do
 if Meshs[i].Ident=ModelIdent then
 begin
 Meshs[i].Mesh.fSmooth:=Smooth;
 if (Meshs[i].Mesh.Meshes.Count>1) and (Frame<Meshs[i].Mesh.Meshes.Count) and
 (Frame>0) then
 Meshs[i].Mesh.CurrentFrame:=Frame else Meshs[i].Mesh.CurrentFrame:=0;
 Meshs[i].Mesh.Draw;
 Exit;
 end;
end;
{------------------------------------------------------------------}
procedure FreeModel(ModelIdent : integer); stdcall;
var i : cardinal;
T : TAMesh;
begin
for i:=0 to MeshsCount-1 do
 if Meshs[i].Ident=ModelIdent then
 begin

 Meshs[i].Mesh.Free;

 if MeshsCount>1 then
 begin
 T:=Meshs[MeshsCount-1];
 Meshs[i]:=T;
 end;

 SetLength(Meshs,MeshsCount-1);
 dec(MeshsCount);

 Exit;
 end;
end;
{Other 3D routines-------------------------------------------------------------}
procedure BeginObj3D; stdcall;
begin
   InBlock:=true;
   // Grow array only when needed — avoids per-frame heap churn
   if Obj3DInfoCount >= Length(Obj3DInfo) then
     SetLength(Obj3DInfo, Obj3DInfoCount + 8);
   Obj3DInfo[Obj3DInfoCount].Texture:=curTexture;
   Obj3DInfo[Obj3DInfoCount].Projecting:=Projecting;
   glGetFloatv(GL_CURRENT_COLOR, @Obj3DInfo[Obj3DInfoCount].Color);
   Inc(Obj3DInfoCount);

  if Projecting then
  glEnable(GL_BLEND);

  glPushMatrix();
end;
{------------------------------------------------------------------}
procedure EndObj3D; stdcall;
begin
  glPopMatrix();
  Dec(Obj3DInfoCount);
  glcolor4f(Obj3DInfo[Obj3DInfoCount].Color[1],Obj3DInfo[Obj3DInfoCount].Color[2],Obj3DInfo[Obj3DInfoCount].Color[3],Obj3DInfo[Obj3DInfoCount].Color[4]);
  SetTexture(Obj3DInfo[Obj3DInfoCount].Texture);
  Projecting:=Obj3DInfo[Obj3DInfoCount].Projecting;
  if not Projecting then
  begin
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
  end;
  glDisable(GL_BLEND);
  InBlock:=false;
end;
{------------------------------------------------------------------}
procedure CalculateFrustum; stdcall;
begin
_frustumcalculated:=true;
Frustum.Calculate;
end;
{------------------------------------------------------------------}
function IsPointInFrustum(X,Y,Z : single) : boolean; stdcall;
begin
result:=Frustum.IsPointWithin(X,Y,Z);
end;
{------------------------------------------------------------------}
function IsSphereInFrustum(X,Y,Z,Radius : single) : boolean; stdcall;
begin
result:=Frustum.IsSphereWithin(X,Y,Z,Radius);
end;
{------------------------------------------------------------------}
function IsBoxInFrustum(X,Y,Z,W,H,D : single) : boolean; stdcall;
begin
result:=Frustum.IsBoxWithin(X,Y,Z,W,H,D);
end;
{------------------------------------------------------------------}
procedure SetMultytexturingLayerOffset(Layer : cardinal; X,Y : single); stdcall;
begin
 MultyCoordOffset[Layer][0]:=X;
 MultyCoordOffset[Layer][1]:=Y;
end;
{------------------------------------------------------------------}
procedure SetMultytexturingLayerTexCoordMulti(Layer : cardinal; X,Y : single); stdcall;
begin
 MultyCoordOffset[Layer][2]:=X;
 MultyCoordOffset[Layer][3]:=Y;
end;
{------------------------------------------------------------------}
procedure DrawPolygon3D(points : array of TVertex3D); stdcall;
var i : integer;
begin
 glBegin(GL_POLYGON);
  for i:=0 to Length(points)-1 do
   begin
   glcolor4ub(GetRValue(points[i].Color),GetGValue(points[i].Color),GetBValue(points[i].Color),points[i].Alpha);
   _glTexCoord2f(points[i].TexX,points[i].TexY);
   glVertex3f(points[i].X,points[i].Y,points[i].Z);
   end;
 glEnd;
end;
{------------------------------------------------------------------}
function CreateTextureToRenderIn(TextureWidth,TextureHeight : integer):GlUint; stdcall;
begin
  result:=CreateRenderTex(TextureWidth,TextureHeight);
end;
{------------------------------------------------------------------}
procedure SetCamera(Camera : TCamera); stdcall;
var
  converted: Int64;
  low, high: Cardinal;
begin
end;
{------------------------------------------------------------------}
procedure ClearZBuffer; stdcall;
begin
glClear(GL_DEPTH_BUFFER_BIT);
end;
{------------------------------------------------------------------}
procedure ZBuffer(Active : boolean); stdcall;
begin
if Active then glEnable(GL_DEPTH_TEST) else glDisable(GL_DEPTH_TEST);
end;
{------------------------------------------------------------------}
procedure ResetMatrix; stdcall;
begin
glLoadIdentity();
end;
{------------------------------------------------------------------}
procedure SetFog(Color : Integer; Fog_Start, Fog_End : single); stdcall;
var fogColor : Array [0..3] of GLFloat;
    scaledStart, scaledEnd, scale, minEnd: Single;
begin
 fogColor[0]:=GetRValue(Color)/255;
 fogColor[1]:=GetGValue(Color)/255;
 fogColor[2]:=GetBValue(Color)/255;
 fogColor[3]:=1.0;
 scaledStart := Fog_Start;
 scaledEnd := Fog_End;

 // При увеличенной дальности штатный clear-weather fog (обычно около
 // 5000 м) успевает стать полностью белым внутри видимой сцены. Отсюда
 // белая плита на горизонте. Растягиваем только дальний fog; короткий
 // погодный туман 50/100/500/1000 м оставляем как задано сценарием.
 if (maxvisibledistance > 1500.0) and (Fog_End > 1500.0) then
 begin
   scale := maxvisibledistance / 1500.0;
   scaledStart := Fog_Start * scale;
   scaledEnd := Fog_End * scale;
   minEnd := maxvisibledistance * 2.5;
   if scaledEnd < minEnd then
     scaledEnd := minEnd;
 end;

 glEnable(GL_FOG);
 glFogi  (GL_FOG_MODE, GL_LINEAR);
 glHint  (GL_FOG_HINT, GL_DONT_CARE);
 glFogf  (GL_FOG_START, scaledStart);
 glFogf  (GL_FOG_END, scaledEnd);
 glFogfv (GL_FOG_COLOR, @fogColor);
end;
{------------------------------------------------------------------}
procedure DeactiveFog; stdcall;
begin
 glDisable(GL_FOG);
end;
{------------------------------------------------------------------}
procedure DrawCylinder(Radius,Height : single); stdcall;
begin
  gluCylinder(QuadraticObject, Radius, Radius, Height, 24, 1);
  gluQuadricOrientation(QuadraticObject, GLU_INSIDE);
  gluDisk(QuadraticObject, 0, Radius, 24, 1);
  gluQuadricOrientation(QuadraticObject, GLU_OUTSIDE);
  glTranslatef(0, 0, Height);
  gluDisk(QuadraticObject, 0, Radius, 24, 1);
end;
{------------------------------------------------------------------}
procedure DrawSphere(Radius : single); stdcall;
begin
glPushMatrix();
glScalef(Radius,Radius,Radius);
glCallList(SphereDL);
glPopMatrix();
end;


{------------------------------------------------------------------}
procedure DrawEllipse(Width,Height,Depth : single); stdcall;
begin
glPushMatrix();
glScalef(Width,Height,Depth);
glCallList(SphereDL);
glPopMatrix();
end;
{------------------------------------------------------------------}
procedure DrawPlane(Width,Height : single); stdcall;
begin
glBegin(GL_QUADS);
 glNormal3f( 0.0, 0.0, 1.0);
 _glTexCoord2f(0,1);
 glVertex2f(-Width/2,-Height/2);
 _glTexCoord2f(1,1);
 glVertex2f(Width/2, -Height/2);
 _glTexCoord2f(1,0);
 glVertex2f(Width/2,  Height/2);
 _glTexCoord2f(0,0);
 glVertex2f(-Width/2, Height/2);
glEnd;
end;
{------------------------------------------------------------------}
procedure DrawSprite(Width,Height : single; FramesXCount, FramesYCount, FrameNumber: integer);stdcall;
var imgWidth, imgHeight : glfloat; XFrame, YFrame : byte;
begin
  if FramesXCount=0 then FramesXCount:=1;
  if FramesYCount=0 then FramesYCount:=1;

  imgWidth:=1.0/FramesXCount;
  imgHeight:=1.0/FramesYCount;

  YFrame:=(FrameNumber div FramesXCount)+1;
  if FrameNumber mod FramesXCount = 0 then YFrame:=YFrame-1;
  XFrame:=FrameNumber - (YFrame-1)*FramesXCount;

  XFrame:=XFrame-1;
  YFrame:=YFrame-1;

      glBegin(GL_QUADS);
        glNormal3f( 0.0, 0.0, 1.0);
        _glTexCoord2f(imgWidth*XFrame, imgHeight*YFrame);
        glVertex2f(-Width/2,-Height/2);

        _glTexCoord2f(imgWidth*XFrame+imgWidth, imgHeight*YFrame);
        glVertex2f(Width/2, -Height/2);

        _glTexCoord2f(imgWidth*XFrame+imgWidth, imgHeight*YFrame+imgHeight);
        glVertex2f( Width/2,  Height/2);

        _glTexCoord2f(imgWidth*XFrame, imgHeight*YFrame+imgHeight);
        glVertex2f(-Width/2,  Height/2);
      glEnd;

end;
{------------------------------------------------------------------}
procedure DrawSprite_BillBoard(Width,Height : single; FramesXCount, FramesYCount, FrameNumber: integer);stdcall;
var imgWidth, imgHeight : glfloat; XFrame, YFrame : byte;
m : TGLMatrixd4;
v : array [0..3] of TVertex;
length : single;
begin
  if FramesXCount=0 then FramesXCount:=1;
  if FramesYCount=0 then FramesYCount:=1;

  imgWidth:=1.0/FramesXCount;
  imgHeight:=1.0/FramesYCount;

  YFrame:=(FrameNumber div FramesXCount)+1;
  if FrameNumber mod FramesXCount = 0 then YFrame:=YFrame-1;
  XFrame:=FrameNumber - (YFrame-1)*FramesXCount;

  XFrame:=XFrame-1;
  YFrame:=YFrame-1;

  glGetDoublev(GL_MODELVIEW_MATRIX, @m );

  Width:=Width/1.41;
  Height:=Height/1.41;

  v[0].X:=-m[0][0]-m[0][1];
  v[0].Y:=-m[1][0]-m[1][1];
  v[0].Z:=-m[2][0]-m[2][1];

  length:=sqrt(sqr(v[0].X) + sqr(v[0].Y) + sqr(v[0].Z));
  v[0].X:=v[0].X/length*Width;
  v[0].Y:=v[0].Y/length*Height;
  v[0].Z:=v[0].Z/length*Width;

  v[1].X:=m[0][0]-m[0][1];
  v[1].Y:=m[1][0]-m[1][1];
  v[1].Z:=m[2][0]-m[2][1];

  length:=sqrt(sqr(v[1].X) + sqr(v[1].Y) + sqr(v[1].Z));
  v[1].X:=v[1].X/length*Width;
  v[1].Y:=v[1].Y/length*Height;
  v[1].Z:=v[1].Z/length*Width;

  v[2].X:=m[0][0]+m[0][1];
  v[2].Y:=m[1][0]+m[1][1];
  v[2].Z:=m[2][0]+m[2][1];

  length:=sqrt(sqr(v[2].X) + sqr(v[2].Y) + sqr(v[2].Z));
  v[2].X:=v[2].X/length*Width;
  v[2].Y:=v[2].Y/length*Height;
  v[2].Z:=v[2].Z/length*Width;

  v[3].X:=-m[0][0]+m[0][1];
  v[3].Y:=-m[1][0]+m[1][1];
  v[3].Z:=-m[2][0]+m[2][1];

  length:=sqrt(sqr(v[3].X) + sqr(v[3].Y) + sqr(v[3].Z));
  v[3].X:=v[3].X/length*Width;
  v[3].Y:=v[3].Y/length*Height;
  v[3].Z:=v[3].Z/length*Width;

      glBegin(GL_QUADS);
        glNormal3f( 0.0, 0.0, 1.0);
        _glTexCoord2f(imgWidth*XFrame, imgHeight*YFrame);
        glVertex3fv(@v[0]);

        _glTexCoord2f(imgWidth*XFrame+imgWidth, imgHeight*YFrame);
        glVertex3fv(@v[1]);

        _glTexCoord2f(imgWidth*XFrame+imgWidth, imgHeight*YFrame+imgHeight);
        glVertex3fv(@v[2]);

        _glTexCoord2f(imgWidth*XFrame, imgHeight*YFrame+imgHeight);
        glVertex3fv(@v[3]);
      glEnd;

end;
{------------------------------------------------------------------}
procedure DrawTextureToTexture(TexSource,TexTarget : GluInt; X,Y : integer); stdcall;
var
pBits : pByteArray;
begin
GetMem(pBits,GetTextureInfo(TexSource).Width*GetTextureInfo(TexSource).Height*3);
glBindTexture(GL_TEXTURE_2D, TexSource);
glGetTexImage(GL_TEXTURE_2D,0,GL_RGB,GL_UNSIGNED_BYTE,pBits);
glBindTexture(GL_TEXTURE_2D, TexTarget);
glTexParameterf(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, 0);
glTexSubImage2D(GL_TEXTURE_2D,0,X,Y,GetTextureInfo(TexSource).Width,GetTextureInfo(TexSource).Height,GL_RGB,GL_UNSIGNED_BYTE,pBits);
glBindTexture(GL_TEXTURE_2D, 0);
FreeMem(pBits);
end;
{------------------------------------------------------------------}
procedure RenderTexToTexFBO(ToTexture,TexSource,TexTarget : GluInt; X,Y : integer); stdcall;
var
w, h, w1, h1 : cardinal;
begin
if GL_EXT_framebuffer_object and _UseFBO then
begin
w:=GetTextureInfo(TexTarget).Width;
h:=GetTextureInfo(TexTarget).Height;

w1:=GetTextureInfo(TexSource).Width;
h1:=GetTextureInfo(TexSource).Height;

if (GetTextureInfo(ToTexture).Width<>w) or (GetTextureInfo(ToTexture).Height<>h) then Exit;

glPushMatrix();
glViewport(0, 0, w, h);
glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo2);
glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, ToTexture, 0);
if glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT) <> GL_FRAMEBUFFER_COMPLETE_EXT then
begin
glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
DrawTextureToTexture(TexSource,TexTarget,x,y);
DrawTextureToTexture(TexTarget,ToTexture,0,0);
AddToLogFile(EngineLog,'Faild to "RenderTexToTexFBO". Error in FBO initialization.');
Exit;
end;
glLoadIdentity();
glClear(GL_COLOR_BUFFER_BIT);
glMatrixMode(GL_PROJECTION);
glPushMatrix;
glLoadIdentity;
gluOrtho2D(0,w,0,h);
glMatrixMode(GL_MODELVIEW);
glPushMatrix;
glLoadIdentity;

glEnable(GL_ALPHA_TEST);
glAlphaFunc(GL_GREATER, 0.1);

glBindTexture(GL_TEXTURE_2D,TexTarget);

  glBegin(GL_QUADS);
    glTexCoord2f(0,0);
    glVertex2f(0,0);
    glTexCoord2f(1,0);
    glVertex2f(w,0);
    glTexCoord2f(1,1);
    glVertex2f(w,h);
    glTexCoord2f(0,1);
    glVertex2f(0,h);
   glEnd;

if TexSource<>0 then
begin
glBindTexture(GL_TEXTURE_2D,TexSource);

  glBegin(GL_QUADS);
    glTexCoord2f(0,0);
    glVertex2f(X,Y);
    glTexCoord2f(1,0);
    glVertex2f(X+W1,Y);
    glTexCoord2f(1,1);
    glVertex2f(X+W1,Y+H1);
    glTexCoord2f(0,1);
    glVertex2f(X,Y+H1);
   glEnd;
end;

glBindTexture(GL_TEXTURE_2D,0);
glDisable(GL_ALPHA_TEST);

glPopMatrix;
glMatrixMode(GL_PROJECTION);
glPopMatrix;
glMatrixMode(GL_MODELVIEW);
glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0);
glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
glViewport(0, 0, CurW, CurH);
glPopMatrix();
glBindTexture(GL_TEXTURE_2D, 0);
glClear(GL_COLOR_BUFFER_BIT);
end;
end;
{------------------------------------------------------------------}
procedure DrawTextureToTextureTransparentColor(TexSource,TexTarget : GluInt; X,Y : integer; Color : Cardinal); stdcall;
type
TRGB = record
  R, G, B : Byte;
 end;
var c : TRGB;
pBits : pByteArray;
i,line : integer;
w, h : cardinal;
begin
w:=GetTextureInfo(TexSource).Width;
h:=GetTextureInfo(TexSource).Height;
GetMem(pBits,w*h*3);
glBindTexture(GL_TEXTURE_2D, TexSource);
glGetTexImage(GL_TEXTURE_2D,0,GL_RGB,GL_UNSIGNED_BYTE,pBits);
glBindTexture(GL_TEXTURE_2D, TexTarget);
glTexParameterf(GL_TEXTURE_2D, GL_GENERATE_MIPMAP_SGIS, 0);
line:=0;
for i:=0 to ((w*h*3) div 3)-1 do
 if not ((pBits[i*3]=GetRvalue(Color)) and (pBits[i*3+1]=GetGvalue(Color)) and (pBits[i*3+2]=GetBvalue(Color))) then
 begin
 if i-w*line > w then line:=line+1;
 c.R:=pBits[i*3];
 c.G:=pBits[i*3+1];
 c.B:=pBits[i*3+2];
 glTexSubImage2D(GL_TEXTURE_2D,0,X+(i-line*w),Y+line,1,1,GL_RGB,GL_UNSIGNED_BYTE,@c);
 end;
glBindTexture(GL_TEXTURE_2D, 0);
FreeMem(pBits);
end;
{------------------------------------------------------------------}
procedure DeactiveLight(ID : integer); stdcall;
var i : integer;
begin
if ID = -1 then
 begin
   for i:=0 to 20 do
   begin
    LightsOn[i]:=False;
    glDisable(GETLIGHT(i));
   end;
  glDisable(GL_LIGHTING);
 end else
 begin
  glDisable(GETLIGHT(ID));
  LightsOn[ID]:=FALSE;
 end;
end;
{------------------------------------------------------------------}
procedure SetLight(ID : integer; X,Y,Z : single; LightColor : integer; Radius : single; Visualize : boolean; Scale : single); stdcall;
var
Color : Array [1..4] of GLFloat;
LightPos : Array [0..3] of GLfloat;
begin

if (ID = -1) or (ID>GL_MAX_LIGHTS) then Exit;

  LightPos[0]:=X;
  LightPos[1]:=Y;
  LightPos[2]:=Z;
  LightPos[3]:=1;

  glLightfv(GETLIGHT(ID), GL_POSITION, @LightPos);

  light_diffuse[0]:=GetRValue(LightColor)/255;
  light_diffuse[1]:=GetGValue(LightColor)/255;
  light_diffuse[2]:=GetBValue(LightColor)/255;
  light_diffuse[3]:=1.0;

  light_specular[0]:=GetRValue(LightColor)/255;
  light_specular[1]:=GetGValue(LightColor)/255;
  light_specular[2]:=GetBValue(LightColor)/255;
  light_specular[3]:=1.0;

  glMaterialfv(GL_FRONT,  GL_SPECULAR, @mat_specular);

  glLightfv(GETLIGHT(ID), GL_AMBIENT,  @light_ambient);
  glLightfv(GETLIGHT(ID), GL_DIFFUSE,  @light_diffuse);
  glLightfv(GETLIGHT(ID), GL_SPECULAR, @light_specular);

  if radius<0 then
  begin
  glLightf(GETLIGHT(ID), GL_CONSTANT_ATTENUATION, 1.0);
  glLightf(GETLIGHT(ID), GL_LINEAR_ATTENUATION, 0);
  end else
  begin
  glLightf(GETLIGHT(ID), GL_CONSTANT_ATTENUATION, 0);
  glLightf(GETLIGHT(ID), GL_LINEAR_ATTENUATION, 10/radius);
  end;

if Visualize then
 begin
  glGetFloatv(GL_CURRENT_COLOR, @Color);
   glDisable(GL_LIGHTING);
   glDisable(GL_TEXTURE_2D);
   glPointSize(Scale);
   glBegin(GL_POINTS);
   glColor3f(light_diffuse[0],light_diffuse[1],light_diffuse[2]);
   glVertex3f(X,Y,Z);
   glEnd();
   glPointSize(1.0);
   glEnable(GL_TEXTURE_2D);
  glcolor4f(Color[1],Color[2],Color[3],Color[4]);
 end;

  glEnable(GL_LIGHTING);
  glEnable(GETLIGHT(ID));
  LightsOn[ID]:=TRUE;
end;
{------------------------------------------------------------------}
procedure Position3D(X,Y,Z : single); stdcall;
begin
if not In2DWeAre then
glTranslatef(x,y,z);
end;
{------------------------------------------------------------------}
procedure Scale3D(Scale : single); stdcall;
begin
glScalef(Scale,Scale,Scale);
end;
{------------------------------------------------------------------}
procedure SetTexture(Texture : gluint); stdcall;
begin
if not Projecting then
begin
 if not InBlock then CurTexture:=Texture;
 glBindTexture(GL_TEXTURE_2D, Texture);
end;
end;
{------------------------------------------------------------------}
procedure ActivateMultitexturingLayer(Layer : Cardinal); stdcall;
begin
 if GL_ARB_multitexture then
 begin
 glActiveTextureARB(GL_TEXTURE0_ARB+Layer);
 glEnable(GL_TEXTURE_2D);
 MultyTexActive:=true;
 end;
end;
{------------------------------------------------------------------}
procedure DeactiveMultytexturing; stdcall;
var i : integer;
begin
 if GL_ARB_multitexture then
 begin
  for i:=0 to 5 do
   begin
   glActiveTextureARB(GL_TEXTURE0_ARB+i);
   if i<>0 then
   glDisable(GL_TEXTURE_2D);
   SetTexture(0);
   end;
   glActiveTextureARB(GL_TEXTURE0_ARB);
 end;
end;
{------------------------------------------------------------------}
procedure Position2D(X,Y : integer); stdcall;
begin
if In2DWeAre then
glTranslatef((4.42/InitResX)*X,-(3.314/InitResY)*Y,0.0);
end;
{------------------------------------------------------------------}
procedure Color3D(Color:integer; Alpha : byte; Diffuse : boolean; MaterialShininess : single); stdcall;
begin
mat_shininess:=MaterialShininess;
glMaterialfv(GL_FRONT, GL_SHININESS, @mat_shininess);

 if Alpha<>255 then
 begin
 GlEnable(GL_Blend);

 if not Projecting then
   if not Diffuse then
     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      else
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);

 end;

 if Alpha=254 then Alpha:=255;

glcolor4ub(GetRValue(Color),GetGValue(Color),GetBValue(Color),Alpha);

end;
{------------------------------------------------------------------}
procedure AdductingMatrix3Dto2D; stdcall;
begin
  glPushMatrix;
  glLoadIdentity;
  glTranslatef(-2.210,1.657,-4.000);
  In2DWeAre:=TRUE;
    //on screen matrix width  4.42
    //                 height 3.314
end;
{------------------------------------------------------------------}
procedure ReturnStandartMatrix3D; stdcall;
begin
  glPopMatrix;
  In2DWeAre:=FALSE;
end;
{------------------------------------------------------------------}
procedure RotateX(Angle : single); stdcall;
begin
glRotatef(Angle,-1,0,0);
end;
{------------------------------------------------------------------}
procedure RotateY(Angle : single); stdcall;
begin
glRotatef(Angle,0,-1,0);
end;
{------------------------------------------------------------------}
procedure RotateZ(Angle : single); stdcall;
begin
glRotatef(Angle,0,0,-1);
end;
{------------------------------------------------------------------}
procedure DrawLine(X,Y,Z,X1,Y1,Z1 : single; LineWidth : real = 1.0; Smooth : boolean = true); stdcall;
begin
 if Smooth then
  begin
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_BLEND);
  end;
 glLineWidth(LineWidth);
  glBegin(GL_LINES);
    glVertex3f(X,Y,Z);
    glVertex3f(X1,Y1,Z1);
  glEnd;
 if Smooth then
  begin
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_BLEND);
  end;
end;
{------------------------------------------------------------------}
procedure DrawPoint(X,Y,Z : single); stdcall;
begin
 glBegin(GL_POINTS);
    glVertex3f(X,Y,Z);
 glEnd;
end;
{------------------------------------------------------------------}
procedure EnableSphereMapping; stdcall;
begin
  glTexGenf(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  glTexGenf(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  glEnable(GL_TEXTURE_GEN_S);
  glEnable(GL_TEXTURE_GEN_T);
end;
{------------------------------------------------------------------}
procedure DisableSphereMapping; stdcall;
begin
 glDisable(GL_TEXTURE_GEN_S);
 glDisable(GL_TEXTURE_GEN_T);
end;
{------------------------------------------------------------------}
procedure StartRenderToTexture(Texture : GlUint); stdcall;
var
b : BYTEBOOL;
mode : Integer;
begin

  RenderTTWidth:=GetTextureInfo(Texture).Width;
  RenderTTHeight:=GetTextureInfo(Texture).Height;
  RenderedTex:=Texture;

glPushMatrix();

  glViewport(0, 0, RenderTTWidth, RenderTTHeight);

  if GetTextureInfo(Texture).FileType=5 then
  begin
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glGetBooleanv(GL_CULL_FACE,@b);
  glGetIntegerv(GL_CULL_FACE_MODE,@mode);
  if b and (mode=GL_FRONT) then
  //glPolygonOffset(4.5, 3.25)
  glPolygonOffset(0.5, 0.25)
  else
  glPolygonOffset(5, 4);
  glEnable(GL_POLYGON_OFFSET_FILL);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(ShadowRenderAngle, RenderTTWidth/RenderTTWidth, InitZNear, InitZFar);
  glMatrixMode(GL_MODELVIEW);
  end;


  if GL_EXT_framebuffer_object and _UseFBO then
  begin
   InitFBO(RenderTTWidth,RenderTTHeight,InitZBuffer);
   glBindTexture(GL_TEXTURE_2D, 0);
   glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo_frame);
    if GetTextureInfo(RenderedTex).FileType=5 then
    begin
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, RenderedTex, 0);
    glDrawBuffer(GL_NONE);
    end else
    begin
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, RenderedTex, 0);
    glDrawBuffer(GL_FRONT);
    end;
   CantRenderInFBO:=false;
   if glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT) <> GL_FRAMEBUFFER_COMPLETE_EXT then
   begin
   CantRenderInFBO:=true;
   glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
   end;

  end;

 glLoadIdentity();
 if GetTextureInfo(RenderedTex).FileType=5 then
 glClear(GL_DEPTH_BUFFER_BIT)
 else
 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

end;
{------------------------------------------------------------------}
procedure EndRenderToTexture; stdcall;
var i : integer;
begin


   if GL_EXT_framebuffer_object and _UseFBO and not CantRenderInFBO then
   begin
   if GetTextureInfo(RenderedTex).FileType=5 then
   glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, 0, 0)
   else
   glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0);
   glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
   end else
   begin
   glBindTexture(GL_TEXTURE_2D, RenderedTex);
   if GetTextureInfo(RenderedTex).FileType=5 then
   glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, 0, 0, RenderTTWidth, RenderTTHeight, 0)
   else
   glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, RenderTTWidth, RenderTTHeight, 0);
   end;

   glViewport(0, 0, CurW, CurH);


   if GetTextureInfo(RenderedTex).FileType=5 then
   begin
    for i:=0 to 20 do
      if LightsOn[i] then glEnable(GL_LIGHTING);
   glEnable(GL_TEXTURE_2D);
   glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
   glDisable(GL_POLYGON_OFFSET_FILL);
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity;
   gluPerspective(initAngle, CurW/CurH, InitZNear, InitZFar);
   glMatrixMode(GL_MODELVIEW);
   end;

glPopMatrix();
glBindTexture(GL_TEXTURE_2D, 0);
if GetTextureInfo(RenderedTex).FileType=5 then
glClear(GL_DEPTH_BUFFER_BIT)
else
glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;
{------------------------------------------------------------------}
procedure DrawAxes(Length : single = 1.0); stdcall;
var
Color : Array [1..4] of GLFloat;
begin
 glGetFloatv(GL_CURRENT_COLOR, @Color);

glBegin (GL_LINES);
 glColor3f(1,0,0);
 glVertex3f(0,0,0);
 glVertex3f(Length,0,0);
 glColor3f(0,1,0);
 glVertex3f(0,0,0);
 glVertex3f(0,Length,0);
 glColor3f(0,0,1);
 glVertex3f(0,0,0);
 glVertex3f(0,0,Length);
glEnd;

 glcolor4f(Color[1],Color[2],Color[3],Color[4]);
end;

// Объявление внешней функции Write3D из DGLEngine.dll
procedure Write3D(FontID: Integer; Text: PChar); stdcall; external 'DGLEngine.dll';




function GetCurrentSeason: string;
begin
  Result := BoosterWorld.GetWorldCurrentSeason;
end;

function IsNightTime: Boolean;
begin
  Result := BoosterWorld.IsWorldNightTime;
end;

// Замените блок загрузки ночной klub текстуры в InitializeDayNightSystem:




procedure InitializeDayNightSystem; stdcall;
begin
  InitializeWorldDayNight;
end;

// Вспомогательная функция для получения размера файла:



procedure ApplyDayNightTextures;
begin
  ApplyWorldDayNightTextures;
end;

// Замените ProcessDayNightSystem на эту версию с диагностикой:


procedure ProcessDayNightSystem; stdcall;
begin
  ProcessWorldDayNight;
end;



// ===== ФУНКЦИЯ КОНВЕРТАЦИИ SINGLE В 80-BIT EXTENDED =====

// ===== ФУНКЦИЯ ЗАПИСИ ЗНАЧЕНИЯ ПО АДРЕСУ =====
procedure ProcessStepForwardConfig; stdcall;
begin
  ProcessWorldStepForward;
end;



procedure SaveConfigLocal;
var
  F: TextFile;
  Lines: TStringList;
  i, idx: Integer;

  procedure UpsertLine(const Key, Value: string);
  var
    j: Integer;
    Full: string;
  begin
    Full := Key + ': ' + Value;
    for j := 0 to Lines.Count - 1 do
      if BoosterLineHasKey(Lines[j], Key) then
      begin
        Lines[j] := Full;
        Exit;
      end;
    Lines.Add(Full);
  end;

begin
  // Read-modify-write: сохраняем ВСЕ существующие ключи из файла, обновляем только свои.
  // Раньше Rewrite стирал language/view_angle/sensitivity/brightness при каждом Alt+X.
  Lines := TStringList.Create;
  try
    if FileExists('zdbooster.cfg') then
    begin
      try
        Lines.LoadFromFile('zdbooster.cfg');
      except
        on E: Exception do
          AddToLogFile(EngineLog, 'Предупреждение: не удалось прочитать конфиг для merge: ' + E.Message);
      end;
    end;

    // Обновляем только те поля, которыми владеет DrawFunc3D
    if Config_Freecam    then UpsertLine('freecam', '1')      else UpsertLine('freecam', '0');
    if Config_MainCamera then UpsertLine('main_camera', '1')  else UpsertLine('main_camera', '0');
    if Config_MaxDistance then UpsertLine('max_distance', '1') else UpsertLine('max_distance', '0');
    if Config_NewSky     then UpsertLine('newsky', '1')       else UpsertLine('newsky', '0');

    UpsertLine('basespeed',          FormatBoosterFloat(MenuFreecamBaseSpeed));
    UpsertLine('fastspeed',          FormatBoosterFloat(MenuFreecamFastSpeed));
    UpsertLine('turnspeed',          FormatBoosterFloat(MenuFreecamTurnSpeed));
    UpsertLine('stepforward',        FormatBoosterFloat(stepforward));
    UpsertLine('maxvisibledistance', IntToStr(Round(maxvisibledistance)));

    try
      Lines.SaveToFile('zdbooster.cfg');
      AddToLogFile(EngineLog, 'Конфиг сохранен из DrawFunc3D (merge)');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'Ошибка сохранения конфига (DrawFunc3D): ' + E.Message);
    end;
  finally
    Lines.Free;
  end;
end;

procedure ProcessFreecam;
var
  CurrentTime: Cardinal;
  CurrentYaw, CurrentPitch: Single;
  CurrentX, CurrentY, CurrentZ: Single;
  YawRad, PitchRad: Single;
  ForwardX, ForwardY, ForwardZ: Single;
  RightX, RightY: Single;
  Speed: Single;
  NewYaw, NewPitch: Single;
  ConfigStateChanged: Boolean;
  DepthValue: Single;
  LinearDepth: Single;
  Viewport: array[0..3] of Integer;
  KeyForward, KeyBack, KeyLeft, KeyRight: Integer;
  CollisionActive: Boolean;
begin
  CurrentTime := GetTickCount;
  
  // ===== ПРИНУДИТЕЛЬНАЯ ПРОВЕРКА СОСТОЯНИЯ ПРИ ПЕРВОМ ЗАПУСКЕ =====
  if not FreecamConfigStateInitialized then
  begin
    
    // Если в конфиге freecam = 0, а фрикам включен - выключаем его
    if not Config_Freecam and FreecamEnabled then
    begin
      AddToLogFile(EngineLog, 'Конфиг требует выключить фрикам при старте');
      FreecamEnabled := False;
      FreecamInitialized := False;
      
      // Восстанавливаем оригинальные байты
      if RestoreOriginalBytes then
      begin
        AddToLogFile(EngineLog, 'Фрикам принудительно выключен при старте');
      end;
    end
    // Если в конфиге freecam = 1, а фрикам выключен - включаем его
    else if Config_Freecam and not FreecamEnabled then
    begin
      AddToLogFile(EngineLog, 'Конфиг требует включить фрикам при старте');
      
      if not FreecamInitialized then
        InitializeFreecam;
        
      FreecamEnabled := True;
      if ApplyNopPatch then
      begin
        AddToLogFile(EngineLog, 'Фрикам принудительно включен при старте');
      end
      else
      begin
        FreecamEnabled := False;
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось включить фрикам при старте');
      end;
    end;
    
    LastFreecamConfigState := Config_Freecam;
    FreecamConfigStateInitialized := True;
  end;
  
  // ===== РУЧНОЕ УПРАВЛЕНИЕ (Alt + X) =====
  // Проверяем фокус окна — иначе Alt+X сработает даже когда игра свёрнута
  if (GetForegroundWindow = h_Wnd) and
     IsKeyPressed(88) and // X
     (GetAsyncKeyState(VK_MENU) < 0) and // Alt зажат
     (CurrentTime - LastFreecamToggle > FreecamKeyDelay) then
  begin
    if not FreecamEnabled then
    begin
      AddToLogFile(EngineLog, 'Ручное включение фрикама (Alt+X)');
      
      if not FreecamInitialized then
        InitializeFreecam;
        
      FreecamEnabled := True;
      if ApplyNopPatch then
      begin
        Config_Freecam := True;
        SaveConfigLocal;
        AddToLogFile(EngineLog, 'УСПЕХ: Фрикам включен вручную');
        LastFreecamToggle := CurrentTime;
      end
      else
      begin
        FreecamEnabled := False;
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось применить NOP patch');
      end;
    end
    else
    begin
      AddToLogFile(EngineLog, 'Ручное выключение фрикама (Alt+X)');
      
      FreecamEnabled := False;
      FreecamInitialized := False;
      
      WriteMemoryDouble(FREEMODE_SWITCH_ADDR, 0.0);
      WriteMemorySingle(ADDR_X, InitialX);
      WriteMemorySingle(ADDR_Y, InitialY);
      WriteMemorySingle(ADDR_Z, InitialZ);
      
      if RestoreOriginalBytes then
      begin
        Config_Freecam := False;
        SaveConfigLocal;
        AddToLogFile(EngineLog, 'УСПЕХ: Фрикам выключен вручную');
        LastFreecamToggle := CurrentTime;
      end;
    end;
  end;

  // ===== АВТОМАТИЧЕСКОЕ ВКЛЮЧЕНИЕ/ВЫКЛЮЧЕНИЕ ПО КОНФИГУ (ИЗ МЕНЮ) =====
  ConfigStateChanged := (Config_Freecam <> LastFreecamConfigState);
  
  if ConfigStateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния фрикама из меню: %s -> %s', 
      [BoolToStr(LastFreecamConfigState, True), BoolToStr(Config_Freecam, True)]));
    
    if Config_Freecam then
    begin
      if not FreecamEnabled then
      begin
        AddToLogFile(EngineLog, 'Автоматически включаем фрикам (из меню)...');
        
        if not FreecamInitialized then
          InitializeFreecam;
        
        FreecamEnabled := True;
        if ApplyNopPatch then
        begin
          AddToLogFile(EngineLog, 'УСПЕХ: Фрикам автоматически включен через меню');
        end
        else
        begin
          FreecamEnabled := False;
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось применить NOP patch при автовключении');
        end;
      end;
    end
    else
    begin
      if FreecamEnabled then
      begin
        AddToLogFile(EngineLog, 'Автоматически выключаем фрикам (из меню)...');
        FreecamEnabled := False;
        FreecamInitialized := False;
        
        WriteMemoryDouble(FREEMODE_SWITCH_ADDR, 0.0);
        WriteMemorySingle(ADDR_X, InitialX);
        WriteMemorySingle(ADDR_Y, InitialY);
        WriteMemorySingle(ADDR_Z, InitialZ);
        
        if RestoreOriginalBytes then
        begin
          AddToLogFile(EngineLog, 'УСПЕХ: Фрикам автоматически выключен через меню');
        end;
      end;
    end;
    
    LastFreecamConfigState := Config_Freecam;
  end;

  // ===== ЕСЛИ ФРИКАМ НЕ ВКЛЮЧЕН, ВЫХОДИМ =====
  if not FreecamEnabled then Exit;

  // Игнорируем клавиши движения, если окно игры не в фокусе
  // (иначе, печатая в другом окне, Space/WASD поднимают/двигают камеру)
  if GetForegroundWindow <> h_Wnd then Exit;

  // ===== ОБРАБОТКА ДВИЖЕНИЯ ФРИКАМА (остается без изменений) =====
  CurrentYaw := ReadMemorySingle(ADDR_LOOKYAW);
  CurrentPitch := ReadMemorySingle(ADDR_LOOKPITCH);
  CurrentX := ReadMemorySingle(ADDR_X);
  CurrentY := ReadMemorySingle(ADDR_Y);
  CurrentZ := ReadMemorySingle(ADDR_Z);

  while CurrentYaw >= 360.0 do CurrentYaw := CurrentYaw - 360.0;
  while CurrentYaw < 0.0 do CurrentYaw := CurrentYaw + 360.0;
  
  YawRad := CurrentYaw * (Pi / 180.0);
  PitchRad := CurrentPitch * (Pi / 180.0);
  
  ForwardX := cos(PitchRad) * sin(YawRad);
  ForwardY := cos(PitchRad) * cos(YawRad);
  ForwardZ := sin(PitchRad);
  
  RightX := sin(YawRad - Pi / 2);
  RightY := cos(YawRad - Pi / 2);
  
  if IsKeyPressed(VK_SHIFT) then
    Speed := MenuFreecamFastSpeed
  else
    Speed := MenuFreecamBaseSpeed;
  
  if Speed <= 0.0 then Speed := 0.01;

  // === ОПРЕДЕЛЕНИЕ КЛАВИШ ДВИЖЕНИЯ (WASD или Стрелочки) ===
  if FreecamControlMode = 1 then
  begin
    KeyForward := VK_UP;    KeyBack := VK_DOWN;
    KeyLeft    := VK_LEFT;  KeyRight := VK_RIGHT;
  end
  else
  begin
    KeyForward := Ord('W'); KeyBack := Ord('S');
    KeyLeft    := Ord('A'); KeyRight := Ord('D');
  end;

  // === ДВИЖЕНИЕ ===
  if FreecamWalkMode then
  begin
    // Free Walk: движение только по горизонтали (XY), Z управляется гравитацией
    if IsKeyPressed(KeyForward) then
    begin
      CurrentX := CurrentX + sin(YawRad) * Speed;
      CurrentY := CurrentY + cos(YawRad) * Speed;
    end;
    if IsKeyPressed(KeyBack) then
    begin
      CurrentX := CurrentX - sin(YawRad) * Speed;
      CurrentY := CurrentY - cos(YawRad) * Speed;
    end;
    if IsKeyPressed(KeyLeft) then
    begin
      CurrentX := CurrentX + RightX * Speed;
      CurrentY := CurrentY + RightY * Speed;
    end;
    if IsKeyPressed(KeyRight) then
    begin
      CurrentX := CurrentX - RightX * Speed;
      CurrentY := CurrentY - RightY * Speed;
    end;
    // Прыжок (только если стоим на земле — vel ~= 0)
    if IsKeyPressed(VK_SPACE) and (Abs(WalkGravityVel) < 0.01) then
      WalkGravityVel := 0.15;
    // Гравитация
    WalkGravityVel := WalkGravityVel - WALK_GRAVITY;
    CurrentZ := CurrentZ + WalkGravityVel;
  end
  else
  begin
    // Обычный фрикам: полный 3D полёт
    if IsKeyPressed(KeyForward) then
    begin
      CurrentX := CurrentX + ForwardX * Speed;
      CurrentY := CurrentY + ForwardY * Speed;
      CurrentZ := CurrentZ + ForwardZ * Speed;
    end;
    if IsKeyPressed(KeyBack) then
    begin
      CurrentX := CurrentX - ForwardX * Speed;
      CurrentY := CurrentY - ForwardY * Speed;
      CurrentZ := CurrentZ - ForwardZ * Speed;
    end;
    if IsKeyPressed(KeyLeft) then
    begin
      CurrentX := CurrentX + RightX * Speed;
      CurrentY := CurrentY + RightY * Speed;
    end;
    if IsKeyPressed(KeyRight) then
    begin
      CurrentX := CurrentX - RightX * Speed;
      CurrentY := CurrentY - RightY * Speed;
    end;
    if IsKeyPressed(VK_SPACE) then
      CurrentZ := CurrentZ + Speed;
    if IsKeyPressed(VK_CONTROL) then
      CurrentZ := CurrentZ - Speed;
  end;

  // === КОЛЛИЗИЯ (depth buffer) ===
  // Временное отключение коллизии: если назначена клавиша и она нажата — пропускаем
  CollisionActive := FreecamCollision;
  if CollisionActive and (FreecamCollisionDisableKey = 1) and IsKeyPressed(VK_SHIFT) then
    CollisionActive := False;
  if CollisionActive and (FreecamCollisionDisableKey = 2) and IsKeyPressed(VK_CONTROL) then
    CollisionActive := False;

  if CollisionActive then
  begin
    // Читаем глубину в центре экрана — показывает расстояние до ближайшего объекта впереди
    glGetIntegerv(GL_VIEWPORT, @Viewport[0]);
    glReadPixels(Viewport[2] div 2, Viewport[3] div 2, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @DepthValue);
    // Конвертируем depth [0..1] в линейное расстояние (near=0.1, far=10000)
    if DepthValue < 1.0 then
      LinearDepth := (0.1 * 10000.0) / (10000.0 - DepthValue * (10000.0 - 0.1))
    else
      LinearDepth := 10000.0;
    // Если объект ближе чем радиус игрока — блокируем движение
    if LinearDepth < WALK_PLAYER_RADIUS then
    begin
      CurrentX := ReadMemorySingle(ADDR_X);
      CurrentY := ReadMemorySingle(ADDR_Y);
      CurrentZ := ReadMemorySingle(ADDR_Z);
      if FreecamWalkMode then
        WalkGravityVel := 0.0;
    end;
  end;

  // Free Walk: не дать провалиться ниже стартовой Z (грубый пол)
  if FreecamWalkMode then
  begin
    if CurrentZ < InitialZ then
    begin
      CurrentZ := InitialZ;
      WalkGravityVel := 0.0;
    end;
  end;

  NewYaw := CurrentYaw;
  NewPitch := ClampPitch(CurrentPitch);

  WriteMemoryDouble(FREEMODE_SWITCH_ADDR, 2.3);
  WriteMemorySingle(ADDR_X, CurrentX);
  WriteMemorySingle(ADDR_Y, CurrentY);
  WriteMemorySingle(ADDR_Z, CurrentZ);
  WriteMemorySingle(ADDR_LOOKYAW, NewYaw);
  WriteMemorySingle(ADDR_LOOKPITCH, NewPitch);
end;

// ЕДИНСТВЕННАЯ функция обработки клавиатуры - убираем все дубли!
function ProcessKeyboard: Boolean;
var
  BaseAddr: Cardinal;
  KeyOffsets: array[0..11] of Cardinal;
  KeyChars: array[0..11] of string;
  i: Integer;
  CurrentVal, PreviousVal: Byte;
  WindowOpen: Byte;
  EnterJustPressed: Boolean;
  ExternalByte: Byte;
begin
  Result := False;
  EnterPressed := False;
  
  try
    
    // Инициализация массивов клавиш
    KeyOffsets[0] := $79;  KeyChars[0] := '0';
    KeyOffsets[1] := $85;  KeyChars[1] := '1';
    KeyOffsets[2] := $91;  KeyChars[2] := '2';
    KeyOffsets[3] := $9D;  KeyChars[3] := '3';
    KeyOffsets[4] := $A9;  KeyChars[4] := '4';
    KeyOffsets[5] := $B5;  KeyChars[5] := '5';
    KeyOffsets[6] := $C1;  KeyChars[6] := '6';
    KeyOffsets[7] := $CD;  KeyChars[7] := '7';
    KeyOffsets[8] := $D9;  KeyChars[8] := '8';
    KeyOffsets[9] := $E5;  KeyChars[9] := '9';
    KeyOffsets[10] := $61; KeyChars[10] := 'ENTER';
    KeyOffsets[11] := $F1; KeyChars[11] := 'CANCEL';

    BaseAddr := PCardinal(PointerAddress)^;

    ExternalByte := PByte(Pointer($00400000 + $34988C))^;
if (ExternalByte <> 30) and (ExternalByte <> 53) then
begin
  CommandBuffer := '';
end;

    // Инициализация состояний клавиш
    if not KeyStatesInitialized then
    begin
      for i := 0 to 11 do
        PreviousKeyStates[i] := PByte(BaseAddr + KeyOffsets[i])^;
      KeyStatesInitialized := True;
    end;
    
    EnterJustPressed := False;
    
    // Проверяем каждую клавишу на изменение состояния
    for i := 0 to 11 do
    begin
      CurrentVal := PByte(BaseAddr + KeyOffsets[i])^;
      PreviousVal := PreviousKeyStates[i];

      // Edge detection: клавиша была нажата (переход 0 -> 1)
      if (PreviousVal = 0) and (CurrentVal = 1) then
      begin
        if KeyChars[i] = 'ENTER' then
        begin
          EnterJustPressed := True;
          EnterPressed := True;
          //AddToLogFile(EngineLog, 'ENTER нажат, буфер: "' + CommandBuffer + '"');
        end
        else
        begin
          // Добавляем цифру к буферу
          CommandBuffer := CommandBuffer + KeyChars[i];
          //AddToLogFile(EngineLog, 'Добавлена цифра: ' + KeyChars[i] + ', буфер: "' + CommandBuffer + '"');
        end;
      end;
      
      // Обновляем предыдущее состояние
      PreviousKeyStates[i] := CurrentVal;
    end;
    
    // ИСПРАВЛЕННАЯ логика: обрабатываем завершенные команды
    if EnterJustPressed and (CommandBuffer <> '') then
    begin
      LastCommand := CommandBuffer;
      //AddToLogFile(EngineLog, 'Команда завершена: "' + LastCommand + '"');
      // НЕ очищаем CommandBuffer здесь - он будет очищен после обработки команды
      Result := True; // Сигнализируем о новой команде
    end;
    
  except
    on E: Exception do
    begin
      //AddToLogFile(EngineLog, 'Ошибка в ProcessKeyboard: ' + E.Message);
      KeyStatesInitialized := False;
    end;
  end;
end;




function ShouldShowFloatDigit(position: Integer): Boolean;
var
  currentFloatValue: Single;
  floatAsInt: Integer;
  floatStr: string;
begin
  Result := False;
  
  try
    currentFloatValue := PSingle(Pointer(FloatValueAddr))^;
    floatAsInt := Round(currentFloatValue);
    if floatAsInt < 0 then floatAsInt := 0;
    
    floatStr := IntToStr(floatAsInt);
    
    case position of
      1: Result := (Length(floatStr) >= 2) or (floatAsInt >= 10); // 1-я позиция - показывать если >= 10
      2: Result := True; // 2-я позиция - всегда показывать
    end;
    
  except
    Result := False;
  end;
end;


function GetFloatDigit(position: Integer): string;
var
  currentFloatValue: Single;
  floatStr: string;
begin
  Result := '0'; // По умолчанию
  
  try
    // Читаем float из памяти
    currentFloatValue := PSingle(Pointer(FloatValueAddr))^;
    
    // Конвертируем в целое число
    FloatAsInt := Round(currentFloatValue);
    if FloatAsInt < 0 then FloatAsInt := 0; // Защита от отрицательных
    
    // Конвертируем в строку
    floatStr := IntToStr(FloatAsInt);
    
    // Дополняем нулями слева до 2 знаков для позиций 1-2
    while Length(floatStr) < 2 do
      floatStr := '0' + floatStr;
    
    // Возвращаем цифру в нужной позиции
    if (position >= 1) and (position <= Length(floatStr)) then
      Result := floatStr[position]
    else
      Result := '0';
      
  except
    on E: Exception do
    begin
      //AddToLogFile(EngineLog, 'Ошибка чтения float: ' + E.Message);
      Result := '0';
    end;
  end;
end;



// Функция для проверки, нужно ли отображать цифру в позиции
function ShouldShowDigit(funcType: Integer): Boolean;
var
  speedValue, limitValue, distanceValue: Integer;
  speedStr, limitStr, distanceStr: string;
begin
  Result := True; // По умолчанию показываем
  
  try
    case funcType of
      // Скорость (позиции 1, 2, 3)
      20..22: begin
        speedValue := GetSpeedValue; // Используем новую функцию
        speedStr := IntToStr(speedValue);
        
        case funcType of
          20: Result := Length(speedStr) >= 3; // 1-я позиция - только если 3+ цифр (100+)
          21: Result := Length(speedStr) >= 2; // 2-я позиция - только если 2+ цифр (10+)
          22: Result := Length(speedStr) >= 1; // 3-я позиция - всегда
        end;
      end;

      // Допустимая скорость (позиции 1, 2, 3)
      23..25: begin
        limitValue := GetLimitSpeedValue; // Используем функцию из KlubData
        limitStr := IntToStr(limitValue);
        
        case funcType of
          23: Result := Length(limitStr) >= 3; // 1-я позиция - только если 3+ цифр (100+)
          24: Result := Length(limitStr) >= 2; // 2-я позиция - только если 2+ цифр (10+)
          25: Result := Length(limitStr) >= 1; // 3-я позиция - всегда
        end;
      end;
      
      // Расстояние до цели (позиции 1, 2, 3, 4)
      26..29: begin
        distanceValue := GetDistanceValue; // Используем новую функцию
        distanceStr := IntToStr(distanceValue);
        
        case funcType of
          26: Result := Length(distanceStr) >= 4; // 1-я позиция - только если 4+ цифр (1000+)
          27: Result := Length(distanceStr) >= 3; // 2-я позиция - только если 3+ цифр (100+)
          28: Result := Length(distanceStr) >= 2; // 3-я позиция - только если 2+ цифр (10+)
          29: Result := Length(distanceStr) >= 1; // 4-я позиция - всегда
        end;
      end;
      34..35: begin
        case funcType of
          34: Result := ShouldShowFloatDigit(1); // 1-я позиция
          35: Result := ShouldShowFloatDigit(2); // 2-я позиция
        end;
      end;

    end;
  except
    Result := False;
  end;
end;


var
  PosX, PosY, PosZ: Double;
  Color: Longint;
  Alpha: Integer;
  UseLighting: Boolean;
  ExtraParam: Double;
  PosLine, ColorLine: string;
  F: TextFile;


function GetSignalSequenceRuntime(AlsAddr: Cardinal): string;
begin
  Result := BoosterTrafficRuntime.GetSignalSequenceRuntime(AlsAddr);
end;

procedure InitializeTrafficLightSystemRuntime(const ARouteName: string);
begin
  BoosterTrafficRuntime.InitializeTrafficLightSystemRuntime(ARouteName);
end;

function GetLocNum: string;
begin
  Result := ReadLocomotiveNumberFromSettings;
end;


procedure HookSkorostemerViaKLUB(x, y, z, AngZ: Single);
begin
  HookSkorostemerViaKLUBRuntime(x, y, z, AngZ);
end;

procedure DrawSkorostemer(x, y, z, AngZ, AngPrivod: Single); stdcall;
begin
  DrawSkorostemerRuntime(x, y, z, AngZ, AngPrivod);
end;

procedure DrawKLUB(x, y, z, AngZ: Single); stdcall;
begin
  DrawKLUBRuntime(x, y, z, AngZ);
end;

procedure HookSkorostemerCHS7(x, y, z, AngZ: Single);
begin
  HookSkorostemerCHS7Runtime(x, y, z, AngZ);
end;

// Функция получения адреса патча для KPD-3 по типу локомотива
function GetKPD3PatchOffset(locType: Integer): Cardinal;
begin
  Result := KPD3PatchOffset(locType);
end;

// Функция получения адреса патча для BLOK по типу локомотива
function GetBLOKPatchOffset(locType: Integer): Cardinal;
begin
  Result := BLOKPatchOffset(locType);
end;

// Функция проверки наличия KPD-3 файлов
function CheckKPD3FilesExist(locType: Integer; locNum: string): Boolean;
begin
  Result := CheckKPD3FilesExistRuntime(locType, locNum);
end;


procedure InitKPD3Models;
begin
  InitKPD3ModelsRuntime;
end;



// Функция применения патча KPD-3
function ApplyKPD3Patch: Boolean;
begin
  Result := ApplyKPD3PatchRuntime;
end;

// Функция проверки наличия KPD-3 файлов

function CheckBLOKFilesExist(locType: Integer; locNum: string): Boolean;
begin
  Result := CheckBLOKFilesExistRuntime(locType, locNum);
end;

// Функция применения патча KPD-3
function ApplyBLOKPatch: Boolean;
begin
  Result := ApplyBLOKPatchRuntime;
end;

// Функция для вызова из основного кода (например, в InitEng или другом месте)
procedure InitializeKPD3System;
begin
  InitializeKPD3SystemRuntime;
end;

procedure DrawKPD3(
  x, y, z: Single; AngZ, AngPrivod: Single
); stdcall; export;
begin
  DrawKPD3Runtime(x, y, z, AngZ, AngPrivod);
end;


procedure DrawKPD3VL85(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
);
begin
  DrawKPD3VL85Runtime;
end;

type
  TVertexArray = array of array[0..2] of GLfloat;

var
  YellowZoneVerts: TVertexArray;
  LastTarget, LastLimit: Single;

procedure DrawSpeedometer3D;
begin
  DrawSpeedometerRuntime;
end;


function HandleBlockKeyboardClick(mouseX, mouseY: Integer): Boolean;
begin
  Result := HandleBlockKeyboardClickRuntime(mouseX, mouseY);
end;

var
  i: Integer;
  val: Double;
  posZ_tc: Double;
  posX_tc: Double;
  posZ_tm: Double;
  posX_tm: Double;
  posZ_ur: Double;
  posX_ur: Double;

  // Переменные для полосок
  barValue: Single;        // значение для полоски ТМ
  barX: Single;           // X координата полоски ТМ
  barWidth: Single;       // ширина полоски
  barBottom: Single;      // нижняя граница (где 0.00)
  barTop: Single;         // верхняя граница (где 10.00)
  barCurrentHeight: Single; // текущая высота полоски ТМ
  barZ: Single;           // Z координата для полоски
  
  // Переменные для ТЦ
  barValue_tc: Single;        
  barX_tc: Single;           
  barCurrentHeight_tc: Single; 
  
  // Переменные для УР
  barValue_ur: Single;        
  barX_ur: Single;           
  barCurrentHeight_ur: Single; 

  // Внутренняя функция проверки файлов
  function CheckBLOCKFiles: Boolean;
  var
    currentLocType: Integer;
    locFolder, blockPath: string;
    blockModelPath, blockDisplayModelPath, blockTexturePath: string;
  begin
    Result := False;
    try
      currentLocType := GetLocomotiveTypeFromMemory;
      locFolder := GetLocomotiveFolder(currentLocType);
      blockPath := 'data\' + locFolder + '\' + GetLocNum + '\blok\';
      
      if not DirectoryExists(blockPath) then
      begin
        AddToLogFile(EngineLog, 'BLOCK папка не найдена: ' + blockPath);
        Exit;
      end;
      
      blockModelPath := blockPath + 'BI-BLOK.dmd';
      blockDisplayModelPath := blockPath + 'BI-blok-displ.dmd';
      blockTexturePath := blockPath + 'blok.bmp';
      
      Result := FileExists(blockModelPath) and FileExists(blockDisplayModelPath) and FileExists(blockTexturePath);
      
      if Result then
        AddToLogFile(EngineLog, 'BLOCK файлы найдены для ' + locFolder + ' ' + GetLocNum)
      else
        AddToLogFile(EngineLog, 'BLOCK файлы неполные для ' + locFolder + ' ' + GetLocNum);
        
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'Ошибка проверки BLOCK файлов: ' + E.Message);
        Result := False;
      end;
    end;
  end;


procedure DrawBLOCK(x: Single; y: Single; z: Single; AngZ: Single);
begin
  DrawBLOCKRuntime(x, y, z, AngZ);
end;

procedure ReinitializeBLOCK;
begin
  ReinitializeBLOCKRuntime;
end;



// RA3-specific code moved to Modules\RA3.pas

// ===========================================================================
//  DrawKlubBilVData — реплика оригинальной функции DRAWKLUBBILVDATA
//  из Launcher.exe+84AB4. Позиции и константы извлечены из дизассемблера.
//
//  Параметры (stdcall, 6 стековых — ret 24 для совместимости с register ABI):
//    ModelParam2, ModelParam1: Cardinal — текстуры моделей (не используются)
//    AngZ: Single — угол поворота панели (45.0)
//    Z: Single — позиция Z панели (3.43)
//    Y: Single — позиция Y панели (7.18)
//    X: Single — позиция X панели (1.17)
// ==========================================================================
procedure DrawKlubBilVData(
  ModelParam2, ModelParam1: Cardinal;
  AngZ, Z, Y, X: Single
); stdcall;
begin
  DrawKlubBilVDataRuntime(ModelParam2, ModelParam1, AngZ, Z, Y, X);
end;

procedure DrawSpeedometerNeedle(FontID: Integer; Speed: Single);
begin
  DrawSpeedometerNeedleRuntime(FontID, Speed);
end;



procedure HookKLUB(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;


type
  TObjectParams = record
    X, Y, Z: Single;
    RotX, RotY, RotZ: Single;
    Scale: Single;
    Text: string;
    Color: Integer;
    FuncType: Integer;
  end;

const
  debug = True;
  BaseAddress: Cardinal = $00400000;

var
  StaticData: array[0..35] of TObjectParams;
  obj: TObjectParams;
  parts: TStringList;
  f: TextFile;
  line: string;
  arrowAngle: Single;
  arrowX, arrowY, arrowZ, arrowScale: Single;
  arrowRotation: Single;
  currentSpeed: Single;
  speedAngle: Single;
  i: Integer;
  initialized: Boolean;
  NewCommandReceived: Boolean;

  currentLocType: Integer;
  locFolder: string;
  // Переменные для светофоров
  signalSequence: string;
  signalChar: Char;
  signalIndex: Integer;
  mainTrafficLight: Byte;
  visibleSignalCount: Integer;

  File3D: TextFile;
  PosX, PosY, PosZ: Single;
  RotX3D, RotY3D: Single;
  Scale3DVal: Single;
    diskX, diskY, diskZ: Single;
    diskRotX, diskRotY: Single;
    diskScale: Single;
    diskRadius: Integer;

  textureAddr, modelAddr: Pointer;
  textureID: Cardinal;
  modelID: Integer;

  baseStructAddr: Cardinal;
  yellowBlockModelAddr, greenBlockModelAddr: Pointer;
  yellowBlockModelID, greenBlockModelID: Word;

      alsValue: Byte;
    diskColor: Integer;

function GetSpeedDigit(position: Integer): string;
var
  speedStr: string;
  speedValue: Integer;
begin
  Result := '';
  try
    speedValue := GetSpeedValue; // Используем новую функцию из KlubData
    speedStr := IntToStr(speedValue);
    
    // Дополняем нулями слева до 3 знаков
    while Length(speedStr) < 3 do
      speedStr := '0' + speedStr;
    
    // Возвращаем цифру в нужной позиции (1, 2 или 3)
    if (position >= 1) and (position <= Length(speedStr)) then
      Result := speedStr[position];
  except
    Result := '0';
  end;
end;

// Функция для получения цифры допустимой скорости по позиции
function GetLimitSpeedDigit(position: Integer): string;
var
  limitStr: string;
  limitValue: Integer;
begin
  Result := '';
  try
    limitValue := GetLimitSpeedValue; // Используем функцию из KlubData
    limitStr := IntToStr(limitValue);
    
    // Дополняем нулями слева до 3 знаков
    while Length(limitStr) < 3 do
      limitStr := '0' + limitStr;
    
    // Возвращаем цифру в нужной позиции (1, 2 или 3)
    if (position >= 1) and (position <= Length(limitStr)) then
      Result := limitStr[position];
  except
    Result := '0';
  end;
end;

// Функция для получения цифры расстояния по позиции
function GetDistanceDigit(position: Integer): string;
var
  distanceStr: string;
  distanceValue: Integer;
begin
  Result := '';
  try
    distanceValue := GetDistanceValue; // Используем новую функцию из KlubData
    distanceStr := IntToStr(distanceValue);
    
    // Дополняем нулями слева до 4 знаков
    while Length(distanceStr) < 4 do
      distanceStr := '0' + distanceStr;
    
    // Возвращаем цифру в нужной позиции (1, 2, 3 или 4)
    if (position >= 1) and (position <= Length(distanceStr)) then
      Result := distanceStr[position];
  except
    Result := '0';
  end;
end;

procedure InitializeStaticData;
begin
  if initialized then Exit;

  // Элементы 0-13 - основные данные (обновленные координаты)
  with StaticData[0] do begin X:=1.0268; Y:=7.3762; Z:=3.558; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.013; Text:='222'; Color:=$FFFFFF; FuncType:=11; end;
  with StaticData[1] do begin X:=1.029; Y:=7.37896; Z:=3.538; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.01; Text:='2222'; Color:=$FFFFFF; FuncType:=2; end;
  with StaticData[2] do begin X:=1.0305; Y:=7.3807; Z:=3.525; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.0055; Text:='25.05.25'; Color:=$FFFFFF; FuncType:=3; end;
  with StaticData[3] do begin X:=1.031; Y:=7.38165; Z:=3.519; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.0055; Text:='22:47:00'; Color:=$FFFFFF; FuncType:=4; end;
  with StaticData[4] do begin X:=0.854; Y:=7.50365; Z:=3.528; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.01'; Color:=$FFFFFF; FuncType:=5; end;
  with StaticData[5] do begin X:=0.855; Y:=7.50489; Z:=3.519; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.02'; Color:=$FFFFFF; FuncType:=0; end;
  with StaticData[6] do begin X:=0.853; Y:=7.5022; Z:=3.538; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.03'; Color:=$FFFFFF; FuncType:=6; end;
  with StaticData[7] do begin X:=0.852; Y:=7.5007; Z:=3.548; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.04'; Color:=$FFFFFF; FuncType:=7; end;
  with StaticData[8] do begin X:=0.87; Y:=7.4842; Z:=3.566; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='1.05'; Color:=$008000; FuncType:=8; end;
  with StaticData[9] do begin X:=0.893; Y:=7.4694; Z:=3.560; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.015; Text:='222'; Color:=$FFFFFF; FuncType:=1; end;
  with StaticData[10] do begin X:=0.992; Y:=7.3987; Z:=3.566; RotX:=-105; RotY:=34; RotZ:=-8.0; Scale:=0.007; Text:='12312311'; Color:=$FFFFFF; FuncType:=9; end;
  with StaticData[11] do begin X:=1.019; Y:=7.3585; Z:=3.665; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.008; Text:='Путь'; Color:=$FFFFFF; FuncType:=0; end;
  with StaticData[12] do begin X:=1.022; Y:=7.3584; Z:=3.656; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.008; Text:='n'; Color:=$FFFFFF; FuncType:=10; end;
  with StaticData[13] do begin X:=1.01; Y:=7.3842; Z:=3.575; RotX:=-105; RotY:=34; RotZ:=-8.0; Scale:=0.008; Text:='222'; Color:=$FFFFFF; FuncType:=11; end;

  // Элементы 14-16 - Скорость (желтый, 3 позиции)
  with StaticData[14] do begin X:=0.9235; Y:=7.455; Z:=3.386; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='1'; Color:=$00FFFF; FuncType:=20; end;
  with StaticData[15] do begin X:=0.935;  Y:=7.451; Z:=3.386; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='2'; Color:=$00FFFF; FuncType:=21; end;
  with StaticData[16] do begin X:=0.9465; Y:=7.447; Z:=3.386; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='3'; Color:=$00FFFF; FuncType:=22; end;

  // Элементы 17-19 - Допустимая скорость (красный, 3 позиции)
  with StaticData[17] do begin X:=0.922;  Y:=7.450; Z:=3.364; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='4'; Color:=$0000FF; FuncType:=23; end;
  with StaticData[18] do begin X:=0.9335; Y:=7.446; Z:=3.364; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='5'; Color:=$0000FF; FuncType:=24; end;
  with StaticData[19] do begin X:=0.945;  Y:=7.442; Z:=3.364; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='6'; Color:=$0000FF; FuncType:=25; end;

  // Элементы 20-23 - Расстояние до цели (красный, 4 позиции)
  with StaticData[20] do begin X:=0.852;  Y:=7.471;  Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='1'; Color:=$0000FF; FuncType:=26; end;
  with StaticData[21] do begin X:=0.863;  Y:=7.4674; Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='2'; Color:=$0000FF; FuncType:=27; end;
  with StaticData[22] do begin X:=0.874;  Y:=7.4638; Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='3'; Color:=$0000FF; FuncType:=28; end;
  with StaticData[23] do begin X:=0.885;  Y:=7.4602; Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='4'; Color:=$0000FF; FuncType:=29; end;



  // Элементы 24-27 - Светофорная система АЛС
  with StaticData[24] do begin X:=1.111; Y:=7.239; Z:=3.729; RotX:=-90; RotY:=40; RotZ:=-90; Scale:=0.015; Text:='|'; Color:=$00FF00; FuncType:=30; end;
  with StaticData[25] do begin X:=1.113; Y:=7.237; Z:=3.733; RotX:=-90; RotY:=40; RotZ:=-90; Scale:=0.015; Text:='l'; Color:=$00FF00; FuncType:=31; end;
  with StaticData[26] do begin X:=1.107; Y:=7.243; Z:=3.7283; RotX:=-90; RotY:=30; RotZ:=-55; Scale:=0.018; Text:='-'; Color:=$00FF00; FuncType:=32; end;
  with StaticData[27] do begin X:=1.111; Y:=7.239; Z:=3.739; RotX:=-90; RotY:=40; RotZ:=-90; Scale:=0.015; Text:='||'; Color:=$00FF00; FuncType:=33; end;
  
  // Элементы 28-29 - "Следует графиком"
  with StaticData[28] do begin X:=0.8665; Y:=7.488; Z:=3.56; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.005; Text:='следует'; Color:=$008000; FuncType:=0; end;
  with StaticData[29] do begin X:=0.8675; Y:=7.4881; Z:=3.556; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.005; Text:='графику'; Color:=$008000; FuncType:=0; end;
  
  // Элементы 30-33 - Задание и расписание
  with StaticData[30] do begin X:=0.865; Y:=7.4918; Z:=3.547; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:=''; Color:=$FFFFFF; FuncType:=12; end; // Задание
  with StaticData[31] do begin X:=0.866; Y:=7.493; Z:=3.538; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='РАСПИСАНИЕ'; Color:=$FFFFFF; FuncType:=13; end;
  with StaticData[32] do begin X:=0.87; Y:=7.4923; Z:=3.528; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:=''; Color:=$FFFFFF; FuncType:=14; end; // Текущая станция
  with StaticData[33] do begin X:=0.871; Y:=7.4936; Z:=3.519; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:=''; Color:=$FFFFFF; FuncType:=15; end; // Следующая станция

  with StaticData[34] do begin X:=0.142; Y:=7.48; Z:=3.162; RotX:=-57.3; RotY:=0.0; RotZ:=0.0; Scale:=0.016; Text:='1'; Color:=$0000FF; FuncType:=34; end;  // 1-я позиция float
  with StaticData[35] do begin X:=0.1533; Y:=7.48; Z:=3.162; RotX:=-57.3; RotY:=0.0; RotZ:=0.0; Scale:=0.016; Text:='3'; Color:=$0000FF; FuncType:=35; end; // 2-я позиция float

  initialized := True;
end;

procedure InitializeStaticData2;
begin
  if initialized then Exit;

  // Элементы 0-13 - основные данные (обновленные координаты)
  with StaticData[0] do begin X:=1.0268; Y:=7.272; Z:=3.558; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.013; Text:='222'; Color:=$FFFFFF; FuncType:=11; end;
  with StaticData[1] do begin X:=1.029; Y:=7.2747; Z:=3.538; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.01; Text:='2222'; Color:=$FFFFFF; FuncType:=2; end;
  with StaticData[2] do begin X:=1.0305; Y:=7.2765; Z:=3.525; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.0055; Text:='25.05.25'; Color:=$FFFFFF; FuncType:=3; end;
  with StaticData[3] do begin X:=1.031; Y:=7.2775; Z:=3.519; RotX:=-105; RotY:=35; RotZ:=-8.5; Scale:=0.0055; Text:='22:47:00'; Color:=$FFFFFF; FuncType:=4; end;
  with StaticData[4] do begin X:=0.854; Y:=7.399; Z:=3.528; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.01'; Color:=$FFFFFF; FuncType:=5; end;
  with StaticData[5] do begin X:=0.855; Y:=7.400; Z:=3.519; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.02'; Color:=$FFFFFF; FuncType:=0; end;
  with StaticData[6] do begin X:=0.853; Y:=7.3975; Z:=3.538; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.03'; Color:=$FFFFFF; FuncType:=6; end;
  with StaticData[7] do begin X:=0.852; Y:=7.396; Z:=3.548; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='8.04'; Color:=$FFFFFF; FuncType:=7; end;
  with StaticData[8] do begin X:=0.87; Y:=7.3799; Z:=3.566; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='2.05'; Color:=$008000; FuncType:=8; end;
  with StaticData[9] do begin X:=0.893; Y:=7.3652; Z:=3.560; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.015; Text:='222'; Color:=$FFFFFF; FuncType:=1; end;
  with StaticData[10] do begin X:=0.992; Y:=7.2945; Z:=3.566; RotX:=-105; RotY:=34; RotZ:=-8.0; Scale:=0.007; Text:='12312311'; Color:=$FFFFFF; FuncType:=9; end;
  with StaticData[11] do begin X:=1.019; Y:=7.2539; Z:=3.665; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.008; Text:='Путь'; Color:=$FFFFFF; FuncType:=0; end;
  with StaticData[12] do begin X:=1.022; Y:=7.2539; Z:=3.656; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.008; Text:='n'; Color:=$FFFFFF; FuncType:=10; end;
  with StaticData[13] do begin X:=1.01; Y:=7.28; Z:=3.575; RotX:=-105; RotY:=34; RotZ:=-8.0; Scale:=0.008; Text:='222'; Color:=$FFFFFF; FuncType:=11; end;

  // Элементы 14-16 - Скорость (желтый, 3 позиции)
  with StaticData[14] do begin X:=0.204; Y:=7.529; Z:=3.353; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='1'; Color:=$00FFFF; FuncType:=20; end;
  with StaticData[15] do begin X:=0.217; Y:=7.532; Z:=3.353; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='2'; Color:=$00FFFF; FuncType:=21; end;
  with StaticData[16] do begin X:=0.23;  Y:=7.533; Z:=3.353; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='3'; Color:=$00FFFF; FuncType:=22; end;

  // Элементы 17-19 - Допустимая скорость (красный, 3 позиции)
  with StaticData[17] do begin X:=0.204; Y:=7.523; Z:=3.329; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='4'; Color:=$0000FF; FuncType:=23; end;
  with StaticData[18] do begin X:=0.217; Y:=7.526; Z:=3.329; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='5'; Color:=$0000FF; FuncType:=24; end;
  with StaticData[19] do begin X:=0.23;  Y:=7.529; Z:=3.329; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='6'; Color:=$0000FF; FuncType:=25; end;

  // Элементы 20-23 - Расстояние до цели (красный, 4 позиции)
  with StaticData[20] do begin X:=0.135;  Y:=7.506;  Z:=3.331; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='1'; Color:=$0000FF; FuncType:=26; end;
  with StaticData[21] do begin X:=0.1455; Y:=7.508;  Z:=3.331; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='2'; Color:=$0000FF; FuncType:=27; end;
  with StaticData[22] do begin X:=0.157;  Y:=7.511;  Z:=3.331; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='3'; Color:=$0000FF; FuncType:=28; end;
  with StaticData[23] do begin X:=0.169;  Y:=7.514;  Z:=3.331; RotX:=-75.0; RotY:=-10.0; RotZ:=-3.0; Scale:=0.019; Text:='4'; Color:=$0000FF; FuncType:=29; end;


  // Элементы 30-33 - Задание и расписание
  with StaticData[30] do begin X:=0.865; Y:=7.3877; Z:=3.547; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:=''; Color:=$FFFFFF; FuncType:=12; end; // Задание
  with StaticData[31] do begin X:=0.866; Y:=7.3889; Z:=3.538; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:='РАСПИСАНИЕ'; Color:=$FFFFFF; FuncType:=13; end;
  with StaticData[32] do begin X:=0.87; Y:=7.3883; Z:=3.528; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:=''; Color:=$FFFFFF; FuncType:=14; end; // Текущая станция
  with StaticData[33] do begin X:=0.871; Y:=7.3896; Z:=3.519; RotX:=-105; RotY:=35; RotZ:=-8.0; Scale:=0.0065; Text:=''; Color:=$FFFFFF; FuncType:=15; end; // Следующая станция

  initialized := true;
end;

// Добавьте эту функцию в начало implementation секции:
function GetTextByType(funcType: Integer): string;
begin
  case funcType of
    0: Result := ''; 
    1: Result := GetSpeed;
    2: Result := GetDistance;
    3: Result := GetCurrentDate;
    4: Result := GetCurrentTime;
    5: Result := GetPressureTM;
    6: Result := GetPressureUR;
    7: Result := GetPressureTC;
    8: Result := GetAcceleration;
    9: Result := GetCoordinatesFormatted;
    10: Result := GetTrackNumber;
    11: Result := GetLimitSpeed;
    12: begin
      // Обновляем станции только по таймеру
      if (timeGetTime - LastStationCheck) > StationCheckInterval then
      begin
        BoosterStationRuntime.FindCurrentAndNextStationRuntime;
        CachedCurrentStation := BoosterStationRuntime.GetCurrentStationRuntime;
        CachedNextStation := BoosterStationRuntime.GetNextStationRuntime;
        LastStationCheck := timeGetTime;
      end;
      Result := 'ЗАДАНИЕ: ' + CachedCurrentStation;
    end;
    13: Result := 'РАСПИСАНИЕ';
    14: begin
      Result := BoosterStationRuntime.GetCurrentStationRuntime;
    end;
    15: begin
      Result := BoosterStationRuntime.GetNextStationRuntime;
    end;
    
    // Новые функции для цифр скорости (20-22)
    20: Result := GetSpeedDigit(1);     // 1-я позиция скорости
    21: Result := GetSpeedDigit(2);     // 2-я позиция скорости  
    22: Result := GetSpeedDigit(3);     // 3-я позиция скорости
    
    // Новые функции для цифр допустимой скорости (23-25)
    23: Result := GetLimitSpeedDigit(1); // 1-я позиция допустимой
    24: Result := GetLimitSpeedDigit(2); // 2-я позиция допустимой
    25: Result := GetLimitSpeedDigit(3); // 3-я позиция допустимой
    
    // Новые функции для цифр расстояния (26-29)
    26: Result := GetDistanceDigit(1);   // 1-я позиция расстояния
    27: Result := GetDistanceDigit(2);   // 2-я позиция расстояния
    28: Result := GetDistanceDigit(3);   // 3-я позиция расстояния
    29: Result := GetDistanceDigit(4);   // 4-я позиция расстояния
    
    // Функции для светофорной системы (30-33) - добавь свою логику
    30, 31, 32, 33: Result := ''; // Здесь добавь логику для светофоров

    34: Result := GetFloatDigit(1);  // 1-я позиция float
    35: Result := GetFloatDigit(2);  // 2-я позиция float

    else Result := '';
  end;
end;


// Обновленная функция DrawObject
procedure DrawObject(o: TObjectParams; ElementIndex: Integer = -1);
var
  textToShow: string;
  fontToUse: Integer;
  shouldDraw: Boolean;
begin
  // Проверяем, нужно ли отображать этот элемент
  shouldDraw := True;
  
  if ElementIndex >= 0 then
 begin
    case ElementIndex of
      0..13: shouldDraw := Config_BGSD;     // Основные данные
      14..27: shouldDraw := Config_SAUT;    // ← ИСПРАВЛЕНО: только цифры скорости и лимитов
      28..29: shouldDraw := Config_BGSD;    // "следует графику" 
      30..33: shouldDraw := Config_BGSD;    // Задание/Расписание
      34..35: shouldDraw := Config_STUPEN;  // Ступени отдельно
    end;
  end;
  
  // Если элемент отключен в конфиге, не рисуем его
  if not shouldDraw then Exit;
  
  // Дополнительная проверка для цифр - показывать только нужные позиции
  if (o.FuncType >= 20) and (o.FuncType <= 29) then
  begin
    if not ShouldShowDigit(o.FuncType) then Exit;
  end;

  // Проверка для float цифр (ступени)
  if (o.FuncType >= 34) and (o.FuncType <= 35) then
  begin
    if not ShouldShowFloatDigit(o.FuncType - 33) then Exit; // 34->1, 35->2
  end;

  if o.FuncType = 0 then
    textToShow := o.Text
  else
    textToShow := GetTextByType(o.FuncType);

  // Выбор шрифта
  if ((ElementIndex >= 14) and (ElementIndex <= 23)) or  // цифры скорости и лимитов
     ((ElementIndex >= 34) and (ElementIndex <= 35)) then // float цифры
    fontToUse := SevenSegmentFont  // 7-Segment для всех цифр
  else
    fontToUse := KLUBUFont;  // KLUBU для остальных
    
  BeginObj3D;
  glDisable(GL_LIGHTING);
  Position3D(o.X, o.Y, o.Z);
  RotateX(o.RotX);
  RotateY(o.RotY);
  RotateZ(o.RotZ);
  Scale3D(o.Scale);
  SetTexture(0);
  Color3D(o.Color, 255, False, 0);
  DrawText3D(fontToUse, textToShow);
  glEnable(GL_LIGHTING);
  EndObj3D;
end;


function SafeStrToFloat(const S: string): Single;
begin
  Result := StrToFloat(StringReplace(S, '.', ',', [rfReplaceAll]));
end;


// ===== ФУНКЦИЯ СОЗДАНИЯ ПАПОК =====
function CreateDirectoryPath(const DirPath: string): Boolean;
var
  i: Integer;
  currentPath: string;
begin
  Result := True;
  currentPath := '';
  
  for i := 1 to Length(DirPath) do
  begin
    if (DirPath[i] = '\') or (i = Length(DirPath)) then
    begin
      if i = Length(DirPath) then
        currentPath := currentPath + DirPath[i];
        
      if not DirectoryExists(currentPath) then
      begin
        try
          CreateDir(currentPath);
        except
          Result := False;
          Exit;
        end;
      end;
      
      if i < Length(DirPath) then
        currentPath := currentPath + '\';
    end
    else
      currentPath := currentPath + DirPath[i];
  end;
end;

// ===== ФУНКЦИЯ ПОЛУЧЕНИЯ ПУТИ К ЭЛЕМЕНТАМ =====
function GetElementsPath: string;
var
  locFolder: string;
  locType: Integer;
begin
  locType := GetLocomotiveTypeFromMemory;
  locFolder := GetLocomotiveFolder(locType);
  Result := 'data\' + locFolder + '\' + LocNum + '\raildriver\';
  
  // ДОБАВЛЯЕМ ОТЛАДКУ
  AddToLogFile(EngineLog, '=== DEBUG GetElementsPath ===');
  AddToLogFile(EngineLog, 'LocType from memory: ' + IntToStr(locType));
  AddToLogFile(EngineLog, 'LocFolder: ' + locFolder);
  AddToLogFile(EngineLog, 'LocNum: ' + LocNum);
  AddToLogFile(EngineLog, 'Generated path: ' + Result);
  AddToLogFile(EngineLog, 'Directory exists: ' + BoolToStr(DirectoryExists(Result), True));
end;

// ===== ЗАГРУЗКА НАСТРОЕК ОТОБРАЖЕНИЯ ИЗ BOOSTER.TXT =====
procedure LoadBoosterConfig(ForceReload: Boolean = False);
var
  f: TextFile;
  line: string;
  paramName, paramValue: string;
  colonPos: Integer;
  currentTime: Cardinal;
  boosterFilePath, elementsPath: string;
begin
  currentTime := timeGetTime;
  
  // Проверяем, нужно ли обновлять
  if not ForceReload and ConfigLoaded and 
     ((currentTime - LastBoosterConfigCheck) < BoosterConfigCheckInterval) then
    Exit;
  
  // Значения по умолчанию
  Config_SAUT := False;
  Config_BGSD := True;
  Config_STUPEN := True;
  
  // Получаем путь к файлу
  elementsPath := GetElementsPath;
  boosterFilePath := elementsPath + 'booster.txt';
  
  // ОТЛАДКА ПУТЕЙ
  AddToLogFile(EngineLog, '=== LoadBoosterConfig DEBUG ===');
  AddToLogFile(EngineLog, 'Elements path: ' + elementsPath);
  AddToLogFile(EngineLog, 'Booster file path: ' + boosterFilePath);
  AddToLogFile(EngineLog, 'Elements directory exists: ' + BoolToStr(DirectoryExists(elementsPath), True));
  AddToLogFile(EngineLog, 'Booster file exists: ' + BoolToStr(FileExists(boosterFilePath), True));
  
  if FileExists(boosterFilePath) then
  begin
    try
      AddToLogFile(EngineLog, 'Попытка загрузки booster.txt...');
      AssignFile(f, boosterFilePath);
      Reset(f);
      
      while not Eof(f) do
      begin
        ReadLn(f, line);
        line := Trim(line);
        
        // Пропускаем пустые строки и комментарии
        if (line = '') or (line[1] = '#') or (line[1] = ';') then Continue;
        
        colonPos := Pos(':', line);
        if colonPos > 0 then
        begin
          paramName := LowerCase(Trim(Copy(line, 1, colonPos - 1)));
          paramValue := Trim(Copy(line, colonPos + 1, Length(line)));
          
          AddToLogFile(EngineLog, 'Читаем параметр: ' + paramName + ' = ' + paramValue);
          
          try
            if paramName = 'saut' then
            begin
              Config_SAUT := (paramValue = '1') or (LowerCase(paramValue) = 'true');
              AddToLogFile(EngineLog, 'SAUT установлен в: ' + BoolToStr(Config_SAUT, True));
            end
            else if paramName = 'bgsd' then
            begin
              Config_BGSD := (paramValue = '1') or (LowerCase(paramValue) = 'true');
              AddToLogFile(EngineLog, 'BGSD установлен в: ' + BoolToStr(Config_BGSD, True));
            end
            else if paramName = 'stupen' then
            begin
              Config_STUPEN := (paramValue = '1') or (LowerCase(paramValue) = 'true');
              AddToLogFile(EngineLog, 'STUPEN установлен в: ' + BoolToStr(Config_STUPEN, True));
            end;
          except
            on E: Exception do
              AddToLogFile(EngineLog, 'Ошибка преобразования параметра ' + paramName + ': ' + E.Message);
          end;
        end;
      end;
      
      CloseFile(f);
      AddToLogFile(EngineLog, 'booster.txt успешно загружен');
      
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'КРИТИЧЕСКАЯ ОШИБКА загрузки booster.txt: ' + E.Message);
        try
          CloseFile(f);
        except
        end;
      end;
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'booster.txt НЕ НАЙДЕН по пути: ' + boosterFilePath);
    
    // Создаем файл только при первой загрузке
    if not ConfigLoaded then
    begin
      try
        AddToLogFile(EngineLog, 'Попытка создания папки: ' + elementsPath);
        // Создаем папки если их нет
        if CreateDirectoryPath(elementsPath) then
        begin
          AddToLogFile(EngineLog, 'Папка создана успешно, создаем booster.txt');
          AssignFile(f, boosterFilePath);
          Rewrite(f);
          WriteLn(f, '# Конфигурация отображения элементов ZDBooster');
          WriteLn(f, '# 1 = включено, 0 = выключено');
          WriteLn(f, '');
          WriteLn(f, '# ===== НАСТРОЙКИ ОТОБРАЖЕНИЯ =====');
          WriteLn(f, '# Отображение скорости и лимитов (элементы 14, 15, 16)');
          WriteLn(f, 'saut: 0');
          WriteLn(f, '');
          WriteLn(f, '# Отображение основных данных (элементы 0-13)');
          WriteLn(f, 'bgsd: 1');
          WriteLn(f, '');
          WriteLn(f, '# Отображение ступеней (элементы 34-35)');
          WriteLn(f, 'stupen: 1');
          CloseFile(f);
          AddToLogFile(EngineLog, 'booster.txt создан успешно: ' + boosterFilePath);
        end
        else
        begin
          AddToLogFile(EngineLog, 'ОШИБКА создания папки: ' + elementsPath);
        end;
      except
        on E: Exception do
          AddToLogFile(EngineLog, 'ОШИБКА создания booster.txt: ' + E.Message);
      end;
    end;
  end;
  
  LastBoosterConfigCheck := currentTime;
  AddToLogFile(EngineLog, 'Финальные настройки: SAUT=' + BoolToStr(Config_SAUT, True) + 
    ', BGSD=' + BoolToStr(Config_BGSD, True) + ', STUPEN=' + BoolToStr(Config_STUPEN, True));
end;

procedure LoadYellowBlockParams;
var
  f: TextFile;
  line: string;
  paramName, paramValue: string;
  colonPos: Integer;
  fileTime: TFileTime;
  handle: THandle;
  changed: Boolean;
begin
  changed := False;
  
  // Проверяем, изменился ли файл (опционально - для оптимизации)
  if FileExists('yellow_block.txt') then
  begin
    // Если файл загружался ранее, проверяем время изменения
    if YellowBlockParamsLoaded then
    begin
      // Простая проверка - загружаем каждый раз (можно оптимизировать позже)
      changed := True;
    end
    else
    begin
      changed := True; // Первая загрузка
    end;
  end;
  
  // Если файл не изменился, выходим (закомментируйте эти строки для постоянной перезагрузки)
  // if not changed then Exit;
  
  // Значения по умолчанию
  YellowBlockRotX := 0.0;
  YellowBlockRotY := 0.0;
  YellowBlockRotZ := 0.0;
  YellowBlockPosX := -0.086499996;
  YellowBlockPosY := 0.0;
  YellowBlockPosZ := 0.223;
  YellowBlockScale := 0.88999999;
  
  if FileExists('yellow_block.txt') then
  begin
    try
      AssignFile(f, 'yellow_block.txt');
      Reset(f);
      
      while not Eof(f) do
      begin
        ReadLn(f, line);
        line := Trim(line);
        
        // Пропускаем пустые строки и комментарии
        if (line = '') or (line[1] = '#') then Continue;
        
        colonPos := Pos(':', line);
        if colonPos > 0 then
        begin
          paramName := LowerCase(Trim(Copy(line, 1, colonPos - 1)));
          paramValue := Trim(Copy(line, colonPos + 1, Length(line)));
          
          try
            if paramName = 'rotx' then
              YellowBlockRotX := SafeStrToFloat(paramValue)
            else if paramName = 'roty' then
              YellowBlockRotY := SafeStrToFloat(paramValue)
            else if paramName = 'rotz' then
              YellowBlockRotZ := SafeStrToFloat(paramValue)
            else if paramName = 'posx' then
              YellowBlockPosX := SafeStrToFloat(paramValue)
            else if paramName = 'posy' then
              YellowBlockPosY := SafeStrToFloat(paramValue)
            else if paramName = 'posz' then
              YellowBlockPosZ := SafeStrToFloat(paramValue)
            else if paramName = 'scale' then
              YellowBlockScale := SafeStrToFloat(paramValue);
          except
            //AddToLogFile(EngineLog, 'Ошибка чтения параметра: ' + paramName + ' = ' + paramValue);
          end;
        end;
      end;
      
      CloseFile(f);

    except
      on E: Exception do
      begin
   //     AddToLogFile(EngineLog, 'Ошибка загрузки yellow_block.txt: ' + E.Message);
        try
          CloseFile(f);
        except
        end;
      end;
    end;
  end;

  YellowBlockParamsLoaded := True;
end;

begin

  DrawRA3;

  // ===== ИНИЦИАЛИЗАЦИЯ СВЕТОФОРНОЙ СИСТЕМЫ =====
  if not SystemInitialized then
  begin
    InitializeTrafficLightSystemRuntime(GetRouteName);
    LoadBoosterConfig;
    InitRA3;
    try
      baseStructAddr := PCardinal(Pointer($00400000 + $8D10D70))^;
      CachedYellowBlockID := PWord(Pointer(baseStructAddr + $1A))^;
      CachedGreenBlockID := PWord(Pointer(baseStructAddr + $1C))^;
      LightBlockIDsCached := True;
    except
      CachedYellowBlockID := 13;
      CachedGreenBlockID := 14;
      LightBlockIDsCached := True;
    end;


    if CheckBLOCKFiles then
      begin
        blokhookklub := True;
        DrawBLOCK(x, y, z, AngZ);
        //DrawBLOCK(x-20, y, z, 0);
      end;


    //InitializeBoosterKeyboard;
    //SetBoosterKeyboardCallback(@MyKeyHandler);

    SystemInitialized := True;
  end;

  if not LightBlockIDsCached then
  begin
    try
      baseStructAddr := PCardinal(Pointer($00400000 + $8D10D70))^;
      CachedYellowBlockID := PWord(Pointer(baseStructAddr + $1A))^;
      CachedGreenBlockID := PWord(Pointer(baseStructAddr + $1C))^;
      LightBlockIDsCached := True;

    except
      // При ошибке используем значения по умолчанию
      CachedYellowBlockID := 13;
      CachedGreenBlockID := 14;
      LightBlockIDsCached := True;
    end;
  end;

  //UpdateBoosterKeyboard;
  //RenderBoosterKeyboard;
  
  // ===== ОБРАБОТКА КЛАВИАТУРЫ =====
  if (timeGetTime - LastKeyboardCheck) > KeyboardCheckInterval then
  begin
    NewCommandReceived := ProcessKeyboard;
    LastKeyboardCheck := timeGetTime;
  end;

  // ===== ОБРАБОТКА КОМАНДЫ 137 =====
  
  // Проверяем новую команду "137" только если она еще не активна
  if NewCommandReceived and (LastCommand = '137') and not statek137 then
  begin
    statek137 := True;
    SavedCommand := LastCommand;
    CommandCompleted := False;

    if WriteBoosterAndVerify($00400000 + $34988C, 52) then
    begin
      //AddToLogFile(EngineLog, 'Команда "137" успешно активирована');
      CommandBuffer := ''; // ← ОЧИЩАЕМ БУФЕР СРАЗУ!
      EnterPressed := False;
    end
    else
    begin
      //AddToLogFile(EngineLog, 'ОШИБКА активации команды "137"');
      statek137 := False;
      CommandBuffer := '';
    end;
  end;

  // Отображаем интерфейс команды 137
  // 1.17, 7.1799998, 3.4319999
  // angZ - 0.033, z + 0.0340002, y + 0.1210001
  if statek137 and not CommandCompleted and (PByte(Pointer($00400000 + $34988C))^ = 53) then
  begin
    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
    BeginObj3D;
    Position3D(angZ - 0.045, z + 0.0400002, y + 0.1420001);
    RotateX(-90.0);
    RotateY(45.0);
    Scale3D(0.011);
     Position3D(-0.045, 0.0400002, 0.1420001);
    Color3D(3407667, 255, False, 0.0);
    SetTexture(0);
    DrawText3D(0, 'ТАБЛИЦА АЛС-ЕН');
    EndObj3D;
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
  end;


  // Завершаем команду 137 при повторном нажатии ENTER
  if statek137 and not CommandCompleted and EnterPressed then
  begin

    if WriteBoosterAndVerify($00400000 + $34988C, 30) then
    begin
      // ПРОВЕРЯЕМ И СОХРАНЯЕМ ВВЕДЕННОЕ ЧИСЛО В en_chastota
    try
      if (CommandBuffer <> '') and (StrToInt(CommandBuffer) <= 3) and (StrToInt(CommandBuffer) > 0) then
      begin
        en_chastota := CommandBuffer + 'ЕН';
        als_en_state := True;
      end
      else
      begin
        en_chastota := 'x';
        als_en_state := False;
      end;
    except
      en_chastota := 'x'; // На случай ошибки преобразования
      als_en_state := False;
    end;


      
      CommandCompleted := True;
      statek137 := False;
      CommandBuffer := '';
      SavedCommand := '';
    end
  end;
  
  // Проверяем новую команду "10" только если она еще не активна
  if NewCommandReceived and (LastCommand = '10') and not statek10 then
  begin
    statek10 := True;
    SavedCommand := LastCommand;
    CommandCompleted := False;
    
    if WriteBoosterAndVerify($00400000 + $34988C, 53) then  // Попробуй 53, потом 54 если не работает
    begin
      CommandBuffer := '';
      EnterPressed := False;
    end
    else
    begin
      statek10 := False;
      CommandBuffer := '';
    end;
  end;




  try
    textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + $34);
    textureID := PWord(textureAddr)^;
  except
    textureID := MyTextureID; // fallback
  end;

  try
    modelAddr := Pointer(PCardinal(Pointer($00400000 + $8D10D70))^ + $04);
    modelID := PWord(modelAddr)^;
  except
    modelID := MyModelID; // fallback
  end;

  if not blokhookklub then
     begin
  BeginObj3D;
  //Position3D(1.17, 7.1799998, 3.4319999);
  Position3D(angZ, z, y);
  RotateZ(x);
  SetTexture(textureID);
  DrawModel(modelID, 0, False);
  EndObj3D;
     end
  else
    DrawBLOCK(x,y,z,angZ);


//



  // ======== АНАЛИЗ СВЕТОФОРОВ ========
  if (timeGetTime - LastSignalUpdate) > SignalUpdateInterval then
  begin
    CachedSignalSequence := GetSignalSequenceRuntime(_dlgAls);
    LastSignalUpdate := timeGetTime;
  end;
  signalSequence := CachedSignalSequence; // используем кэш


  // Частота ЕН
  if (not BLOCKInitialized) and (PByte(Pointer(PCardinal(Pointer($00400000 + $348638))^ + $2D0))^ > 0) then
  begin
    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
    BeginObj3D;
    Position3D(angZ - 0.033, z + 0.0340002, y + 0.1210001);
    // Position3D(1.137, 7.214, 3.553);
    RotateX(-90.0);
    RotateY(45.0);
    Scale3D(0.011);
    Color3D(3407667, 255, False, 0.0);
    SetTexture(0);
    DrawText3D(0, en_chastota);
    EndObj3D;
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
  end;


  // ======== ЛОГИКА ОТОБРАЖЕНИЯ МОДЕЛЕЙ ОТКЛОНЕНИЯ ========
  if (not BLOCKInitialized) and (als_en_state) and (PByte($905B754)^ <> 0) and (PByte(Pointer(PCardinal(Pointer($00400000 + $348638))^ + $2D0))^ > 0) then
  begin
    if KlubBilIndPModelID > 0 then
    begin
      BeginObj3D;
      glDisable(GL_LIGHTING);
      Position3D(0, 0, 0);
      Color3D($00FF00, 255, False, 0.0);
      SetTexture(0);
      DrawModel(KlubBilIndPModelID, 0, False);
      glEnable(GL_LIGHTING);
      EndObj3D;
    end;
  end
  else if (als_en_state) then
  begin
    if KlubBilIndPModelID > 0 then
    begin
      BeginObj3D;
      glDisable(GL_LIGHTING);
      Position3D(0, 0, 0);
      Color3D($00FF00, 255, False, 0.0);
      SetTexture(0);
      DrawModel(KlubBilIndPModelID, 0, False);
      glEnable(GL_LIGHTING);
      EndObj3D;
    end;
  end;

  BeginObj3D;




  // Получаем состояние основного светофора
  mainTrafficLight := PByte(Pointer(_dlgAls))^;

  // Показываем блоки ТОЛЬКО при mainTrafficLight == 5
  if (mainTrafficLight = 5) and (als_en_state) and (PByte($905B754)^ = 0) then
  begin
    // ИСПРАВЛЕННАЯ ЛОГИКА: Подсчитываем видимые сигналы ДО первого КРАСНОГО (К)
    visibleSignalCount := 0;

    // Сначала найдем позицию красного сигнала или конец строки
    signalIndex := Length(signalSequence);
    for i := 1 to Length(signalSequence) do
    begin
      if signalSequence[i] = 'К' then  // ← ИЗМЕНЕНО: ищем красный вместо черного
      begin
        signalIndex := i - 1; // Берем все символы ДО красного
        Break;
      end;
    end;
    
    // Теперь считаем видимые сигналы (Ж и З) в диапазоне от 1 до signalIndex
    for i := 1 to signalIndex do
    begin
      signalChar := signalSequence[i];
      if (signalChar = 'Ж') or (signalChar = 'З') then
        Inc(visibleSignalCount);
    end;

    try
      // Получаем базовый адрес структуры
      baseStructAddr := PCardinal(Pointer($00400000 + $8D10D70))^;
      
      // Читаем ID желтого блока светофора - klub_ls_zh.dmd
      yellowBlockModelAddr := Pointer(baseStructAddr + $1A);
      yellowBlockModelID := PWord(yellowBlockModelAddr)^;
      
      // Читаем ID зеленого блока светофора - klub_ls_z.dmd  
      greenBlockModelAddr := Pointer(baseStructAddr + $1C);
      greenBlockModelID := PWord(greenBlockModelAddr)^;

    except
      // В случае ошибки используем значения по умолчанию
      yellowBlockModelID := $0D; // 13
      greenBlockModelID := $0E;  // 14
    end;


    SetTexture(textureID);
    // ======== СИСТЕМА КЛУБ ========
    if (visibleSignalCount > 0) and (not BLOCKInitialized) then

    begin
      // Нижний блок - желтый
      BeginObj3D;
      glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
      // angZ, z, y 1.17, 7.1799998, 3.4319999
      Position3D(angZ, z, y);
      RotateZ(x);
      Position3D(-0.086499996, 0.0, 0.223);
      Scale3D(0.88999999);
      SetTexture(textureID);
      DrawModel(yellowBlockModelID, 0, False); // Читаем из памяти
      glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
      EndObj3D;

      // Остальные блоки выше - зеленые
      for i := 1 to visibleSignalCount - 1 do
      begin
        BeginObj3D;
        glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
        // angZ, z, y
        Position3D(angZ, z, y);
        RotateZ(x);
        Position3D(-0.086499996, 0.0, 0.223 + i * 0.013);
        Scale3D(0.88999999);
        SetTexture(textureID);
        DrawModel(greenBlockModelID, 0, False); // Читаем из памяти
        glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
        EndObj3D;

        if i >= 4 then Break; // Ограничиваем максимум 5 блоков
      end;
    end;


    if GetLocomotiveFolder(GetLocomotiveTypeFromSettings) = 'chs8' then
    begin
      bilpom_x := 32.0;
      bilpom_y := 0.0;
      bilpom_z := 3.572;
      bilpom_AngZ := 7.467;
      bilpom_AngX := 0.0020000001;
    end
    else if GetLocomotiveFolder(GetLocomotiveTypeFromSettings) = 'chs7' then
    begin
      bilpom_x := 30.0;
      bilpom_y := 0.0;
      bilpom_z := 3.5699999;
      bilpom_AngZ := 7.5500002;
      bilpom_AngX := 0.0020000001;
    end
    else if GetLocomotiveFolder(GetLocomotiveTypeFromSettings) = 'ed4m' then
    begin
      bilpom_x := 5.0;
      bilpom_y := 0.0;
      bilpom_z := 3.1059999;
      bilpom_AngZ := 10.087;
      bilpom_AngX := -0.0089999996;
    end;


    // ======== СИСТЕМА BIL POM ========
    if visibleSignalCount > 0 then
    begin
      // Нижний блок - желтый
      BeginObj3D;
      glDisable(GL_LIGHTING); // ← ДОБАВИТЬ

      // DRAWKLUBLS Single x, Single y, Single z, Single AngZ, Single AngX
      // AngX AngZ z
      Position3D(bilpom_AngX, bilpom_AngZ, bilpom_z);
      RotateZ(bilpom_y);
      RotateX(-bilpom_x);
      Position3D(0.0, 0.0, 0.07);
      SetTexture(textureID);
      DrawModel(yellowBlockModelID, 0, False); // Читаем из памяти
      glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
      EndObj3D;

      // Остальные блоки выше - зеленые
      for i := 1 to visibleSignalCount - 1 do
      begin
        BeginObj3D;
        glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
        // DRAWKLUBLS Single x, Single y, Single z, Single AngZ, Single AngX
        // AngX AngZ z
        Position3D(bilpom_AngX, bilpom_AngZ, bilpom_z);
        RotateZ(bilpom_y);
        RotateX(-bilpom_x);
        Position3D(0.0, 0.0, 0.07 + i * 0.014);
        SetTexture(textureID);
        DrawModel(greenBlockModelID, 0, False); // Читаем из памяти

        glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
        EndObj3D;

        if i >= 4 then Break; // Ограничиваем максимум 5 блоков
      end;
    end;
  end;

  EndObj3D;

  // ======== ИНИЦИАЛИЗАЦИЯ ========
  initialized := False;

  // Загрузка моделей
  if MyModelID = 0 then
  begin
    MyModelID := LoadModel('data\loc\klub_bil_v.dmd', 0, False);
    MyTextureID := LoadTextureFromFile('data\loc\klub_bil.bmp', 0, -1);
    UsavppDisplayModelID := LoadModel('data\' + GetLocomotiveFolder(GetLocomotiveTypeFromMemory) + '\' + LocNum + '\loc\usavpp-display.dmd', 0, False);
    UsavppDisplayTextureID := LoadTextureFromFile('data\' + GetLocomotiveFolder(GetLocomotiveTypeFromMemory) + '\' + LocNum + '\loc\usavpp-display.bmp', 0, -1);
    strelka := LoadModel('data\' + GetLocomotiveFolder(GetLocomotiveTypeFromMemory) + '\' + LocNum + '\strelka-m.dmd', 0, False);
  end;

//  BeginObj3D;
//  glDisable(GL_LIGHTING);
//  Position3D(0,0,0);
//  SetTexture(UsavppDisplayTextureID);
//  DrawModel(UsavppDisplayModelID, 0, False); // Читаем из памяти
//  glEnable(GL_LIGHTING);
//  EndObj3D;

  currentLocType := GetLocomotiveTypeFromMemory;
  locFolder := GetLocomotiveFolder(currentLocType);

  if not ((currentLocType = 812) or (currentLocType = 822)) then
  begin
    Exit; // Выходим из процедуры
  end;


  // Создание шрифта Impact
  if SevenSegmentFont = 0 then
    SevenSegmentFont := CreateFont3D('7-Segment');

  if KLUBUFont = 0 then
    KLUBUFont := CreateFont3D('KLUBU');  // ← ЗАГРУЗКА ШРИФТА KLUBU

  // Инициализация данных
  if GetLocomotiveFolder(GetLocomotiveTypeFromSettings) = 'chs8' then
    InitializeStaticData2
  else
    InitializeStaticData;


  // Получаем текущую скорость
  try
    currentSpeed := PSingle(BaseAddress + $4F8C28C)^;
    currentSpeed := Abs(currentSpeed);
  except
    currentSpeed := 0;
  end;


  // Отображение элементов УСАВПП при включенном КЛУБ
  if (PByte(Pointer(PCardinal(Pointer($00400000 + $348638))^ + $2D0))^ > 0) then
  begin
    for i := 0 to 35 do
      DrawObject(StaticData[i], i);
  end;




  // ===== ОТРИСОВКА СТРЕЛКИ С НОВЫМИ ПАРАМЕТРАМИ =====
  if Config_BGSD and (PByte(Pointer(PCardinal(Pointer($00400000 + $348638))^ + $2D0))^ > 0) then
  begin
    if StrToFloat(GetSpeed) <= 90 then
      ArrowKoef := 1.58
    else
      ArrowKoef := 1.68;

    speedAngle := 155 - GetSpeedValue2 * ArrowKoef;

    BeginObj3D;
    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
    if GetLocomotiveTypeFromMemory = 812 then
      Position3D(0.8972, 7.448-0.1042, 3.6407)
    else
      Position3D(0.8972, 7.448, 3.6407);
    RotateX(-10.4);
    RotateY(0);
    RotateZ(x + -10.0);
    SetTexture(0);
    Color3D($ffffffff, 255, False, 0.0);
    RotateY(speedAngle);
    Scale3D(1.61);                            
    DrawModel(strelka, 0, False);
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
    EndObj3D;
  end;

  if Config_BGSD then
  begin
    // Получаем значение ALS
    alsValue := GetALS;

  if alsValue = 3 then
  begin
    glDisable(GL_LIGHTING);

    // Верхняя половина - желтая
    BeginObj3D;
    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
    if GetLocomotiveTypeFromMemory = 812 then
      Position3D(0.896, 7.4482-0.1042, 3.641)
    else
      Position3D(0.896, 7.4482, 3.641);
    RotateX(-103.0);
    RotateY(34.2);
    RotateZ(-7.0); // Поворот против часовой на 15 градусов
    if GetLocomotiveTypeFromMemory = 812 then
      Scale3D(0.0017)
    else
      Scale3D(0.002);
    Color3D($00FFFF, 255, False, 0.0); // Желтый
    SetTexture(0);
    DrawBooster3DSemiCircle(5.2, 0, 180);
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
    EndObj3D;

    // Нижняя половина - красная
    BeginObj3D;
    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
    if GetLocomotiveTypeFromMemory = 812 then
      Position3D(0.896, 7.4482-0.1042, 3.641)
    else
      Position3D(0.896, 7.4482, 3.641);
    RotateX(-103.0);
    RotateY(34.2);
    RotateZ(-7.0); // Тот же поворот для совпадения
    if GetLocomotiveTypeFromMemory = 812 then
      Scale3D(0.0017)
    else
      Scale3D(0.002);
    Color3D($0000FF, 255, False, 0.0); // Красный
    SetTexture(0);
    DrawBooster3DSemiCircle(5.2, 180, 360);
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
    EndObj3D;
  
  end
  else
  begin
    // Обычный одноцветный диск
    case alsValue of
      1: diskColor := $FFFFFF;  // Белый
      2: diskColor := $0000FF;  // Красный  
      4: diskColor := $00FFFF;  // Желтый
      5: diskColor := $00FF00;  // Зеленый
      else diskColor := $808080; // Серый по умолчанию
    end;
    
    BeginObj3D;
    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
    if GetLocomotiveTypeFromMemory = 812 then
      Position3D(0.8965, 7.4482-0.1042, 3.64)
    else
      Position3D(0.896, 7.4482, 3.641);
    RotateX(-103.0);
    RotateY(34.2);
    if GetLocomotiveTypeFromMemory = 812 then
      Scale3D(0.00017)
    else
      Scale3D(0.0002);
    Color3D(diskColor, 255, False, 0.0);
    SetTexture(0);
    DrawBooster3DDisk(52);
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
    EndObj3D;
  end;
end;

  // Кастомные 3D-тексты + гизмо для текущей кабины. Дедуплекс внутри
  // — за кадр выполнится только один раз, даже если ниже по pipeline'у
  // тоже сработает DrawKPD3 / DrawBLOCK.
  RenderCustomTextsAndGizmoForFrame;

end;




{------------------------------------------------------------------}
procedure DrawCube(Width,Height,Depth : single); stdcall;
begin
glPushMatrix();
    glScalef(Width,Height,Depth);
    glBegin(GL_QUADS);
      // Front Face
      glNormal3f( 0.0, 0.0, 1.0);
      _glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      _glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      _glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
      _glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
      // Back Face
      glNormal3f( 0.0, 0.0,-1.0);
      _glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
      _glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
      _glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      _glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
      // Top Face
      glNormal3f( 0.0, 1.0, 0.0);
      _glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
      _glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);
      _glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);
      _glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      // Bottom Face
      glNormal3f( 0.0,-1.0, 0.0);
      _glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);
      _glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);
      _glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      _glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      // Right face
      glNormal3f( 1.0, 0.0, 0.0);
      _glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
      _glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      _glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
      _glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      // Left Face
      glNormal3f(-1.0, 0.0, 0.0);
      _glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
      _glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      _glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
      _glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glEnd();
glPopMatrix();
end;

function GetAlsEnState: Boolean;
begin
  Result := als_en_state;
end;

function GetAlsEnVisibleSignalCount: Integer;
var
  i, idx, count: Integer;
  seq: string;
  ch: Char;
begin
  Result := 0;
  seq := CachedSignalSequence;
  if seq = '' then Exit;

  idx := Length(seq);
  for i := 1 to Length(seq) do
    if seq[i] = 'К' then
    begin
      idx := i - 1;
      Break;
    end;

  count := 0;
  for i := 1 to idx do
  begin
    ch := seq[i];
    if (ch = 'Ж') or (ch = 'З') then
      Inc(count);
  end;

  Result := count;
end;

initialization
  if ZDSVersion = 54 then
  begin
    _dlgMode := $00400000 + $32CA84;
    _dlgCommandBlock := $00400000 + $32CA88;
    _dlgVk := $00400000 + $4F711B8;
    _dlgPathNumber := $00400000 + $4F711D0;
    _dlgPathDirection := $00400000 + $32CA8C;
    _dlgRb := $00400000 + $32CB08;
    _dlgRbs := $00400000 + $32CB04;
    _dlgAls := $00400000 + $8BEB744;
    _dlgPressureTm := $00400000 + $8CF3FA8;
    _dlgPressureAltPtr := $00400000 + $8CF45D0;
    FloatValueAddr := 0;
    PointerAddress := 0;
    WindowOpenAddress := 0;
  end
  else
  begin
    _dlgMode := _dlgMode;
    _dlgCommandBlock := $00400000 + $34988C;
    _dlgVk := $00400000 + $4F8D940;
    _dlgPathNumber := $00400000 + $4F8D958;
    _dlgPathDirection := $00400000 + $349890;
    _dlgRb := $00400000 + $349914;
    _dlgRbs := $00400000 + $349910;
    _dlgAls := $00400000 + $8C07ECC;
    _dlgPressureTm := $00400000 + $8D10738;
    _dlgPressureAltPtr := $00400000 + $8D10D78;
    FloatValueAddr := $00400000 + $8D1072C;
    PointerAddress := $900421C;
    WindowOpenAddress := $00400000 + $4F8D915;
  end;

end.
