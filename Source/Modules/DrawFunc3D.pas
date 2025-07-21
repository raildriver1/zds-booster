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
     Textures, DPC_Packages, Classes, KlubData, KlubProcessor, Math, TlHelp32,   MMSystem;


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

procedure WriteHookAddress; stdcall;
function GetCurrentHour: Integer; stdcall;  // если еще нет
procedure ProcessFreecam; stdcall;
procedure LoadSettingsAndCustomModels; stdcall;
procedure ProcessDayNightSystem; stdcall;
procedure LoadConfigFile; stdcall;
var
  ConfigLoaded: Boolean;
procedure ApplyMaxVisibleDistance; stdcall;
procedure ProcessStepForwardConfig; stdcall;
procedure DrawSky(x, y, z: Single); stdcall;
procedure PatchDrawSkyCall; stdcall;
procedure ProcessAllModules; stdcall;

// Функции для синхронизации с меню
procedure SyncConfigFromMenu(Freecam, MainCamera, MaxDistance, NewSky: Boolean); stdcall;
function GetConfigFreecam: Boolean; stdcall;
function GetConfigMainCamera: Boolean; stdcall;
function GetConfigMaxDistance: Boolean; stdcall;
function GetConfigNewSky: Boolean; stdcall;

procedure HookKLUB(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
); stdcall; export;

exports
  HookKLUB, DrawSky;
procedure FreeEng;
procedure InitEng;

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
  TimeCheckInterval: Cardinal = 100; // раз в 100мс

  LastFreecamConfigState: Boolean = False;  // Предыдущее состояние из конфига
  FreecamConfigStateInitialized: Boolean = False;  // Флаг инициализации

StationsList: TStringList;
StationsLoaded: Boolean = False;
CurrentStationName: string = '';
NextStationName: string = '';

LastCollide : integer = -1;

Obj3DInfo : array of TObj3DInfo;
InBlock : boolean = false;

CantRenderInFBO : boolean;
fbo_frame : cardinal = 0;
fbo_depth : cardinal = 0;
fbo_w, fbo_h, fbo_z : cardinal;
fbo2 : cardinal;

implementation
uses Advanced3D, DrawFunc2D;

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

  FloatValueAddr: Cardinal = $00400000 + $8D1072C; // Адрес float значения
  LastFloatValue: Single = -999.0; // Для отслеживания изменений
  FloatAsInt: Integer;
  FloatStr: string;

    // Переменные конфигурации
  Config_SAUT: Boolean = True;   // Отображение элементов скорости и лимитов (14, 15, 16)
  Config_BGSD: Boolean = True;   // Отображение основных данных (0-13)
  Config_STUPEN: Boolean = True;  // ← ДОБАВИТЬ ЭТУ СТРОКУ

  Config_Freecam: Boolean = True;      // Включен ли фрикам
  Config_MainCamera: Boolean = True;   // Включена ли настройка основной камеры
  Config_MaxDistance: Boolean = True;  // Включена ли настройка дистанции
  Config_NewSky: Boolean = True;       // Включено ли новое небо (уже было)

  // ===== НОВЫЕ ПЕРЕМЕННЫЕ ДЛЯ ФРИКАМА =====
  Config_BaseSpeed: Single = 0.0;        // Базовая скорость фрикама
  Config_FastSpeed: Single = 2.2;        // Быстрая скорость фрикама (Shift)
  Config_TurnSpeed: Single = 1.5;        // Скорость поворота камеры
  Config_MaxVisibleDistance: Integer = 820; // Максимальная дистанция видимости

  Config_StepForward: Single;              // Значение stepForward
  LastStepForwardCheck: Cardinal;
  StepForwardCheckInterval: Cardinal;      // Проверка каждую секунду

  BoosterSunriseDawnTextureID: Cardinal = 0;
  BoosterSunsetTwilightTextureID: Cardinal = 0;

  // НОВЫЕ переменные для зимы
  BoosterDaySnowTextureID: Cardinal = 0;
  BoosterSunsetSnowTextureID: Cardinal = 0;
  BoosterSunsetTwilightSnowTextureID: Cardinal = 0;
  BoosterNightSnowTextureID: Cardinal = 0;
  BoosterSunriseDawnSnowTextureID: Cardinal = 0;
  BoosterSunriseSnowTextureID: Cardinal = 0;

  BoosterTexturesLoaded: Boolean = False;

  SkyPatchApplied: Boolean = False; // Флаг применения патча


const
  PANEL_BASE_WIDTH = 80;      // Базовая ширина панели
  PANEL_EXPANDED_WIDTH = 300; // Расширенная ширина
  PANEL_HEIGHT = 200;         // Высота панели
  PANEL_MARGIN = 10;          // Отступ от края экрана
  ANIMATION_SPEED = 8.0;      // Скорость анимации

  // Тип для хранения информации о станции
type
  TStationInfo = record
    name: string;
    piket: Integer;
    distance: Integer;
  end;
  TExtendedBytes = array[0..9] of Byte;

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

  // Переменные для работы со светофорами
  s1, s2: TStringList;
  TrafficSystemInitialized: Boolean = False;
  HookAddressWritten: Boolean = False;

    LastSignalUpdate: Cardinal = 0;
  SignalUpdateInterval: Cardinal = 2000; // каждые 500мс
  CachedSignalSequence: string = '';

  // Глобальные переменные для обработки команд
  LastCommand: string = '';
  CommandBuffer: string = '';
  PointerAddress: Cardinal = $900421C;
  WindowOpenAddress: Cardinal = $00400000 + $4F8D915;

  // Массивы для отслеживания состояний клавиш
  PreviousKeyStates: array[0..11] of Byte;
  KeyStatesInitialized: Boolean = False;

  LastMaxDistanceState: Boolean = False;
  MaxDistanceInitialized: Boolean = False;
  OriginalMaxDistancePatched: Boolean = False;
  
  // Для StepForward  
  LastMainCameraState: Boolean = False;
  MainCameraInitialized: Boolean = False;
  OriginalStepForwardValue: Single = 0.1; // Оригинальное значение
  
  // Для патча неба
  LastNewSkyState: Boolean = False;
  NewSkyInitialized: Boolean = False;
  OriginalMaxDistanceValue: Integer = 800; // Сохраняем оригинальное значение

  // Флаги для обработки команд  
  statek137: Boolean = False;
  statek10: Boolean = False;  // <- ДОБАВИТЬ ЭТУ СТРОКУ
  SavedCommand: string = '';
  CommandCompleted: Boolean = False;
  EnterPressed: Boolean = False;

  // Переменные для моделей
  MyModelID: integer = 0;
  KlubBilIndPModelID: Integer = 0;  // для klub-bil-ind_p.dmd (||)
  KlubBilIndBModelID: Integer = 0;  // для klub-bil-ind_b.dmd (|, l, -)
  MyTextureID: cardinal = 0;
  strelka: integer = 0;
  ImpactFont: Integer = 0;
  KLUBUFont: Integer = 0;        // ← ДОБАВИТЬ ЭТУ ПЕРЕМЕННУЮ

  LastKeyboardCheck: Cardinal = 0;
  KeyboardCheckInterval: Cardinal = 1; // ~60 FPS = каждые 16мс

  // Переменные переменных
  en_chastota: string = 'x';
  MemoryWritten: Boolean = False;

  als_en_state: Boolean = False;

  YellowBlockRotX: Single = 0.0;
  YellowBlockRotY: Single = 0.0;
  YellowBlockRotZ: Single = 0.0;
  YellowBlockPosX: Single = -0.086499996;
  YellowBlockPosY: Single = 0.0;
  YellowBlockPosZ: Single = 0.223;
  YellowBlockScale: Single = 0.88999999;
  YellowBlockParamsLoaded: Boolean = False;
  // Переменные для фрикамы
  FreecamEnabled: Boolean = False;
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
  BoosterConfigCheckInterval: Cardinal = 1000; // Реже обновляем настройки отображения
  
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
  FreecamKeyDelay: Cardinal = 100; // 200мс задержка между нажатиями
  MaxDistanceWritten: Boolean = False;  // ← ДОБАВИТЬ ЭТУ СТРОКУ
  
  // NOP патч
  OriginalBytes: array[0..4] of Byte;
  NopBytes: array[0..4] of Byte = ($90, $90, $90, $90, $90);

  // Переменные для системы день/ночи
  LastTimeCheck1: Cardinal = 0;
  TimeCheckInterval1: Cardinal = 7000; // проверка каждые 7 секунд
  CurrentTimeMode: Integer = -1; // -1 = не инициализировано, 0 = день, 1 = ночь
  DayNightInitialized: Boolean = False;
  HasDayNightFolders: Boolean = False;
  
  // ID текстур для дня и ночи
  DayCabTextureID: Cardinal = 0;
  DayPultTextureID: Cardinal = 0;
  Day254TextureID: Cardinal = 0;
  DayKlubTextureID: Cardinal = 0;
  
  NightCabTextureID: Cardinal = 0;
  NightPultTextureID: Cardinal = 0;
  Night254TextureID: Cardinal = 0;
  NightKlubTextureID: Cardinal = 0;

  CurrentIsNight: Boolean = False;

procedure SyncConfigFromMenu(Freecam, MainCamera, MaxDistance, NewSky: Boolean); stdcall;
begin
  Config_Freecam := Freecam;
  Config_MainCamera := MainCamera;
  Config_MaxDistance := MaxDistance;
  Config_NewSky := NewSky;
  
  AddToLogFile(EngineLog, Format('Синхронизация из меню: F=%s M=%s D=%s S=%s',
    [BoolToStr(Freecam, True), BoolToStr(MainCamera, True), 
     BoolToStr(MaxDistance, True), BoolToStr(NewSky, True)]));
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
procedure LoadBoosterSkyTextures;
var
  routeName: string;
  skyTexturesPath: string;
  // Летние пути (только дополнительные)
  sunriseDawnPath, sunsetTwilightPath: string;
  // Зимние пути (все)
  daySnowPath, sunsetSnowPath, sunsetTwilightSnowPath: string;
  nightSnowPath, sunriseDawnSnowPath, sunriseSnowPath: string;
begin
  if BoosterTexturesLoaded then Exit;
  
  try
    routeName := GetRouteName;
    if routeName = '' then
    begin
      AddToLogFile(EngineLog, 'Route name not found, skipping sky textures loading');
      Exit;
    end;
    
    skyTexturesPath := 'routes\' + routeName + '\textures\';
    
    // Летние дополнительные пути
    sunriseDawnPath := skyTexturesPath + 'sky_sunriseDawn.bmp';
    sunsetTwilightPath := skyTexturesPath + 'sky_sunsetTwilight.bmp';
    
    // Зимние пути (все текстуры)
    daySnowPath := skyTexturesPath + 'sky_day_snow.bmp';
    sunsetSnowPath := skyTexturesPath + 'sky_sunset_snow.bmp';
    sunsetTwilightSnowPath := skyTexturesPath + 'sky_sunsetTwilight_snow.bmp';
    nightSnowPath := skyTexturesPath + 'sky_night_snow.bmp';
    sunriseDawnSnowPath := skyTexturesPath + 'sky_sunriseDawn_snow.bmp';
    sunriseSnowPath := skyTexturesPath + 'sky_sunrise_snow.bmp';

    AddToLogFile(EngineLog, '=== ЗАГРУЗКА ТЕКСТУР НЕБА (ЛЕТО + ЗИМА) ===');
    AddToLogFile(EngineLog, 'Route: ' + routeName);
    
    // Проверяем наличие летних дополнительных файлов
    if not (FileExists(sunriseDawnPath) and FileExists(sunsetTwilightPath)) then
    begin
      AddToLogFile(EngineLog, 'Summer additional sky files not found');
      Exit;
    end;
    
    // Проверяем наличие всех зимних файлов
    if not (FileExists(daySnowPath) and FileExists(sunsetSnowPath) and 
            FileExists(sunsetTwilightSnowPath) and FileExists(nightSnowPath) and
            FileExists(sunriseDawnSnowPath) and FileExists(sunriseSnowPath)) then
    begin
      AddToLogFile(EngineLog, 'Winter sky files not found');
      Exit;
    end;
    
    // Загружаем летние дополнительные текстуры
    BoosterSunriseDawnTextureID := LoadTextureFromFile(sunriseDawnPath, 0, -1);
    if BoosterSunriseDawnTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded summer sky_sunriseDawn.bmp, ID: ' + IntToStr(BoosterSunriseDawnTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load summer sky_sunriseDawn.bmp');
      Exit;
    end;

    BoosterSunsetTwilightTextureID := LoadTextureFromFile(sunsetTwilightPath, 0, -1);
    if BoosterSunsetTwilightTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded summer sky_sunsetTwilight.bmp, ID: ' + IntToStr(BoosterSunsetTwilightTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load summer sky_sunsetTwilight.bmp');
      Exit;
    end;

    // Загружаем все зимние текстуры
    BoosterDaySnowTextureID := LoadTextureFromFile(daySnowPath, 0, -1);
    if BoosterDaySnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_day_snow.bmp, ID: ' + IntToStr(BoosterDaySnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_day_snow.bmp');
      Exit;
    end;

    BoosterSunsetSnowTextureID := LoadTextureFromFile(sunsetSnowPath, 0, -1);
    if BoosterSunsetSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunset_snow.bmp, ID: ' + IntToStr(BoosterSunsetSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunset_snow.bmp');
      Exit;
    end;

    BoosterSunsetTwilightSnowTextureID := LoadTextureFromFile(sunsetTwilightSnowPath, 0, -1);
    if BoosterSunsetTwilightSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunsetTwilight_snow.bmp, ID: ' + IntToStr(BoosterSunsetTwilightSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunsetTwilight_snow.bmp');
      Exit;
    end;

    BoosterNightSnowTextureID := LoadTextureFromFile(nightSnowPath, 0, -1);
    if BoosterNightSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_night_snow.bmp, ID: ' + IntToStr(BoosterNightSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_night_snow.bmp');
      Exit;
    end;

    BoosterSunriseDawnSnowTextureID := LoadTextureFromFile(sunriseDawnSnowPath, 0, -1);
    if BoosterSunriseDawnSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunriseDawn_snow.bmp, ID: ' + IntToStr(BoosterSunriseDawnSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunriseDawn_snow.bmp');
      Exit;
    end;

    BoosterSunriseSnowTextureID := LoadTextureFromFile(sunriseSnowPath, 0, -1);
    if BoosterSunriseSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunrise_snow.bmp, ID: ' + IntToStr(BoosterSunriseSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunrise_snow.bmp');
      Exit;
    end;

    BoosterTexturesLoaded := True;
    AddToLogFile(EngineLog, 'Route sky textures (summer + winter) loaded successfully');

  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error loading route sky textures: ' + E.Message);
      // Очищаем все частично загруженные текстуры
      if BoosterSunriseDawnTextureID > 0 then
      begin
        FreeTexture(BoosterSunriseDawnTextureID);
        BoosterSunriseDawnTextureID := 0;
      end;
      if BoosterSunsetTwilightTextureID > 0 then
      begin
        FreeTexture(BoosterSunsetTwilightTextureID);
        BoosterSunsetTwilightTextureID := 0;
      end;
      // Очищаем зимние текстуры
      if BoosterDaySnowTextureID > 0 then
      begin
        FreeTexture(BoosterDaySnowTextureID);
        BoosterDaySnowTextureID := 0;
      end;
      if BoosterSunsetSnowTextureID > 0 then
      begin
        FreeTexture(BoosterSunsetSnowTextureID);
        BoosterSunsetSnowTextureID := 0;
      end;
      if BoosterSunsetTwilightSnowTextureID > 0 then
      begin
        FreeTexture(BoosterSunsetTwilightSnowTextureID);
        BoosterSunsetTwilightSnowTextureID := 0;
      end;
      if BoosterNightSnowTextureID > 0 then
      begin
        FreeTexture(BoosterNightSnowTextureID);
        BoosterNightSnowTextureID := 0;
      end;
      if BoosterSunriseDawnSnowTextureID > 0 then
      begin
        FreeTexture(BoosterSunriseDawnSnowTextureID);
        BoosterSunriseDawnSnowTextureID := 0;
      end;
      if BoosterSunriseSnowTextureID > 0 then
      begin
        FreeTexture(BoosterSunriseSnowTextureID);
        BoosterSunriseSnowTextureID := 0;
      end;
    end;
  end;
end;

procedure DrawSkyLayer(textureID: Cardinal; alpha: Byte; modelID: Word);
begin
  try
    if (textureID > 0) and (modelID > 0) and (alpha > 0) then
    begin
      Color3D($FFFFFF, alpha, False, 0.0);
      SetTexture(textureID);
      DrawModel(modelID, 0, True);
    end;
  except
    // Безопасный fallback - пропускаем проблемную текстуру
  end;
end;

procedure DrawSky(x, y, z: Single);
var
  v5: Single;
  alpha: Byte;
  currentHour, currentMinute: Integer;
  lightingCheck: Byte;
  modelAddr, textureAddr: Pointer;
  modelID: Word;
  textureID: Word;
  timePtr: PInteger;
  v7: Pointer;
  a1: Boolean;
  
  // Текстуры для разных периодов (игровые)
  dayTextureID, sunsetTextureID, nightTextureID, sunriseTextureID: Word;
  
  // Переменные для плавных переходов
  totalMinutes: Integer;
  
  // Переменные для определения сезона
  isWinter: Boolean;
  
begin
  try
    // Загружаем дополнительные текстуры при первом вызове
    if not BoosterTexturesLoaded then
      LoadBoosterSkyTextures;
    
    // Определяем сезон
    try
      isWinter := PByte(Pointer($00400000 + $349968))^ = 1;
    except
      isWinter := False; // По умолчанию лето
    end;
    
    // Читаем указатель на структуру времени
    try
      v7 := PPointer(Pointer($00400000 + $34B5F0))^;
    except
      v7 := nil;
    end;
    
    BeginObj3D;
    
    // Отключаем освещение
    DeactiveLight(-1);
    
    // Устанавливаем позицию
    Position3D(z, y, x);
    
    // Читаем абсолютное время и вычисляем поворот
    try
      v5 := PDouble(Pointer($0538D920))^ * 15.0 / 3600.0 - 180.0;
      RotateZ(v5);
    except
      v5 := 0.0;
    end;
    
    // Проверяем флаг освещения
    try
      lightingCheck := PByte(Pointer($090043A4))^;
      if lightingCheck = 0 then
        Scale3D(1.2);
    except
      Scale3D(1.2);
    end;
    
    // Читаем текущий час и минуты
    try
      if v7 <> nil then
      begin
        currentHour := PInteger(v7)^;
        currentMinute := PInteger(Pointer($00400000 + $8C08038))^;
      end
      else
      begin
        currentHour := 12;
        currentMinute := 0;
      end;
    except
      currentHour := 12;
      currentMinute := 0;
    end;
    
    // Читаем ID модели неба
    try
      modelAddr := Pointer(PCardinal(Pointer($09110D70))^ + $02);
      modelID := PWord(modelAddr)^;
      if modelID = 0 then modelID := 1; // Fallback
    except
      modelID := 1;
    end;
    
    // Читаем стандартные игровые текстуры с проверками
    try
      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $42);
      dayTextureID := PWord(textureAddr)^;
      if dayTextureID = 0 then dayTextureID := 1;
      
      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $44);
      sunsetTextureID := PWord(textureAddr)^;
      if sunsetTextureID = 0 then sunsetTextureID := 1;
      
      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $02);
      nightTextureID := PWord(textureAddr)^;
      if nightTextureID = 0 then nightTextureID := 1;
      
      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $40);
      sunriseTextureID := PWord(textureAddr)^;
      if sunriseTextureID = 0 then sunriseTextureID := 1;
    except
      dayTextureID := 1;
      sunsetTextureID := 1;
      nightTextureID := 1;
      sunriseTextureID := 1;
    end;
    
    a1 := False;
    totalMinutes := currentHour * 60 + currentMinute;
    
    if isWinter then
    begin
      // ===== ЗИМНЕЕ ВРЕМЯ С СМЕЩЕНИЕМ НА +1 ЧАС =====
      
      // 7:00-8:30: Чистые предрассветные сумерки
      if (totalMinutes >= 420) and (totalMinutes < 510) then // 7:00-8:30
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterSunriseDawnSnowTextureID, alpha, modelID);
      end
      
      // 8:30-9:00: ПЛАВНЫЙ переход предрассветные сумерки → рассвет
      else if (totalMinutes >= 510) and (totalMinutes < 540) then // 8:30-9:00
      begin
        // Новая текстура (рассвет) плавно появляется
        alpha := Round((totalMinutes - 510) * 255 / 30);
        DrawSkyLayer(BoosterSunriseSnowTextureID, alpha, modelID);
        
        // Старая текстура (предрассветные сумерки) плавно исчезает
        alpha := Round(255 - (totalMinutes - 510) * 255 / 30);
        DrawSkyLayer(BoosterSunriseDawnSnowTextureID, alpha, modelID);
      end
      
      // 9:00-10:30: Чистый рассвет
      else if (totalMinutes >= 540) and (totalMinutes < 630) then // 9:00-10:30
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterSunriseSnowTextureID, alpha, modelID);
      end
      
      // 10:30-11:00: ПЛАВНЫЙ переход рассвет → день
      else if (totalMinutes >= 630) and (totalMinutes < 660) then // 10:30-11:00
      begin
        // Новая текстура (день) плавно появляется
        alpha := Round((totalMinutes - 630) * 255 / 30);
        DrawSkyLayer(BoosterDaySnowTextureID, alpha, modelID);
        
        // Старая текстура (рассвет) плавно исчезает
        alpha := Round(255 - (totalMinutes - 630) * 255 / 30);
        DrawSkyLayer(BoosterSunriseSnowTextureID, alpha, modelID);
      end
      
      // 11:00-15:30: Чистый день (ИЗМЕНЕНО: продлен до 15:30)
      else if (totalMinutes >= 660) and (totalMinutes < 930) then // 11:00-15:30
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterDaySnowTextureID, alpha, modelID);
      end
      
      // 15:30-16:00: ПЛАВНЫЙ переход день → закат (ИЗМЕНЕНО: было 14:30-15:00)
      else if (totalMinutes >= 930) and (totalMinutes < 960) then // 15:30-16:00
      begin
        // Новая текстура (закат) плавно появляется
        alpha := Round((totalMinutes - 930) * 255 / 30);
        DrawSkyLayer(BoosterSunsetSnowTextureID, alpha, modelID);
        
        // Старая текстура (день) плавно исчезает
        alpha := Round(255 - (totalMinutes - 930) * 255 / 30);
        DrawSkyLayer(BoosterDaySnowTextureID, alpha, modelID);
      end
      
      // 16:00-18:30: Чистый закат (ИЗМЕНЕНО: было 15:00-17:30)
      else if (totalMinutes >= 960) and (totalMinutes < 1110) then // 16:00-18:30
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterSunsetSnowTextureID, alpha, modelID);
      end
      
      // 18:30-19:00: ПЛАВНЫЙ переход закат → сумерки заката (ИЗМЕНЕНО: было 17:30-18:00)
      else if (totalMinutes >= 1110) and (totalMinutes < 1140) then // 18:30-19:00
      begin
        // Новая текстура (сумерки заката) плавно появляется
        alpha := Round((totalMinutes - 1110) * 255 / 30);
        DrawSkyLayer(BoosterSunsetTwilightSnowTextureID, alpha, modelID);
        
        // Старая текстура (закат) плавно исчезает
        alpha := Round(255 - (totalMinutes - 1110) * 255 / 30);
        DrawSkyLayer(BoosterSunsetSnowTextureID, alpha, modelID);
      end
      
      // 19:00-20:30: Чистые сумерки заката (ИЗМЕНЕНО: было 18:00-19:30)
      else if (totalMinutes >= 1140) and (totalMinutes < 1230) then // 19:00-20:30
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterSunsetTwilightSnowTextureID, alpha, modelID);
      end
      
      // 20:30-21:00: ПЛАВНЫЙ переход сумерки заката → ночь (ИЗМЕНЕНО: было 19:30-20:00)
      else if (totalMinutes >= 1230) and (totalMinutes < 1260) then // 20:30-21:00
      begin
        // Новая текстура (ночь) плавно появляется
        alpha := Round((totalMinutes - 1230) * 240 / 30); // До 240, не до 255
        DrawSkyLayer(BoosterNightSnowTextureID, alpha, modelID);
        
        // Старая текстура (сумерки заката) плавно исчезает
        alpha := Round(255 - (totalMinutes - 1230) * 255 / 30);
        DrawSkyLayer(BoosterSunsetTwilightSnowTextureID, alpha, modelID);
      end
      
      // 21:00-6:30: Чистая ночь (ИЗМЕНЕНО: было 20:00-6:30)
      else if (totalMinutes >= 1260) or (totalMinutes < 390) then // 21:00-6:30
      begin
        alpha := 240; // стандартная ночная яркость
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterNightSnowTextureID, alpha, modelID);
      end
      
      // 6:30-7:00: ПЛАВНЫЙ переход ночь → предрассветные сумерки
      else if (totalMinutes >= 390) and (totalMinutes < 420) then // 6:30-7:00
      begin
        // Новая текстура (предрассветные сумерки) плавно появляется
        alpha := Round((totalMinutes - 390) * 255 / 30);
        DrawSkyLayer(BoosterSunriseDawnSnowTextureID, alpha, modelID);
        
        // Старая текстура (ночь) плавно исчезает
        alpha := Round(240 - (totalMinutes - 390) * 240 / 30);
        DrawSkyLayer(BoosterNightSnowTextureID, alpha, modelID);
      end
      
      // Fallback на ночь
      else
      begin
        alpha := 240;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterNightSnowTextureID, alpha, modelID);
      end;
    end
    else
    begin
      // ===== ЛЕТНЕЕ ВРЕМЯ С ПЛАВНЫМИ ПЕРЕХОДАМИ (БЕЗ ИЗМЕНЕНИЙ) =====
      
      // 5:00-6:00: ПЛАВНЫЙ переход предрассветные сумерки → рассвет
      if (totalMinutes >= 300) and (totalMinutes < 360) then // 5:00-6:00
      begin
        // Новая текстура (рассвет) плавно появляется
        alpha := Round((totalMinutes - 300) * 255 / 60);
        DrawSkyLayer(sunriseTextureID, alpha, modelID);
        
        // Старая текстура (предрассветные сумерки) плавно исчезает
        alpha := Round(255 - (totalMinutes - 300) * 255 / 60);
        DrawSkyLayer(BoosterSunriseDawnTextureID, alpha, modelID);
      end
      
      // 6:00-7:00: Чистый рассвет
      else if (totalMinutes >= 360) and (totalMinutes < 420) then // 6:00-7:00
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(sunriseTextureID, alpha, modelID);
      end
      
      // 7:00-8:00: ПЛАВНЫЙ переход рассвет → день
      else if (totalMinutes >= 420) and (totalMinutes < 480) then // 7:00-8:00
      begin
        // Новая текстура (день) плавно появляется
        alpha := Round((totalMinutes - 420) * 255 / 60);
        DrawSkyLayer(dayTextureID, alpha, modelID);
        
        // Старая текстура (рассвет) плавно исчезает
        alpha := Round(255 - (totalMinutes - 420) * 255 / 60);
        DrawSkyLayer(sunriseTextureID, alpha, modelID);
      end
      
      // 8:00-17:00: Чистый день
      else if (totalMinutes >= 480) and (totalMinutes < 1020) then // 8:00-17:00
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(dayTextureID, alpha, modelID);
      end
      
      // 17:00-18:00: ПЛАВНЫЙ переход день → закат
      else if (totalMinutes >= 1020) and (totalMinutes < 1080) then // 17:00-18:00
      begin
        // Новая текстура (закат) плавно появляется
        alpha := Round((totalMinutes - 1020) * 255 / 60);
        DrawSkyLayer(sunsetTextureID, alpha, modelID);
        
        // Старая текстура (день) плавно исчезает
        alpha := Round(255 - (totalMinutes - 1020) * 255 / 60);
        DrawSkyLayer(dayTextureID, alpha, modelID);
      end
      
      // 18:00-19:00: Чистый закат
      else if (totalMinutes >= 1080) and (totalMinutes < 1140) then // 18:00-19:00
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(sunsetTextureID, alpha, modelID);
      end
      
      // 19:00-20:00: ПЛАВНЫЙ переход закат → сумерки заката
      else if (totalMinutes >= 1140) and (totalMinutes < 1200) then // 19:00-20:00
      begin
        // Новая текстура (сумерки заката) плавно появляется
        alpha := Round((totalMinutes - 1140) * 255 / 60);
        DrawSkyLayer(BoosterSunsetTwilightTextureID, alpha, modelID);
        
        // Старая текстура (закат) плавно исчезает
        alpha := Round(255 - (totalMinutes - 1140) * 255 / 60);
        DrawSkyLayer(sunsetTextureID, alpha, modelID);
      end
      
      // 20:00-21:00: Чистые сумерки заката
      else if (totalMinutes >= 1200) and (totalMinutes < 1260) then // 20:00-21:00
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterSunsetTwilightTextureID, alpha, modelID);
      end
      
      // 21:00-22:00: ПЛАВНЫЙ переход сумерки заката → ночь
      else if (totalMinutes >= 1260) and (totalMinutes < 1320) then // 21:00-22:00
      begin
        // Новая текстура (ночь) плавно появляется
        alpha := Round((totalMinutes - 1260) * 240 / 60); // До 240, не до 255
        DrawSkyLayer(nightTextureID, alpha, modelID);
        
        // Старая текстура (сумерки заката) плавно исчезает
        alpha := Round(255 - (totalMinutes - 1260) * 255 / 60);
        DrawSkyLayer(BoosterSunsetTwilightTextureID, alpha, modelID);
      end
      
      // 22:00-3:00: Чистая ночь
      else if (totalMinutes >= 1320) or (totalMinutes < 180) then // 22:00-3:00
      begin
        alpha := 240; // стандартная ночная яркость
        if a1 then alpha := 255;
        DrawSkyLayer(nightTextureID, alpha, modelID);
      end
      
      // 3:00-4:00: ПЛАВНЫЙ переход ночь → предрассветные сумерки
      else if (totalMinutes >= 180) and (totalMinutes < 240) then // 3:00-4:00
      begin
        // Новая текстура (предрассветные сумерки) плавно появляется
        alpha := Round((totalMinutes - 180) * 255 / 60);
        DrawSkyLayer(BoosterSunriseDawnTextureID, alpha, modelID);
        
        // Старая текстура (ночь) плавно исчезает до 240, не до 0
        alpha := Round(240 - (totalMinutes - 180) * 240 / 60);
        DrawSkyLayer(nightTextureID, alpha, modelID);
      end
      
      // 4:00-5:00: Чистые предрассветные сумерки
      else if (totalMinutes >= 240) and (totalMinutes < 300) then // 4:00-5:00
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawSkyLayer(BoosterSunriseDawnTextureID, alpha, modelID);
      end
      
      // Fallback на ночь
      else
      begin
        alpha := 240;
        if a1 then alpha := 255;
        DrawSkyLayer(nightTextureID, alpha, modelID);
      end;
    end;
    
  finally
    EndObj3D;
  end;
end;

procedure PatchDrawSkyCall;
var
  CallAddress: Cardinal;
  DrawSkyAddress: Cardinal;  
  NewOffset: Integer;
  OldProtect: DWORD;
  routeName: string;
  skyTexturesPath: string;
  // Летние дополнительные
  sunriseDawnPath, sunsetTwilightPath: string;
  // Зимние (все)
  daySnowPath, sunsetSnowPath, sunsetTwilightSnowPath: string;
  nightSnowPath, sunriseDawnSnowPath, sunriseSnowPath: string;
begin
  // ПРОВЕРЯЕМ ФЛАГ ВКЛЮЧЕНИЯ СИСТЕМЫ
  if not Config_NewSky then
  begin
    AddToLogFile(EngineLog, 'Sky patching disabled in config (newsky: 0)');
    Exit;
  end;
  
  if SkyPatchApplied then
  begin
    AddToLogFile(EngineLog, 'Sky patch already applied');
    Exit;
  end;
  
  try
    // Получаем название маршрута
    routeName := GetRouteName;
    if routeName = '' then
    begin
      AddToLogFile(EngineLog, 'Route name not found, skipping sky patch');
      Exit;
    end;
    
    // Проверяем наличие необходимых файлов текстур
    skyTexturesPath := 'routes\' + routeName + '\textures\';
    
    // Летние дополнительные пути
    sunriseDawnPath := skyTexturesPath + 'sky_sunriseDawn.bmp';
    sunsetTwilightPath := skyTexturesPath + 'sky_sunsetTwilight.bmp';

    // Зимние пути (все)
    daySnowPath := skyTexturesPath + 'sky_day_snow.bmp';
    sunsetSnowPath := skyTexturesPath + 'sky_sunset_snow.bmp';
    sunsetTwilightSnowPath := skyTexturesPath + 'sky_sunsetTwilight_snow.bmp';
    nightSnowPath := skyTexturesPath + 'sky_night_snow.bmp';
    sunriseDawnSnowPath := skyTexturesPath + 'sky_sunriseDawn_snow.bmp';
    sunriseSnowPath := skyTexturesPath + 'sky_sunrise_snow.bmp';

    // Проверяем наличие всех необходимых файлов
    if not (FileExists(sunriseDawnPath) and FileExists(sunsetTwilightPath) and
            FileExists(daySnowPath) and FileExists(sunsetSnowPath) and
            FileExists(sunsetTwilightSnowPath) and FileExists(nightSnowPath) and
            FileExists(sunriseDawnSnowPath) and FileExists(sunriseSnowPath)) then
    begin
      AddToLogFile(EngineLog, 'Sky textures (summer/winter) not found in route, skipping patch');
      Exit;
    end;

    // Остальная логика патчинга...
    CallAddress := $00400000 + $335FFF;
    DrawSkyAddress := Cardinal(@DrawSky);
    NewOffset := Integer(DrawSkyAddress) - Integer(CallAddress + 5);

    if VirtualProtect(Pointer(CallAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      PInteger(CallAddress + 1)^ := NewOffset;
      VirtualProtect(Pointer(CallAddress + 1), 4, OldProtect, OldProtect);
      SkyPatchApplied := True;
      AddToLogFile(EngineLog, 'DrawSky call patched successfully for route: ' + routeName + ' (full summer + winter support)');
    end;
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error patching DrawSky call: ' + E.Message);
    end;
  end;
end;

procedure RestoreOriginalSkyCall;
var
  CallAddress: Cardinal;
  OriginalOffset: Integer;
  OldProtect: DWORD;
begin
  if not SkyPatchApplied then
  begin
    AddToLogFile(EngineLog, 'Sky patch not applied, nothing to restore');
    Exit;
  end;
  
  try
    CallAddress := $00400000 + $335FFF;
    // Восстанавливаем оригинальный вызов: E8 1894D4FF (call Launcher.exe+7F41C)
    OriginalOffset := $FFD49418; // Оригинальное значение little-endian
    
    if VirtualProtect(Pointer(CallAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      PInteger(CallAddress + 1)^ := OriginalOffset;
      VirtualProtect(Pointer(CallAddress + 1), 4, OldProtect, OldProtect);
      SkyPatchApplied := False;
      AddToLogFile(EngineLog, 'Original DrawSky call restored successfully');
    end;
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error restoring original DrawSky call: ' + E.Message);
    end;
  end;
end;

procedure ProcessNewSkyPatch;
var
  StateChanged: Boolean;
begin
  // Проверяем изменилось ли состояние модуля
  StateChanged := not NewSkyInitialized or (Config_NewSky <> LastNewSkyState);
  
  if StateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния NewSky: %s -> %s',
      [BoolToStr(LastNewSkyState, True), BoolToStr(Config_NewSky, True)]));
      
    if Config_NewSky then
    begin
      // ===== МОДУЛЬ ВКЛЮЧЕН - ПРИМЕНЯЕМ ПАТЧ НЕБА =====
      AddToLogFile(EngineLog, 'Применяем патч неба (newsky: 1)');
      PatchDrawSkyCall;
    end
    else
    begin
      // ===== МОДУЛЬ ВЫКЛЮЧЕН - ВОССТАНАВЛИВАЕМ ОРИГИНАЛ =====
      AddToLogFile(EngineLog, 'Восстанавливаем оригинальный вызов неба (newsky: 0)');
      RestoreOriginalSkyCall;
    end;
      
    // Обновляем состояние
    LastNewSkyState := Config_NewSky;
    NewSkyInitialized := True;
  end;
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
      PInteger(Pointer($91D4DD0))^ := Config_MaxVisibleDistance;

      // Преобразование и запись float по адресу 00791DD8
      PatchValue := Config_MaxVisibleDistance;
      PatchAddress := Pointer($00791DD8);
      PatchAddress^ := PatchValue;

      // Патчим инструкцию
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

        // Восстанавливаем оригинальный адрес в инструкции
        InstructionAddress := Pointer($492258);

        if VirtualProtect(InstructionAddress, 6, PAGE_EXECUTE_READWRITE, @OldProtect) then
        begin
          InstructionAddress^ := $D8;
          Inc(InstructionAddress);
          InstructionAddress^ := $1D;
          Inc(InstructionAddress);
          PDWORD(InstructionAddress)^ := $00942CC; // восстанавливаем оригинальный адрес

          VirtualProtect(Pointer($492258), 6, OldProtect, @OldProtect);
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

procedure ProcessAllModules;
var
  currentTime: Cardinal;
begin
  currentTime := timeGetTime;
  
  // Обновляем конфиги по таймеру
  if (currentTime - LastBoosterConfigCheck) > BoosterConfigCheckInterval then
  begin
    LoadConfigFile; // Обновляем все конфиги из zdbooster.cfg
    LastBoosterConfigCheck := currentTime;
  end;
  
  // Всегда обрабатываем фрикам (ручное управление должно работать всегда)
  ProcessFreecam;
  
  // Обрабатываем модули в зависимости от их состояния
  ProcessStepForwardConfig;  // Обрабатывает включение/выключение автоматически
  ApplyMaxVisibleDistance;
  ProcessNewSkyPatch;   // Обрабатывает включение/выключение автоматически  
  //ProcessDayNightSystem;     // Обрабатывает включение/выключение автоматически
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
  case locType of
    812: Result := 'chs8';
    822: Result := 'chs7';
    882: Result := 'vl82';
    880: Result := 'vl80t';
    523: Result := 'chs4';
    524: Result := 'chs4kvr';
    621: Result := 'chs4t';
    2070: Result := 'tep70';
    2071: Result := 'tep70bs';
    1462: Result := 'm62';
    21014: Result := '2te10u';
    3154: Result := 'ed4m';
    3159: Result := 'ed9m';
    23152: Result := '2es5k';
    23142: Result := '2es4k';  // Предполагаю, что es4k = 2es4k
    343: Result := 'chs2k';
    31714: Result := 'ep1m';
    811: Result := 'vl11m';
    885: Result := 'vl85';
    201318: Result := 'tem18dm';
    else
      Result := 'chs7'; // по умолчанию
  end;
end;

function GetLocomotiveTypeFromSettings: Integer;
var
  f: TextFile;
  line, paramName, paramValue: string;
  equalPos: Integer;
  resultValue: Integer;
begin
  // Значение по умолчанию
  resultValue := 822;

  if FileExists('settings.ini') then
  begin
    try
      AssignFile(f, 'settings.ini');
      Reset(f);

      while not Eof(f) do
      begin
        ReadLn(f, line);
        line := Trim(line);

        // Пропускаем комментарии и пустые строки
        if (line = '') or (line[1] = '#') or (line[1] = ';') then
          Continue;

        equalPos := Pos('=', line);
        if equalPos > 0 then
        begin
          paramName := LowerCase(Trim(Copy(line, 1, equalPos - 1)));
          paramValue := Trim(Copy(line, equalPos + 1, Length(line)));

          if paramName = 'locomotivetype' then
          begin
            try
              resultValue := StrToInt(paramValue);
              Break;
            except
              // Ошибка преобразования — оставим значение по умолчанию
            end;
          end;
        end;
      end;

      CloseFile(f);
    except
      on E: Exception do
      begin
        try CloseFile(f); except end;
      end;
    end;
  end;

  Result := resultValue;
end;

function GetLocomotiveTypeFromMemory: Integer;
begin
  try
    // Читаем 4 байта (Integer) из памяти по адресу Launcher.exe+4F8D93C
    Result := PInteger(Pointer($00400000 + $4F8D93C))^;
  except
    // В случае ошибки используем значение по умолчанию
    Result := 822; // ЧС7 по умолчанию
  end;
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
  pantoPodstavkaPath, pantoSkiPath, pantoLever2BotPath, pantoLever2TopPath: string;
  pantoLever1BotPath, pantoLever1TopPath, pantoTexturePath: string;
  
  // Пути к новым моделям
  skorPrivod1Path, skorPrivod2Path: string;
  pisec1Path, pisec2Path: string;
  strelkaSkorPath, strelkaSkorTimePath: string;
  
  // Пути к LS моделям
  lsPath, lsBPath, lsKPath, lsKzhPath, lsZPath, lsZhPath: string;
  lsTexturePath: string;
  
  // Пути к KLUB_LS моделям
  klubLsPath, klubLsBPath, klubLsKPath, klubLsKzhPath, klubLsZPath, klubLsZhPath: string;
  
  // ID загруженных ресурсов
  KolparaModelID, KolparaTextureID: Integer;
  KlubModelID, KlubTextureID, KlubTexture2ID: Cardinal;
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
  if SettingsLoaded then Exit;
  
  // Читаем настройки
  LocNum := '068';
  LocomotiveType := 822;
  
  if FileExists('settings.ini') then
  begin
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
            LocNum := paramValue
          else if paramName = 'locomotivetype' then
            LocomotiveType := StrToInt(paramValue);
        end;
      end;
      CloseFile(f);
    except
      try CloseFile(f); except end;
    end;
  end;
  
  // Загружаем индикаторы
  if KlubBilIndPModelID = 0 then
    KlubBilIndPModelID := LoadModel('booster\klub-bil-ind_p.dmd', 0, False);
  if KlubBilIndBModelID = 0 then
    KlubBilIndBModelID := LoadModel('booster\klub-bil-ind_b.dmd', 0, False);
  
  // Определяем тип локомотива и оффсеты
  currentLocType := GetLocomotiveTypeFromMemory;
  locFolder := GetLocomotiveFolder(LocomotiveType);
  directoryPath := 'data\' + locFolder + '\' + LocNum + '\';

  // Проверяем существование кастомной папки
  hasCustomLoc := DirectoryExists(directoryPath + 'loc');
  if not hasCustomLoc then
  begin
    SettingsLoaded := True;
    Exit;
  end;
  
  // Вычисляем оффсеты в зависимости от типа локомотива
  case currentLocType of
    23152, 31714, 201318: begin // 2ЭС5К, ЭП1М - специальные оффсеты
      klubBilOffset := $5A;        // 0BA246F2-0BA24698 = $5A
      
      // Пантографы для 2ЭС5К/ЭП1М
      pantoPodstavkaOffset := $38;
      pantoSkiOffset := $3A;
      pantoLever2BotOffset := $3C;
      pantoLever2TopOffset := $3E;
      pantoLever1BotOffset := $40;
      pantoLever1TopOffset := $42;
      
      // Новые модели для 2ЭС5К/ЭП1М
      skorPrivod1Offset := $0E;
      skorPrivod2Offset := $10;
      pisec1Offset := $0A;
      pisec2Offset := $0C;
      strelkaSkorOffset := $06;
      strelkaSkorTimeOffset := $08;
      
      // LS оффсеты для 2ЭС5К/ЭП1М
      lsOffset := $44;
      lsBOffset := $1A-6;
      lsKOffset := $1A-$4;
      lsKzhOffset := $1A-$2;
      lsZOffset := $1A+$2;
      lsZhOffset := $1A;
      
      // KLUB_LS оффсеты для 2ЭС5К/ЭП1М
      klubLsOffset := $44;
      klubLsBOffset := $1A-6;
      klubLsKOffset := $1A-$4;
      klubLsKzhOffset := $1A-$2;
      klubLsZOffset := $1A+$2;
      klubLsZhOffset := $1A;
      
      // Оффсеты текстур для 2ЭС5К/ЭП1М
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $34;
      klubBil2TextureOffset := $36; // TODO: уточнить
      pantoTextureOffset := $3C;
      lsTextureOffset := $38; // TODO: уточнить
    end;

811, 882: begin
      klubBilOffset := $04;        // оригинальный
      
      // Пантографы для ВЛ11М: 0BA046C0-0BA04698 = $28
      pantoPodstavkaOffset := $28;   // tp_podstavka.dmd
      pantoSkiOffset := $2A;         // tp_ski.dmd (+$2)
      pantoLever2BotOffset := $2C;   // l13u_lever1bot.dmd
      pantoLever2TopOffset := $2E;   // l13u_lever1top.dmd
      pantoLever1BotOffset := $30;   // l13u_lever2bot.dmd
      pantoLever1TopOffset := $32;   // l13u_lever2top.dmd
      
      // Новые модели - оффсеты для ВЛ11М (пропишете сами)
      skorPrivod1Offset := $0E;
      skorPrivod2Offset := $10;
      pisec1Offset := $0A;
      pisec2Offset := $0C;
      strelkaSkorOffset := $06;
      strelkaSkorTimeOffset := $08;
      
      // LS оффсеты для остальных
      lsOffset := $12;
      lsBOffset := $1C-8;
      lsKOffset := $1C-6;
      lsKzhOffset := $1C-4;
      lsZOffset := $1C;
      lsZhOffset := $1C-2;
      
      // KLUB_LS оффсеты для остальных
      klubLsOffset := $12;
      klubLsBOffset := $1C-8;
      klubLsKOffset := $1C-6;
      klubLsKzhOffset := $1C-4;
      klubLsZOffset := $1C;
      klubLsZhOffset := $1C-2;

      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $34;
      klubBil2TextureOffset := $00; // TODO: уточнить
      pantoTextureOffset := $3C;

      // Специальное значение для 811
//      if currentLocType = 882 then
//        lsTextureOffset := $36
//      else
//        lsTextureOffset := $38; // TODO: уточнить

      lsTextureOffset := $36;

    end;
    880, 885: begin // Остальные ВЛ локомотивы
      klubBilOffset := $04;        // оригинальный
      
      // Пантографы для остальных ВЛ
      pantoPodstavkaOffset := $38;
      pantoSkiOffset := $3A;
      pantoLever2BotOffset := $3C;
      pantoLever2TopOffset := $3E;
      pantoLever1BotOffset := $40;
      pantoLever1TopOffset := $42;
      
      // Новые модели - оффсеты для остальных ВЛ (пропишете сами)
      skorPrivod1Offset := $0E;
      skorPrivod2Offset := $10;
      pisec1Offset := $0A;
      pisec2Offset := $0C;
      strelkaSkorOffset := $06;
      strelkaSkorTimeOffset := $08;

      // LS оффсеты для остальных
      lsOffset := $12;
      lsBOffset := $1C-8;
      lsKOffset := $1C-6;
      lsKzhOffset := $1C-4;
      lsZOffset := $1C;
      lsZhOffset := $1C-2;
      
      // KLUB_LS оффсеты для остальных
      klubLsOffset := $12;
      klubLsBOffset := $1C-8;
      klubLsKOffset := $1C-6;
      klubLsKzhOffset := $1C-4;
      klubLsZOffset := $1C;
      klubLsZhOffset := $1C-2;

      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $34;
      klubBil2TextureOffset := $00; // TODO: уточнить
      pantoTextureOffset := $3C;
      lsTextureOffset := $38; // TODO: уточнить

      if currentLocType = 880 then
        lsTextureOffset := $36;
      


    end;

    else begin // Остальные локомотивы (ЧС2К и др)
      klubBilOffset := $04;        // оригинальный
      
      // Пантографы для остальных
      pantoPodstavkaOffset := $28;
      pantoSkiOffset := $2A;
      pantoLever2BotOffset := $2C;
      pantoLever2TopOffset := $2E;
      pantoLever1BotOffset := $30;
      pantoLever1TopOffset := $32;
      
      // Новые модели для остальных
      skorPrivod1Offset := $0E;
      skorPrivod2Offset := $10;
      pisec1Offset := $0A;
      pisec2Offset := $0C;
      strelkaSkorOffset := $06;
      strelkaSkorTimeOffset := $08;
      
      // LS оффсеты для остальных
      lsOffset := $12;
      lsBOffset := $1C-8;
      lsKOffset := $1C-6;
      lsKzhOffset := $1C-4;
      lsZOffset := $1C;
      lsZhOffset := $1C-2;
      
      // KLUB_LS оффсеты для остальных
      klubLsOffset := $12;
      klubLsBOffset := $1C-8;
      klubLsKOffset := $1C-6;
      klubLsKzhOffset := $1C-4;
      klubLsZOffset := $1C;
      klubLsZhOffset := $1C-2;

      // Оффсеты текстур для остальных
      kolparaTextureOffset := $3E;
      klubBilTextureOffset := $34;
      klubBil2TextureOffset := $00; // TODO: уточнить
      pantoTextureOffset := $3C;
      lsTextureOffset := $36; // TODO: уточнить
    end;
  end;
  
  try
    klubFlag := PByte(Pointer($7498A8))^;
  except
    klubFlag := 1;
  end;
  
  baseStructAddr := PCardinal(Pointer($00400000 + $8D10D70))^;
  
  // ===== ЗАГРУЖАЕМ КОЛПАРА =====
  kolparaModelPath := directoryPath + 'loc\kolpara_el.dmd';
  kolparaTexturePath := directoryPath + 'loc\kolpara_el.bmp';
  
  if not (FileExists(kolparaModelPath) and FileExists(kolparaTexturePath)) then
  begin
    kolparaModelPath := directoryPath + 'loc\kolpara_tepl.dmd';
    kolparaTexturePath := directoryPath + 'loc\kolpara_tepl.bmp';
  end;
  
  if FileExists(kolparaModelPath) and FileExists(kolparaTexturePath) then
  begin
    try
      KolparaModelID := LoadModel(kolparaModelPath, 0, False);
      KolparaTextureID := LoadTextureFromFile(kolparaTexturePath, 0, -1);
      
      if (KolparaModelID > 0) and (KolparaTextureID > 0) then
      begin
        // Колпара модель
        if (currentLocType = 23152) or (currentLocType = 31714) then
          modelAddr := Pointer(baseStructAddr + $68) // $34 * 2 для 2ЭС5К/ЭП1М
        else
          modelAddr := Pointer(baseStructAddr + $34); // оригинальный
          
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KolparaModelID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
        
        // Колпара текстура
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + kolparaTextureOffset);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(KolparaTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки загрузки колпара
    end;
  end;
  
// ===== ЗАГРУЖАЕМ КЛУБ =====
if klubFlag = 1 then
begin
  // Основной клуб - сначала пробуем стандартные файлы
  klubModelPath := directoryPath + 'loc\klub_bil_v.dmd';
  klubTexturePath := directoryPath + 'loc\klub_bil.bmp';
  klubTexture2Path := directoryPath + 'loc\klub_bil2.bmp';
  
  // Если стандартных файлов нет, пробуем альтернативные
  if not (FileExists(klubModelPath) and FileExists(klubTexturePath)) then
  begin
    klubModelPath := directoryPath + 'loc\klub_bil_v2.dmd';
    klubTexturePath := directoryPath + 'loc\klub_bil2.bmp';
  end;
  
  if FileExists(klubModelPath) and FileExists(klubTexturePath) then
  begin
    try
      KlubModelID := LoadModel(klubModelPath, 0, False);
      KlubTextureID := LoadTextureFromFile(klubTexturePath, 0, -1);
      
      if (KlubModelID > 0) and (KlubTextureID > 0) then
      begin
        // Клуб модель
        modelAddr := Pointer(baseStructAddr + klubBilOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubModelID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
        
        // Клуб текстура
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + klubBilTextureOffset);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(KlubTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки загрузки клуба
    end;
  end;
  
  // Вторая текстура клуба
  if FileExists(klubTexture2Path) then
  begin
    try
      KlubTexture2ID := LoadTextureFromFile(klubTexture2Path, 0, -1);
      if KlubTexture2ID > 0 then
      begin
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + klubBil2TextureOffset);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(KlubTexture2ID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки загрузки второй текстуры клуба
    end;
  end;
end;
  
  // ===== ЗАГРУЖАЕМ ПАНТОГРАФЫ =====
  pantoPodstavkaPath := directoryPath + 'loc\tp_podstavka.dmd';
  pantoSkiPath := directoryPath + 'loc\tp_ski.dmd';
  pantoLever2BotPath := directoryPath + 'loc\l13u_lever1bot.dmd';
  pantoLever2TopPath := directoryPath + 'loc\l13u_lever1top.dmd';
  pantoLever1BotPath := directoryPath + 'loc\l13u_lever2bot.dmd';
  pantoLever1TopPath := directoryPath + 'loc\l13u_lever2top.dmd';
  pantoTexturePath := directoryPath + 'loc\2sls1.bmp';
  
  // Fallback на tp_ файлы если l13u_ не найдены
  if not FileExists(pantoLever2BotPath) then
    pantoLever2BotPath := directoryPath + 'loc\tp_lever1bot.dmd';
  if not FileExists(pantoLever2TopPath) then
    pantoLever2TopPath := directoryPath + 'loc\tp_lever1top.dmd';
  if not FileExists(pantoLever1BotPath) then
    pantoLever1BotPath := directoryPath + 'loc\tp_lever2bot.dmd';
  if not FileExists(pantoLever1TopPath) then
    pantoLever1TopPath := directoryPath + 'loc\tp_lever2top.dmd';
  
  // Загружаем текстуру пантографа
  if FileExists(pantoTexturePath) then
  begin
    try
      PantoTextureID := LoadTextureFromFile(pantoTexturePath, 0, -1);
      if PantoTextureID > 0 then
      begin
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + $3C);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(PantoTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки загрузки текстуры пантографа
    end;
  end;
  
  // Загружаем все части пантографа
  if FileExists(pantoPodstavkaPath) then
  begin
    try
      PantoPodstavkaID := LoadModel(pantoPodstavkaPath, 0, False);
      if PantoPodstavkaID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + pantoPodstavkaOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoPodstavkaID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(pantoSkiPath) then
  begin
    try
      PantoSkiID := LoadModel(pantoSkiPath, 0, False);
      if PantoSkiID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + pantoSkiOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoSkiID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(pantoLever2BotPath) then
  begin
    try
      PantoLever2BotID := LoadModel(pantoLever2BotPath, 0, False);
      if PantoLever2BotID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + pantoLever2BotOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever2BotID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(pantoLever2TopPath) then
  begin
    try
      PantoLever2TopID := LoadModel(pantoLever2TopPath, 0, False);
      if PantoLever2TopID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + pantoLever2TopOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever2TopID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(pantoLever1BotPath) then
  begin
    try
      PantoLever1BotID := LoadModel(pantoLever1BotPath, 0, False);
      if PantoLever1BotID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + pantoLever1BotOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever1BotID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(pantoLever1TopPath) then
  begin
    try
      PantoLever1TopID := LoadModel(pantoLever1TopPath, 0, False);
      if PantoLever1TopID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + pantoLever1TopOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(PantoLever1TopID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // ===== ЗАГРУЖАЕМ НОВЫЕ МОДЕЛИ =====
  
  // Инициализируем пути к новым моделям
  skorPrivod1Path := directoryPath + 'loc\skor_privod1.dmd';
  skorPrivod2Path := directoryPath + 'loc\skor_privod2.dmd';
  pisec1Path := directoryPath + 'loc\pisec1.dmd';
  pisec2Path := directoryPath + 'loc\pisec2.dmd';
  strelkaSkorPath := directoryPath + 'loc\strelka_skor.dmd';
  strelkaSkorTimePath := directoryPath + 'loc\strelka_skor_time.dmd';
  
  // Загружаем skor_privod1.dmd
  if FileExists(skorPrivod1Path) then
  begin
    try
      SkorPrivod1ID := LoadModel(skorPrivod1Path, 0, False);
      if (SkorPrivod1ID > 0) and (skorPrivod1Offset <> $00) then
      begin
        modelAddr := Pointer(baseStructAddr + skorPrivod1Offset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(SkorPrivod1ID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // Загружаем skor_privod2.dmd
  if FileExists(skorPrivod2Path) then
  begin
    try
      SkorPrivod2ID := LoadModel(skorPrivod2Path, 0, False);
      if (SkorPrivod2ID > 0) and (skorPrivod2Offset <> $00) then
      begin
        modelAddr := Pointer(baseStructAddr + skorPrivod2Offset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(SkorPrivod2ID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // Загружаем pisec1.dmd
  if FileExists(pisec1Path) then
  begin
    try
      Pisec1ID := LoadModel(pisec1Path, 0, False);
      if (Pisec1ID > 0) and (pisec1Offset <> $00) then
      begin
        modelAddr := Pointer(baseStructAddr + pisec1Offset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(Pisec1ID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // Загружаем pisec2.dmd
  if FileExists(pisec2Path) then
  begin
    try
      Pisec2ID := LoadModel(pisec2Path, 0, False);
      if (Pisec2ID > 0) and (pisec2Offset <> $00) then
      begin
        modelAddr := Pointer(baseStructAddr + pisec2Offset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(Pisec2ID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // Загружаем strelka_skor.dmd
  if FileExists(strelkaSkorPath) then
  begin
    try
      StrelkaSkorID := LoadModel(strelkaSkorPath, 0, False);
      if (StrelkaSkorID > 0) and (strelkaSkorOffset <> $00) then
      begin
        modelAddr := Pointer(baseStructAddr + strelkaSkorOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(StrelkaSkorID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // Загружаем strelka_skor_time.dmd
  if FileExists(strelkaSkorTimePath) then
  begin
    try
      StrelkaSkorTimeID := LoadModel(strelkaSkorTimePath, 0, False);
      if (StrelkaSkorTimeID > 0) and (strelkaSkorTimeOffset <> $00) then
      begin
        modelAddr := Pointer(baseStructAddr + strelkaSkorTimeOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(StrelkaSkorTimeID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // ===== ИНИЦИАЛИЗИРУЕМ ПУТИ К LS МОДЕЛЯМ =====
  lsPath := directoryPath + 'loc\ls.dmd';
  lsBPath := directoryPath + 'loc\ls_b.dmd';
  lsKPath := directoryPath + 'loc\ls_k.dmd';
  lsKzhPath := directoryPath + 'loc\ls_kzh.dmd';
  lsZPath := directoryPath + 'loc\ls_z.dmd';
  lsZhPath := directoryPath + 'loc\ls_zh.dmd';
  lsTexturePath := directoryPath + 'loc\ls.bmp';
  
  // Загружаем текстуру LS отдельно
  if FileExists(lsTexturePath) then
  begin
    try
      LsTextureID := LoadTextureFromFile(lsTexturePath, 0, -1);
      if LsTextureID > 0 then
      begin
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + lsTextureOffset);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(LsTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки загрузки LS текстуры
    end;
  end;
  
  // ===== ИНИЦИАЛИЗИРУЕМ ПУТИ К KLUB_LS МОДЕЛЯМ =====
  klubLsPath := directoryPath + 'loc\klub_ls.dmd';
  klubLsBPath := directoryPath + 'loc\klub_ls_b.dmd';
  klubLsKPath := directoryPath + 'loc\klub_ls_k.dmd';
  klubLsKzhPath := directoryPath + 'loc\klub_ls_kzh.dmd';
  klubLsZPath := directoryPath + 'loc\klub_ls_z.dmd';
  klubLsZhPath := directoryPath + 'loc\klub_ls_zh.dmd';
  
  // ===== ЗАГРУЖАЕМ LS МОДЕЛИ =====
  if FileExists(lsPath) then
  begin
    try
      LsID := LoadModel(lsPath, 0, False);
      if LsID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + lsOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(lsBPath) then
  begin
    try
      LsBID := LoadModel(lsBPath, 0, False);
      if LsBID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + lsBOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsBID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(lsKPath) then
  begin
    try
      LsKID := LoadModel(lsKPath, 0, False);
      if LsKID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + lsKOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsKID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(lsKzhPath) then
  begin
    try
      LsKzhID := LoadModel(lsKzhPath, 0, False);
      if LsKzhID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + lsKzhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsKzhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(lsZPath) then
  begin
    try
      LsZID := LoadModel(lsZPath, 0, False);
      if LsZID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + lsZOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsZID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(lsZhPath) then
  begin
    try
      LsZhID := LoadModel(lsZhPath, 0, False);
      if LsZhID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + lsZhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(LsZhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  // ===== ЗАГРУЖАЕМ KLUB_LS МОДЕЛИ =====
  if FileExists(klubLsPath) then
  begin
    try
      KlubLsID := LoadModel(klubLsPath, 0, False);
      if KlubLsID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + klubLsOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(klubLsBPath) then
  begin
    try
      KlubLsBID := LoadModel(klubLsBPath, 0, False);
      if KlubLsBID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + klubLsBOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsBID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(klubLsKPath) then
  begin
    try
      KlubLsKID := LoadModel(klubLsKPath, 0, False);
      if KlubLsKID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + klubLsKOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsKID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(klubLsKzhPath) then
  begin
    try
      KlubLsKzhID := LoadModel(klubLsKzhPath, 0, False);
      if KlubLsKzhID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + klubLsKzhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsKzhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(klubLsZPath) then
  begin
    try
      KlubLsZID := LoadModel(klubLsZPath, 0, False);
      if KlubLsZID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + klubLsZOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsZID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  if FileExists(klubLsZhPath) then
  begin
    try
      KlubLsZhID := LoadModel(klubLsZhPath, 0, False);
      if KlubLsZhID > 0 then
      begin
        modelAddr := Pointer(baseStructAddr + klubLsZhOffset);
        if VirtualProtect(modelAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(modelAddr)^ := Word(KlubLsZhID);
          VirtualProtect(modelAddr, SizeOf(Word), OldProtect, OldProtect);
        end;
      end;
    except
      // Игнорируем ошибки
    end;
  end;
  
  SettingsLoaded := True;
end;

function GetLocoPatchOffset(locType: Integer): Cardinal;
begin
  case locType of
    812: Result := $7245D8;   // ЧС8
    822: Result := $7245EF;   // ЧС7 - работает
    882: Result := $724606;   // ВЛ82М
    880: Result := $72461D;   // ВЛ80Т
    621: Result := $724634;   // ЧС4Т
    523: Result := $724643;   // ЧС4
    524: Result := $724662;   // ЧС4КВР
    2070: Result := $724679;  // ТЭП70
    2071: Result := $724690;  // ТЭП70БС
    1462: Result := $7246A7;  // М62
    21014: Result := $7246BE; // 2ТЭ10У
    3154: Result := $7246D5;  // ЭД4М
    3159: Result := $7246EC;  // ЭД9М
    23152: Result := $724703; // 2ЭС5К
    23142: Result := $724717; // 2ЭС4К
    343: Result := $72472B;   // ЧС2К
    31714: Result := $72473F; // ЭП1М
    811: Result := $724753;   // ВЛ11М
    885: Result := $724767;   // ВЛ85
    201318: Result := $72477B; // ТЭМ18ДМ
    else
      Result := $3246D5; // По умолчанию для ED4M
  end;
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
function ReadMemorySingle(Address: Cardinal): Single;
begin
  try
    Result := PSingle(Pointer(Address))^;
  except
    Result := 0.0;
  end;
end;

// Функция для записи Single в память
procedure WriteMemorySingle(Address: Cardinal; Value: Single);
begin
  try
    PSingle(Pointer(Address))^ := Value;
  except
    // Игнорируем ошибки записи
  end;
end;

// Функция для записи Double в память
procedure WriteMemoryDouble(Address: Cardinal; Value: Double);
begin
  try
    PDouble(Pointer(Address))^ := Value;
  except
    // Игнорируем ошибки записи
  end;
end;



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
  F: TextFile;
  HookAddr: Cardinal;
  CallAddress: Cardinal;
  NewOffset: Integer;
  OldProtect: DWORD;
  i: Integer;
  StartTime: Cardinal;


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
//        Sleep(1); // Небольшая пауза между попытками

        // Если больше 10 попыток или прошло больше секунды - выходим
        if (Attempts > 10) or ((GetTickCount - AttemptStart) > 1000) then
          Break;
          
      except
        // Игнорируем исключения и пробуем еще раз
        Inc(Attempts);
        if Attempts > 5 then Break;
      end;
    until False;
  end;

  function IsTimeoutExceeded: Boolean;
  begin
    Result := (GetTickCount - StartTime) > 3000; // 3 секунды тайм-аут
  end;

begin
  StartTime := GetTickCount;



  try
    // Быстрое получение адреса
    try
      HookAddr := Cardinal(@HookKLUB);
    except
      Exit; // Если не можем получить адрес, выходим
    end;
    
    // Быстрая запись в файл без излишних проверок
    try
      AssignFile(F, 'hook.txt');
      Rewrite(F);
      WriteLn(F, IntToStr(HookAddr));
      CloseFile(F);
    except
      // Если не можем записать в файл, продолжаем без него
    end;
    
    if IsTimeoutExceeded then Exit;
    
    // Основная операция патчинга с тайм-аутом
    try
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
      // Игнорируем ошибки основного патчинга
    end;
    
    if IsTimeoutExceeded then Exit;
    
    // Быстрое применение всех патчей с проверкой тайм-аута
    try
      // Скорость X
      if not IsTimeoutExceeded and SafeVirtualProtect(Pointer(SpeedXAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        try
          for i := 0 to 3 do
            PByte(SpeedXAddr + i)^ := NewSpeedXValue[i];
          SafeVirtualProtect(Pointer(SpeedXAddr), 4, OldProtect, OldProtect);
        except
          // Игнорируем ошибки
        end;
      end;
      
      // Допустимая скорость
      if not IsTimeoutExceeded and SafeVirtualProtect(Pointer(AllowedSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        try
          for i := 0 to 3 do
            PByte(AllowedSpeedAddr + i)^ := NewAllowedSpeedValue[i];
          SafeVirtualProtect(Pointer(AllowedSpeedAddr), 4, OldProtect, OldProtect);
        except
          // Игнорируем ошибки
        end;
      end;
      
      // Остальные патчи применяем аналогично, но с проверкой тайм-аута
      if not IsTimeoutExceeded then
      begin
        // Маневровый режим
        if SafeVirtualProtect(Pointer(ShuntingSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            for i := 0 to 3 do
              PByte(ShuntingSpeedAddr + i)^ := NewShuntingSpeedValue[i];
            SafeVirtualProtect(Pointer(ShuntingSpeedAddr), 4, OldProtect, OldProtect);
          except
            // Игнорируем ошибки
          end;
        end;
        
        // Поездной режим
        if SafeVirtualProtect(Pointer(TrainSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            for i := 0 to 3 do
              PByte(TrainSpeedAddr + i)^ := NewTrainSpeedValue[i];
            SafeVirtualProtect(Pointer(TrainSpeedAddr), 4, OldProtect, OldProtect);
          except
            // Игнорируем ошибки
          end;
        end;
        
        // Время
        if SafeVirtualProtect(Pointer(TimeAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            for i := 0 to 3 do
              PByte(TimeAddr + i)^ := NewTimeValue[i];
            SafeVirtualProtect(Pointer(TimeAddr), 4, OldProtect, OldProtect);
          except
            // Игнорируем ошибки
          end;
        end;
        
        // Номер и ускорение
        if SafeVirtualProtect(Pointer(NumberAccelAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            for i := 0 to 3 do
              PByte(NumberAccelAddr + i)^ := NewNumberAccelValue[i];
            SafeVirtualProtect(Pointer(NumberAccelAddr), 4, OldProtect, OldProtect);
          except
            // Игнорируем ошибки
          end;
        end;
        
        // Реверс
        if SafeVirtualProtect(Pointer(ReverseAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            for i := 0 to 3 do
              PByte(ReverseAddr + i)^ := NewReverseValue[i];
            SafeVirtualProtect(Pointer(ReverseAddr), 4, OldProtect, OldProtect);
          except
            // Игнорируем ошибки
          end;
        end;
        
        // Дополнительный параметр
        if SafeVirtualProtect(Pointer(AdditionalAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            for i := 0 to 3 do
              PByte(AdditionalAddr + i)^ := NewAdditionalValue[i];
            SafeVirtualProtect(Pointer(AdditionalAddr), 4, OldProtect, OldProtect);
          except
            // Игнорируем ошибки
          end;
        end;
        
        // Обнуляем массив радиуса
        if SafeVirtualProtect(Pointer(RadiusAddr), 10, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          try
            for i := 0 to 9 do
              PByte(RadiusAddr + i)^ := $00;
            SafeVirtualProtect(Pointer(RadiusAddr), 10, OldProtect, OldProtect);
          except
            // Игнорируем ошибки
          end;
        end;
      end;
      
    except
      // Игнорируем любые ошибки в патчинге
    end;
    
  except
    on E: Exception do
    begin
      // Записываем ошибку в лог только если это возможно
      try
        //AddToLogFile(EngineLog, 'WriteHookAddress error: ' + E.Message);
      except
        // Если даже запись в лог не работает, просто выходим
      end;
    end;
  end;

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
   SetLength(Obj3DInfo,length(Obj3DInfo)+1);
   Obj3DInfo[length(Obj3DInfo)-1].Texture:=curTexture;
   Obj3DInfo[length(Obj3DInfo)-1].Projecting:=Projecting;
   glGetFloatv(GL_CURRENT_COLOR, @Obj3DInfo[length(Obj3DInfo)-1].Color);

  if Projecting then
  glEnable(GL_BLEND);

  glPushMatrix();
end;
{------------------------------------------------------------------}
procedure EndObj3D; stdcall;
begin
  glPopMatrix();
  glcolor4f(Obj3DInfo[length(Obj3DInfo)-1].Color[1],Obj3DInfo[length(Obj3DInfo)-1].Color[2],Obj3DInfo[length(Obj3DInfo)-1].Color[3],Obj3DInfo[length(Obj3DInfo)-1].Color[4]);
  SetTexture(Obj3DInfo[length(Obj3DInfo)-1].Texture);
  Projecting:=Obj3DInfo[length(Obj3DInfo)-1].Projecting;
  SetLength(Obj3DInfo,length(Obj3DInfo)-1);
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
begin
 fogColor[0]:=GetRValue(Color)/255;
 fogColor[1]:=GetGValue(Color)/255;
 fogColor[2]:=GetBValue(Color)/255;
 fogColor[3]:=1.0;
 glEnable(GL_FOG);
 glFogi  (GL_FOG_MODE, GL_LINEAR);
 glHint  (GL_FOG_HINT, GL_DONT_CARE);
 glFogf  (GL_FOG_START, Fog_Start);
 glFogf  (GL_FOG_END, Fog_End);
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
  try
    if PByte(Pointer($00400000 + $349968))^ = 1 then
      Result := 'ЗИМА'
    else
      Result := 'ЛЕТО';
  except
    Result := 'НЕИЗВЕСТНО';
  end;
end;

function IsNightTime: Boolean;
var
  hour: Integer;
  season: Byte;
begin
  hour := GetCurrentHour;
  
  try
    // Читаем сезон: 0 = лето, 1 = зима
    season := PByte(Pointer($00400000 + $349968))^;
    
    if season = 1 then
    begin
      // ЗИМА: ночь с 18:00 до 8:00
      Result := (hour >= 18) or (hour <= 7);
    end
    else
    begin
      // ЛЕТО: ночь с 21:00 до 5:00  
      Result := (hour >= 21) or (hour <= 4);
    end;
    
  except
    // При ошибке используем летнее время
    Result := (hour >= 21) or (hour <= 4);
  end;
end;

function FindTextureFileInFolder(folderPath: string): string;
var
  searchRec: TSearchRec;
  fileName: string;
begin
  Result := '';
  
  // Ищем файл, содержащий "254" и "395" в имени
  if FindFirst(folderPath + '\*254*395*.bmp', faAnyFile, searchRec) = 0 then
  begin
    Result := folderPath + '\' + searchRec.Name;
    FindClose(searchRec);
    Exit;
  end;
  FindClose(searchRec);
  
  // Если не найден точный, ищем любой файл с "254"
  if FindFirst(folderPath + '\*254*.bmp', faAnyFile, searchRec) = 0 then
  begin
    Result := folderPath + '\' + searchRec.Name;
    FindClose(searchRec);
    Exit;
  end;
  FindClose(searchRec);
  
  // Если не найден и такой, ищем любой файл с "395"
  if FindFirst(folderPath + '\*395*.bmp', faAnyFile, searchRec) = 0 then
  begin
    Result := folderPath + '\' + searchRec.Name;
    FindClose(searchRec);
  end;
  FindClose(searchRec);
end;

// Замените блок загрузки ночной klub текстуры в InitializeDayNightSystem:



procedure InitializeDayNightSystem;
var
  locFolder: string;
  directoryPath: string;
  dayFolderPath, nightFolderPath: string;
  cabDayPath, pultDayPath, day254Path, klubDayPath: string;
  cabNightPath, pultNightPath, night254Path, klubNightPath: string;
begin
  if DayNightInitialized then Exit;
  
  try
    // Определяем папку локомотива
    locFolder := GetLocomotiveFolder(GetLocomotiveTypeFromMemory);
    directoryPath := 'data\' + locFolder + '\' + LocNum + '\';
    
    AddToLogFile(EngineLog, '=== ДИАГНОСТИКА СИСТЕМЫ ДЕНЬ/НОЧЬ ===');
    AddToLogFile(EngineLog, 'LocNum: ' + LocNum);
    AddToLogFile(EngineLog, 'locFolder: ' + locFolder);
    AddToLogFile(EngineLog, 'directoryPath: ' + directoryPath);
    
    dayFolderPath := directoryPath;
    nightFolderPath := directoryPath + 'night';
    
    AddToLogFile(EngineLog, 'dayFolderPath: ' + dayFolderPath);
    AddToLogFile(EngineLog, 'nightFolderPath: ' + nightFolderPath);
    AddToLogFile(EngineLog, 'day folder exists: ' + BoolToStr(DirectoryExists(dayFolderPath), True));
    AddToLogFile(EngineLog, 'night folder exists: ' + BoolToStr(DirectoryExists(nightFolderPath), True));
    
    // Проверяем наличие папок day и night
    HasDayNightFolders := DirectoryExists(dayFolderPath) and DirectoryExists(nightFolderPath);
    
    if HasDayNightFolders then
    begin
      AddToLogFile(EngineLog, 'Найдены папки day/night, инициализируем систему смены текстур');
      
      // Формируем пути к файлам дня
      cabDayPath := dayFolderPath + '\cab.bmp';
      pultDayPath := dayFolderPath + '\pult.bmp';
      day254Path := FindTextureFileInFolder(dayFolderPath);
      klubDayPath := dayFolderPath + '\klub_bil.bmp';
      
      // Формируем пути к файлам ночи
      cabNightPath := nightFolderPath + '\cab.bmp';
      pultNightPath := nightFolderPath + '\pult.bmp';
      night254Path := FindTextureFileInFolder(nightFolderPath);
      klubNightPath := nightFolderPath + '\klub_bil.bmp';
      
      // === ДЕТАЛЬНАЯ ДИАГНОСТИКА KLUB ФАЙЛОВ ===
      AddToLogFile(EngineLog, '=== ДИАГНОСТИКА KLUB ФАЙЛОВ ===');
      AddToLogFile(EngineLog, 'klubDayPath: ' + klubDayPath);
      AddToLogFile(EngineLog, 'klubNightPath: ' + klubNightPath);
      AddToLogFile(EngineLog, 'Day klub file exists: ' + BoolToStr(FileExists(klubDayPath), True));
      AddToLogFile(EngineLog, 'Night klub file exists: ' + BoolToStr(FileExists(klubNightPath), True));
      
      // Проверяем размер файлов
      if FileExists(klubDayPath) then
      begin
        try
          //AddToLogFile(EngineLog, 'Day klub file size: ' + IntToStr(GetFileSize(klubDayPath)) + ' bytes');
        except
          AddToLogFile(EngineLog, 'Ошибка получения размера day klub файла');
        end;
      end;
      
      if FileExists(klubNightPath) then
      begin
        try
          //AddToLogFile(EngineLog, 'Night klub file size: ' + IntToStr(GetFileSize(klubNightPath)) + ' bytes');
        except
          AddToLogFile(EngineLog, 'Ошибка получения размера night klub файла');
        end;
      end;
      
      // Загружаем дневную klub текстуру
      if FileExists(klubDayPath) then
      begin
        try
          AddToLogFile(EngineLog, 'Попытка загрузки дневной klub текстуры...');
          DayKlubTextureID := LoadTextureFromFile(klubDayPath, 0, -1);
          if DayKlubTextureID > 0 then
            AddToLogFile(EngineLog, 'УСПЕШНО: Загружена дневная текстура klub: ' + IntToStr(DayKlubTextureID))
          else
            AddToLogFile(EngineLog, 'ОШИБКА: LoadTextureFromFile вернул 0 для дневной klub текстуры');
        except
          on E: Exception do
            AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке дневной klub текстуры: ' + E.Message);
        end;
      end
      else
      begin
        AddToLogFile(EngineLog, 'ФАЙЛ НЕ НАЙДЕН: ' + klubDayPath);
      end;

      // Загружаем ночную klub текстуру  
      if FileExists(klubNightPath) then
      begin
        try
          AddToLogFile(EngineLog, 'Попытка загрузки ночной klub текстуры...');
          NightKlubTextureID := LoadTextureFromFile(klubNightPath, 0, -1);
          if NightKlubTextureID > 0 then
            AddToLogFile(EngineLog, 'УСПЕШНО: Загружена ночная текстура klub: ' + IntToStr(NightKlubTextureID))
          else
            AddToLogFile(EngineLog, 'ОШИБКА: LoadTextureFromFile вернул 0 для ночной klub текстуры');
        except
          on E: Exception do
            AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ночной klub текстуры: ' + E.Message);
        end;
      end
      else
      begin
        AddToLogFile(EngineLog, 'ФАЙЛ НЕ НАЙДЕН: ' + klubNightPath);
      end;
      
      // Загружаем текстуры дня
      if FileExists(cabDayPath) then
      begin
        DayCabTextureID := LoadTextureFromFile(cabDayPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена дневная текстура cab: ' + IntToStr(DayCabTextureID));
      end;
      
      if FileExists(pultDayPath) then
      begin
        DayPultTextureID := LoadTextureFromFile(pultDayPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена дневная текстура pult: ' + IntToStr(DayPultTextureID));
      end;
      
      if day254Path <> '' then
      begin
        Day254TextureID := LoadTextureFromFile(day254Path, 0, -1);
        AddToLogFile(EngineLog, 'Загружена дневная текстура 254: ' + IntToStr(Day254TextureID));
      end;


      if FileExists(cabNightPath) then
      begin
        NightCabTextureID := LoadTextureFromFile(cabNightPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена ночная текстура cab: ' + IntToStr(NightCabTextureID));
      end;
      
      if FileExists(pultNightPath) then
      begin
        NightPultTextureID := LoadTextureFromFile(pultNightPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена ночная текстура pult: ' + IntToStr(NightPultTextureID));
      end;
      
      if night254Path <> '' then
      begin
        Night254TextureID := LoadTextureFromFile(night254Path, 0, -1);
        AddToLogFile(EngineLog, 'Загружена ночная текстура 254: ' + IntToStr(Night254TextureID));
      end;
      
      AddToLogFile(EngineLog, '=== ИТОГИ ИНИЦИАЛИЗАЦИИ ===');
      AddToLogFile(EngineLog, 'DayKlubTextureID: ' + IntToStr(DayKlubTextureID));
      AddToLogFile(EngineLog, 'NightKlubTextureID: ' + IntToStr(NightKlubTextureID));
      AddToLogFile(EngineLog, 'Система день/ночь инициализирована успешно');
    end
    else
    begin
      AddToLogFile(EngineLog, 'Папки day/night не найдены, система день/ночь отключена');
    end;
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'КРИТИЧЕСКАЯ ОШИБКА инициализации системы день/ночь: ' + E.Message);
      HasDayNightFolders := False;
    end;
  end;
  
  DayNightInitialized := True;
end;

// Вспомогательная функция для получения размера файла:
function GetFileSize(const FileName: string): Int64;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := -1;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindData.nFileSizeLow;
    Windows.FindClose(Handle);
  end;
end;


procedure ApplyDayNightTextures;
var
  isNight: Boolean;
  newTimeMode: Integer;
  textureAddr: Pointer;
  cabTextureID, pultTextureID, texture254ID, klubTextureID: Cardinal; // ← Добавили klubTextureID
  OldProtect: DWORD;
begin
  if not HasDayNightFolders then Exit;
  
  try
    // Определяем текущий режим времени
    isNight := IsNightTime;
    newTimeMode := Integer(isNight);
    
    // Если режим не изменился, выходим
    if newTimeMode = CurrentTimeMode then Exit;
    
    CurrentTimeMode := newTimeMode;
    CurrentIsNight := isNight;
    
    if isNight then
    begin
      AddToLogFile(EngineLog, 'Переключение на ночные текстуры (' + GetCurrentSeason + ', час: ' + IntToStr(GetCurrentHour) + ')');
      cabTextureID := NightCabTextureID;
      pultTextureID := NightPultTextureID;
      texture254ID := Night254TextureID;
      klubTextureID := NightKlubTextureID; // ← Добавили
    end
    else
    begin
      AddToLogFile(EngineLog, 'Переключение на дневные текстуры (' + GetCurrentSeason + ', час: ' + IntToStr(GetCurrentHour) + ')');
      cabTextureID := DayCabTextureID;
      pultTextureID := DayPultTextureID;
      texture254ID := Day254TextureID;
      klubTextureID := DayKlubTextureID; // ← Добавили
    end;
    
    // Применяем cab.bmp → pointer 91D427C [0x06]
    if cabTextureID > 0 then
    begin
      try
        textureAddr := Pointer(PCardinal(Pointer($91D427C))^ + $06);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(cabTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена cab текстура ID: ' + IntToStr(cabTextureID));
        end;
      except
        AddToLogFile(EngineLog, 'Ошибка установки cab текстуры');
      end;
    end;
    
    // Применяем pult.bmp → pointer 91D427C [0x08]
    if pultTextureID > 0 then
    begin
      try
        textureAddr := Pointer(PCardinal(Pointer($91D427C))^ + $08);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(pultTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена pult текстура ID: ' + IntToStr(pultTextureID));
        end;
      except
        AddToLogFile(EngineLog, 'Ошибка установки pult текстуры');
      end;
    end;
    
    // Применяем *254*395.bmp → pointer 9110D60 [0x38]
    if texture254ID > 0 then
    begin
      try
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + $38);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(texture254ID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена 254 текстура ID: ' + IntToStr(texture254ID));
        end;
      except
        AddToLogFile(EngineLog, 'Ошибка установки 254 текстуры');
      end;
    end;
    
    // ← НОВЫЙ БЛОК: Применяем klub_bil.bmp → pointer 9110D60 [0x34]
   if klubTextureID > 0 then
    begin
      try
        // Записываем по тому же адресу, что в LoadSettingsAndCustomModels
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + $34);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(klubTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена klub текстура ID: ' + IntToStr(klubTextureID) + ' по адресу: ' + IntToHex(Cardinal(textureAddr), 8));
        end;
        
      except
        AddToLogFile(EngineLog, 'Ошибка установки klub текстуры');
      end;
    end;
    
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'Ошибка применения текстур день/ночь: ' + E.Message);
  end;
end;



// Замените ProcessDayNightSystem на эту версию с диагностикой:

procedure ProcessDayNightSystem;
var
  currentTime: Cardinal;
  StateChanged: Boolean;
begin
  currentTime := timeGetTime;
  
  // Проверяем изменилось ли состояние модуля
  StateChanged := not NewSkyInitialized or (Config_NewSky <> LastNewSkyState);
  
  if StateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния NewSky: %s -> %s',
      [BoolToStr(LastNewSkyState, True), BoolToStr(Config_NewSky, True)]));
      
    // Обновляем состояние сразу
    LastNewSkyState := Config_NewSky;
    NewSkyInitialized := True;
  end;

  // Проверяем время только по таймеру
  if (currentTime - LastTimeCheck) > TimeCheckInterval1 then
  begin
    // Инициализируем систему при первом запуске (если включена)
    if not DayNightInitialized and Config_NewSky then
    begin
      AddToLogFile(EngineLog, 'Инициализируем систему день/ночь');
      InitializeDayNightSystem;
    end;
    
    // Применяем текстуры только если система включена
    if Config_NewSky and HasDayNightFolders then
    begin
      ApplyDayNightTextures;
    end
    else if StateChanged and not Config_NewSky then
    begin
      AddToLogFile(EngineLog, 'Система день/ночь отключена в конфиге');
      // Здесь можно добавить восстановление оригинальных текстур если нужно
    end;
    
    LastTimeCheck := currentTime;
  end;
end;



function WriteAndVerify(Address: Cardinal; Value: Integer; MaxAttempts: Integer = 10): Boolean;
var
  Addr: Pointer;
  OldProtect: DWORD;
  CurrentValue: Integer;
  Attempt: Integer;
begin
  Result := False;
  Addr := Pointer(Address);
  Attempt := 0;
  
  while (Attempt < MaxAttempts) do
  begin
    Inc(Attempt);
    
    try
      // Записываем значение
      if VirtualProtect(Addr, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PInteger(Addr)^ := Value;
        VirtualProtect(Addr, SizeOf(Integer), OldProtect, OldProtect);
        
        // Проверяем что записалось
        CurrentValue := PInteger(Addr)^;
        
        if CurrentValue = Value then
        begin
          Result := True;
          Break;
        end
        else
        begin
        end;
      end
      else
      begin
      end;
      
    except
      on E: Exception do
        //AddToLogFile(EngineLog, Format('Исключение на попытке %d: %s', [Attempt, E.Message]));
    end;

    // Небольшая задержка между попытками
    Sleep(1);
  end;

end;


// ===== ФУНКЦИЯ КОНВЕРТАЦИИ SINGLE В 80-BIT EXTENDED =====
function SingleToExtended80(value: Single): TExtendedBytes;
var
  extValue: Extended;
  extBytes: TExtendedBytes;
  i: Integer;
begin
  extValue := value;
  Move(extValue, extBytes, 10);
  Result := extBytes;
end;

// ===== ФУНКЦИЯ ЗАПИСИ ЗНАЧЕНИЯ ПО АДРЕСУ =====
procedure WriteStepForwardToMemory(value: Single);
const
  TARGET_ADDRESS = $00725C24;
var
  extBytes: TExtendedBytes;
  OldProtect: DWORD;
  i: Integer;
begin
  try
    // Конвертируем Single в 80-bit Extended
    extBytes := SingleToExtended80(value);
    
    // Снимаем защиту памяти
    if VirtualProtect(Pointer(TARGET_ADDRESS), 10, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      // Записываем 10 байтов
      for i := 0 to 9 do
        PByte(TARGET_ADDRESS + i)^ := extBytes[i];
      
      // Восстанавливаем защиту
      VirtualProtect(Pointer(TARGET_ADDRESS), 10, OldProtect, OldProtect);
      
      AddToLogFile(EngineLog, Format('stepForward записан в память: %.6f', [value]));
    end
    else
    begin
      AddToLogFile(EngineLog, 'Ошибка изменения защиты памяти для stepForward');
    end;
    
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'Ошибка записи stepForward в память: ' + E.Message);
  end;
end;

// ===== ОБНОВЛЕННАЯ ФУНКЦИЯ LoadConfigFile =====
procedure LoadConfigFile;
var
  f: TextFile;
  line: string;
  paramName, paramValue: string;
  colonPos: Integer;
  newStepForward: Single;
begin
  // Инициализация значений по умолчанию только при первом запуске
  if LastStepForwardCheck = 0 then
  begin
    // Флаги систем
    Config_Freecam := True;
    Config_MainCamera := True;
    Config_MaxDistance := True;
    Config_NewSky := True;
    
    // Параметры фрикама
    Config_BaseSpeed := 0.5;
    Config_FastSpeed := 1.0;
    Config_TurnSpeed := 1.5;
    
    // Параметры основной камеры
    Config_StepForward := 0.1;
    
    // Параметры дистанции
    Config_MaxVisibleDistance := 1200;
    
    LastStepForwardCheck := 0;
    StepForwardCheckInterval := 1000;
  end;
  
  newStepForward := Config_StepForward;
  
  if FileExists('zdbooster.cfg') then
  begin
    try
      AssignFile(f, 'zdbooster.cfg');
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
          
          try
            // ===== ФЛАГИ ВКЛЮЧЕНИЯ/ВЫКЛЮЧЕНИЯ =====
            if paramName = 'freecam' then
              Config_Freecam := (paramValue = '1') or (LowerCase(paramValue) = 'true')
            else if paramName = 'main_camera' then
              Config_MainCamera := (paramValue = '1') or (LowerCase(paramValue) = 'true')
            else if paramName = 'max_distance' then
              Config_MaxDistance := (paramValue = '1') or (LowerCase(paramValue) = 'true')
            else if paramName = 'newsky' then
              Config_NewSky := (paramValue = '1') or (LowerCase(paramValue) = 'true')
              
            // ===== ПАРАМЕТРЫ ФРИКАМА =====
            else if paramName = 'basespeed' then
              Config_BaseSpeed := StrToFloat(StringReplace(paramValue, '.', ',', [rfReplaceAll]))
            else if paramName = 'fastspeed' then
              Config_FastSpeed := StrToFloat(StringReplace(paramValue, '.', ',', [rfReplaceAll]))
            else if paramName = 'turnspeed' then
              Config_TurnSpeed := StrToFloat(StringReplace(paramValue, '.', ',', [rfReplaceAll]))
              
            // ===== ПАРАМЕТРЫ ОСНОВНОЙ КАМЕРЫ =====
            else if paramName = 'stepforward' then
            begin
              Config_StepForward := StrToFloat(StringReplace(paramValue, '.', ',', [rfReplaceAll]));
            end
            
            // ===== ПАРАМЕТРЫ ДИСТАНЦИИ =====
            else if paramName = 'maxvisibledistance' then
              Config_MaxVisibleDistance := StrToInt(paramValue);
              
          except
            // Игнорируем ошибки преобразования
          end;
        end;
      end;
      
      CloseFile(f);
      
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'Ошибка загрузки zdbooster.cfg: ' + E.Message);
        try
          CloseFile(f);
        except
        end;
      end;
    end;
  end
  else
  begin
    // Создаем файл конфигурации со всеми параметрами
    try
      AssignFile(f, 'zdbooster.cfg');
      Rewrite(f);
      WriteLn(f, '# Конфигурация ZDBooster');
      WriteLn(f, '# 1 = включено, 0 = выключено для флагов');
      WriteLn(f, '# Числовые значения для параметров');
      WriteLn(f, '');
      WriteLn(f, '# ===== ВКЛЮЧЕНИЕ/ВЫКЛЮЧЕНИЕ СИСТЕМ =====');
      WriteLn(f, '# Включить/выключить фрикам (свободную камеру)');
      WriteLn(f, 'freecam: 1');
      WriteLn(f, '');
      WriteLn(f, '# Включить/выключить настройку основной камеры');
      WriteLn(f, 'main_camera: 1');
      WriteLn(f, '');
      WriteLn(f, '# Включить/выключить настройку дистанции видимости');
      WriteLn(f, 'max_distance: 1');
      WriteLn(f, '');
      WriteLn(f, '# Включить/выключить новое небо');
      WriteLn(f, 'newsky: 1');
      WriteLn(f, '');
      WriteLn(f, '# ===== ПАРАМЕТРЫ ФРИКАМА =====');
      WriteLn(f, '# (работают только если freecam: 1)');
      WriteLn(f, '# Базовая скорость движения (обычная)');
      WriteLn(f, 'basespeed: 0.5');
      WriteLn(f, '');
      WriteLn(f, '# Быстрая скорость движения (с зажатым Shift)');
      WriteLn(f, 'fastspeed: 1.0');
      WriteLn(f, '');
      WriteLn(f, '# Скорость поворота камеры (стрелками)');
      WriteLn(f, 'turnspeed: 1.5');
      WriteLn(f, '');
      WriteLn(f, '# ===== ПАРАМЕТРЫ ОСНОВНОЙ КАМЕРЫ =====');
      WriteLn(f, '# (работают только если main_camera: 1)');
      WriteLn(f, '# Шаг движения камеры (Shift + стрелки)');
      WriteLn(f, 'stepforward: 0.1');
      WriteLn(f, '');
      WriteLn(f, '# ===== ПАРАМЕТРЫ ДИСТАНЦИИ =====');
      WriteLn(f, '# (работают только если max_distance: 1)');
      WriteLn(f, '# Максимальная дистанция видимости');
      WriteLn(f, 'maxvisibledistance: 1200');
      CloseFile(f);
      AddToLogFile(EngineLog, 'Создан zdbooster.cfg с дефолтными значениями');
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'Ошибка создания zdbooster.cfg: ' + E.Message);
    end;
  end;

  AddToLogFile(EngineLog, Format('Флаги систем: Freecam=%s, MainCamera=%s, MaxDistance=%s, NewSky=%s',
    [BoolToStr(Config_Freecam, True), BoolToStr(Config_MainCamera, True), 
     BoolToStr(Config_MaxDistance, True), BoolToStr(Config_NewSky, True)]));
     
  AddToLogFile(EngineLog, Format('Параметры: BaseSpeed=%.1f, FastSpeed=%.1f, TurnSpeed=%.1f, StepForward=%.6f, MaxDist=%d',
    [Config_BaseSpeed, Config_FastSpeed, Config_TurnSpeed, Config_StepForward, Config_MaxVisibleDistance]));

  ConfigLoaded := True;
end;

procedure SaveConfigFile;
var
  f: TextFile;
begin
  try
    AssignFile(f, 'zdbooster.cfg');
    Rewrite(f);
    if Config_Freecam then WriteLn(f, 'freecam: 1') else WriteLn(f, 'freecam: 0');
    if Config_MainCamera then WriteLn(f, 'main_camera: 1') else WriteLn(f, 'main_camera: 0');
    if Config_MaxDistance then WriteLn(f, 'max_distance: 1') else WriteLn(f, 'max_distance: 0');
    if Config_NewSky then WriteLn(f, 'newsky: 1') else WriteLn(f, 'newsky: 0');
    WriteLn(f, Format('basespeed: %.1f', [Config_BaseSpeed]));
    WriteLn(f, Format('fastspeed: %.1f', [Config_FastSpeed])); 
    WriteLn(f, Format('turnspeed: %.1f', [Config_TurnSpeed]));
    WriteLn(f, Format('stepforward: %.6f', [Config_StepForward]));
    WriteLn(f, Format('maxvisibledistance: %d', [Config_MaxVisibleDistance]));
    CloseFile(f);
  except
    try CloseFile(f); except end;
  end;
end;

// ===== ФУНКЦИЯ ПЕРИОДИЧЕСКОЙ ПРОВЕРКИ STEPFORWARD =====
procedure ProcessStepForwardConfig;
var
  currentTime: Cardinal;
  StateChanged: Boolean;
begin
  currentTime := timeGetTime;
  
  // Проверяем изменилось ли состояние модуля
  StateChanged := not MainCameraInitialized or (Config_MainCamera <> LastMainCameraState);
  
  if StateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния MainCamera: %s -> %s',
      [BoolToStr(LastMainCameraState, True), BoolToStr(Config_MainCamera, True)]));
  end;

  if Config_MainCamera then
  begin
    // ===== МОДУЛЬ ВКЛЮЧЕН - ПРИМЕНЯЕМ ЗНАЧЕНИЕ ИЗ КОНФИГА =====
    
    // Проверяем изменения только по таймеру
    if (currentTime - LastStepForwardCheck) > StepForwardCheckInterval then
    begin
      // Перезагружаем только stepForward из конфига
      if FileExists('zdbooster.cfg') then
      begin
        LoadConfigFile; // Это обновит Config_StepForward
      end;
      
      WriteStepForwardToMemory(Config_StepForward);
      
      if StateChanged then
        AddToLogFile(EngineLog, Format('stepForward применен: %.6f', [Config_StepForward]));
        
      LastStepForwardCheck := currentTime;
    end;
  end
  else
  begin
    // ===== МОДУЛЬ ВЫКЛЮЧЕН - ВОССТАНАВЛИВАЕМ ОРИГИНАЛ =====
    if StateChanged then
    begin
      AddToLogFile(EngineLog, 'Восстанавливаем оригинальный stepForward: 0.1');
      WriteStepForwardToMemory(OriginalStepForwardValue);
    end;
  end;
  
  // Обновляем состояние
  LastMainCameraState := Config_MainCamera;
  MainCameraInitialized := True;
end;

// Обработка фрикамы
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
begin
  CurrentTime := GetTickCount;
  
  // ===== РУЧНОЕ УПРАВЛЕНИЕ (ВСЕГДА ДОСТУПНО) =====
  // Обработка переключения фрикамы (Page Down) - работает ВСЕГДА
  if IsKeyPressed(VK_NEXT) and // Page Down
     (CurrentTime - LastFreecamToggle > FreecamKeyDelay) then
  begin
    if not FreecamEnabled then
    begin
      // ВКЛЮЧАЕМ ФРИКАМ
      AddToLogFile(EngineLog, 'Ручное включение фрикама (Page Down)');
      
      if not FreecamInitialized then
        InitializeFreecam;
        
      FreecamEnabled := True;
      if ApplyNopPatch then
      begin
        Config_Freecam := True;
        SaveConfigFile;
        AddToLogFile(EngineLog, 'УСПЕХ: Фрикам включен вручную + freecam: 1 в конфиг');
        LastFreecamToggle := CurrentTime;
      end
      else
      begin
        FreecamEnabled := False;
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось применить NOP patch при ручном включении');
      end;
    end
    else
    begin
      // ВЫКЛЮЧАЕМ ФРИКАМ
      AddToLogFile(EngineLog, 'Ручное выключение фрикама (Page Down)');
      
      FreecamEnabled := False;
      
      // Восстанавливаем позицию
      WriteMemoryDouble(FREEMODE_SWITCH_ADDR, 0.0);
      WriteMemorySingle(ADDR_X, InitialX);
      WriteMemorySingle(ADDR_Y, InitialY);
      WriteMemorySingle(ADDR_Z, InitialZ);
      
      if RestoreOriginalBytes then
      begin
        Config_Freecam := False;
        SaveConfigFile;
        AddToLogFile(EngineLog, 'УСПЕХ: Фрикам выключен вручную + freecam: 0 в конфиг');
        LastFreecamToggle := CurrentTime;
      end
      else
      begin
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось восстановить байты при ручном выключении');
      end;
    end;
  end;

  // ===== АВТОМАТИЧЕСКОЕ ВКЛЮЧЕНИЕ/ВЫКЛЮЧЕНИЕ ПО КОНФИГУ =====
  
  // Проверяем, изменилось ли состояние конфига
  ConfigStateChanged := not FreecamConfigStateInitialized or 
                       (Config_Freecam <> LastFreecamConfigState);
  
  if ConfigStateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния фрикама в конфиге: %s -> %s', 
      [BoolToStr(LastFreecamConfigState, True), BoolToStr(Config_Freecam, True)]));
    
    if Config_Freecam then
    begin
      // ===== АВТОМАТИЧЕСКИ ВКЛЮЧАЕМ ФРИКАМ =====
      if not FreecamEnabled then
      begin
        AddToLogFile(EngineLog, 'Автоматически включаем фрикам (freecam: 1)...');
        
        if not FreecamInitialized then
        begin
          AddToLogFile(EngineLog, 'Инициализируем фрикам...');
          InitializeFreecam;
        end;
        
        FreecamEnabled := True;
        if ApplyNopPatch then
        begin
          AddToLogFile(EngineLog, 'УСПЕХ: Фрикам автоматически включен через конфиг');
        end
        else
        begin
          FreecamEnabled := False;
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось применить NOP patch при автовключении');
        end;
      end
      else
      begin
        AddToLogFile(EngineLog, 'Фрикам уже включен');
      end;
    end
    else
    begin
      // ===== АВТОМАТИЧЕСКИ ВЫКЛЮЧАЕМ ФРИКАМ =====
      if FreecamEnabled then
      begin
        AddToLogFile(EngineLog, 'Автоматически выключаем фрикам (freecam: 0)...');
        FreecamEnabled := False;
        
        // Восстанавливаем позицию
        WriteMemoryDouble(FREEMODE_SWITCH_ADDR, 0.0);
        WriteMemorySingle(ADDR_X, InitialX);
        WriteMemorySingle(ADDR_Y, InitialY);
        WriteMemorySingle(ADDR_Z, InitialZ);
        
        if RestoreOriginalBytes then
        begin
          AddToLogFile(EngineLog, 'УСПЕХ: Фрикам автоматически выключен через конфиг');
        end
        else
        begin
          AddToLogFile(EngineLog, 'ОШИБКА: Не удалось восстановить байты при автовыключении');
        end;
      end
      else
      begin
        AddToLogFile(EngineLog, 'Фрикам уже выключен');
      end;
    end;
    
    // Обновляем состояние
    LastFreecamConfigState := Config_Freecam;
    FreecamConfigStateInitialized := True;
  end;

  // ===== ЕСЛИ ФРИКАМ НЕ ВКЛЮЧЕН, ВЫХОДИМ =====
  if not FreecamEnabled then Exit;
  
  // ===== ОБРАБОТКА ДВИЖЕНИЯ ФРИКАМА =====
  
  // Читаем текущие значения камеры
  CurrentYaw := ReadMemorySingle(ADDR_LOOKYAW);
  CurrentPitch := ReadMemorySingle(ADDR_LOOKPITCH);
  CurrentX := ReadMemorySingle(ADDR_X);
  CurrentY := ReadMemorySingle(ADDR_Y);
  CurrentZ := ReadMemorySingle(ADDR_Z);

  // Нормализуем yaw
  while CurrentYaw >= 360.0 do
    CurrentYaw := CurrentYaw - 360.0;
  while CurrentYaw < 0.0 do
    CurrentYaw := CurrentYaw + 360.0;
  
  // Конвертируем в радианы
  YawRad := CurrentYaw * (Pi / 180.0);
  PitchRad := CurrentPitch * (Pi / 180.0);
  
  // Вычисляем направления
  ForwardX := cos(PitchRad) * sin(YawRad);
  ForwardY := cos(PitchRad) * cos(YawRad);
  ForwardZ := sin(PitchRad);
  
  RightX := sin(YawRad - Pi / 2);
  RightY := cos(YawRad - Pi / 2);
  
  // Определяем скорость - ИСПОЛЬЗУЕМ ПЕРЕМЕННЫЕ ИЗ КОНФИГА
  if IsKeyPressed(VK_SHIFT) then
    Speed := Config_FastSpeed
  else
    Speed := Config_BaseSpeed;
  
  // Обработка движения
  if IsKeyPressed(Ord('W')) then
  begin
    CurrentX := CurrentX + ForwardX * Speed;
    CurrentY := CurrentY + ForwardY * Speed;
    CurrentZ := CurrentZ + ForwardZ * Speed;
  end;
  
  if IsKeyPressed(Ord('S')) then
  begin
    CurrentX := CurrentX - ForwardX * Speed;
    CurrentY := CurrentY - ForwardY * Speed;
    CurrentZ := CurrentZ - ForwardZ * Speed;
  end;
  
  if IsKeyPressed(Ord('A')) then
  begin
    CurrentX := CurrentX + RightX * Speed;
    CurrentY := CurrentY + RightY * Speed;
  end;
  
  if IsKeyPressed(Ord('D')) then
  begin
    CurrentX := CurrentX - RightX * Speed;
    CurrentY := CurrentY - RightY * Speed;
  end;
  
  if IsKeyPressed(VK_SPACE) then
  begin
    CurrentZ := CurrentZ + Speed;
  end;
  
  // Обработка поворота камеры - ИСПОЛЬЗУЕМ ПЕРЕМЕННУЮ ИЗ КОНФИГА
  NewYaw := CurrentYaw;
  NewPitch := CurrentPitch;
  
  if IsKeyPressed(VK_UP) then
    NewPitch := NewPitch + Config_TurnSpeed;
  
  if IsKeyPressed(VK_DOWN) then
    NewPitch := NewPitch - Config_TurnSpeed;
  
  if IsKeyPressed(VK_LEFT) then
    NewYaw := NewYaw - Config_TurnSpeed;
  
  if IsKeyPressed(VK_RIGHT) then
    NewYaw := NewYaw + Config_TurnSpeed;
  
  // Ограничиваем pitch
  NewPitch := ClampPitch(NewPitch);

  // Применяем изменения
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




function SignalColor(code: Integer): Char;
begin
  case code of
    0: Result := 'Ч'; // Черный
    1: Result := 'К'; // Красный  
    2: Result := 'Ж'; // Желтый
    3: Result := 'З'; // Зеленый
    else Result := '?';
  end;
end;

function LoadDataFile(filename: string): TStringList;
var
  f: TextFile;
  line: string;
  piketNum: Integer;
begin
  Result := TStringList.Create;
  if FileExists(filename) then
  begin
    try
      AssignFile(f, filename);
      Reset(f);
      while not Eof(f) do
      begin
        ReadLn(f, line);
        line := Trim(line);
        if line <> '' then
        begin
          piketNum := StrToInt(Copy(line, 1, Pos(#9, line + #9) - 1));
          Result.Add(IntToStr(piketNum));
        end;
      end;
      CloseFile(f);
    except
      // Игнорируем ошибки
    end;
  end;
end;

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToInt(List[Index1]) - StrToInt(List[Index2]);
end;


// Добавляем недостающую функцию ArtificialSignalColor
function ArtificialSignalColor(code: Integer): string;
begin
  case code of
    0: Result := 'В';  // Выключен  
    1: Result := 'Б';  // Белый
    2: Result := 'К';  // Красный
    3: Result := 'КЖ'; // Красно-желтый
    4: Result := 'Ж';  // Желтый
    5: Result := 'З';  // Зеленый
    else Result := '?';
  end;
end;

function GetSignalSequence: string;
var
  i, j, piketNum, signalState, currentPiket, closestIndex, minDistance: Integer;
  offset, baseAddr, currentPiketAddr, trafficLightAddr: Cardinal;
  oneDirection: Boolean;
  trafficLightState: Byte;
  piketData: array of record
    offset: Cardinal;
    piketNum: Integer;
    signalState: Integer;
  end;
  filteredPikets, uniquePikets: TStringList;
  displayStart, displayEnd: Integer;
  tempIndex: Integer;
  existingOffset, newOffset: Cardinal;
begin
  Result := '';
  
  baseAddr := $900805C;
  currentPiketAddr := $749A0C;
  trafficLightAddr := $400000 + $8C07ECC;
  oneDirection := PByte(Pointer($749818))^ = 1;
  currentPiket := PInteger(Pointer(currentPiketAddr))^;
  trafficLightState := PByte(Pointer(trafficLightAddr))^;
  
  // Собираем данные о пикетах
  SetLength(piketData, s1.Count + s2.Count);
  for i := 0 to High(piketData) do
  begin
    offset := baseAddr + $858 * Cardinal(i);
    piketData[i].offset := offset;
    piketData[i].piketNum := PInteger(Pointer(offset))^;
    piketData[i].signalState := PInteger(Pointer(offset + $70))^;
  end;
  
  // Фильтруем по направлению
  filteredPikets := TStringList.Create;
  uniquePikets := TStringList.Create;
  try
    // Сначала фильтруем по направлению
    for i := 0 to High(piketData) do
    begin
      if oneDirection then
      begin
        if s1.IndexOf(IntToStr(piketData[i].piketNum)) >= 0 then
          filteredPikets.AddObject(IntToStr(piketData[i].piketNum), TObject(i));
      end
      else
      begin
        if s2.IndexOf(IntToStr(piketData[i].piketNum)) >= 0 then
          filteredPikets.AddObject(IntToStr(piketData[i].piketNum), TObject(i));
      end;
    end;
    
    // ВАЖНО: Убираем дубликаты, выбирая нужный адрес в зависимости от направления
    for i := 0 to filteredPikets.Count - 1 do
    begin
      piketNum := StrToInt(filteredPikets[i]);
      tempIndex := Integer(filteredPikets.Objects[i]);
      newOffset := piketData[tempIndex].offset;
      
      j := uniquePikets.IndexOf(IntToStr(piketNum));
      if j = -1 then
      begin
        // Пикета еще нет - добавляем
        uniquePikets.AddObject(IntToStr(piketNum), TObject(tempIndex));
      end
      else
      begin
        // Пикет уже есть - выбираем нужный адрес
        existingOffset := piketData[Integer(uniquePikets.Objects[j])].offset;
        
        if oneDirection then
        begin
          // Для прямого направления выбираем меньший адрес
          if newOffset < existingOffset then
            uniquePikets.Objects[j] := TObject(tempIndex);
        end
        else
        begin
          // Для обратного направления выбираем больший адрес
          if newOffset > existingOffset then
            uniquePikets.Objects[j] := TObject(tempIndex);
        end;
      end;
    end;
    
    // Сортируем по номеру пикета
    uniquePikets.CustomSort(CompareStrings);
    
    // Находим ближайший следующий пикет
    closestIndex := -1;
    minDistance := MaxInt;
    
    for i := 0 to uniquePikets.Count - 1 do
    begin
      piketNum := StrToInt(uniquePikets[i]);
      if oneDirection then
      begin
        if (piketNum > currentPiket) and (piketNum - currentPiket < minDistance) then
        begin
          closestIndex := i;
          minDistance := piketNum - currentPiket;
        end;
      end
      else
      begin
        if (piketNum < currentPiket) and (currentPiket - piketNum < minDistance) then
        begin
          closestIndex := i;
          minDistance := currentPiket - piketNum;
        end;
      end;
    end;
    
    if closestIndex >= 0 then
    begin
      // Определяем диапазон для анализа (как в Python)
      if oneDirection then
      begin
        displayStart := Max(closestIndex - 1, 0);
        displayEnd := Min(closestIndex + 4, uniquePikets.Count - 1);
      end
      else
      begin
        displayStart := Max(closestIndex - 5, 0);
        displayEnd := closestIndex;
      end;
      
      // Формируем последовательность сигналов
      if oneDirection then
      begin
        // Прямое направление - как есть
        for i := displayStart to displayEnd do
        begin
          j := Integer(uniquePikets.Objects[i]);
          Result := Result + SignalColor(piketData[j].signalState);
          // Останавливаемся на черном сигнале
          if piketData[j].signalState = 0 then Break;
        end;
      end
      else
      begin
        // Обратное направление - в обратном порядке (как [::-1] в Python)
        for i := displayEnd downto displayStart do
        begin
          j := Integer(uniquePikets.Objects[i]);
          Result := Result + SignalColor(piketData[j].signalState);
          // Останавливаемся на черном сигнале
          if piketData[j].signalState = 0 then Break;
        end;
      end;
    end;
    
    // Добавляем префикс искусственного светофора для обратного направления (как в Python)
    if not oneDirection then
    begin
      Result := ArtificialSignalColor(trafficLightState) + Result;
    end;
    
  finally
    filteredPikets.Free;
    uniquePikets.Free;
  end;
end;

procedure InitializeTrafficLightSystem;
var
  routeName: string;
  filePath1, filePath2: string;
begin
  if TrafficSystemInitialized then Exit;
  
  // Получаем название маршрута
  routeName := GetRouteName;
  
  // Формируем пути к файлам
  if routeName <> '' then
  begin
    filePath1 := 'routes\' + routeName + '\svetofor1.dat';
    filePath2 := 'routes\' + routeName + '\svetofor2.dat';
  end
  else
  begin
    // Fallback на стандартные файлы
    filePath1 := 'svetofor1.dat';
    filePath2 := 'svetofor2.dat';
  end;
  
  // Инициализируем списки пикетов
  if not Assigned(s1) then s1 := LoadDataFile(filePath1);
  if not Assigned(s2) then s2 := LoadDataFile(filePath2);
  
  //AddToLogFile(EngineLog, 'Загружен файл svetofor1: ' + filePath1);
  //AddToLogFile(EngineLog, 'Загружен файл svetofor2: ' + filePath2);
  
  TrafficSystemInitialized := True;
end;

procedure Draw3DDisk(Radius: Single);
var
  quad: PGLUquadric;
begin
  quad := gluNewQuadric;
  gluQuadricDrawStyle(quad, GLU_FILL);
  gluDisk(quad, 0.0, Radius, 64, 1); // внутренний радиус = 0 → круг
  gluDeleteQuadric(quad);
end;


procedure Draw3DSemiCircle(Radius: Single; StartAngle, EndAngle: Single);
var
  i: Integer;
  angle: Single;
  x, y: Single;
  segments: Integer;
begin
  segments := 32; // Количество сегментов для плавности
  
  glBegin(GL_TRIANGLE_FAN);
    glNormal3f(0, 0, 1); // ДОБАВЛЯЕМ НОРМАЛЬ - указывает в сторону наблюдателя
    glVertex3f(0, 0, 0); // Центр полукруга
    
    for i := 0 to segments do
    begin
      angle := StartAngle + (EndAngle - StartAngle) * i / segments;
      angle := angle * (Pi / 180.0); // Конвертируем в радианы
      x := Radius * cos(angle);
      y := Radius * sin(angle);
      glNormal3f(0, 0, 1); // Нормаль для каждой вершины
      glVertex3f(x, y, 0);
    end;
  glEnd;
end;

procedure Draw3DArcProgress(Radius: Single; StartAngleDeg, EndAngleDeg: Single; CurrentValue, MaxValue: Single; Segments: Integer);
var
  i: Integer;
  angle, angleStep, endAngleCurrent: Single;
  x, y: Single;
begin
  // Рассчитываем угол текущей скорости (прогресса)
  if CurrentValue > MaxValue then CurrentValue := MaxValue;
  if CurrentValue < 0 then CurrentValue := 0;
  
  angleStep := (EndAngleDeg - StartAngleDeg) / Segments;
  endAngleCurrent := StartAngleDeg + (CurrentValue / MaxValue) * (EndAngleDeg - StartAngleDeg);

  glBegin(GL_TRIANGLE_FAN);
    glVertex3f(0, 0, 0); // центр дуги

    // Рисуем дугу от StartAngleDeg до endAngleCurrent (текущий прогресс)
    for i := 0 to Segments do
    begin
      angle := StartAngleDeg + i * angleStep;
      if angle > endAngleCurrent then
        break;  // не выходить за текущий угол прогресса
      
      angle := angle * (Pi / 180.0); // в радианы
      x := Radius * cos(angle);
      y := Radius * sin(angle);
      glVertex3f(x, y, 0);
    end;

  glEnd;
end;


var
  PosX, PosY, PosZ: Double;
  Color: Longint;
  Alpha: Integer;
  UseLighting: Boolean;
  ExtraParam: Double;
  PosLine, ColorLine: string;
  F: TextFile;


procedure HookKLUBED4M(
 x: Single;
        y: Single;
        z: Single;
        AngX: Single;
        AngZ: Single
); stdcall; export;
begin
  
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
  with StaticData[14] do begin X:=0.9235; Y:=7.455; Z:=3.386; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='1'; Color:=$00FFFF; FuncType:=20; end; // 1-я позиция скорости
  with StaticData[15] do begin X:=0.935; Y:=7.451; Z:=3.386; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='2'; Color:=$00FFFF; FuncType:=21; end; // 2-я позиция скорости
  with StaticData[16] do begin X:=0.9465; Y:=7.447; Z:=3.386; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='3'; Color:=$00FFFF; FuncType:=22; end; // 3-я позиция скорости
  
  // Элементы 17-19 - Допустимая скорость (красный, 3 позиции)
  with StaticData[17] do begin X:=0.922; Y:=7.450; Z:=3.364; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='4'; Color:=$0000FF; FuncType:=23; end; // 1-я позиция допустимой
  with StaticData[18] do begin X:=0.9335; Y:=7.446; Z:=3.364; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='5'; Color:=$0000FF; FuncType:=24; end; // 2-я позиция допустимой
  with StaticData[19] do begin X:=0.945; Y:=7.442; Z:=3.364; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='6'; Color:=$0000FF; FuncType:=25; end; // 3-я позиция допустимой
  
  // Элементы 20-23 - Расстояние до цели (красный, 4 позиции)
  with StaticData[20] do begin X:=0.852; Y:=7.471; Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='1'; Color:=$0000FF; FuncType:=26; end; // 1-я позиция расстояния
  with StaticData[21] do begin X:=0.863; Y:=7.4674; Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='2'; Color:=$0000FF; FuncType:=27; end; // 2-я позиция расстояния
  with StaticData[22] do begin X:=0.874; Y:=7.4638; Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='3'; Color:=$0000FF; FuncType:=28; end; // 3-я позиция расстояния
  with StaticData[23] do begin X:=0.885; Y:=7.4602; Z:=3.366; RotX:=-70; RotY:=20; RotZ:=7.0; Scale:=0.019; Text:='4'; Color:=$0000FF; FuncType:=29; end; // 4-я позиция расстояния
  
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

// Функция для проверки, нужно ли показывать цифру float
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

// Добавьте эту функцию в начало implementation секции:
function ExtractField(const S: string; FieldNum: Integer; Delimiter: Char): string;
var
  i, FieldCount, StartPos: Integer;
begin
  Result := '';
  FieldCount := 1;
  StartPos := 1;
  
  for i := 1 to Length(S) do
  begin
    if S[i] = Delimiter then
    begin
      if FieldCount = FieldNum then
      begin
        Result := Copy(S, StartPos, i - StartPos);
        Exit;
      end;
      Inc(FieldCount);
      StartPos := i + 1;
    end;
  end;
  
  if FieldCount = FieldNum then
    Result := Copy(S, StartPos, Length(S) - StartPos + 1);
end;

function TrimString(const S: string): string;
var
  i, L: Integer;
begin
  L := Length(S);
  i := 1;
  while (i <= L) and (S[i] <= ' ') do Inc(i);
  if i > L then Result := '' else
  begin
    while (L > 0) and (S[L] <= ' ') do Dec(L);
    Result := Copy(S, i, L - i + 1);
  end;
end;

// Загрузка станций из памяти игры (исправленная версия)
procedure LoadStations;
var
  baseStationAddress: Cardinal;
  stationsCount: Byte;
  i: Integer;
  nameAddress, piketAddress: Cardinal;
  stationName: string;
  stationPiket: Integer;
  buffer: array[0..63] of Char; // Увеличили буфер до 64 символов
  nameLength: Byte;
begin
  if StationsLoaded then Exit;
  
  if not Assigned(StationsList) then
    StationsList := TStringList.Create;
  
  try
    // Получаем базовый адрес станций (как в Python)
    baseStationAddress := PCardinal(Pointer($00400000 + $403AEC))^ - $04;
    stationsCount := PByte(Pointer(baseStationAddress))^;
    
    //AddToLogFile(EngineLog, 'Найдено станций: ' + IntToStr(stationsCount));
    
    // Читаем все станции
    for i := 0 to stationsCount - 1 do
    begin
      try
        // Читаем имя станции
        nameAddress := PCardinal(Pointer($00400000 + $403AEC))^ + $70 + i * $48;
        
        // Сначала читаем длину строки
        nameLength := PByte(Pointer(nameAddress))^;
        if nameLength > 63 then nameLength := 63; // Ограничиваем для безопасности
        
        // Очищаем буфер
        FillChar(buffer, SizeOf(buffer), 0);
        
        // Читаем строку полностью по её длине
        if nameLength > 0 then
          Move(Pointer(nameAddress + 1)^, buffer, nameLength);
        
        stationName := Trim(UpperCase(string(buffer))); // ВЕРХНИЙ РЕГИСТР
        
        // Читаем пикет станции
        piketAddress := PCardinal(Pointer($00400000 + $403AEC))^ + $48 + i * $48;
        stationPiket := PInteger(Pointer(piketAddress))^;
        
        // Сохраняем как "название|пикет"
        if stationName <> '' then
        begin
          StationsList.Add(stationName + '|' + IntToStr(stationPiket));
          //AddToLogFile(EngineLog, 'Станция: ' + stationName + ' пикет: ' + IntToStr(stationPiket));
        end;
        
      except
        // Пропускаем ошибки чтения отдельных станций
        Continue;
      end;
    end;
    
    StationsLoaded := True;
    //AddToLogFile(EngineLog, 'Станции загружены из памяти игры');
    
  except
    on E: Exception do
    begin
      //AddToLogFile(EngineLog, 'Ошибка загрузки станций из памяти: ' + E.Message);
      // Fallback на пустой список
      StationsList.Clear;
    end;
  end;
end;

// Поиск станции по пикету (как в Python с погрешностью ±50)
function FindStationByPiket(currentPiket: Integer): string;
var
  i, stationPiket, minDistance, distance: Integer;
  parts: TStringList;
  bestMatch: string;
begin
  Result := '';
  LoadStations;
  
  if StationsList.Count = 0 then Exit;
  
  minDistance := MaxInt;
  bestMatch := '';
  parts := TStringList.Create;
  try
    for i := 0 to StationsList.Count - 1 do
    begin
      parts.Delimiter := '|';
      parts.DelimitedText := StationsList[i];
      if parts.Count >= 2 then
      begin
        try
          stationPiket := StrToInt(parts[1]);
          distance := Abs(currentPiket - stationPiket);
          
          // Ищем станцию с погрешностью ±50 км
          if (distance <= 50) and (distance < minDistance) then
          begin
            minDistance := distance;
            bestMatch := UpperCase(parts[0]); // ВЕРХНИЙ РЕГИСТР - ИСПРАВЛЕНО!
          end;
        except
          Continue;
        end;
      end;
    end;
  finally
    parts.Free;
  end;
  
  Result := bestMatch;
end;

// Улучшенная функция поиска текущей и следующей станции
procedure FindCurrentAndNextStation;
type
  TStationRecord = record
    name: string;
    piket: Integer;
    distance: Integer;
  end;
var
  currentPiket: Integer;
  i, stationPiket: Integer;
  parts: TStringList;
  currentTime: Cardinal;
  
  // Массивы станций по направлениям  
  stations: array[0..49] of TStationRecord;
  stationCount: Integer;
  
  currentStationIndex: Integer;
  nextStationIndex: Integer;
  minDistance: Integer;
  j: Integer;
  tempStation: TStationRecord;
  
  // Переменные для определения направления
  oneDirection: Boolean;
  directionStr: string;

begin
  // Проверяем кэш
  currentTime := timeGetTime;
  if (LastStationUpdate > 0) and (currentTime - LastStationUpdate < StationUpdateInterval) then
    Exit;
  
  LoadStations;
  
  // Читаем текущий пикет и направление из памяти
  currentPiket := PWord(Pointer($00400000 + $8C08054))^;
  
  // Определяем направление движения (как в Python коде)
  oneDirection := PByte(Pointer($749818))^ = 1;
  
  if oneDirection then
    directionStr := 'прямое'
  else
    directionStr := 'обратное';
    
  //AddToLogFile(EngineLog, Format('Текущий пикет: %d, Направление: %s',
  //  [currentPiket, directionStr]));
  
  // Собираем все станции
  stationCount := 0;
  parts := TStringList.Create;
  try
    for i := 0 to StationsList.Count - 1 do
    begin
      parts.Delimiter := '|';
      parts.DelimitedText := StationsList[i];
      if parts.Count >= 2 then
      begin
        try
          stationPiket := StrToInt(parts[1]);
          
          if stationCount < 50 then
          begin
            stations[stationCount].name := UpperCase(Trim(parts[0]));
            stations[stationCount].piket := stationPiket;
            stations[stationCount].distance := Abs(currentPiket - stationPiket);
            Inc(stationCount);
          end;
        except
          Continue;
        end;
      end;
    end;
  finally
    parts.Free;
  end;
  
  // Сортируем станции по пикетам (по возрастанию)
  for i := 0 to stationCount - 2 do
  begin
    for j := i + 1 to stationCount - 1 do
    begin
      if stations[j].piket < stations[i].piket then
      begin
        tempStation := stations[i];
        stations[i] := stations[j];
        stations[j] := tempStation;
      end;
    end;
  end;
  
  // Ищем текущую станцию (ближайшую с погрешностью ±50)
  CurrentStationName := 'НЕТ ДАННЫХ';
  currentStationIndex := -1;
  minDistance := MaxInt;
  
  for i := 0 to stationCount - 1 do
  begin
    if stations[i].distance <= 50 then
    begin
      if stations[i].distance < minDistance then
      begin
        minDistance := stations[i].distance;
        CurrentStationName := UpperCase(stations[i].name);
        currentStationIndex := i;
      end;
    end;
  end;
  
  // Если НЕТ ДАННЫХ - используем следующую станцию как текущую
  if CurrentStationName = 'НЕТ ДАННЫХ' then
  begin
    // Ищем ближайшую станцию по направлению движения
    minDistance := MaxInt;
    currentStationIndex := -1;
    
    if oneDirection then
    begin
      // Прямое направление - ищем ближайшую станцию впереди
      for i := 0 to stationCount - 1 do
      begin
        if stations[i].piket > currentPiket then
        begin
          if (stations[i].piket - currentPiket) < minDistance then
          begin
            minDistance := stations[i].piket - currentPiket;
            CurrentStationName := UpperCase(stations[i].name);
            currentStationIndex := i;
          end;
        end;
      end;
    end
    else
    begin
      // Обратное направление - ищем ближайшую станцию позади
      for i := stationCount - 1 downto 0 do
      begin
        if stations[i].piket < currentPiket then
        begin
          if (currentPiket - stations[i].piket) < minDistance then
          begin
            minDistance := currentPiket - stations[i].piket;
            CurrentStationName := UpperCase(stations[i].name);
            currentStationIndex := i;
          end;
        end;
      end;
    end;
  end;
  
  // Ищем следующую станцию по маршруту (ВСЕГДА ищем от найденной текущей)
  NextStationName := 'КОНЕЧНАЯ';
  nextStationIndex := -1;
  
  if (currentStationIndex >= 0) and (currentStationIndex < stationCount) then
  begin
    if oneDirection then
    begin
      // Прямое направление - ищем станцию с большим пикетом
      for i := currentStationIndex + 1 to stationCount - 1 do
      begin
        NextStationName := UpperCase(stations[i].name);
        nextStationIndex := i;
        Break;
      end;
    end
    else
    begin
      // Обратное направление - ищем станцию с меньшим пикетом
      for i := currentStationIndex - 1 downto 0 do
      begin
        NextStationName := UpperCase(stations[i].name);
        nextStationIndex := i;
        Break;
      end;
    end;
  end;
  
  // Обновляем время последнего обновления
  LastStationUpdate := currentTime;
  
 // AddToLogFile(EngineLog, Format('Найдено станций: %d, Текущая: "%s" (индекс: %d), Следующая: "%s" (индекс: %d)',
 //   [stationCount, CurrentStationName, currentStationIndex, NextStationName, nextStationIndex]));
end;


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
        FindCurrentAndNextStation;
        CachedCurrentStation := CurrentStationName;
        CachedNextStation := NextStationName;
        LastStationCheck := timeGetTime;
      end;
      Result := 'ЗАДАНИЕ: ' + CachedCurrentStation;
    end;
    13: Result := 'РАСПИСАНИЕ';
    14: begin
      Result := CurrentStationName;
    end;
    15: begin
      Result := NextStationName;
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
    fontToUse := ImpactFont  // 7-Segment для всех цифр
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

// ===== ЗАГРУЗКА ПАРАМЕТРОВ СТРЕЛКИ ИЗ НОВОЙ ПАПКИ =====
procedure LoadArrowParams(ForceReload: Boolean = False);
var
  f: TextFile;
  line: string;
  paramName, paramValue: string;
  colonPos: Integer;
  currentTime: Cardinal;
  arrowFilePath, elementsPath: string;
begin
  currentTime := timeGetTime;
  
  // Проверяем, нужно ли обновлять (по таймеру или принудительно)
  if not ForceReload and ArrowParamsLoaded and 
     ((currentTime - LastArrowParamsCheck) < ArrowParamsCheckInterval) then
    Exit;
  
  // Значения по умолчанию
  ArrowAngle := 150.0;
  ArrowRotation := 90.0;
  ArrowScale := 1.61;
  ArrowX := 0.895;
  ArrowY := 7.45;
  ArrowZ := 3.64;
  ArrowRotateX := -40.4;
  ArrowRotateY := 0.0;
  ArrowRotateZ := 0.0;
  ArrowCurrentSpeed := 0.0;
  ArrowKoef := 1.63;
  
  // Получаем путь к файлу
  elementsPath := GetElementsPath;
  arrowFilePath := elementsPath + 'arrow_usavpp.txt';
  
  // ОТЛАДКА ПУТЕЙ
  AddToLogFile(EngineLog, '=== LoadArrowParams DEBUG ===');
  AddToLogFile(EngineLog, 'Elements path: ' + elementsPath);
  AddToLogFile(EngineLog, 'Arrow file path: ' + arrowFilePath);
  AddToLogFile(EngineLog, 'Elements directory exists: ' + BoolToStr(DirectoryExists(elementsPath), True));
  AddToLogFile(EngineLog, 'Arrow file exists: ' + BoolToStr(FileExists(arrowFilePath), True));
  
  if FileExists(arrowFilePath) then
  begin
    try
      AddToLogFile(EngineLog, 'Попытка загрузки arrow_usavpp.txt...');
      AssignFile(f, arrowFilePath);
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
            if paramName = 'arrowangle' then
              ArrowAngle := SafeStrToFloat(paramValue)
            else if paramName = 'arrowrotation' then
              ArrowRotation := SafeStrToFloat(paramValue)
            else if paramName = 'arrowscale' then
              ArrowScale := SafeStrToFloat(paramValue)
            else if paramName = 'arrowx' then
              ArrowX := SafeStrToFloat(paramValue)
            else if paramName = 'arrowy' then
              ArrowY := SafeStrToFloat(paramValue)
            else if paramName = 'arrowz' then
              ArrowZ := SafeStrToFloat(paramValue)
            else if paramName = 'arrowrotatex' then
              ArrowRotateX := SafeStrToFloat(paramValue)
            else if paramName = 'arrowrotatey' then
              ArrowRotateY := SafeStrToFloat(paramValue)
            else if paramName = 'arrowrotatez' then
              ArrowRotateZ := SafeStrToFloat(paramValue)
            else if paramName = 'currentspeed' then
              ArrowCurrentSpeed := SafeStrToFloat(paramValue)
            else if paramName = 'koef' then
              ArrowKoef := SafeStrToFloat(paramValue)
            else if paramName = 'updateinterval' then
            begin
              ArrowParamsCheckInterval := Round(SafeStrToFloat(paramValue));
              if ArrowParamsCheckInterval < 50 then ArrowParamsCheckInterval := 50;
              if ArrowParamsCheckInterval > 5000 then ArrowParamsCheckInterval := 5000;
            end;
          except
            on E: Exception do
              AddToLogFile(EngineLog, 'Ошибка преобразования параметра ' + paramName + ': ' + E.Message);
          end;
        end;
      end;
      
      CloseFile(f);
      AddToLogFile(EngineLog, 'arrow_usavpp.txt успешно загружен');
      
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'КРИТИЧЕСКАЯ ОШИБКА загрузки arrow_usavpp.txt: ' + E.Message);
        try
          CloseFile(f);
        except
        end;
      end;
    end;
  end
  else
  begin
    AddToLogFile(EngineLog, 'arrow_usavpp.txt НЕ НАЙДЕН по пути: ' + arrowFilePath);
    
    // Создаем файл и папки только при первой загрузке
    if not ArrowParamsLoaded then
    begin
      try
        AddToLogFile(EngineLog, 'Попытка создания папки: ' + elementsPath);
        // Создаем папки если их нет
        if CreateDirectoryPath(elementsPath) then
        begin
          AddToLogFile(EngineLog, 'Папка создана успешно, создаем arrow_usavpp.txt');
          AssignFile(f, arrowFilePath);
          Rewrite(f);
          WriteLn(f, '# Конфигурация параметров стрелки спидометра УСАВП');
          WriteLn(f, '# Используйте точку как разделитель десятичных');
          WriteLn(f, '');
          WriteLn(f, '# Интервал обновления файла в миллисекундах (50-5000)');
          WriteLn(f, 'updateInterval: ', ArrowParamsCheckInterval);
          WriteLn(f, '');
          WriteLn(f, '# Угол стрелки (базовый)');
          WriteLn(f, 'arrowAngle: 150.0');
          WriteLn(f, '');
          WriteLn(f, '# Поворот стрелки');
          WriteLn(f, 'arrowRotation: 90.0');
          WriteLn(f, '');
          WriteLn(f, '# Масштаб стрелки');
          WriteLn(f, 'arrowScale: 1.61');
          WriteLn(f, '');
          WriteLn(f, '# Позиция стрелки');
          WriteLn(f, 'arrowX: 0.895');
          WriteLn(f, 'arrowY: 7.45');
          WriteLn(f, 'arrowZ: 3.64');
          WriteLn(f, '');
          WriteLn(f, '# Поворот по оси X');
          WriteLn(f, 'arrowRotateX: -40.4');
          WriteLn(f, '');
          WriteLn(f, '# Поворот по оси Y');
          WriteLn(f, 'arrowRotateY: 0.0');
          WriteLn(f, '');
          WriteLn(f, '# Поворот по оси Z');
          WriteLn(f, 'arrowRotateZ: 0.0');
          WriteLn(f, '');
          WriteLn(f, '# Текущая скорость (для расчета угла)');
          WriteLn(f, 'currentSpeed: 0.0');
          WriteLn(f, '');
          WriteLn(f, '# Коэффициент (множитель скорости)');
          WriteLn(f, 'koef: 1.63');
          CloseFile(f);
          AddToLogFile(EngineLog, 'arrow_usavpp.txt создан успешно: ' + arrowFilePath);
        end
        else
        begin
          AddToLogFile(EngineLog, 'ОШИБКА создания папки: ' + elementsPath);
        end;
      except
        on E: Exception do
          AddToLogFile(EngineLog, 'ОШИБКА создания arrow_usavpp.txt: ' + E.Message);
      end;
    end;
  end;
  
  ArrowParamsLoaded := True;
  LastArrowParamsCheck := currentTime;
  
  AddToLogFile(EngineLog, Format('Финальные параметры стрелки: Angle=%.2f, X=%.3f, Y=%.3f, Z=%.3f',
    [ArrowAngle, ArrowX, ArrowY, ArrowZ]));
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

// ===== ОБЩАЯ ФУНКЦИЯ ИНИЦИАЛИЗАЦИИ ВСЕХ КОНФИГОВ =====
procedure InitializeAllConfigs;
begin
  if not ConfigLoaded then
  begin
    LoadConfigFile;        // Загружаем настройки фрикама
    LoadBoosterConfig;     // Загружаем настройки отображения
    LoadArrowParams;       // Загружаем параметры стрелки
  end
  else
  begin
    // Обновляем в реал-тайм
    LoadBoosterConfig;     // Проверяем booster.txt
    LoadArrowParams;       // Проверяем arrow_usavpp.txt
  end;
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


//    ProcessFreecam;

//if not HookAddressWritten then
//begin
//  WriteHookAddress;  // ← ВОТ ЗДЕСЬ ВЫЗЫВАЕТСЯ
//  HookAddressWritten := True;
//end;

//SetLight(5, cos((90.0 - 54254 * 15.0 / 3600.0) * 3.141592653589793238 / 180.0) * 700.0,
 //sin((90.0 - 54254 * 15.0 / 3600.0) * 3.141592653589793238 / 180.0) * 700.0, 200.0, $FFFFFF,
  //9000.0, False, 10.0);



  // ===== ИНИЦИАЛИЗАЦИЯ СВЕТОФОРНОЙ СИСТЕМЫ =====
if not SystemInitialized then
  begin
    InitializeTrafficLightSystem;
    LoadBoosterConfig;
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
    SystemInitialized := True;
  end;
  //LoadYellowBlockParams;

  if not LightBlockIDsCached then
  begin
    try
      baseStructAddr := PCardinal(Pointer($00400000 + $8D10D70))^;
      CachedYellowBlockID := PWord(Pointer(baseStructAddr + $1A))^;
      CachedGreenBlockID := PWord(Pointer(baseStructAddr + $1C))^;
      LightBlockIDsCached := True;
      
   //   AddToLogFile(EngineLog, Format('Кэшированы ID: Желтый=%d, Зеленый=%d',
   //     [CachedYellowBlockID, CachedGreenBlockID]));
    except
      // При ошибке используем значения по умолчанию
      CachedYellowBlockID := 13;
      CachedGreenBlockID := 14;
      LightBlockIDsCached := True;
    end;
  end;


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
    
    //AddToLogFile(EngineLog, 'Активируем команду "137"');

    if WriteAndVerify($00400000 + $34988C, 52) then
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
if statek137 and not CommandCompleted and (PByte(Pointer($00400000 + $34988C))^ = 53) then
begin
  glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
  BeginObj3D;
  Position3D(1.137 - 0.012, 7.214 + 0.006, 3.553 + 0.021);
  RotateX(-90.0);
  RotateY(45.0);
  Scale3D(0.011);
  Color3D(3407667, 255, False, 0.0);
  SetTexture(0);
  DrawText3D(0, 'ТАБЛИЦА АЛС-ЕН');
  EndObj3D;
  glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
end;


  // Завершаем команду 137 при повторном нажатии ENTER
  if statek137 and not CommandCompleted and EnterPressed then
  begin
    //AddToLogFile(EngineLog, 'Завершаем команду "137"');
    
    if WriteAndVerify($00400000 + $34988C, 30) then
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
      
      ///AddToLogFile(EngineLog, 'ТАБЛИЦА АЛС-ЕН: ' + SavedCommand);
      //AddToLogFile(EngineLog, 'Сохранено значение частоты: ' + en_chastota);
      
  //    AddToLogFile(EngineLog, 'Команда "137" успешно завершена');
      SavedCommand := '';
    end
    else
    begin
      //AddToLogFile(EngineLog, 'ОШИБКА завершения команды "137"');
    end;
  end;

 // ===== ОБРАБОТКА КОМАНДЫ 10 ===== <- НОВЫЙ БЛОК
  
  // Проверяем новую команду "10" только если она еще не активна
  if NewCommandReceived and (LastCommand = '10') and not statek10 then
  begin
    statek10 := True;
    SavedCommand := LastCommand;
    CommandCompleted := False;
    
    if WriteAndVerify($00400000 + $34988C, 53) then  // Попробуй 53, потом 54 если не работает
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

  // Отображаем интерфейс команды 10
  if statek10 and not CommandCompleted then
  begin
    BeginObj3D;
    glDisable(GL_LIGHTING);
    Position3D(1.125, 7.22, 3.574);
    RotateX(-90.0);
    RotateY(45.0);
    Scale3D(0.011);
    Color3D(3407667, 255, False, 0.0);
    SetTexture(0);
    DrawText3D(0, 'K:raildriver');  // <- ДРУГОЙ ТЕКСТ
    glEnable(GL_LIGHTING);
    EndObj3D;
  end;

  // Завершаем команду 10 при повторном нажатии ENTER
  if statek10 and not CommandCompleted and EnterPressed then
  begin
    if WriteAndVerify($00400000 + $34988C, 30) then
    begin
      CommandCompleted := True;
      statek10 := False;
      CommandBuffer := '';
      
      AddToLogFile(EngineLog, 'Команда "10" успешно завершена');
      SavedCommand := '';
    end
    else
    begin
      //AddToLogFile(EngineLog, 'ОШИБКА завершения команды "10"');
    end;
  end;
  // ===== КОНЕЦ НОВОГО БЛОКА =====

  // ======== ИНИЦИАЛИЗАЦИЯ ========
  initialized := False;

  // Создание шрифта Impact
  if ImpactFont = 0 then
    ImpactFont := CreateFont3D('7-Segment');

  if KLUBUFont = 0 then
    KLUBUFont := CreateFont3D('KLUBU');  // ← ЗАГРУЗКА ШРИФТА KLUBU

  // Инициализация данных
  InitializeStaticData;
  
  // Загрузка моделей
  if MyModelID = 0 then
  begin
    MyModelID := LoadModel('data\loc\klub_bil_v.dmd', 0, False);
    MyTextureID := LoadTextureFromFile('data\loc\klub_bil.bmp', 0, -1);
    strelka := LoadModel('data\2te10u\0071\strelka_temp.dmd', 0, False);
  end;   

  // Получаем текущую скорость
  try
    currentSpeed := PSingle(BaseAddress + $4F8C28C)^;
    currentSpeed := Abs(currentSpeed);
  except
    currentSpeed := 0;
  end;

  // ======== АНАЛИЗ СВЕТОФОРОВ ========
if (timeGetTime - LastSignalUpdate) > SignalUpdateInterval then
begin
  CachedSignalSequence := GetSignalSequence;
  LastSignalUpdate := timeGetTime;
end;
signalSequence := CachedSignalSequence; // используем кэш
  
  // ======== ОТРИСОВКА МОДЕЛЕЙ ========



// Частота ЕН
if PByte(Pointer(PCardinal(Pointer($00400000 + $348638))^ + $2D0))^ > 0 then
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


BeginObj3D;
//Position3D(1.17, 7.1799998, 3.4319999);
Position3D(angZ, z, y);
RotateZ(x);
SetTexture(textureID);
DrawModel(modelID, 0, False);
EndObj3D;




// ======== ЛОГИКА ОТОБРАЖЕНИЯ СВЕТОФОРОВ (ДВЕ СИСТЕМЫ) ========

 if (als_en_state) and (PByte($905B754)^ <> 0) and (PByte(Pointer(PCardinal(Pointer($00400000 + $348638))^ + $2D0))^ > 0) then
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
//SETTEXTURE($1A);

  // Получаем состояние основного светофора
  mainTrafficLight := PByte(Pointer($400000 + $8C07ECC))^;

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



    // ======== СИСТЕМА КЛУБ (старые координаты) ========
    if visibleSignalCount > 0 then
    begin
      // Нижний блок - желтый
      BeginObj3D;
        glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
      Position3D(1.17, 7.1799998, 3.4319999);
      RotateZ(x);
      Position3D(-0.086499996, 0.0, 0.223);
      Scale3D(0.88999999);
      DrawModel(yellowBlockModelID, 0, False); // Читаем из памяти
        glEnable(GL_LIGHTING); // ← ДОБАВИТЬ

      EndObj3D;

      // Остальные блоки выше - зеленые
      for i := 1 to visibleSignalCount - 1 do
      begin
        BeginObj3D;
                glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
        Position3D(1.17, 7.1799998, 3.4319999);
        RotateZ(x);
        Position3D(-0.086499996, 0.0, 0.223 + i * 0.013);
        Scale3D(0.88999999);
        DrawModel(greenBlockModelID, 0, False); // Читаем из памяти
                glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
        EndObj3D;

        if i >= 4 then Break; // Ограничиваем максимум 5 блоков
      end;
    end;

    //SetLight(5, cos((90.0 - 54254 * 15.0 / 3600.0) * 3.141592653589793238 / 180.0) * 700.0,
     //sin((90.0 - 54254 * 15.0 / 3600.0) * 3.141592653589793238 / 180.0) * 700.0, 200.0, $FFFFFF,
      //9000.0, False, 10.0);

    // ======== СИСТЕМА БИЛ ПОМЕ (новые координаты) ========
    if visibleSignalCount > 0 then
    begin
      // Желтый блок (нижний по Z=3.631)
        glDisable(GL_LIGHTING); // ← ДОБАВИТЬ

      BeginObj3D;
      Position3D(0.002, 7.515, 3.631);
      RotateX(-30);
      Scale3D(1.0);
      DrawModel(yellowBlockModelID, 0, False); // Читаем из памяти
        glEnable(GL_LIGHTING); // ← ДОБАВИТЬ

      EndObj3D;

      // Зеленый блок 1
      if visibleSignalCount > 1 then
      begin
        BeginObj3D;
                glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
        Position3D(0.002, 7.508, 3.6438);
        RotateX(-30);
        Scale3D(1.0);
        DrawModel(CachedGreenBlockID, 0, False); // Читаем из памяти
                glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
        EndObj3D;
      end;
      
      // Зеленый блок 2  
      if visibleSignalCount > 2 then
      begin
        BeginObj3D;
                        glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
        Position3D(0.00160, 7.5, 3.6576);
        RotateX(-30);
        Scale3D(1.0);
        DrawModel(greenBlockModelID, 0, True); // Читаем из памяти
                        glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
        EndObj3D;
      end;

      // Зеленый блок 3
      if visibleSignalCount > 3 then
      begin
        BeginObj3D;
                                glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
        Position3D(0.00160, 7.492, 3.67);
        RotateX(-30);
        Scale3D(1.0);
        DrawModel(greenBlockModelID, 0, True); // Читаем из памяти
                                glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
        EndObj3D;
      end;
      
      // Зеленый блок 4
      if visibleSignalCount > 4 then
      begin
        BeginObj3D;
                                        glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
        Position3D(0.00160, 7.4847, 3.684);
        RotateX(-30);
        Scale3D(1.0);
        DrawModel(greenBlockModelID, 0, True); // Читаем из памяти
                                        glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
        EndObj3D;
      end;
    end;
  end;

  EndObj3D;

if not MemoryWritten then
begin
  WriteAndVerify($00400000 + $349898, 255);
  MemoryWritten := True;
end;

  // Дополнительный элемент
//  BeginObj3D;
//  Position3D(1.17, 0.223, 3.4319999);
//  Color3D($FFFFFF, 254, False, 0.0);
//  Scale3D(-0.086499996);
//  SetTexture(50);
//  DrawModel(38, 0, False);
//  DrawCube(0.3, 0.3, 0.3);
//  EndObj3D;



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
//
//  InitializeAllConfigs;
//
//  speedAngle := ArrowAngle - ArrowCurrentSpeed * ArrowKoef;
//
//  BeginObj3D;
//  Position3D(ArrowX, ArrowY, ArrowZ);
//  RotateX(ArrowRotateX);
//  RotateY(ArrowRotateY);
//  RotateZ(x + ArrowRotateZ);
//  SetTexture(0);
//  Color3D($ffffffff, 255, False, 0.0);
//  RotateY(speedAngle);
//  Scale3D(ArrowScale);
//  DrawModel(strelka, 0, False);
//  EndObj3D;

if Config_BGSD then  // ← ДОБАВИЛИ ПРОВЕРКУ
begin
  // Получаем значение ALS
  alsValue := GetALS;
  
if alsValue = 3 then
begin
  glDisable(GL_LIGHTING);
  
  // Верхняя половина - желтая
  BeginObj3D;
    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ

  Position3D(0.8958, 7.4479, 3.6386);
  RotateX(-103.0);
  RotateY(34.2);
  RotateZ(-7.0); // Поворот против часовой на 15 градусов
  Scale3D(0.002);
  Color3D($00FFFF, 255, False, 0.0); // Желтый
  SetTexture(0);
  Draw3DSemiCircle(5.2, 0, 180);
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ

  EndObj3D;

  // Нижняя половина - красная
  BeginObj3D;
      glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
  Position3D(0.8958, 7.4479, 3.6385);
  RotateX(-103.0);
  RotateY(34.2);
  RotateZ(-7.0); // Тот же поворот для совпадения
  Scale3D(0.002);
  Color3D($0000FF, 255, False, 0.0); // Красный
  SetTexture(0);
  Draw3DSemiCircle(5.2, 180, 360);
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
    Position3D(0.896, 7.4482, 3.641);
    RotateX(-103.0);
    RotateY(34.2);
    Scale3D(0.0002);
    Color3D(diskColor, 255, False, 0.0);
    SetTexture(0);
    Draw3DDisk(52);
    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
    EndObj3D;
  end;
end;

  // ======== ТЕКСТОВЫЕ 3D-ОБЪЕКТЫ ========
if debug then
begin
  // Проверяем значение по адресу Launcher.exe+8C07ECC (1 byte)
  if (PByte(Pointer(PCardinal(Pointer($00400000 + $348638))^ + $2D0))^ > 0) then
  begin
    for i := 0 to 35 do
      DrawObject(StaticData[i], i);
  end;
end;
//
//    if signalSequence <> '' then
//    begin
//      BeginObj3D;
//        glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
//
//      Position3D(1.0, 7.0, 3.75);
//      RotateX(-90);
//      Scale3D(0.01);
//      SetTexture(0);
//      Color3D($00FF00, 255, False, 0);
//      DrawText3D(0, 'SIG: ' + signalSequence);
//        glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
//
//      EndObj3D;
//    end;

  // ======== ОТЛАДОЧНАЯ ИНФОРМАЦИЯ ========
  if not debug then
  begin
    if CommandBuffer <> '' then
    begin
      BeginObj3D;
              glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
      Position3D(1.0, 7.0, 3.8);
      RotateX(-90);
      Scale3D(0.01);
      SetTexture(0);
      Color3D($FF0000, 255, False, 0);
      DrawText3D(0, 'CMD: ' + CommandBuffer);
              glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
      EndObj3D;
    end;
    
    // Отображаем текущую последовательность сигналов
    if signalSequence <> '' then
    begin
      BeginObj3D;
                    glDisable(GL_LIGHTING); // ← ДОБАВИТЬ
      Position3D(1.0, 7.0, 3.75);
      RotateX(-90);
      Scale3D(0.01);
      SetTexture(0);
      Color3D($00FF00, 255, False, 0);
      DrawText3D(0, 'SIG: ' + signalSequence);
                    glEnable(GL_LIGHTING); // ← ДОБАВИТЬ
      EndObj3D;
    end;
  end;
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

end.

