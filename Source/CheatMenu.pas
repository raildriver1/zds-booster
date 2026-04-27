unit CheatMenu;

interface
uses
  Windows, SysUtils, Classes, Variables, DrawFunc2D, DrawFunc3D, EngineUtils,
  KlubData, ShellAPI, Math, OpenGL, Advanced3D;

type
  TSlider = record
    Value: Single;
    MinValue: Single;
    MaxValue: Single;
    IsDragging: Boolean;
    HoverProgress: Single;
  end;
  
  TExpandableSection = record
    Expanded: Boolean;
    AnimProgress: Single;
  end;
  
  TWindow = record
    X, Y: Integer;
    Width, Height: Integer;
    IsDragging: Boolean;
    DragOffsetX, DragOffsetY: Integer;
    Title: string;
    Alpha: Single;
    TargetAlpha: Single;
    OriginalX, OriginalY: Integer;
    Scale: Single;
    TargetScale: Single;
    // Transform анимация
    TransformProgress: Single;
    TargetTransformProgress: Single;
    Rotation: Single;
    TargetRotation: Single;
    TranslateY: Single;
    TargetTranslateY: Single;
    // Drag анимация
    DragWidthExpansion: Single;
    TargetDragWidthExpansion: Single;
    ShadowIntensity: Single;
    TargetShadowIntensity: Single;
    OriginalWidth: Integer;
    // "Paper-drag" feel: окно немного отстаёт в наклоне за курсором,
    // как лист бумаги, который держишь сверху и тянешь.
    // PrevDragX — X-курсор в прошлом кадре (валидно только при IsDragging),
    // DragVelX  — сглаженная горизонтальная velocity курсора (low-pass).
    PrevDragX: Integer;
    DragVelX: Single;
    // Анимация появления
    SpawnDelay: Single;
  end;

  // ЯЗЫКИ
  TLanguage = (langRussian, langUkrainian, langEnglish);

  TCheatSettings = record
    // Общая яркость (глобальная)
    BrightnessSlider: TSlider;
    
    // Render
    Freecam: Boolean;
    FreecamSection: TExpandableSection;
    BasespeedSlider: TSlider;
    FastspeedSlider: TSlider;
    TurnspeedSlider: TSlider;
    
    MainCamera: Boolean;
    MainCameraSection: TExpandableSection;
    StepForwardSlider: TSlider;
    NewViewAngle: Boolean;              
    ViewAngleSlider: TSlider;           
    CameraSensitivity: Boolean;         
    CameraSensitivitySlider: TSlider;   

    // Освещение
    Lighting: Boolean;
    LightingSection: TExpandableSection;
    MainLightIntensitySlider: TSlider;      
    AdditionalLightIntensitySlider: TSlider; 
    CabinBrightnessSlider: TSlider;         
    CabinContrastSlider: TSlider;           
    SunOrbitRadiusSlider: TSlider;          
    SunHeightSlider: TSlider;               
    
    // World
    MaxVisibleDistance: Boolean;
    MaxVisibleDistanceSection: TExpandableSection;
    MaxVisibleDistanceSlider: TSlider;
    ShowWires: Boolean;        
    ShowDistantModels: Boolean;  
    ShowTrafficLights: Boolean; 
    
    NewSky: Boolean;

    // Подмена внутриигрового AbsoluteTime на системное время Windows
    // (toggle в WORLD-вкладке). Зеркалит InitSystemTimeEnable.
    SystemTimeEnable: Boolean;

    // Visual — engine-level post-process toggles (ZDS-Booster).
    FXAAEnable: Boolean;
    BloomEnable: Boolean;
    // PostFX render-graph (Phase 1 + Phase 2). Each toggle is mirrored to
    // its Init* global in Variables.pas which PostFX.pas re-reads every
    // frame, so flips are visible immediately with no restart.
    PostFXEnable: Boolean;     // master switch for the whole chain
    TonemapEnable: Boolean;    // ACES Filmic curve + sRGB roundtrip
    SharpenEnable: Boolean;    // CAS contrast-adaptive sharpening
    VignetteEnable: Boolean;   // corner darkening
    GrainEnable: Boolean;      // film-grain noise
    SSAOEnable: Boolean;       // screen-space ambient occlusion
    FogEnable: Boolean;        // depth-based atmospheric fog
    DOFEnable: Boolean;        // depth of field
    // Раскрывающаяся секция со списком 9 эффектов. Когда AnimProgress > 0
    // секция разворачивается с анимацией; высота секции вычисляется в
    // GetPostFXContentHeight, и WORLD-окно соответственно растёт.
    PostFXSection: TExpandableSection;

    // Shadow-копия 9 индивидуальных toggle, которая запоминается в момент
    // выключения мастера ("Графич. эффекты"), чтобы при следующем
    // включении восстановить ровно ту конфигурацию, что была у пользователя.
    // Init=False означает что shadow ещё ни разу не сохранялся (свежая
    // сессия / сразу после старта); в этом случае master-on восстанавливает
    // дефолты Variables.pas вместо shadow.
    PostFXSaved: record
      Init: Boolean;
      FXAA, Bloom, Tonemap, Sharpen, Vignette, Grain, SSAO, Fog, DOF: Boolean;
    end;

    // Locomotive
    NewClubPositions: Boolean;
    DeveloperMenu: Boolean;
    DeveloperSection: TExpandableSection;
    RA3Hover: Boolean;
  end;

  // Источник значения для кастомного 3D-текста — соответствует функциям из KlubData.
  TKlubSource = (
    ksSpeed, ksDistance, ksCurrentDate, ksCurrentTime, ksLimitSpeed,
    ksPressureTM, ksPressureUR, ksPressureTC, ksTrackNumber, ksCoords,
    ksAccel, ksTrafficLightsSeq, ksLimitSpeedValue, ksSpeedValue2,
    ksCurrentStation, ksChannel, ksTrackWithDir, ksTargetType
  );

  // Один пользовательский 3D-текст в кабине РА-3.
  // Локальные координаты кабины (как в DrawTextSimple/DrawAllInfoFields).
  TCustomText3D = record
    Source: TKlubSource;
    X, Y, Z: Single;
    RX, RY, RZ: Single; // углы в градусах
    Scale: Single;
    Visible: Boolean;
    Color: Cardinal;    // COLORREF, $FFFFFF = белый. Применяется в DrawCustomTextsRA3.
  end;

procedure InitCheatMenu; stdcall;
procedure DrawCheatMenu; stdcall;
procedure HandleMenuClick(X, Y: Integer); stdcall;
procedure HandleMenuHover(X, Y: Integer); stdcall;
procedure HandleMenuMouseUp; stdcall;
procedure ToggleMenu; stdcall;
procedure ApplyMenuPatch;
procedure RemoveMenuPatch;
function IsRA3HoverEnabled: Boolean;

// Кастомные 3D-тексты в кабине РА-3 (Меню разработчика → Custom Texts).
// Вызывать из RA3.DrawRA3 — рендерит все видимые элементы из CustomTexts.
procedure DrawCustomTextsRA3;

// Гизмо «как в Блендере» для редактирования выбранного текста — стрелки
// (Translate), кольца (Rotate) или кубики (Scale). Рисуется при наличии
// выбранного элемента (даже без F12-меню). Зовётся из RA3.DrawRA3 после
// DrawCustomTextsRA3.
procedure DrawGizmoRA3;
// Per-frame стейт-машина (hover/click/drag/release) — зовётся из RA3.DrawRA3
// до DrawGizmoRA3. Сам управляет ApplyMenuPatch/RemoveMenuPatch.
procedure UpdateGizmoFrameRA3;
// True если мышь над хэндлом гизмо или идёт drag — РА-3 использует это для
// удержания патча.
function IsGizmoActive: Boolean;

// Универсальная точка входа: рендерит кастомные тексты + гизмо ОДИН РАЗ
// за кадр в текущей cabin-local матрице. Безопасно вызывать из любого
// hook'а DLL (HookKLUB, DrawKPD3, DrawBLOCK, DrawRA3) — внутренний
// per-frame дедуплекс гарантирует один рендер в кадр.
procedure RenderCustomTextsAndGizmoForFrame; stdcall;

function GetFreecamBasespeed: Single; stdcall;
function GetFreecamFastspeed: Single; stdcall;
function GetFreecamTurnspeed: Single; stdcall;

procedure LoadConfig; stdcall;

implementation

uses
  EngineCore; // только в implementation — иначе circular reference (EngineCore uses CheatMenu)

var
  MenuVisible: Boolean = False;
  Settings: TCheatSettings;
  RenderWindow, WorldWindow, LocomotiveWindow, MenuWindow: TWindow;
  LastFrameTime: Cardinal = 0;
  
  // === ПЕРЕМЕННЫЕ ДЛЯ ЯЗЫКА ===
  CurrentLanguage: TLanguage = langRussian;
  
  // === ПЕРЕМЕННЫЕ ДЛЯ АНИМАЦИИ МЕНЮ ===
  MenuAnimationProgress: Single = 0.0;
  MenuTargetProgress: Single = 0.0;
  MenuAnimationSpeed: Single = 12.0;
  // Таймер от момента открытия — используется для каскадного появления окон (SpawnDelay)
  MenuOpenTimer: Single = 0.0;

  // === ОПТИМИЗАЦИЯ: Ограничение частоты применения настроек ===
  LastApplyTime: Cardinal = 0;
  ApplyInterval: Cardinal = 100;
  PendingApply: Boolean = False;
  
  // === ОПТИМИЗАЦИЯ: Ограничение частоты чтения конфига ===
  LastConfigReadTime: Cardinal = 0;
  ConfigReadInterval: Cardinal = 200;
  
  // Переменные для патча "Новые позиции КЛУБ"
  ClubPositionsPatched: Boolean = False;
  SpeedXAddr: Cardinal;
  AllowedSpeedAddr: Cardinal;
  ShuntingSpeedAddr: Cardinal;
  TrainSpeedAddr: Cardinal;
  TimeAddr: Cardinal;
  NumberAccelAddr: Cardinal;
  ReverseAddr: Cardinal;
  AdditionalAddr: Cardinal;
  RadiusAddr: Cardinal;
  
  // Адреса для освещения
  MainLightAddr: Cardinal;        
  AdditionalLightAddr: Cardinal;  
  SunOrbitAddr: Cardinal;         
  SunHeightAddr: Cardinal;        
  
  // Оригинальные значения освещения
  OrigMainLightValue: Single;
  OrigAdditionalLightValue: Single;
  OrigSunOrbitValue: Single;
  OrigSunHeightValue: Single;
  LightingValuesRead: Boolean = False;
  
  // Адреса для дальности (3 группы)
  WireAddrs: array[0..1] of Cardinal;        
  DistantModelAddr: Cardinal;                
  TrafficLightAddrs: array[0..2] of Cardinal; 
  
  // Оригинальные значения дальности
  OrigWireValues: array[0..1] of Single;
  OrigDistantModelValue: Single;
  OrigTrafficLightValues: array[0..2] of Single;
  DistanceValuesRead: Boolean = False;

  // Переменные для патча "Новый угол обзора"
  ViewAnglePatched: Boolean = False;
  ViewAngleNopAddr1: Cardinal;        
  ViewAngleNopAddr2: Cardinal;        
  ViewAngleSliderAddr: Cardinal;      
  OrigViewAngleBytes1: array[0..5] of Byte; 
  OrigViewAngleBytes2: array[0..6] of Byte; 
  OrigViewAngleValue: Single;
  ViewAngleValuesRead: Boolean = False;

  // Переменные для чувствительности камеры
  CameraSensitivityAddr: Cardinal;    
  OrigCameraSensitivityValue: Single;
  CameraSensitivityValuesRead: Boolean = False;

  // ОРИГИНАЛЬНЫЕ ЗНАЧЕНИЯ
  OrigSpeedXValue: array[0..3] of Byte = ($58, $39, $34, $BC);
  OrigAllowedSpeedValue: array[0..3] of Byte = ($58, $39, $34, $BC);
  OrigShuntingSpeedValue: array[0..3] of Byte = ($96, $43, $8B, $3D);
  OrigTrainSpeedValue: array[0..3] of Byte = ($96, $43, $8B, $3D);
  OrigTimeValue: array[0..3] of Byte = ($31, $08, $AC, $3C);
  OrigNumberAccelValue: array[0..3] of Byte = ($0A, $D7, $A3, $3C);
  OrigReverseValue: array[0..3] of Byte = ($0A, $D7, $A3, $BB);
  OrigAdditionalValue: array[0..3] of Byte = ($7F, $6A, $3C, $3D);
  OrigRadiusValue: array[0..9] of Byte = ($0A, $D7, $A3, $70, $3D, $0A, $D7, $A3, $F7, $BF);
  
  // НОВЫЕ ЗНАЧЕНИЯ
  NewSpeedXValue: array[0..3] of Byte = ($7B, $12, $83, $BB);
  NewAllowedSpeedValue: array[0..3] of Byte = ($7B, $12, $83, $BB);
  NewShuntingSpeedValue: array[0..3] of Byte = ($BD, $CA, $A1, $3D);
  NewTrainSpeedValue: array[0..3] of Byte = ($5A, $E3, $A5, $3D);
  NewTimeValue: array[0..3] of Byte = ($46, $60, $E5, $3C);
  NewNumberAccelValue: array[0..3] of Byte = ($91, $C2, $F5, $3C);
  NewReverseValue: array[0..3] of Byte = ($74, $12, $03, $3B);
  NewAdditionalValue: array[0..3] of Byte = ($E9, $FD, $54, $3D);
  NewRadiusValue: array[0..9] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00);

  // Переменные для патча меню
  MenuCallAddr: Cardinal;
  OrigMenuCallBytes: array[0..4] of Byte;
  MenuCallPatched: Boolean = False;

const
  ITEM_HEIGHT = 32;
  MARGIN = 16;
  HEADER_HEIGHT = 42;
  SLIDER_WIDTH = 180;
  BUTTON_SIZE = 24;
  CHECKBOX_SIZE = 18;
  CORNER_RADIUS = 12.0;
  DRAG_EXPANSION = 25.0; // Расширение окна при перетаскивании

  // Современные цвета
  COLOR_BACKGROUND = $1A1A1A;
  COLOR_SURFACE = $2A2A2A;
  COLOR_SURFACE_VARIANT = $343434;
  COLOR_PRIMARY = $0078D4;
  COLOR_PRIMARY_VARIANT = $005A9E;
  COLOR_ACCENT = $00BCF2;
  COLOR_ON_SURFACE = $E1E1E1;
  COLOR_ON_PRIMARY = $FFFFFF;
  COLOR_BORDER = $404040;
  COLOR_BORDER_LIGHT = $505050;
  COLOR_SUCCESS = $16C60C;
  COLOR_WARNING = $FF8C00;
  COLOR_SHADOW = $000000;

// === МАССИВЫ ТЕКСТОВ ДЛЯ РАЗНЫХ ЯЗЫКОВ ===
type
  TLanguageTexts = record
    // Заголовки окон
    RenderTitle: string;
    WorldTitle: string;
    LocomotiveTitle: string;
    MenuTitle: string;
    
    // Основные элементы
    FreeCameraText: string;
    MainCameraText: string;
    MaxDistanceText: string;
    NewSkyText: string;
    SystemTimeText: string;
    FXAAText: string;
    BloomText: string;
    PostFXText: string;
    TonemapText: string;
    SharpenText: string;
    VignetteText: string;
    GrainText: string;
    SSAOText: string;
    FogText: string;
    DOFText: string;
    ClubFixesText: string;
    DeveloperMenuText: string;
    RA3HoverText: string;

    // Подэлементы
    BaseSpeedText: string;
    FastSpeedText: string;
    StepForwardText: string;
    NewZoomText: string;
    SensitivityText: string;
    ValueText: string;
    DistanceText: string;
    WiresText: string;
    DistantModelsText: string;
    TrafficLightsText: string;
    
    // Языки
    LanguageText: string;
    RussianText: string;
    UkrainianText: string;
    EnglishText: string;
    
    // Информация
    InfoText: string;
  end;

const
  LanguageTexts: array[TLanguage] of TLanguageTexts = (
    // Русский
    (
      RenderTitle: 'RENDER';
      WorldTitle: 'WORLD';
      LocomotiveTitle: 'LOCOMOTIVE';
      MenuTitle: 'MENU';
      FreeCameraText: 'Свободная Камера';
      MainCameraText: 'Основная Камера';
      MaxDistanceText: 'Макс. дальность';
      NewSkyText: 'Новая логика неба';
      SystemTimeText: 'Системное время';
      FXAAText: 'Сглаживание FXAA';
      BloomText: 'Свечение (Bloom)';
      PostFXText: 'Графич. эффекты';
      TonemapText: 'Кинокорректор (ACES)';
      SharpenText: 'Резкость (CAS)';
      VignetteText: 'Виньетка';
      GrainText: 'Зернистость';
      SSAOText: 'Объёмная тень (SSAO)';
      FogText: 'Атмосферная дымка';
      DOFText: 'Глубина резкости';
      ClubFixesText: 'Исправления БИЛ-В';
      DeveloperMenuText: 'Меню разработчика';
      RA3HoverText: 'RA3 подсветка';
      BaseSpeedText: 'Базовая скорость';
      FastSpeedText: 'Скорость с Shift';
      StepForwardText: 'Шаг вперёд';
      NewZoomText: 'Новый Zoom';
      SensitivityText: 'Чувствительность';
      ValueText: 'Значение';
      DistanceText: 'Дальность (м.)';
      WiresText: 'Провода';
      DistantModelsText: 'Дальние модели';
      TrafficLightsText: 'Светофоры';
      LanguageText: 'Язык интерфейса';
      RussianText: 'Русский';
      UkrainianText: 'Українська';
      EnglishText: 'English';
      InfoText: 'ZDS-Booster v1.3 | vk.com/raildriver';
    ),
    // Украинский
    (
      RenderTitle: 'RENDER';
      WorldTitle: 'WORLD';
      LocomotiveTitle: 'LOCOMOTIVE';
      MenuTitle: 'MENU';
      FreeCameraText: 'Вільна Камера';
      MainCameraText: 'Основна Камера';
      MaxDistanceText: 'Макс. відстань';
      NewSkyText: 'Нова логіка неба';
      SystemTimeText: 'Системний час';
      FXAAText: 'Згладжування FXAA';
      BloomText: 'Сяйво (Bloom)';
      PostFXText: 'Граф. ефекти';
      TonemapText: 'Кінокорекція (ACES)';
      SharpenText: 'Різкість (CAS)';
      VignetteText: 'Віньєтка';
      GrainText: 'Зернистість';
      SSAOText: 'Об''ємна тінь (SSAO)';
      FogText: 'Атмосферна імла';
      DOFText: 'Глибина різкості';
      ClubFixesText: 'Виправлення БІЛ-В';
      DeveloperMenuText: 'Меню розробника';
      RA3HoverText: 'RA3 підсвітка';
      BaseSpeedText: 'Базова швидкість';
      FastSpeedText: 'Швидкість з Shift';
      StepForwardText: 'Крок вперед';
      NewZoomText: 'Новий Zoom';
      SensitivityText: 'Чутливість';
      ValueText: 'Значення';
      DistanceText: 'Відстань (м.)';
      WiresText: 'Проводи';
      DistantModelsText: 'Далекі моделі';
      TrafficLightsText: 'Світлофори';
      LanguageText: 'Мова інтерфейсу';
      RussianText: 'Русский';
      UkrainianText: 'Українська';
      EnglishText: 'English';
      InfoText: 'ZDS-Booster v1.3 | t.me/raildrive';
    ),
    // Английский
    (
      RenderTitle: 'RENDER';
      WorldTitle: 'WORLD';
      LocomotiveTitle: 'LOCOMOTIVE';
      MenuTitle: 'MENU';
      FreeCameraText: 'Free Camera';
      MainCameraText: 'Main Camera';
      MaxDistanceText: 'Max Distance';
      NewSkyText: 'New Sky Logic';
      SystemTimeText: 'System Time';
      FXAAText: 'FXAA Antialiasing';
      BloomText: 'Bloom Glow';
      PostFXText: 'Graphics FX';
      TonemapText: 'Filmic Tonemap (ACES)';
      SharpenText: 'Sharpen (CAS)';
      VignetteText: 'Vignette';
      GrainText: 'Film Grain';
      SSAOText: 'Ambient Occlusion';
      FogText: 'Atmospheric Fog';
      DOFText: 'Depth of Field';
      ClubFixesText: 'BIL-V Fixes';
      DeveloperMenuText: 'Developer Menu';
      RA3HoverText: 'RA3 Highlight';
      BaseSpeedText: 'Base Speed';
      FastSpeedText: 'Shift Speed';
      StepForwardText: 'Step Forward';
      NewZoomText: 'New Zoom';
      SensitivityText: 'Sensitivity';
      ValueText: 'Value';
      DistanceText: 'Distance (m.)';
      WiresText: 'Wires';
      DistantModelsText: 'Distant Models';
      TrafficLightsText: 'Traffic Lights';
      LanguageText: 'Interface Language';
      RussianText: 'Русский';
      UkrainianText: 'Українська';
      EnglishText: 'English';
      InfoText: 'ZDS-Booster v1.3 | t.me/raildrive';
    )
  );

// === ФУНКЦИЯ ПОЛУЧЕНИЯ ТЕКСТА ===
function GetText(TextType: string): string;
begin
  if TextType = 'RenderTitle' then Result := LanguageTexts[CurrentLanguage].RenderTitle
  else if TextType = 'WorldTitle' then Result := LanguageTexts[CurrentLanguage].WorldTitle
  else if TextType = 'LocomotiveTitle' then Result := LanguageTexts[CurrentLanguage].LocomotiveTitle
  else if TextType = 'MenuTitle' then Result := LanguageTexts[CurrentLanguage].MenuTitle
  else if TextType = 'FreeCameraText' then Result := LanguageTexts[CurrentLanguage].FreeCameraText
  else if TextType = 'MainCameraText' then Result := LanguageTexts[CurrentLanguage].MainCameraText
  else if TextType = 'MaxDistanceText' then Result := LanguageTexts[CurrentLanguage].MaxDistanceText
  else if TextType = 'NewSkyText' then Result := LanguageTexts[CurrentLanguage].NewSkyText
  else if TextType = 'SystemTimeText' then Result := LanguageTexts[CurrentLanguage].SystemTimeText
  else if TextType = 'FXAAText' then Result := LanguageTexts[CurrentLanguage].FXAAText
  else if TextType = 'BloomText' then Result := LanguageTexts[CurrentLanguage].BloomText
  else if TextType = 'PostFXText' then Result := LanguageTexts[CurrentLanguage].PostFXText
  else if TextType = 'TonemapText' then Result := LanguageTexts[CurrentLanguage].TonemapText
  else if TextType = 'SharpenText' then Result := LanguageTexts[CurrentLanguage].SharpenText
  else if TextType = 'VignetteText' then Result := LanguageTexts[CurrentLanguage].VignetteText
  else if TextType = 'GrainText' then Result := LanguageTexts[CurrentLanguage].GrainText
  else if TextType = 'SSAOText' then Result := LanguageTexts[CurrentLanguage].SSAOText
  else if TextType = 'FogText' then Result := LanguageTexts[CurrentLanguage].FogText
  else if TextType = 'DOFText' then Result := LanguageTexts[CurrentLanguage].DOFText
  else if TextType = 'ClubFixesText' then Result := LanguageTexts[CurrentLanguage].ClubFixesText
  else if TextType = 'DeveloperMenuText' then Result := LanguageTexts[CurrentLanguage].DeveloperMenuText
  else if TextType = 'RA3HoverText' then Result := LanguageTexts[CurrentLanguage].RA3HoverText
  else if TextType = 'BaseSpeedText' then Result := LanguageTexts[CurrentLanguage].BaseSpeedText
  else if TextType = 'FastSpeedText' then Result := LanguageTexts[CurrentLanguage].FastSpeedText
  else if TextType = 'StepForwardText' then Result := LanguageTexts[CurrentLanguage].StepForwardText
  else if TextType = 'NewZoomText' then Result := LanguageTexts[CurrentLanguage].NewZoomText
  else if TextType = 'SensitivityText' then Result := LanguageTexts[CurrentLanguage].SensitivityText
  else if TextType = 'ValueText' then Result := LanguageTexts[CurrentLanguage].ValueText
  else if TextType = 'DistanceText' then Result := LanguageTexts[CurrentLanguage].DistanceText
  else if TextType = 'WiresText' then Result := LanguageTexts[CurrentLanguage].WiresText
  else if TextType = 'DistantModelsText' then Result := LanguageTexts[CurrentLanguage].DistantModelsText
  else if TextType = 'TrafficLightsText' then Result := LanguageTexts[CurrentLanguage].TrafficLightsText
  else if TextType = 'LanguageText' then Result := LanguageTexts[CurrentLanguage].LanguageText
  else if TextType = 'RussianText' then Result := LanguageTexts[CurrentLanguage].RussianText
  else if TextType = 'UkrainianText' then Result := LanguageTexts[CurrentLanguage].UkrainianText
  else if TextType = 'EnglishText' then Result := LanguageTexts[CurrentLanguage].EnglishText
  else if TextType = 'InfoText' then Result := LanguageTexts[CurrentLanguage].InfoText
  else Result := TextType;
end;

// === ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ===
function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

function MinF(A, B: Single): Single;
begin
  if A < B then Result := A else Result := B;
end;

function MaxF(A, B: Single): Single;
begin
  if A > B then Result := A else Result := B;
end;

function LerpF(A, B, T: Single): Single;
begin
  Result := A + (B - A) * T;
end;

function LerpColor(ColorA, ColorB: Integer; T: Single): Integer;
var
  RA, GA, BA: Byte;
  RB, GB, BB: Byte;
  RT, GT, BT: Byte;
begin
  // Извлекаем компоненты цвета A
  RA := ColorA and $FF;
  GA := (ColorA shr 8) and $FF;
  BA := (ColorA shr 16) and $FF;
  
  // Извлекаем компоненты цвета B
  RB := ColorB and $FF;
  GB := (ColorB shr 8) and $FF;
  BB := (ColorB shr 16) and $FF;
  
  // Интерполируем каждый компонент
  RT := Round(RA + (RB - RA) * T);
  GT := Round(GA + (GB - GA) * T);
  BT := Round(BA + (BB - BA) * T);
  
  // Собираем результирующий цвет
  Result := RT or (GT shl 8) or (BT shl 16);
end;

function EaseOutBack(T: Single): Single;
var
  c1, c3: Single;
begin
  c1 := 1.70158;
  c3 := c1 + 1;
  Result := 1 + c3 * Power(T - 1, 3) + c1 * Power(T - 1, 2);
end;

function EaseOutCubic(T: Single): Single;
begin
  T := T - 1;
  Result := 1 + T * T * T;
end;

function EaseInOutQuart(T: Single): Single;
begin
  if T < 0.5 then
    Result := 8 * T * T * T * T
  else
    Result := 1 - Power(-2 * T + 2, 4) / 2;
end;

function EaseOutElastic(T: Single): Single;
var
  c4: Single;
begin
  c4 := (2 * PI) / 3;
  
  if T = 0 then
    Result := 0
  else if T = 1 then
    Result := 1
  else
    Result := Power(2, -10 * T) * Sin((T * 10 - 0.75) * c4) + 1;
end;

function GetFreecamBasespeed: Single; stdcall;
begin
  Result := Settings.BasespeedSlider.Value;
end;

function GetFreecamFastspeed: Single; stdcall;
begin
  Result := Settings.FastspeedSlider.Value;
end;

function GetFreecamTurnspeed: Single; stdcall;
begin
  Result := Settings.TurnspeedSlider.Value;
end;

// Современная отрисовка текста с тенью
procedure DrawModernText(X, Y: Integer; Text: string; Color: Integer; Alpha: Integer = 255; Size: Single = 0.8);
begin
  // Тень
  DrawText2D(0, X + 1, Y + 1, Text, COLOR_SHADOW, Alpha div 4, Size);
  // Основной текст
  DrawText2D(0, X, Y, Text, Color, Alpha, Size);
end;

// Простая но эффективная отрисовка прямоугольника с границами
procedure DrawStyledRect(X, Y, Width, Height: Integer; Color: Integer; Alpha: Integer; Filled: Boolean; BorderColor: Integer = 0);
begin
  if Filled then
    DrawRectangle2D(X, Y, Width, Height, Color, Alpha, True);
  
  // Рамка
  DrawRectangle2D(X, Y, Width, Height, BorderColor, Alpha div 2, False);
end;

// Отрисовка тени
procedure DrawShadow(X, Y, Width, Height: Integer; Alpha: Integer; Intensity: Single = 1.0);
var
  ShadowOffset: Integer;
  ShadowAlpha: Integer;
  i: Integer;
begin
  ShadowOffset := Round(6 * Intensity);
  
  for i := 0 to Round(4 * Intensity) do
  begin
    ShadowAlpha := Round(Alpha * Intensity / (8 + i * 2));
    DrawRectangle2D(X + ShadowOffset + i, Y + ShadowOffset + i, Width, Height, COLOR_SHADOW, ShadowAlpha, True);
  end;
end;

function InRect(X, Y, RX, RY, RW, RH: Integer): Boolean;
begin
  Result := (X >= RX) and (X <= RX + RW) and (Y >= RY) and (Y <= RY + RH);
end;

function FormatValue(Value: Single): string;
var
  OldSep: Char;
begin
  // Принудительно пишем точку как разделитель — иначе на русской локали
  // получим "0,5" и при следующей загрузке поплывёт парсинг.
  OldSep := DecimalSeparator;
  try
    DecimalSeparator := '.';
    if Value >= 1000 then
      Result := IntToStr(Round(Value))
    else if Value = Round(Value) then
      Result := IntToStr(Round(Value))
    else
      Result := Format('%.2f', [Value]);
  finally
    DecimalSeparator := OldSep;
  end;
end;

// Парсинг дробного значения из конфига — принимаем и точку, и запятую.
function ParseFloat(const S: string; DefaultValue: Single): Single;
var
  OldSep: Char;
  Normalized: string;
begin
  Normalized := StringReplace(S, ',', '.', [rfReplaceAll]);
  OldSep := DecimalSeparator;
  try
    DecimalSeparator := '.';
    Result := StrToFloatDef(Normalized, DefaultValue);
  finally
    DecimalSeparator := OldSep;
  end;
end;

// ===========================================================================
//  CUSTOM 3D TEXTS (Меню разработчика → Custom Texts)
// ===========================================================================

const
  KLUB_SOURCE_COUNT = 18;
  CUSTOM_TEXT_DEFAULT_SCALE = 0.007;

var
  CustomTexts: array of TCustomText3D;
  CustomTextSelectedIdx: Integer = -1;

  // Слайдеры редактора — общие, переиспользуются для текущего выбранного текста.
  CustomXSlider, CustomYSlider, CustomZSlider: TSlider;
  CustomRXSlider, CustomRYSlider, CustomRZSlider: TSlider;
  CustomScaleSlider: TSlider;

type
  TGizmoMode = (gmTranslate, gmRotate, gmScale);

const
  GIZMO_AXIS_NONE      = -1;
  GIZMO_AXIS_X         = 0;
  GIZMO_AXIS_Y         = 1;
  GIZMO_AXIS_Z         = 2;
  GIZMO_AXIS_VIEW      = 3;      // центральный «свободный» хэндл (drag в плоскости экрана)
  GIZMO_PICK_RADIUS    = 22;     // px — порог попадания мышью в хэндл оси
  GIZMO_PICK_CENTER_R  = 14;     // px — порог попадания в центральный хэндл
  GIZMO_ARROW_LEN      = 0.06;   // длина стрелки в локальных координатах кабины
  GIZMO_ROTATE_RADIUS  = 0.05;   // радиус кругов
  GIZMO_BOX_TRANSLATE  = 0.012;  // размер кубика на конце для translate
  GIZMO_BOX_SCALE      = 0.020;  // больше для scale, чтоб отличать визуально
  GIZMO_BOX_CENTER     = 0.013;  // размер центрального кубика (free-move)
  GIZMO_RING_SAMPLES   = 32;     // точек на кольце (для пика ротации)
  // Снап-шаги (зажми Shift во время drag)
  GIZMO_SNAP_TRANSLATE = 0.005;  // 5 мм по локальной системе кабины
  GIZMO_SNAP_ROTATE    = 5.0;    // 5° на щелчок
  GIZMO_SNAP_SCALE     = 0.05;   // шаг 0.05× по масштабу
  // Множитель чувствительности при зажатом Ctrl (precision-режим)
  GIZMO_PRECISION_FACTOR = 0.2;

var
  GizmoMode: TGizmoMode = gmTranslate;
  GizmoActiveAxis: Integer = GIZMO_AXIS_NONE;   // 0/1/2/3 во время drag, иначе -1
  HoveredGizmoAxis: Integer = GIZMO_AXIS_NONE;  // 0/1/2/3 если мышь над хэндлом
  GizmoDragging: Boolean = False;
  GizmoLastLMB: Boolean = False;                // для rising-edge детекта клика
  GizmoLastEsc: Boolean = False;                // для rising-edge детекта Esc
  GizmoPatchActive: Boolean = False;            // мы ли держим ApplyMenuPatch
  GizmoDragMouseStartX, GizmoDragMouseStartY: Integer;
  // На начало драга: для translate — стартовые X/Y/Z, для rotate — стартовые RX/RY/RZ.
  GizmoDragValueStart: array[0..2] of Single;
  GizmoDragScaleStart: Single;
  // Полный снимок трансформа на начало drag — для отмены по Esc.
  GizmoCancelX, GizmoCancelY, GizmoCancelZ: Single;
  GizmoCancelRX, GizmoCancelRY, GizmoCancelRZ: Single;
  GizmoCancelScale: Single;
  // Для rotate: angular tracking в стиле Блендера — мышь крутится вокруг центра гизмо.
  GizmoRotPrevAngle: Single;   // последний просэмплированный угол atan2 от центра (рад)
  GizmoRotAccum: Single;       // сумма приращений с unwrap'ом (рад) от начала драга
  GizmoRotStartAngle: Single;  // угол курсора в момент начала драга — для рисовки арки
  // Кэш экранных позиций — заполняется в DrawGizmoRA3, читается в PickGizmoAxis.
  GizmoOriginScreen: TPoint;
  GizmoTipScreen: array[0..2] of TPoint;        // концы осей translate/scale
  GizmoRingScreen: array[0..2, 0..GIZMO_RING_SAMPLES - 1] of TPoint; // 3 кольца × 32 точки
  GizmoCacheValid: Boolean = False;

// ===========================================================================
//  DEV EDITOR (полноэкранный редактор кастомных текстов)
// ===========================================================================
// Включается тумблером в меню разработчика. Когда активен, 4 стандартных окна
// прячутся и вместо них рисуется фуллскрин-редактор: список карточек, в каждой —
// Source-combobox, X/Y/Z/RX/RY/RZ/Scale с +/− кнопками, color-swatch, Visible,
// Delete. Поддержка hover/scroll/выделения. При выделении карточки гизмо
// работает на её тексте — ровно как раньше.

const
  DE_PAD_X            = 20;     // отступ слева/справа от края экрана
  DE_PAD_Y            = 12;
  DE_HEADER_H         = 56;     // верхняя плашка с заголовком и кнопкой Back
  DE_TOOLBAR_H        = 44;     // тулбар с Add Text + counter
  DE_CARD_H           = 76;     // высота одной карточки (с местом под column-labels)
  DE_CARD_GAP         = 6;
  DE_SCROLLBAR_W      = 12;
  DE_BTN_SMALL        = 22;     // -/+ кнопки (квадратные)
  DE_VAL_W            = 78;     // ширина числового поля внутри ячейки
  DE_CELL_GAP         = 6;      // промежуток между соседними ячейками
  // Внутренняя ширина ячейки = btn + 2 + val + 2 + btn = 22+2+78+2+22 = 126
  // С промежутком DE_CELL_GAP — общий шаг = 132 px
  DE_CELL_W           = 132;    // полный шаг ячейки (включая gap)
  DE_SOURCE_W         = 150;    // ширина Source-combobox
  DE_IDX_W            = 38;     // ширина колонки #N
  DE_SWATCH_W         = 44;
  DE_VISIBLE_W        = 38;
  DE_DELETE_W         = 38;
  DE_COMBO_DROP_H     = 22;     // высота строки в выпадашке combobox
  DE_PICKER_SIZE      = 28;     // сторона цветного квадратика в палитре
  DE_PICKER_COLS      = 6;      // 6×3 = 18 цветов
  DE_PICKER_ROWS      = 3;
  DE_AUTO_REPEAT_DELAY = 350;   // мс до начала auto-repeat для +/-
  DE_AUTO_REPEAT_RATE  = 50;    // мс между тиками

  // Коды hot-spot'ов (button id внутри карточки), одно из ниже:
  DE_HOT_NONE     = 0;
  DE_HOT_SOURCE   = 1;
  DE_HOT_X_MINUS  = 10; DE_HOT_X_PLUS  = 11;
  DE_HOT_Y_MINUS  = 12; DE_HOT_Y_PLUS  = 13;
  DE_HOT_Z_MINUS  = 14; DE_HOT_Z_PLUS  = 15;
  DE_HOT_RX_MINUS = 20; DE_HOT_RX_PLUS = 21;
  DE_HOT_RY_MINUS = 22; DE_HOT_RY_PLUS = 23;
  DE_HOT_RZ_MINUS = 24; DE_HOT_RZ_PLUS = 25;
  DE_HOT_S_MINUS  = 30; DE_HOT_S_PLUS  = 31;
  DE_HOT_COLOR    = 40;
  DE_HOT_VISIBLE  = 41;
  DE_HOT_DELETE   = 42;
  DE_HOT_BACK     = 100;
  DE_HOT_ADD      = 101;
  DE_HOT_MODE_T   = 110;  // Move / Translate
  DE_HOT_MODE_R   = 111;  // Rotate
  DE_HOT_MODE_S   = 112;  // Scale

  // Шаги +/− (одиночный клик)
  DE_STEP_POS    = 0.005;
  DE_STEP_ANG    = 1.0;
  DE_STEP_SCALE  = 0.001;

var
  DevEditorVisible: Boolean = False;
  DevEditorScrollY: Integer = 0;             // px смещение прокрутки списка
  DevEditorMaxScroll: Integer = 0;           // вычисляется в Draw, читается в HandleClick
  DevEditorViewportTop: Integer = 0;         // вычисляется в Draw — y нач. области списка
  DevEditorViewportBottom: Integer = 0;
  DevEditorScrollbarDragging: Boolean = False;
  DevEditorScrollbarDragOffsetY: Integer = 0;
  // hover: индекс карточки и hot-spot ID внутри неё (0 = карточка целиком/пусто)
  DevEditorHoveredCard: Integer = -1;
  DevEditorHoveredHotspot: Integer = DE_HOT_NONE;
  // Combobox state
  DevEditorComboboxOpen: Boolean = False;
  DevEditorComboboxCardIdx: Integer = -1;
  // Color picker state
  DevEditorColorPickerOpen: Boolean = False;
  DevEditorColorPickerCardIdx: Integer = -1;
  // Auto-repeat для +/- кнопок: какой hot-spot зажат, на какой карточке, и когда last-fire
  DevEditorBtnHeldHotspot: Integer = DE_HOT_NONE;
  DevEditorBtnHeldCardIdx: Integer = -1;
  DevEditorBtnHeldStartTime: Cardinal = 0;
  DevEditorBtnLastFireTime: Cardinal = 0;
  // Палитра для color picker (18 цветов)
  DevEditorPalette: array[0..17] of Cardinal = (
    $FFFFFF, $C0C0C0, $808080, $404040, $0000FF, $00FF00,
    $FF0000, $00FFFF, $FF00FF, $FFFF00, $0080FF, $00FF80,
    $8000FF, $FF8000, $80FF80, $80FFFF, $FF80FF, $FF8080
  );

procedure DrawDevEditor; forward;
procedure HandleDevEditorClick(MX, MY: Integer); forward;
procedure HandleDevEditorMouseUp; forward;
procedure UpdateDevEditorPerFrame; forward;
function DevEditorPointInUI(MX, MY: Integer): Boolean; forward;

// Forward — определения ниже в этом же файле; гизмо-обработчики используют их раньше.
procedure SaveConfig; forward;
function PointInAnyMenuWindow(X, Y: Integer): Boolean; forward;

function KlubSourceName(S: TKlubSource): string;
begin
  case S of
    ksSpeed:             Result := 'Speed';
    ksDistance:          Result := 'Distance';
    ksCurrentDate:       Result := 'Date';
    ksCurrentTime:       Result := 'Time';
    ksLimitSpeed:        Result := 'Limit Speed';
    ksPressureTM:        Result := 'Pressure TM';
    ksPressureUR:        Result := 'Pressure UR';
    ksPressureTC:        Result := 'Pressure TC';
    ksTrackNumber:       Result := 'Track #';
    ksCoords:            Result := 'Coords';
    ksAccel:             Result := 'Accel';
    ksTrafficLightsSeq:  Result := 'Lights Seq';
    ksLimitSpeedValue:   Result := 'Limit (int)';
    ksSpeedValue2:       Result := 'Speed (f)';
    ksCurrentStation:    Result := 'Station';
    ksChannel:           Result := 'Channel';
    ksTrackWithDir:      Result := 'Track Dir';
    ksTargetType:        Result := 'Target Type';
  else
    Result := '?';
  end;
end;

// Безопасное чтение значения KlubData — все исключения проглатываем.
function GetKlubSourceValue(S: TKlubSource): string;
begin
  try
    case S of
      ksSpeed:             Result := GetSpeed;
      ksDistance:          Result := GetDistance;
      ksCurrentDate:       Result := GetCurrentDate;
      ksCurrentTime:       Result := GetCurrentTime;
      ksLimitSpeed:        Result := GetLimitSpeed;
      ksPressureTM:        Result := GetPressureTM;
      ksPressureUR:        Result := GetPressureUR;
      ksPressureTC:        Result := GetPressureTC;
      ksTrackNumber:       Result := GetTrackNumber;
      ksCoords:            Result := GetCoordinatesFormatted;
      ksAccel:             Result := GetAcceleration;
      ksTrafficLightsSeq:  Result := GetTrafficLightsSequence;
      ksLimitSpeedValue:   Result := IntToStr(GetLimitSpeedValue);
      ksSpeedValue2:       Result := Format('%.2f', [GetSpeedValue2]);
      ksCurrentStation:    Result := GetCurrentStation;
      ksChannel:           Result := GetChannel;
      ksTrackWithDir:      Result := GetTrackWithDirection;
      ksTargetType:        Result := GetTargetType;
    else
      Result := '';
    end;
  except
    Result := '';
  end;
end;

// Сброс range/значений редакторных слайдеров.
procedure InitCustomTextSliders;
  procedure InitS(var S: TSlider; Min, Max, Val: Single);
  begin
    S.Value := Val;
    S.MinValue := Min;
    S.MaxValue := Max;
    S.HoverProgress := 0.0;
    S.IsDragging := False;
  end;
begin
  InitS(CustomXSlider,    -1.0,   1.0,  0.0);
  InitS(CustomYSlider,    -1.0,   1.0,  0.0);
  InitS(CustomZSlider,    -0.5,   1.0,  0.2);
  InitS(CustomRXSlider, -180.0, 180.0, -90.0); // дефолт как в DrawTextSimple
  InitS(CustomRYSlider, -180.0, 180.0,  0.0);
  InitS(CustomRZSlider, -180.0, 180.0,  0.0);
  InitS(CustomScaleSlider, 0.001, 0.05, CUSTOM_TEXT_DEFAULT_SCALE);
end;

procedure SyncSlidersFromSelected;
begin
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then Exit;
  with CustomTexts[CustomTextSelectedIdx] do
  begin
    CustomXSlider.Value     := X;
    CustomYSlider.Value     := Y;
    CustomZSlider.Value     := Z;
    CustomRXSlider.Value    := RX;
    CustomRYSlider.Value    := RY;
    CustomRZSlider.Value    := RZ;
    CustomScaleSlider.Value := Scale;
  end;
end;

procedure WriteSlidersToSelected;
begin
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then Exit;
  with CustomTexts[CustomTextSelectedIdx] do
  begin
    X     := CustomXSlider.Value;
    Y     := CustomYSlider.Value;
    Z     := CustomZSlider.Value;
    RX    := CustomRXSlider.Value;
    RY    := CustomRYSlider.Value;
    RZ    := CustomRZSlider.Value;
    Scale := CustomScaleSlider.Value;
  end;
end;

// True если хоть один слайдер из редакторных сейчас перетаскивается.
function AnyCustomSliderDragging: Boolean;
begin
  Result := CustomXSlider.IsDragging or CustomYSlider.IsDragging or CustomZSlider.IsDragging or
            CustomRXSlider.IsDragging or CustomRYSlider.IsDragging or CustomRZSlider.IsDragging or
            CustomScaleSlider.IsDragging;
end;

// =====================================================================
//  Per-loco конфиги кастомных текстов
//  Путь: data\<loc_name>\<loc_number>\raildriver\custom_texts.cfg
// =====================================================================
// Каждое сочетание (тип локомотива × LocNum) получает свой файл с массивом
// CustomTexts. При смене локо/состава — прозрачно перезагружаем (раз в кадр
// в DrawCheatMenu). При изменении CustomTexts (Add/Delete/гизмо/+/-) пишем
// туда же — SaveConfig после сохранения zdbooster.cfg ещё пишет per-loco файл.
// Глобальный zdbooster.cfg остаётся для обратной совместимости (он содержит
// настройки бустера + тексты как fallback при первом запуске на этой кабине).

var
  // Кэш — чтобы детектить смену локомотива и перезагрузиться один раз.
  LastLocoIdent: string = '';
  // Throttling: GetLocNum читает settings.ini И пишет лог каждый раз — нельзя
  // дёргать каждый кадр. Проверяем смену локо раз в LOCO_CHECK_INTERVAL мс.
  LastLocoCheckTime: Cardinal = 0;
const
  LOCO_CHECK_INTERVAL = 2000; // 2 секунды между проверками смены локо

// Резолв папки локомотива: учитываем РА3, у которого LocNum содержит "RA3"
// и который не зашит в GetLocomotiveFolder (там case по числовому типу).
// Все остальные кабины — берём папку через GetLocomotiveFolder(GetLocomotiveTypeFromMemory).
function ResolveLocoFolder(const Num: string): string;
var
  LocType: Integer;
begin
  if Pos('RA3', UpperCase(Num)) > 0 then
  begin
    Result := 'ra3';
    Exit;
  end;
  try
    LocType := GetLocomotiveTypeFromMemory;
    Result := GetLocomotiveFolder(LocType);
  except
    Result := '';
  end;
  if Result = '' then Result := 'unknown';
end;

// Нормализуем номер для использования как имя папки. Поддерживает РА3 (например
// "RA3-068" → "RA3-068") — оставляем как есть, иначе используем как есть с
// fallback "000" если пусто.
function ResolveLocoNumber: string;
begin
  try
    Result := GetLocNum;
  except
    Result := '';
  end;
  if Result = '' then Result := '000';
end;

function GetCurrentLocoIdent: string;
var
  Folder, Num: string;
begin
  try
    Num := ResolveLocoNumber;
    Folder := ResolveLocoFolder(Num);
    Result := Folder + '|' + Num;
  except
    // Если что-то пошло не так — возвращаем пусто, тогда reload не сработает.
    Result := '';
  end;
end;

function GetCustomTextsConfigPath: string;
var
  Folder, Num: string;
begin
  // Формат пути: data\<loc_name>\<loc_number>\raildriver\custom_texts.cfg
  //   <loc_name>   — папка локомотива (ra3, chs7, vl80t, ...). Для РА3 жёстко
  //                  ставим 'ra3' — у неё нет числового LocomotiveType.
  //   <loc_number> — номер из settings.ini (LocNum), нормализован до непустой строки.
  //   custom_texts.cfg — фиксированное имя файла внутри per-loco/per-num папки.
  try
    Num := ResolveLocoNumber;
    Folder := ResolveLocoFolder(Num);
    Result := 'data\' + Folder + '\' + Num + '\raildriver\custom_texts.cfg';
  except
    Result := '';
  end;
end;

// Создаёт директорию (со всеми родителями).
function EnsureDirExists(const Path: string): Boolean;
var
  Dir: string;
begin
  Result := False;
  Dir := ExtractFilePath(Path);
  if Dir = '' then begin Result := True; Exit; end;
  if DirectoryExists(Dir) then begin Result := True; Exit; end;
  Result := ForceDirectories(Dir);
end;

// Сохраняем CustomTexts в per-loco файл. Формат тот же, что в SaveConfig
// для совместимости логики парсинга.
procedure SaveCustomTextsForCurrentLoco;
var
  Path: string;
  F: TextFile;
  i: Integer;
begin
  Path := GetCustomTextsConfigPath;
  if Path = '' then Exit;
  if not EnsureDirExists(Path) then Exit;
  try
    AssignFile(F, Path);
    Rewrite(F);
    try
      WriteLn(F, '# ZDSimulator Booster — Custom texts for this loco/LocNum.');
      WriteLn(F, '# Auto-generated. Edit via in-game F12 → Меню разработчика.');
      WriteLn(F, 'custom_text_count: ' + IntToStr(Length(CustomTexts)));
      for i := 0 to Length(CustomTexts) - 1 do
      begin
        WriteLn(F, 'ct' + IntToStr(i) + '_source: ' + IntToStr(Integer(CustomTexts[i].Source)));
        WriteLn(F, 'ct' + IntToStr(i) + '_x: '      + FormatValue(CustomTexts[i].X));
        WriteLn(F, 'ct' + IntToStr(i) + '_y: '      + FormatValue(CustomTexts[i].Y));
        WriteLn(F, 'ct' + IntToStr(i) + '_z: '      + FormatValue(CustomTexts[i].Z));
        WriteLn(F, 'ct' + IntToStr(i) + '_rx: '     + FormatValue(CustomTexts[i].RX));
        WriteLn(F, 'ct' + IntToStr(i) + '_ry: '     + FormatValue(CustomTexts[i].RY));
        WriteLn(F, 'ct' + IntToStr(i) + '_rz: '     + FormatValue(CustomTexts[i].RZ));
        WriteLn(F, 'ct' + IntToStr(i) + '_scale: '  + FormatValue(CustomTexts[i].Scale));
        WriteLn(F, 'ct' + IntToStr(i) + '_color: '  + IntToStr(Integer(CustomTexts[i].Color)));
        if CustomTexts[i].Visible then
          WriteLn(F, 'ct' + IntToStr(i) + '_visible: 1')
        else
          WriteLn(F, 'ct' + IntToStr(i) + '_visible: 0');
      end;
    finally
      CloseFile(F);
    end;
  except
    // Не валим программу из-за проблем с записью.
  end;
end;

// Загружаем CustomTexts из per-loco файла. Если файла нет — оставляем как есть
// (т.е. содержимое последнего LoadConfig'а / то что было в предыдущей кабине).
// Возвращает True если файл был и загрузка прошла.
function LoadCustomTextsForCurrentLoco: Boolean;
var
  Path, Line, Key, Value: string;
  F: TextFile;
  ColonPos, CTCount, CTIdx, SrcVal, USCorePos: Integer;
  CTPrefix: string;
begin
  Result := False;
  Path := GetCustomTextsConfigPath;
  if Path = '' then Exit;
  if not FileExists(Path) then Exit;
  try
    AssignFile(F, Path);
    Reset(F);
    try
      // Очищаем текущие тексты перед загрузкой нового набора.
      SetLength(CustomTexts, 0);
      CustomTextSelectedIdx := -1;
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        Line := Trim(Line);
        if (Line = '') or (Line[1] = '#') or (Line[1] = ';') then Continue;
        ColonPos := Pos(':', Line);
        if ColonPos < 1 then Continue;
        Key := LowerCase(Trim(Copy(Line, 1, ColonPos - 1)));
        Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));

        if Key = 'custom_text_count' then
        begin
          CTCount := StrToIntDef(Value, 0);
          if CTCount < 0 then CTCount := 0;
          if CTCount > 256 then CTCount := 256;
          SetLength(CustomTexts, CTCount);
          for CTIdx := 0 to CTCount - 1 do
          begin
            CustomTexts[CTIdx].Source  := ksSpeed;
            CustomTexts[CTIdx].X       := 0.0;
            CustomTexts[CTIdx].Y       := 0.0;
            CustomTexts[CTIdx].Z       := 0.2;
            CustomTexts[CTIdx].RX      := -90.0;
            CustomTexts[CTIdx].RY      := 0.0;
            CustomTexts[CTIdx].RZ      := 0.0;
            CustomTexts[CTIdx].Scale   := CUSTOM_TEXT_DEFAULT_SCALE;
            CustomTexts[CTIdx].Visible := True;
            CustomTexts[CTIdx].Color   := $FFFFFF;
          end;
        end
        else if (Length(Key) > 2) and (Copy(Key, 1, 2) = 'ct') then
        begin
          USCorePos := Pos('_', Key);
          if USCorePos < 4 then Continue;
          CTIdx := StrToIntDef(Copy(Key, 3, USCorePos - 3), -1);
          if (CTIdx < 0) or (CTIdx >= Length(CustomTexts)) then Continue;
          CTPrefix := Copy(Key, USCorePos + 1, Length(Key));
          if CTPrefix = 'source' then
          begin
            SrcVal := StrToIntDef(Value, 0);
            if (SrcVal >= 0) and (SrcVal < KLUB_SOURCE_COUNT) then
              CustomTexts[CTIdx].Source := TKlubSource(SrcVal);
          end
          else if CTPrefix = 'color'   then CustomTexts[CTIdx].Color   := Cardinal(StrToIntDef(Value, $FFFFFF))
          else if CTPrefix = 'x'       then CustomTexts[CTIdx].X       := ParseFloat(Value, 0.0)
          else if CTPrefix = 'y'       then CustomTexts[CTIdx].Y       := ParseFloat(Value, 0.0)
          else if CTPrefix = 'z'       then CustomTexts[CTIdx].Z       := ParseFloat(Value, 0.2)
          else if CTPrefix = 'rx'      then CustomTexts[CTIdx].RX      := ParseFloat(Value, -90.0)
          else if CTPrefix = 'ry'      then CustomTexts[CTIdx].RY      := ParseFloat(Value, 0.0)
          else if CTPrefix = 'rz'      then CustomTexts[CTIdx].RZ      := ParseFloat(Value, 0.0)
          else if CTPrefix = 'scale'   then CustomTexts[CTIdx].Scale   := ParseFloat(Value, CUSTOM_TEXT_DEFAULT_SCALE)
          else if CTPrefix = 'visible' then CustomTexts[CTIdx].Visible := (Value = '1');
        end;
      end;
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    // Игнорим — оставим текущие тексты.
  end;
  if (CustomTextSelectedIdx >= Length(CustomTexts)) then
    CustomTextSelectedIdx := -1;
end;

// Per-frame: проверяем, не сменился ли локомотив. Если да — загружаем
// его конфиг с диска. Зовётся из DrawCheatMenu.
// Throttle: GetLocNum дорогой (читает settings.ini + пишет лог) — поэтому
// реально проверяем смену не чаще раза в LOCO_CHECK_INTERVAL мс.
procedure CheckLocoChangeAndReloadTexts;
var
  Cur: string;
  Now_: Cardinal;
begin
  Now_ := GetTickCount;
  if (LastLocoCheckTime <> 0) and (Now_ - LastLocoCheckTime < LOCO_CHECK_INTERVAL) then
    Exit;
  LastLocoCheckTime := Now_;
  Cur := GetCurrentLocoIdent;
  if Cur = '' then Exit; // не смогли определить — пропускаем
  if Cur = LastLocoIdent then Exit;
  LastLocoIdent := Cur;
  // Пытаемся загрузить per-loco файл. Если его нет — оставим то, что было,
  // и при следующем сохранении создадим файл для этой кабины.
  LoadCustomTextsForCurrentLoco;
end;

procedure AddCustomText;
var
  N: Integer;
  T: TCustomText3D;
begin
  N := Length(CustomTexts);
  T.Source := ksSpeed;
  T.X := 0.0;
  T.Y := 0.0;
  T.Z := 0.2;
  T.RX := -90.0; // как в DrawTextSimple — текст лежит на «полу» кабины
  T.RY := 0.0;
  T.RZ := 0.0;
  T.Scale := CUSTOM_TEXT_DEFAULT_SCALE;
  T.Visible := True;
  T.Color := $FFFFFF; // белый по умолчанию
  SetLength(CustomTexts, N + 1);
  CustomTexts[N] := T;
  CustomTextSelectedIdx := N;
  SyncSlidersFromSelected;
end;

procedure DeleteSelectedCustomText;
var
  i: Integer;
begin
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then Exit;
  for i := CustomTextSelectedIdx to Length(CustomTexts) - 2 do
    CustomTexts[i] := CustomTexts[i + 1];
  SetLength(CustomTexts, Length(CustomTexts) - 1);
  if Length(CustomTexts) = 0 then
    CustomTextSelectedIdx := -1
  else if CustomTextSelectedIdx >= Length(CustomTexts) then
    CustomTextSelectedIdx := Length(CustomTexts) - 1;
  if CustomTextSelectedIdx >= 0 then
    SyncSlidersFromSelected;
end;

procedure CycleSelectedSource(Delta: Integer);
var
  V: Integer;
begin
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then Exit;
  V := Integer(CustomTexts[CustomTextSelectedIdx].Source) + Delta;
  // wrap-around
  while V < 0 do Inc(V, KLUB_SOURCE_COUNT);
  V := V mod KLUB_SOURCE_COUNT;
  CustomTexts[CustomTextSelectedIdx].Source := TKlubSource(V);
end;

// Write3D живёт в DGLEngine.dll (см. DGLEngine.dpr exports). DrawFunc3D держит
// собственную приватную обёртку DrawText3D, поэтому подключаем Write3D напрямую.
procedure Write3DExt(FontID: Integer; Text: PChar); stdcall; external 'DGLEngine.dll' name 'Write3D';

// Рендер всех видимых пользовательских текстов в локальной системе координат
// кабины РА-3. Вызывается из RA3.DrawRA3 каждый кадр.
procedure DrawCustomTextsRA3;
var
  i: Integer;
  S: string;
  Buf: PChar;
begin
  if Length(CustomTexts) = 0 then Exit;
  for i := 0 to Length(CustomTexts) - 1 do
  begin
    if not CustomTexts[i].Visible then Continue;
    S := GetKlubSourceValue(CustomTexts[i].Source);
    if S = '' then Continue;
    BeginObj3D;
    glDisable(GL_LIGHTING);
    Position3D(CustomTexts[i].X, CustomTexts[i].Y, CustomTexts[i].Z);
    RotateX(CustomTexts[i].RX);
    RotateY(CustomTexts[i].RY);
    RotateZ(CustomTexts[i].RZ);
    Scale3D(CustomTexts[i].Scale);
    Color3D(Integer(CustomTexts[i].Color), 255, False, 0.0);
    SetTexture(0);
    Buf := PChar(S);
    Write3DExt(0, Buf);
    glEnable(GL_LIGHTING);
    EndObj3D;
  end;
end;

// =====================================================================
//  Per-frame дедуплекс — флаг сбрасывается в DrawCheatMenu (раз в кадр).
// =====================================================================
var
  CustomTextsRenderedThisFrame: Boolean = False;

procedure RenderCustomTextsAndGizmoForFrame; stdcall;
begin
  if CustomTextsRenderedThisFrame then Exit;
  CustomTextsRenderedThisFrame := True;
  // Защита от любых проблем с матрицей/состоянием GL — не валим DLL.
  try
    DrawCustomTextsRA3;
    UpdateGizmoFrameRA3;
    DrawGizmoRA3;
  except
    // Игнорируем — рендер кабины должен продолжаться.
  end;
end;

// ===========================================================================
//  3D-ГИЗМО (Translate / Rotate / Scale)
// ===========================================================================

// Цвет хэндла гизмо: ярко-синий при активном drag'е (мышь зажата на оси),
// осветлённый при hover, стандартный по оси иначе.
// Серия свапов курсор-feedback'а: жёлтый↔синий, потом зел↔син, потом снова син↔зел.
// Чистый итог = только первый свап: drag = синий, X = красный, Y = зелёный, Z = синий.
// Статические цвета осей не трогаем.
// COLORREF: low byte = R, mid = G, high = B.
function GizmoAxisColor(AxisIdx: Integer): Integer;
var
  IsActive, IsHover: Boolean;
begin
  IsActive := GizmoDragging and (AxisIdx = GizmoActiveAxis);
  IsHover  := (not GizmoDragging) and (AxisIdx = HoveredGizmoAxis);
  if IsActive then
  begin
    Result := $FF0000; // ярко-синий — активная ось при drag
    Exit;
  end;
  if IsHover then
  begin
    // Осветлённые подсветки — feedback ДО клика.
    case AxisIdx of
      GIZMO_AXIS_X:    Result := $7F7FFF; // светлый красный
      GIZMO_AXIS_Y:    Result := $7FFF7F; // светлый зелёный
      GIZMO_AXIS_Z:    Result := $FFCF7F; // светлый синий
      GIZMO_AXIS_VIEW: Result := $FFFFFF; // белый
    else
      Result := $FFFFFF;
    end;
    Exit;
  end;
  case AxisIdx of
    GIZMO_AXIS_X:    Result := $0000FF; // красный
    GIZMO_AXIS_Y:    Result := $00FF00; // зелёный
    GIZMO_AXIS_Z:    Result := $FF0000; // синий
    GIZMO_AXIS_VIEW: Result := $A0A0A0; // серый — пассивный центр
  else
    Result := $FFFFFF;
  end;
end;

// Чтение модификаторов — Shift = snap, Ctrl = precision (раз в 5 медленнее).
function GizmoShiftHeld: Boolean;
begin
  Result := (GetAsyncKeyState(VK_SHIFT) and $8000) <> 0;
end;

function GizmoCtrlHeld: Boolean;
begin
  Result := (GetAsyncKeyState(VK_CONTROL) and $8000) <> 0;
end;

// Округление к ближайшему кратному Step. Step должен быть > 0.
function GizmoSnap(V, Step: Single): Single;
begin
  if Step <= 0 then Result := V
  else Result := Round(V / Step) * Step;
end;

// Безопасное форматирование Single для overlay: всегда DecimalSeparator='.',
// округление до Decimals цифр, без Format()-специфичного парсинга.
// На русской локали Format('%.3f', [0.5]) даёт '0,5' и в каких-то местах
// дальше парсится через StrToFloat → EConvertError. Здесь мы всё контролируем.
function FormatGizmoFloat(V: Single; Decimals: Integer): string;
var
  OldSep: Char;
begin
  OldSep := DecimalSeparator;
  try
    DecimalSeparator := '.';
    // FloatToStrF не использует DecimalSeparator в Delphi 2007 в некоторых сборках,
    // а Format(%f) — использует. На всякий случай и руками.
    if Decimals < 0 then Decimals := 0;
    if Decimals > 8 then Decimals := 8;
    Result := FloatToStrF(V, ffFixed, 15, Decimals);
    // Подстраховка — на случай если FloatToStrF всё-таки вписал запятую.
    if Pos(',', Result) > 0 then
      Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
  finally
    DecimalSeparator := OldSep;
  end;
end;

// "+0.123" / "-0.456" — подписанная версия для углов/смещений.
function FormatGizmoFloatSigned(V: Single; Decimals: Integer): string;
begin
  if V >= 0 then
    Result := '+' + FormatGizmoFloat(V, Decimals)
  else
    Result := FormatGizmoFloat(V, Decimals);
end;

// 2D-оверлей гизмо: значение во время drag + индикатор арки для rotate +
// маленький значок текущего режима. Рисуется в отдельном Begin2D/End2D,
// после того как 3D-часть гизмо уже отрисована. Безопасно вызывать всегда —
// сам решает, что показывать. Полностью обёрнут в try/except, чтобы любой
// сбой форматирования / OpenGL-операции не валил DLL.
procedure DrawGizmoOverlay2D;
var
  Idx: Integer;
  T: TCustomText3D;
  S, DegSym: string;
  TX, TY: Integer;
  StartX, StartY, CurX, CurY: Integer;
  Ang0, Ang1, ArcA, ArcB: Single;
  ArcSegments, i: Integer;
  ModeLetter: string;
  ModeColor: Integer;
  PadX, PadY: Integer;
  Begin2DActive: Boolean;
begin
  if not GizmoCacheValid then Exit;
  Idx := CustomTextSelectedIdx;
  if (Idx < 0) or (Idx >= Length(CustomTexts)) then Exit;
  T := CustomTexts[Idx];
  // Знак градуса берём как одиночный байт CP1251 (B0). Если шрифт не умеет —
  // в любом случае не упадёт.
  DegSym := Chr(176);
  Begin2DActive := False;

  try
    Begin2D;
    Begin2DActive := True;

    // 1) Лейбл режима у origin'а — постоянная подсказка, что сейчас активно.
    case GizmoMode of
      gmTranslate: begin ModeLetter := 'MOVE';   ModeColor := $00BFFF; end;
      gmRotate:    begin ModeLetter := 'ROTATE'; ModeColor := $00FF80; end;
      gmScale:     begin ModeLetter := 'SCALE';  ModeColor := $FF80FF; end;
    else
      ModeLetter := ''; ModeColor := $FFFFFF;
    end;
    if ModeLetter <> '' then
    begin
      TX := GizmoOriginScreen.X + 18;
      TY := GizmoOriginScreen.Y - 26;
      DrawText2D(0, TX + 1, TY + 1, ModeLetter, $000000, 100, 0.55);
      DrawText2D(0, TX,     TY,     ModeLetter, ModeColor, 200, 0.55);
    end;

    // 2) Индикатор арки и стартовой/текущей радиальных линий — только во время rotate-drag.
    if GizmoDragging and (GizmoMode = gmRotate) and
       (GizmoActiveAxis >= 0) and (GizmoActiveAxis <= 2) then
    begin
      StartX := GizmoOriginScreen.X + Round(Cos(GizmoRotStartAngle) * 57.2);
      StartY := GizmoOriginScreen.Y + Round(Sin(GizmoRotStartAngle) * 57.2);
      Ang0 := GizmoRotStartAngle;
      Ang1 := Ang0 + GizmoRotAccum;
      CurX := GizmoOriginScreen.X + Round(Cos(Ang1) * 57.2);
      CurY := GizmoOriginScreen.Y + Round(Sin(Ang1) * 57.2);

      // Заливка арки — лучи, расходящиеся от origin'а; полупрозрачный синий.
      ArcSegments := 48;
      ArcA := Ang0;
      ArcB := Ang1;
      // Если оборотов больше одного — нормализуем визуально к одному обороту,
      // иначе арка превратится в кашу. Сама величина угла останется честной в тексте.
      if Abs(ArcB - ArcA) > 2 * PI then
      begin
        if ArcB > ArcA then ArcB := ArcA + 2 * PI - 0.001
        else ArcB := ArcA - 2 * PI + 0.001;
      end;
      glDisable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glColor4ub($00, $00, $FF, 70); // синий (свап вернул из зелёного)
      glBegin(GL_TRIANGLE_FAN);
        glVertex2f(GizmoOriginScreen.X + 0.0, GizmoOriginScreen.Y + 0.0);
        for i := 0 to ArcSegments do
        begin
          Ang0 := ArcA + (ArcB - ArcA) * i / ArcSegments;
          glVertex2f(
            GizmoOriginScreen.X + Cos(Ang0) * 57.2,
            GizmoOriginScreen.Y + Sin(Ang0) * 57.2);
        end;
      glEnd;
      glColor4ub(255, 255, 255, 255);
      glEnable(GL_TEXTURE_2D);

      // Лучи: стартовый — белый, текущий — синий (свап вернул из зелёного).
      DrawLine2D(GizmoOriginScreen.X, GizmoOriginScreen.Y, StartX, StartY, $FFFFFF, 180, 1.5, True);
      DrawLine2D(GizmoOriginScreen.X, GizmoOriginScreen.Y, CurX,   CurY,   $FF0000, 230, 2.0, True);
    end;

    // 3) Численный readout во время drag — рядом с курсором мыши.
    if GizmoDragging then
    begin
      S := '';
      case GizmoMode of
        gmTranslate:
          case GizmoActiveAxis of
            GIZMO_AXIS_X:    S := 'X: '   + FormatGizmoFloat(T.X, 4);
            GIZMO_AXIS_Y:    S := 'Y: '   + FormatGizmoFloat(T.Y, 4);
            GIZMO_AXIS_Z:    S := 'Z: '   + FormatGizmoFloat(T.Z, 4);
            GIZMO_AXIS_VIEW: S := 'XYZ: ' + FormatGizmoFloat(T.X, 3) + ' / ' +
                                            FormatGizmoFloat(T.Y, 3) + ' / ' +
                                            FormatGizmoFloat(T.Z, 3);
          end;
        gmRotate:
          // Свап Y↔Z: показываем тот угол, который реально меняется кольцом.
          case GizmoActiveAxis of
            GIZMO_AXIS_X: S := 'RX: ' + FormatGizmoFloatSigned(T.RX, 1) + DegSym;
            GIZMO_AXIS_Y: S := 'RZ: ' + FormatGizmoFloatSigned(T.RZ, 1) + DegSym;
            GIZMO_AXIS_Z: S := 'RY: ' + FormatGizmoFloatSigned(T.RY, 1) + DegSym;
          end;
        gmScale:
          S := 'Scale: ' + FormatGizmoFloat(T.Scale, 3) + 'x';
      end;
      if GizmoShiftHeld then S := S + '   [snap]';
      if GizmoCtrlHeld  then S := S + '   [precise]';

      if S <> '' then
      begin
        TX := Round(MoveXcoord) + 18;
        TY := Round(MoveYcoord) + 18;
        // Полупрозрачный плашечный фон под текст для читаемости.
        PadX := 6; PadY := 4;
        DrawRectangle2D(TX - PadX, TY - PadY,
          Length(S) * 8 + PadX * 2, 18 + PadY, $000000, 160, True);
        DrawRectangle2D(TX - PadX, TY - PadY,
          Length(S) * 8 + PadX * 2, 18 + PadY, $FFFFFF, 60, False);
        DrawText2D(0, TX + 1, TY + 1, S, $000000, 200, 0.65);
        DrawText2D(0, TX,     TY,     S, $FFFFFF, 255, 0.65);
      end;
    end
    else if HoveredGizmoAxis <> GIZMO_AXIS_NONE then
    begin
      // Лёгкая подсказка при наведении (без drag).
      TX := GizmoOriginScreen.X + 18;
      TY := GizmoOriginScreen.Y + 6;
      DrawText2D(0, TX + 1, TY + 1, 'Shift=snap  Ctrl=precise  Esc=cancel',
        $000000, 90, 0.45);
      DrawText2D(0, TX,     TY,     'Shift=snap  Ctrl=precise  Esc=cancel',
        $C0C0C0, 180, 0.45);
    end;
  except
    // Любой сбой в overlay — глотаем, чтобы не валить рендер кабины.
    // Это чисто косметика, без неё гизмо всё равно работает.
  end;
  if Begin2DActive then End2D;
end;

procedure DrawGizmoRA3;
var
  T: TCustomText3D;
  V: TVertex;
  i: Integer;
  Theta, R, BoxSize: Single;
  RingV: array[0..GIZMO_RING_SAMPLES - 1] of TVertex;
  CenterStr: string;
  CenterLen: Integer;
  HalfWWorld, HalfHWorld: Single;
begin
  // ВАЖНО: гизмо рисуется всегда, когда есть выбранный текст — даже без F12-меню,
  // чтобы можно было редактировать «как РА-3 контроллер».
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then
  begin
    GizmoCacheValid := False;
    Exit;
  end;
  T := CustomTexts[CustomTextSelectedIdx];

  // === ВСЁ В ОДНОМ BeginObj3D: матрица не «гуляет» между объектами. ===
  BeginObj3D;
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);

  // ---- Origin гизмо = визуальный центр текста ----
  // Берём текущее значение источника (а не последний кадр), чтобы центр
  // двигался при изменении длины строки. Эвристика на ширину/высоту глифа:
  //   глиф ≈ 0.55 em wide, 0.7 em tall (em = font baseline-to-baseline ~1).
  // После Scale3D в DrawCustomTextsRA3 это превращается в локальные единицы кабины.
  // Здесь Scale3D не применяем (гизмо не должен дрейфовать в размере вместе
  // с текстом), а сразу даём оффсет в world-units.
  CenterStr := GetKlubSourceValue(T.Source);
  CenterLen := Length(CenterStr);
  if CenterLen < 1 then CenterLen := 1;
  HalfWWorld := CenterLen * 0.55 * 0.5 * T.Scale; // half width в world-units
  HalfHWorld := 0.35 * T.Scale;                   // half height (≈ cap-height/2)

  // Матричный трюк: T(anchor) * R * T(local_offset) * R^-1.
  // → origin сдвинут на R * (HalfW, HalfH, 0), а оси остаются мировыми XYZ.
  // Это сохраняет совместимость с математикой drag (она в world-XYZ).
  Position3D(T.X, T.Y, T.Z);
  RotateX(T.RX); RotateY(T.RY); RotateZ(T.RZ);
  glTranslatef(HalfWWorld, HalfHWorld, 0);
  RotateZ(-T.RZ); RotateY(-T.RY); RotateX(-T.RX);

  SetTexture(0);

  // -----------------------------------------------------------------------
  // СНАЧАЛА — кэшируем все экранные позиции (Get2DPos нельзя звать внутри
  // glBegin/glEnd — это UB в OpenGL, gluProject возвращает мусор).
  // -----------------------------------------------------------------------
  V.X := 0; V.Y := 0; V.Z := 0;
  GizmoOriginScreen := Get2DPos(V);

  if (GizmoMode = gmTranslate) or (GizmoMode = gmScale) then
  begin
    V.X := GIZMO_ARROW_LEN; V.Y := 0; V.Z := 0;
    GizmoTipScreen[GIZMO_AXIS_X] := Get2DPos(V);
    V.X := 0; V.Y := GIZMO_ARROW_LEN; V.Z := 0;
    GizmoTipScreen[GIZMO_AXIS_Y] := Get2DPos(V);
    V.X := 0; V.Y := 0; V.Z := GIZMO_ARROW_LEN;
    GizmoTipScreen[GIZMO_AXIS_Z] := Get2DPos(V);
  end
  else // gmRotate
  begin
    R := GIZMO_ROTATE_RADIUS;
    // Кольцо X (плоскость YZ)
    for i := 0 to GIZMO_RING_SAMPLES - 1 do
    begin
      Theta := i * 2 * PI / GIZMO_RING_SAMPLES;
      V.X := 0; V.Y := R * Cos(Theta); V.Z := R * Sin(Theta);
      GizmoRingScreen[GIZMO_AXIS_X, i] := Get2DPos(V);
    end;
    // Кольцо Y (плоскость XZ)
    for i := 0 to GIZMO_RING_SAMPLES - 1 do
    begin
      Theta := i * 2 * PI / GIZMO_RING_SAMPLES;
      V.X := R * Cos(Theta); V.Y := 0; V.Z := R * Sin(Theta);
      GizmoRingScreen[GIZMO_AXIS_Y, i] := Get2DPos(V);
    end;
    // Кольцо Z (плоскость XY)
    for i := 0 to GIZMO_RING_SAMPLES - 1 do
    begin
      Theta := i * 2 * PI / GIZMO_RING_SAMPLES;
      V.X := R * Cos(Theta); V.Y := R * Sin(Theta); V.Z := 0;
      GizmoRingScreen[GIZMO_AXIS_Z, i] := Get2DPos(V);
    end;
  end;

  // -----------------------------------------------------------------------
  // ТЕПЕРЬ — рендерим всё без вызовов Get2DPos.
  // -----------------------------------------------------------------------
  if (GizmoMode = gmTranslate) or (GizmoMode = gmScale) then
  begin
    // Линии-оси (чуть толще оригинальных — лучше видно издалека).
    Color3D(GizmoAxisColor(GIZMO_AXIS_X), 255, False, 0.0);
    DrawLine(0, 0, 0, GIZMO_ARROW_LEN, 0, 0, 3.5, True);
    Color3D(GizmoAxisColor(GIZMO_AXIS_Y), 255, False, 0.0);
    DrawLine(0, 0, 0, 0, GIZMO_ARROW_LEN, 0, 3.5, True);
    Color3D(GizmoAxisColor(GIZMO_AXIS_Z), 255, False, 0.0);
    DrawLine(0, 0, 0, 0, 0, GIZMO_ARROW_LEN, 3.5, True);

    // Кубики на концах. glPushMatrix/glPopMatrix вокруг каждого, чтобы
    // glTranslatef'ы НЕ суммировались (DrawCube сам делает push/scale/pop).
    if GizmoMode = gmTranslate then BoxSize := GIZMO_BOX_TRANSLATE
    else BoxSize := GIZMO_BOX_SCALE;

    // Активный/hover-кубик чуть больше — даём тактильный feedback.
    Color3D(GizmoAxisColor(GIZMO_AXIS_X), 255, False, 0.0);
    glPushMatrix;
      glTranslatef(GIZMO_ARROW_LEN, 0, 0);
      if (GizmoActiveAxis = GIZMO_AXIS_X) or (HoveredGizmoAxis = GIZMO_AXIS_X) then
        DrawCube(BoxSize * 1.35, BoxSize * 1.35, BoxSize * 1.35)
      else
        DrawCube(BoxSize, BoxSize, BoxSize);
    glPopMatrix;

    Color3D(GizmoAxisColor(GIZMO_AXIS_Y), 255, False, 0.0);
    glPushMatrix;
      glTranslatef(0, GIZMO_ARROW_LEN, 0);
      if (GizmoActiveAxis = GIZMO_AXIS_Y) or (HoveredGizmoAxis = GIZMO_AXIS_Y) then
        DrawCube(BoxSize * 1.35, BoxSize * 1.35, BoxSize * 1.35)
      else
        DrawCube(BoxSize, BoxSize, BoxSize);
    glPopMatrix;

    Color3D(GizmoAxisColor(GIZMO_AXIS_Z), 255, False, 0.0);
    glPushMatrix;
      glTranslatef(0, 0, GIZMO_ARROW_LEN);
      if (GizmoActiveAxis = GIZMO_AXIS_Z) or (HoveredGizmoAxis = GIZMO_AXIS_Z) then
        DrawCube(BoxSize * 1.35, BoxSize * 1.35, BoxSize * 1.35)
      else
        DrawCube(BoxSize, BoxSize, BoxSize);
    glPopMatrix;

    // Центральный «свободный» хэндл — только в режиме translate.
    // В scale он избыточен (скейл уже uniform по всем осям).
    if GizmoMode = gmTranslate then
    begin
      Color3D(GizmoAxisColor(GIZMO_AXIS_VIEW), 255, False, 0.0);
      glPushMatrix;
        if (GizmoActiveAxis = GIZMO_AXIS_VIEW) or (HoveredGizmoAxis = GIZMO_AXIS_VIEW) then
          DrawCube(GIZMO_BOX_CENTER * 1.35, GIZMO_BOX_CENTER * 1.35, GIZMO_BOX_CENTER * 1.35)
        else
          DrawCube(GIZMO_BOX_CENTER, GIZMO_BOX_CENTER, GIZMO_BOX_CENTER);
      glPopMatrix;
    end;
  end
  else // gmRotate
  begin
    R := GIZMO_ROTATE_RADIUS;
    // Активное/hover-кольцо толще — лучше видно, что выбрано.
    glLineWidth(2.0);

    // Кольцо X (YZ)
    for i := 0 to GIZMO_RING_SAMPLES - 1 do
    begin
      Theta := i * 2 * PI / GIZMO_RING_SAMPLES;
      RingV[i].X := 0; RingV[i].Y := R * Cos(Theta); RingV[i].Z := R * Sin(Theta);
    end;
    Color3D(GizmoAxisColor(GIZMO_AXIS_X), 255, False, 0.0);
    if (GizmoActiveAxis = GIZMO_AXIS_X) or (HoveredGizmoAxis = GIZMO_AXIS_X) then
      glLineWidth(3.5)
    else
      glLineWidth(2.0);
    glBegin(GL_LINE_LOOP);
      for i := 0 to GIZMO_RING_SAMPLES - 1 do
        glVertex3f(RingV[i].X, RingV[i].Y, RingV[i].Z);
    glEnd;

    // Кольцо Y (XZ)
    for i := 0 to GIZMO_RING_SAMPLES - 1 do
    begin
      Theta := i * 2 * PI / GIZMO_RING_SAMPLES;
      RingV[i].X := R * Cos(Theta); RingV[i].Y := 0; RingV[i].Z := R * Sin(Theta);
    end;
    Color3D(GizmoAxisColor(GIZMO_AXIS_Y), 255, False, 0.0);
    if (GizmoActiveAxis = GIZMO_AXIS_Y) or (HoveredGizmoAxis = GIZMO_AXIS_Y) then
      glLineWidth(3.5)
    else
      glLineWidth(2.0);
    glBegin(GL_LINE_LOOP);
      for i := 0 to GIZMO_RING_SAMPLES - 1 do
        glVertex3f(RingV[i].X, RingV[i].Y, RingV[i].Z);
    glEnd;

    // Кольцо Z (XY)
    for i := 0 to GIZMO_RING_SAMPLES - 1 do
    begin
      Theta := i * 2 * PI / GIZMO_RING_SAMPLES;
      RingV[i].X := R * Cos(Theta); RingV[i].Y := R * Sin(Theta); RingV[i].Z := 0;
    end;
    Color3D(GizmoAxisColor(GIZMO_AXIS_Z), 255, False, 0.0);
    if (GizmoActiveAxis = GIZMO_AXIS_Z) or (HoveredGizmoAxis = GIZMO_AXIS_Z) then
      glLineWidth(3.5)
    else
      glLineWidth(2.0);
    glBegin(GL_LINE_LOOP);
      for i := 0 to GIZMO_RING_SAMPLES - 1 do
        glVertex3f(RingV[i].X, RingV[i].Y, RingV[i].Z);
    glEnd;

    glLineWidth(1.0);
  end;

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  EndObj3D;

  GizmoCacheValid := True;

  // 2D-оверлей: цифры/арка/подсказки. Безопасно вызывать после EndObj3D —
  // Begin2D/End2D сами восстанавливают состояние GL.
  DrawGizmoOverlay2D;
end;

// Квадрат расстояния от точки до отрезка в 2D — для широкого пика по оси.
function DistPointToSegmentSq(PX, PY, AX, AY, BX, BY: Integer): Integer;
var
  ABX, ABY, APX, APY: Integer;
  LenSq, Num: Integer;
  CX, CY: Integer;
begin
  ABX := BX - AX;
  ABY := BY - AY;
  APX := PX - AX;
  APY := PY - AY;
  LenSq := ABX * ABX + ABY * ABY;
  if LenSq = 0 then
  begin
    Result := APX * APX + APY * APY;
    Exit;
  end;
  Num := APX * ABX + APY * ABY;
  if Num <= 0 then
  begin
    Result := APX * APX + APY * APY;
    Exit;
  end;
  if Num >= LenSq then
  begin
    Result := (PX - BX) * (PX - BX) + (PY - BY) * (PY - BY);
    Exit;
  end;
  CX := AX + (ABX * Num) div LenSq;
  CY := AY + (ABY * Num) div LenSq;
  Result := (PX - CX) * (PX - CX) + (PY - CY) * (PY - CY);
end;

function PointDistSq(const P: TPoint; X, Y: Integer): Integer;
begin
  Result := Sqr(P.X - X) + Sqr(P.Y - Y);
end;

// Какую ось гизмо мышь сейчас «накрывает». Приоритет: центральный хэндл
// (только в translate) → оси/кольца. Translate/Scale — пик по линии origin→tip.
// Rotate — пик по ближайшей точке кольца (32 семпла на каждое).
function PickGizmoAxis(MX, MY: Integer): Integer;
var
  i, j, d, bestD, best, dCenter: Integer;
begin
  Result := GIZMO_AXIS_NONE;
  if not GizmoCacheValid then Exit;

  // Центр имеет приоритет — он внутри пересечения всех осей и должен
  // быть кликабельным даже если пиксель формально ближе к началу оси.
  if GizmoMode = gmTranslate then
  begin
    dCenter := PointDistSq(GizmoOriginScreen, MX, MY);
    if dCenter <= GIZMO_PICK_CENTER_R * GIZMO_PICK_CENTER_R then
    begin
      Result := GIZMO_AXIS_VIEW;
      Exit;
    end;
  end;

  best := GIZMO_AXIS_NONE;
  bestD := GIZMO_PICK_RADIUS * GIZMO_PICK_RADIUS;

  if (GizmoMode = gmTranslate) or (GizmoMode = gmScale) then
  begin
    for i := 0 to 2 do
    begin
      d := DistPointToSegmentSq(MX, MY,
        GizmoOriginScreen.X, GizmoOriginScreen.Y,
        GizmoTipScreen[i].X, GizmoTipScreen[i].Y);
      if d < bestD then
      begin
        bestD := d;
        best := i;
      end;
    end;
  end
  else // gmRotate
  begin
    for i := 0 to 2 do
      for j := 0 to GIZMO_RING_SAMPLES - 1 do
      begin
        d := PointDistSq(GizmoRingScreen[i, j], MX, MY);
        if d < bestD then
        begin
          bestD := d;
          best := i;
        end;
      end;
  end;
  Result := best;
end;

procedure GizmoStartDrag(MX, MY, AxisIdx: Integer);
var
  Idx: Integer;
begin
  Idx := CustomTextSelectedIdx;
  if (Idx < 0) or (Idx >= Length(CustomTexts)) then Exit;

  GizmoDragging := True;
  GizmoActiveAxis := AxisIdx;
  GizmoDragMouseStartX := MX;
  GizmoDragMouseStartY := MY;

  // Полный снимок трансформа на начало drag — чтобы Esc мог откатить всё.
  GizmoCancelX     := CustomTexts[Idx].X;
  GizmoCancelY     := CustomTexts[Idx].Y;
  GizmoCancelZ     := CustomTexts[Idx].Z;
  GizmoCancelRX    := CustomTexts[Idx].RX;
  GizmoCancelRY    := CustomTexts[Idx].RY;
  GizmoCancelRZ    := CustomTexts[Idx].RZ;
  GizmoCancelScale := CustomTexts[Idx].Scale;

  if GizmoMode = gmTranslate then
  begin
    // X/Y/Z по обычной оси либо по центру (free-move) — и в том и в другом
    // случае запоминаем стартовые позиции по всем трём осям.
    GizmoDragValueStart[0] := CustomTexts[Idx].X;
    GizmoDragValueStart[1] := CustomTexts[Idx].Y;
    GizmoDragValueStart[2] := CustomTexts[Idx].Z;
  end
  else if GizmoMode = gmRotate then
  begin
    GizmoDragValueStart[0] := CustomTexts[Idx].RX;
    GizmoDragValueStart[1] := CustomTexts[Idx].RY;
    GizmoDragValueStart[2] := CustomTexts[Idx].RZ;
    // Инициализация angular tracking: запоминаем угол от центра гизмо до курсора.
    GizmoRotStartAngle := ArcTan2(MY - GizmoOriginScreen.Y, MX - GizmoOriginScreen.X);
    GizmoRotPrevAngle := GizmoRotStartAngle;
    GizmoRotAccum := 0.0;
  end
  else // gmScale
    GizmoDragScaleStart := CustomTexts[Idx].Scale;
end;

// Возвращает мировой сдвиг по оси axis_idx (0/1/2) для текущего курсора.
// Считает signed-projection экранного дельта (DX,DY) на screen-axis.
// Применяет Ctrl-precision и Shift-snap.
function GizmoTranslateDelta(AxisIdx, DX, DY: Integer): Single;
var
  AxisVecX, AxisVecY: Single;
  AxisLenSq, DotI: Single;
  WorldDelta: Single;
begin
  Result := 0.0;
  if (AxisIdx < 0) or (AxisIdx > 2) then Exit;
  AxisVecX := GizmoTipScreen[AxisIdx].X - GizmoOriginScreen.X;
  AxisVecY := GizmoTipScreen[AxisIdx].Y - GizmoOriginScreen.Y;
  AxisLenSq := AxisVecX * AxisVecX + AxisVecY * AxisVecY;
  if AxisLenSq < 4 then Exit;
  DotI := DX * AxisVecX + DY * AxisVecY;
  // pixels_along / axis_len, затем * world_per_pixel = (1/axis_len) * GIZMO_ARROW_LEN
  WorldDelta := (DotI / AxisLenSq) * GIZMO_ARROW_LEN;
  if GizmoCtrlHeld  then WorldDelta := WorldDelta * GIZMO_PRECISION_FACTOR;
  if GizmoShiftHeld then WorldDelta := GizmoSnap(WorldDelta, GIZMO_SNAP_TRANSLATE);
  Result := WorldDelta;
end;

procedure GizmoApplyDrag(MX, MY: Integer);
var
  Idx: Integer;
  DX, DY: Integer;
  WorldDelta, AngleDelta, ScaleFactor: Single;
  AxisVecX, AxisVecY, AxisLenSq, DotI, PixelsAlongAxis: Single;
  // Для view-handle (free-2D translate): выбираем 2 наиболее «экранных» оси
  // (с наибольшей длиной screen-проекции), решаем 2x2 систему, третья = 0.
  Sx0, Sy0, Sx1, Sy1, Sx2, Sy2: Single;
  Len0, Len1, Len2: Single;
  AxA, AxB, AxSkip: Integer;
  Det, A, B: Single;
  DeltaA, DeltaB: Single;
  DAxis: array[0..2] of Single;
begin
  Idx := CustomTextSelectedIdx;
  if (GizmoActiveAxis = GIZMO_AXIS_NONE) or (Idx < 0) or (Idx >= Length(CustomTexts)) then Exit;
  if not GizmoCacheValid then Exit;

  DX := MX - GizmoDragMouseStartX;
  DY := MY - GizmoDragMouseStartY;

  case GizmoMode of
    gmTranslate:
    begin
      if GizmoActiveAxis = GIZMO_AXIS_VIEW then
      begin
        // Free-move в плоскости экрана: пропускаем ось, наиболее
        // «уходящую от камеры» (с наименьшей длиной screen-проекции),
        // решаем 2x2-систему по двум оставшимся.
        Sx0 := GizmoTipScreen[0].X - GizmoOriginScreen.X;
        Sy0 := GizmoTipScreen[0].Y - GizmoOriginScreen.Y;
        Sx1 := GizmoTipScreen[1].X - GizmoOriginScreen.X;
        Sy1 := GizmoTipScreen[1].Y - GizmoOriginScreen.Y;
        Sx2 := GizmoTipScreen[2].X - GizmoOriginScreen.X;
        Sy2 := GizmoTipScreen[2].Y - GizmoOriginScreen.Y;
        Len0 := Sx0 * Sx0 + Sy0 * Sy0;
        Len1 := Sx1 * Sx1 + Sy1 * Sy1;
        Len2 := Sx2 * Sx2 + Sy2 * Sy2;
        if (Len0 <= Len1) and (Len0 <= Len2) then
        begin AxSkip := 0; AxA := 1; AxB := 2; end
        else if (Len1 <= Len0) and (Len1 <= Len2) then
        begin AxSkip := 1; AxA := 0; AxB := 2; end
        else
        begin AxSkip := 2; AxA := 0; AxB := 1; end;

        // 2x2: [SxA SxB; SyA SyB] * [a b]' = [DX DY]'
        Det := (GizmoTipScreen[AxA].X - GizmoOriginScreen.X) *
               (GizmoTipScreen[AxB].Y - GizmoOriginScreen.Y) -
               (GizmoTipScreen[AxA].Y - GizmoOriginScreen.Y) *
               (GizmoTipScreen[AxB].X - GizmoOriginScreen.X);
        if Abs(Det) < 4.0 then
        begin
          // Выродившийся случай (две оси почти параллельны) — fallback на одну ось.
          DeltaA := GizmoTranslateDelta(AxA, DX, DY);
          DAxis[AxA] := DeltaA; DAxis[AxB] := 0; DAxis[AxSkip] := 0;
        end
        else
        begin
          A := (DX * (GizmoTipScreen[AxB].Y - GizmoOriginScreen.Y) -
                DY * (GizmoTipScreen[AxB].X - GizmoOriginScreen.X)) / Det;
          B := (DY * (GizmoTipScreen[AxA].X - GizmoOriginScreen.X) -
                DX * (GizmoTipScreen[AxA].Y - GizmoOriginScreen.Y)) / Det;
          DeltaA := A * GIZMO_ARROW_LEN;
          DeltaB := B * GIZMO_ARROW_LEN;
          if GizmoCtrlHeld then
          begin
            DeltaA := DeltaA * GIZMO_PRECISION_FACTOR;
            DeltaB := DeltaB * GIZMO_PRECISION_FACTOR;
          end;
          if GizmoShiftHeld then
          begin
            DeltaA := GizmoSnap(DeltaA, GIZMO_SNAP_TRANSLATE);
            DeltaB := GizmoSnap(DeltaB, GIZMO_SNAP_TRANSLATE);
          end;
          DAxis[AxA] := DeltaA; DAxis[AxB] := DeltaB; DAxis[AxSkip] := 0;
        end;
        CustomTexts[Idx].X := GizmoDragValueStart[0] + DAxis[0];
        CustomTexts[Idx].Y := GizmoDragValueStart[1] + DAxis[1];
        CustomTexts[Idx].Z := GizmoDragValueStart[2] + DAxis[2];
      end
      else
      begin
        WorldDelta := GizmoTranslateDelta(GizmoActiveAxis, DX, DY);
        case GizmoActiveAxis of
          GIZMO_AXIS_X: CustomTexts[Idx].X := GizmoDragValueStart[0] + WorldDelta;
          GIZMO_AXIS_Y: CustomTexts[Idx].Y := GizmoDragValueStart[1] + WorldDelta;
          GIZMO_AXIS_Z: CustomTexts[Idx].Z := GizmoDragValueStart[2] + WorldDelta;
        end;
      end;
    end;
    gmRotate:
    begin
      // Угол курсора от центра гизмо (в экранных пикселях). Каждый тик берём
      // приращение прошлого угла, делаем unwrap (через ±π), и аккумулируем.
      // Так драг следует за круговым движением мыши — как в Блендере.
      AngleDelta := ArcTan2(MY - GizmoOriginScreen.Y, MX - GizmoOriginScreen.X) - GizmoRotPrevAngle;
      if AngleDelta >  PI then AngleDelta := AngleDelta - 2 * PI;
      if AngleDelta < -PI then AngleDelta := AngleDelta + 2 * PI;
      // Ctrl: дельта медленнее на каждый тик — мышь движется быстрее, поворот тонкий.
      if GizmoCtrlHeld then AngleDelta := AngleDelta * GIZMO_PRECISION_FACTOR;
      GizmoRotAccum := GizmoRotAccum + AngleDelta;
      GizmoRotPrevAngle := ArcTan2(MY - GizmoOriginScreen.Y, MX - GizmoOriginScreen.X);
      // Преобразуем накопленный угол в градусы — наши RX/RY/RZ в градусах.
      AngleDelta := GizmoRotAccum * 180.0 / PI;
      // Snap: применяется к итоговому offset'у, а не к каждому приращению,
      // иначе при Ctrl/мелких движениях ничего не двигалось бы.
      if GizmoShiftHeld then AngleDelta := GizmoSnap(AngleDelta, GIZMO_SNAP_ROTATE);
      // Свап ротации Y↔Z: клик по зелёному (Y) кольцу крутит RZ, по синему (Z)
      // кольцу — RY. Это даёт ожидаемое визуальное поведение для текстов
      // с базовым RX = -90 (когда текст лежит на полу кабины).
      case GizmoActiveAxis of
        GIZMO_AXIS_X: CustomTexts[Idx].RX := GizmoDragValueStart[0] + AngleDelta;
        GIZMO_AXIS_Y: CustomTexts[Idx].RZ := GizmoDragValueStart[2] + AngleDelta;
        GIZMO_AXIS_Z: CustomTexts[Idx].RY := GizmoDragValueStart[1] + AngleDelta;
      end;
    end;
    gmScale:
    begin
      // Двойной режим:
      //  • signed projection вдоль ScreenAxis выбранной оси
      //    (drag tip-кубика наружу = больше, внутрь к центру = меньше).
      //  • с Ctrl/Shift — поведение «как у translate».
      // ScaleFactor = 1 + проекция_в_пикс / длина_оси_в_пикс
      //             = 1 + DotI / AxisLenSq
      if (GizmoActiveAxis >= 0) and (GizmoActiveAxis <= 2) then
      begin
        AxisVecX := GizmoTipScreen[GizmoActiveAxis].X - GizmoOriginScreen.X;
        AxisVecY := GizmoTipScreen[GizmoActiveAxis].Y - GizmoOriginScreen.Y;
        AxisLenSq := AxisVecX * AxisVecX + AxisVecY * AxisVecY;
        if AxisLenSq < 4 then Exit;
        DotI := DX * AxisVecX + DY * AxisVecY;
        PixelsAlongAxis := DotI / AxisLenSq;
        ScaleFactor := 1.0 + PixelsAlongAxis;
      end
      else
      begin
        // Fallback (на всякий случай) — старая горизонтальная логика.
        ScaleFactor := 1.0 + DX * 0.005;
      end;

      if GizmoCtrlHeld then
        ScaleFactor := 1.0 + (ScaleFactor - 1.0) * GIZMO_PRECISION_FACTOR;
      if GizmoShiftHeld then
        ScaleFactor := GizmoSnap(ScaleFactor, GIZMO_SNAP_SCALE);
      if ScaleFactor < 0.05 then ScaleFactor := 0.05;
      CustomTexts[Idx].Scale := GizmoDragScaleStart * ScaleFactor;
      if CustomTexts[Idx].Scale < 0.0005 then CustomTexts[Idx].Scale := 0.0005;
    end;
  end;
  SyncSlidersFromSelected;
end;

// Откат drag'а — восстанавливаем все поля по снэпшоту, который сделали
// в GizmoStartDrag. Зовётся из UpdateGizmoFrameRA3 при нажатии Esc.
procedure GizmoCancelDrag;
var
  Idx: Integer;
begin
  if not GizmoDragging then Exit;
  Idx := CustomTextSelectedIdx;
  if (Idx >= 0) and (Idx < Length(CustomTexts)) then
  begin
    CustomTexts[Idx].X     := GizmoCancelX;
    CustomTexts[Idx].Y     := GizmoCancelY;
    CustomTexts[Idx].Z     := GizmoCancelZ;
    CustomTexts[Idx].RX    := GizmoCancelRX;
    CustomTexts[Idx].RY    := GizmoCancelRY;
    CustomTexts[Idx].RZ    := GizmoCancelRZ;
    CustomTexts[Idx].Scale := GizmoCancelScale;
    SyncSlidersFromSelected;
  end;
  GizmoDragging := False;
  GizmoActiveAxis := GIZMO_AXIS_NONE;
end;

// Per-frame state machine — копия паттерна РА-3 контроллера. Вызывается из RA3.DrawRA3
// каждый кадр; читает мышь напрямую (MoveXcoord/MoveYcoord + GetAsyncKeyState),
// поэтому работает БЕЗ открытого F12-меню. Сама зовёт ApplyMenuPatch/RemoveMenuPatch
// при hover/drag — точно как UpdateHover для контроллера.
procedure UpdateGizmoFrameRA3;
var
  MX, MY: Integer;
  LMB, EscNow: Boolean;
begin
  // Нет выбранного текста — погасить всё.
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then
  begin
    HoveredGizmoAxis := GIZMO_AXIS_NONE;
    if GizmoDragging then
    begin
      GizmoDragging := False;
      GizmoActiveAxis := GIZMO_AXIS_NONE;
    end;
    if GizmoPatchActive then
    begin
      RemoveMenuPatch;
      GizmoPatchActive := False;
    end;
    GizmoLastLMB := False;
    GizmoLastEsc := False;
    Exit;
  end;

  if not GizmoCacheValid then
  begin
    HoveredGizmoAxis := GIZMO_AXIS_NONE;
    Exit;
  end;

  MX := Round(MoveXcoord);
  MY := Round(MoveYcoord);
  HoveredGizmoAxis := PickGizmoAxis(MX, MY);
  LMB := (GetAsyncKeyState(VK_LBUTTON) and $8000) <> 0;
  EscNow := (GetAsyncKeyState(VK_ESCAPE) and $8000) <> 0;

  // Esc во время drag → откат.
  if GizmoDragging and EscNow and (not GizmoLastEsc) then
  begin
    GizmoCancelDrag;
    SaveConfig;
    GizmoLastEsc := EscNow;
    GizmoLastLMB := LMB;
    Exit;
  end;
  GizmoLastEsc := EscNow;

  // Rising edge → старт drag'а (если не над окном меню и не в Dev Editor'е).
  if (not GizmoDragging) and LMB and (not GizmoLastLMB) and (HoveredGizmoAxis <> GIZMO_AXIS_NONE) then
  begin
    if (MenuVisible and PointInAnyMenuWindow(MX, MY)) or DevEditorVisible then
    begin
      // Клик попал в UI — пускай его обработает соответствующий код.
    end
    else
    begin
      GizmoStartDrag(MX, MY, HoveredGizmoAxis);
    end;
  end;

  // Продолжение drag'а — каждый тик пересчитываем + сохраняем (фиксит дрифт от
  // LoadConfigThrottled, который иначе восстанавливал бы старые значения).
  if GizmoDragging and LMB then
  begin
    GizmoApplyDrag(MX, MY);
    SaveConfig;
  end;

  // Falling edge → конец drag'а.
  if GizmoDragging and (not LMB) then
  begin
    GizmoDragging := False;
    GizmoActiveAxis := GIZMO_AXIS_NONE;
    SaveConfig;
  end;

  // Патч игрового меню: накладывается, пока hover ИЛИ drag активны.
  if (GizmoDragging or (HoveredGizmoAxis <> GIZMO_AXIS_NONE)) and (not GizmoPatchActive) then
  begin
    ApplyMenuPatch;
    GizmoPatchActive := True;
  end
  else if (not GizmoDragging) and (HoveredGizmoAxis = GIZMO_AXIS_NONE) and GizmoPatchActive then
  begin
    RemoveMenuPatch;
    GizmoPatchActive := False;
  end;

  GizmoLastLMB := LMB;
end;

function IsGizmoActive: Boolean;
begin
  Result := GizmoDragging or (HoveredGizmoAxis <> GIZMO_AXIS_NONE);
end;

// Функция записи float значения в память
procedure WriteFloatToMemory(Address: Cardinal; Value: Single);
var
  OldProtect: Cardinal;
  FloatBytes: array[0..3] of Byte absolute Value;
  i: Integer;
begin
  try
    if VirtualProtect(Pointer(Address), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(Address + i)^ := FloatBytes[i];
      VirtualProtect(Pointer(Address), 4, OldProtect, OldProtect);
    end;
  except
    // Игнорируем ошибки
  end;
end;

// Функция чтения float значения из памяти
function ReadFloatFromMemory(Address: Cardinal): Single;
var
  OldProtect: Cardinal;
  FloatBytes: array[0..3] of Byte;
  i: Integer;
begin
  Result := 0.0;
  try
    if VirtualProtect(Pointer(Address), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        FloatBytes[i] := PByte(Address + i)^;
      VirtualProtect(Pointer(Address), 4, OldProtect, OldProtect);
      Result := PSingle(@FloatBytes[0])^;
    end;
  except
    // Игнорируем ошибки
  end;
end;

// Чтение оригинальных значений освещения
procedure ReadOriginalLightingValues;
begin
  if LightingValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Читаем оригинальные значения освещения...');
  OrigMainLightValue := ReadFloatFromMemory(MainLightAddr);
  OrigAdditionalLightValue := ReadFloatFromMemory(AdditionalLightAddr);
  OrigSunOrbitValue := ReadFloatFromMemory(SunOrbitAddr);
  OrigSunHeightValue := ReadFloatFromMemory(SunHeightAddr);
  LightingValuesRead := True;
  AddToLogFile(EngineLog, 'Оригинальные значения освещения сохранены');
end;

// Чтение оригинального значения угла обзора
procedure ReadOriginalViewAngleValues;
begin
  if ViewAngleValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Читаем оригинальные значения угла обзора...');
  OrigViewAngleValue := ReadFloatFromMemory(ViewAngleSliderAddr);
  ViewAngleValuesRead := True;
  AddToLogFile(EngineLog, 'Оригинальные значения угла обзора сохранены');
end;

// Чтение оригинального значения чувствительности камеры
procedure ReadOriginalCameraSensitivityValues;
begin
  if CameraSensitivityValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Читаем оригинальные значения чувствительности камеры...');
  OrigCameraSensitivityValue := ReadFloatFromMemory(CameraSensitivityAddr);
  CameraSensitivityValuesRead := True;
  AddToLogFile(EngineLog, 'Оригинальные значения чувствительности камеры сохранены');
end;

// Применение чувствительности камеры
procedure ApplyCameraSensitivity;
begin
  if not Settings.MainCamera or not Settings.CameraSensitivity then Exit;
  
  AddToLogFile(EngineLog, 'Применяем чувствительность камеры...');
  WriteFloatToMemory(CameraSensitivityAddr, Settings.CameraSensitivitySlider.Value);
  AddToLogFile(EngineLog, 'Чувствительность камеры применена');
end;

// Восстановление чувствительности камеры
procedure RestoreCameraSensitivity;
begin
  if not CameraSensitivityValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Восстанавливаем чувствительность камеры...');
  WriteFloatToMemory(CameraSensitivityAddr, OrigCameraSensitivityValue);
  AddToLogFile(EngineLog, 'Чувствительность камеры восстановлена');
end;

// Применение патча угла обзора
procedure ApplyViewAnglePatch;
var
  OldProtect: Cardinal;
  NopBytes1: array[0..5] of Byte;
  NopBytes2: array[0..6] of Byte;
  BytesWritten: Cardinal;
  i: Integer;
begin
  if ViewAnglePatched then Exit;
  
  if not Settings.MainCamera or not Settings.NewViewAngle then Exit;
  
  try
    AddToLogFile(EngineLog, 'Применяем патч угла обзора...');
    
    if not ReadProcessMemory(GetCurrentProcess, Pointer(ViewAngleNopAddr1), @OrigViewAngleBytes1[0], 6, BytesWritten) then
    begin
      AddToLogFile(EngineLog, 'Ошибка чтения оригинальных байтов угла обзора (адрес 1)');
      Exit;
    end;
    
    if not ReadProcessMemory(GetCurrentProcess, Pointer(ViewAngleNopAddr2), @OrigViewAngleBytes2[0], 6, BytesWritten) then
    begin
      AddToLogFile(EngineLog, 'Ошибка чтения оригинальных байтов угла обзора (адрес 2)');
      Exit;
    end;
    
    for i := 0 to 5 do
      NopBytes1[i] := $90;
    
    for i := 0 to 6 do
      NopBytes2[i] := $90;
    
    if VirtualProtect(Pointer(ViewAngleNopAddr1), 6, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      if WriteProcessMemory(GetCurrentProcess, Pointer(ViewAngleNopAddr1), @NopBytes1[0], 6, BytesWritten) then
        AddToLogFile(EngineLog, 'Патч угла обзора (адрес 1) успешно применен')
      else
      begin
        AddToLogFile(EngineLog, 'Ошибка записи патча угла обзора (адрес 1)');
        Exit;
      end;
      VirtualProtect(Pointer(ViewAngleNopAddr1), 6, OldProtect, OldProtect);
    end
    else
    begin
      AddToLogFile(EngineLog, 'Ошибка получения прав на запись патча угла обзора (адрес 1)');
      Exit;
    end;
    
    if VirtualProtect(Pointer(ViewAngleNopAddr2), 6, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      if WriteProcessMemory(GetCurrentProcess, Pointer(ViewAngleNopAddr2), @NopBytes2[0], 6, BytesWritten) then
      begin
        ViewAnglePatched := True;
        WriteFloatToMemory(ViewAngleSliderAddr, Settings.ViewAngleSlider.Value);
        AddToLogFile(EngineLog, 'Патч угла обзора (адрес 2) успешно применен');
      end
      else
        AddToLogFile(EngineLog, 'Ошибка записи патча угла обзора (адрес 2)');
      VirtualProtect(Pointer(ViewAngleNopAddr2), 6, OldProtect, OldProtect);
    end
    else
      AddToLogFile(EngineLog, 'Ошибка получения прав на запись патча угла обзора (адрес 2)');
      
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Исключение при применении патча угла обзора: ' + E.Message);
      ViewAnglePatched := False;
    end;
  end;
end;

// Восстановление патча угла обзора
procedure RemoveViewAnglePatch;
var
  OldProtect: Cardinal;
  BytesWritten: Cardinal;
begin
  if not ViewAnglePatched then Exit;
  
  try
    AddToLogFile(EngineLog, 'Восстанавливаем патч угла обзора...');
    
    if VirtualProtect(Pointer(ViewAngleNopAddr1), 6, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      if WriteProcessMemory(GetCurrentProcess, Pointer(ViewAngleNopAddr1), @OrigViewAngleBytes1[0], 6, BytesWritten) then
        AddToLogFile(EngineLog, 'Патч угла обзора (адрес 1) успешно восстановлен')
      else
        AddToLogFile(EngineLog, 'Ошибка восстановления патча угла обзора (адрес 1)');
      VirtualProtect(Pointer(ViewAngleNopAddr1), 6, OldProtect, OldProtect);
    end
    else
      AddToLogFile(EngineLog, 'Ошибка получения прав на запись при восстановлении патча угла обзора (адрес 1)');
    
    if VirtualProtect(Pointer(ViewAngleNopAddr2), 7, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      if WriteProcessMemory(GetCurrentProcess, Pointer(ViewAngleNopAddr2), @OrigViewAngleBytes2[0], 7, BytesWritten) then
      begin
        ViewAnglePatched := False;
        WriteFloatToMemory(ViewAngleSliderAddr, OrigViewAngleValue);
        AddToLogFile(EngineLog, 'Патч угла обзора (адрес 2) успешно восстановлен');
      end
      else
        AddToLogFile(EngineLog, 'Ошибка восстановления патча угла обзора (адрес 2)');
      VirtualProtect(Pointer(ViewAngleNopAddr2), 7, OldProtect, OldProtect);
    end
    else
      AddToLogFile(EngineLog, 'Ошибка получения прав на запись при восстановлении патча угла обзора (адрес 2)');
      
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Исключение при восстановлении патча угла обзора: ' + E.Message);
    end;
  end;
end;

// Чтение оригинальных значений дальности
procedure ReadOriginalDistanceValues;
var
  i: Integer;
begin
  if DistanceValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Читаем оригинальные значения дальности...');
  
  for i := 0 to 1 do
    OrigWireValues[i] := ReadFloatFromMemory(WireAddrs[i]);
  
  OrigDistantModelValue := ReadFloatFromMemory(DistantModelAddr);
  
  for i := 0 to 2 do
    OrigTrafficLightValues[i] := ReadFloatFromMemory(TrafficLightAddrs[i]);
  
  DistanceValuesRead := True;
  AddToLogFile(EngineLog, 'Оригинальные значения дальности сохранены');
end;

// Восстановление дефолтных значений освещения
procedure RestoreOriginalLightingValues;
begin
  if not LightingValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Восстанавливаем дефолтные значения освещения...');
  
  WriteFloatToMemory(MainLightAddr, OrigMainLightValue);
  WriteFloatToMemory(AdditionalLightAddr, OrigAdditionalLightValue);
  
  WriteFloatToMemory($482824, 9000.0);
  WriteFloatToMemory($48A7A8, 9000.0);
  
  WriteFloatToMemory($48282C, 8000.0);
  WriteFloatToMemory($48A7B0, 8000.0);
  WriteFloatToMemory($48DBC4, 8000.0);
  
  WriteFloatToMemory(SunOrbitAddr, OrigSunOrbitValue);
  WriteFloatToMemory(SunHeightAddr, OrigSunHeightValue);
  
  AddToLogFile(EngineLog, 'Дефолтные значения освещения восстановлены');
end;

// Восстановление оригинальных значений дальности
procedure RestoreOriginalDistanceValues;
var
  i: Integer;
begin
  if not DistanceValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Восстанавливаем оригинальные значения дальности...');
  
  for i := 0 to 1 do
    WriteFloatToMemory(WireAddrs[i], OrigWireValues[i]);
  
  WriteFloatToMemory(DistantModelAddr, OrigDistantModelValue);
  
  for i := 0 to 2 do
    WriteFloatToMemory(TrafficLightAddrs[i], OrigTrafficLightValues[i]);
  
  AddToLogFile(EngineLog, 'Оригинальные значения дальности восстановлены');
end;

// Применение настроек освещения
procedure ApplyLightingSettings;
begin
  if not Settings.Lighting then Exit;
  
  AddToLogFile(EngineLog, 'Применяем настройки освещения...');
  
  WriteFloatToMemory(MainLightAddr, Settings.MainLightIntensitySlider.Value);
  WriteFloatToMemory(AdditionalLightAddr, Settings.AdditionalLightIntensitySlider.Value);
  
  WriteFloatToMemory($482824, Settings.CabinBrightnessSlider.Value);
  WriteFloatToMemory($48A7A8, Settings.CabinBrightnessSlider.Value);
  
  WriteFloatToMemory($48282C, Settings.CabinContrastSlider.Value);
  WriteFloatToMemory($48A7B0, Settings.CabinContrastSlider.Value);
  WriteFloatToMemory($48DBC4, Settings.CabinContrastSlider.Value);
  
  WriteFloatToMemory(SunOrbitAddr, Settings.SunOrbitRadiusSlider.Value);
  WriteFloatToMemory(SunHeightAddr, Settings.SunHeightSlider.Value);
  
  AddToLogFile(EngineLog, 'Настройки освещения применены');
end;

// Применение настроек дальности с использованием слайдера
procedure ApplyDistanceSettings;
var
  SliderDistance: Single;
  i: Integer;
begin
  if not Settings.MaxVisibleDistance then 
  begin
    RestoreOriginalDistanceValues;
    Exit;
  end;
  
  SliderDistance := Settings.MaxVisibleDistanceSlider.Value;
  
  AddToLogFile(EngineLog, 'Применяем настройки дальности...');
  
  if Settings.ShowWires then
  begin
    for i := 0 to 1 do
      WriteFloatToMemory(WireAddrs[i], SliderDistance);
  end
  else
  begin
    for i := 0 to 1 do
      WriteFloatToMemory(WireAddrs[i], OrigWireValues[i]);
  end;
  
  if Settings.ShowDistantModels then
    WriteFloatToMemory(DistantModelAddr, SliderDistance)
  else
    WriteFloatToMemory(DistantModelAddr, OrigDistantModelValue);
  
  if Settings.ShowTrafficLights then
  begin
    for i := 0 to 2 do
      WriteFloatToMemory(TrafficLightAddrs[i], SliderDistance);
  end
  else
  begin
    for i := 0 to 2 do
      WriteFloatToMemory(TrafficLightAddrs[i], OrigTrafficLightValues[i]);
  end;
  
  AddToLogFile(EngineLog, 'Настройки дальности применены');
end;

// Отложенное применение настроек
procedure ApplySettingsThrottled;
var
  CurrentTime: Cardinal;
begin
  CurrentTime := GetTickCount;
  
  if (CurrentTime - LastApplyTime) >= ApplyInterval then
  begin
    if Settings.MaxVisibleDistance then
      ApplyDistanceSettings;
    if Settings.MainCamera and Settings.CameraSensitivity then
      ApplyCameraSensitivity;
    
    LastApplyTime := CurrentTime;
    PendingApply := False;
  end
  else
  begin
    PendingApply := True;
  end;
end;

procedure LoadConfig;
var
  F: TextFile;
  Line, Key, Value: string;
  ColonPos: Integer;
  OldFreecamState: Boolean;
  LangValue: Integer;
  CTCount, CTIdx, SrcVal: Integer;
  CTPrefix: string;

  // Возвращает True, если Key начинается с 'ctN_' и помещает индекс N в CTIdx,
  // а суффикс (без префикса 'ctN_') — в Out.
  function MatchCT(const K: string; out Idx: Integer; out Suffix: string): Boolean;
  var
    P, J: Integer;
    NumS: string;
  begin
    Result := False;
    if Length(K) < 4 then Exit;
    if (K[1] <> 'c') or (K[2] <> 't') then Exit;
    NumS := '';
    J := 3;
    while (J <= Length(K)) and (K[J] >= '0') and (K[J] <= '9') do
    begin
      NumS := NumS + K[J];
      Inc(J);
    end;
    if (NumS = '') or (J > Length(K)) or (K[J] <> '_') then Exit;
    Idx := StrToIntDef(NumS, -1);
    if Idx < 0 then Exit;
    Suffix := Copy(K, J + 1, Length(K));
    Result := True;
  end;
begin
  if not FileExists('zdbooster.cfg') then Exit;

  OldFreecamState := Settings.Freecam;
  
  try
    AssignFile(F, 'zdbooster.cfg');
    Reset(F);
    
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Line := SysUtils.Trim(Line);
      ColonPos := Pos(':', Line);
      if ColonPos > 0 then
      begin
        Key := SysUtils.Trim(Copy(Line, 1, ColonPos - 1));
        Value := SysUtils.Trim(Copy(Line, ColonPos + 1, Length(Line)));
        
        // ДОБАВЛЯЕМ ЗАГРУЗКУ ЯЗЫКА
        if Key = 'language' then 
        begin
          LangValue := StrToIntDef(Value, 0);
          if (LangValue >= 0) and (LangValue <= 2) then
            CurrentLanguage := TLanguage(LangValue);
        end;
        
        // Состояния модулей (0/1)
        if Key = 'freecam' then 
        begin
          Settings.Freecam := (Value = '1');
          Config_Freecam := Settings.Freecam;
        end;
        if Key = 'main_camera' then Settings.MainCamera := (Value = '1');
        if Key = 'max_distance' then Settings.MaxVisibleDistance := (Value = '1');
        if Key = 'newsky' then Settings.NewSky := (Value = '1');
        if Key = 'new_club_positions' then Settings.NewClubPositions := (Value = '1');
        if Key = 'ra3_hover' then Settings.RA3Hover := (Value = '1');
        if Key = 'new_view_angle' then Settings.NewViewAngle := (Value = '1');
        if Key = 'camera_sensitivity' then Settings.CameraSensitivity := (Value = '1');

        // Чекбоксы дальности
        if Key = 'show_wires' then Settings.ShowWires := (Value = '1');
        if Key = 'show_distant_models' then Settings.ShowDistantModels := (Value = '1');
        if Key = 'show_traffic_lights' then Settings.ShowTrafficLights := (Value = '1');

        // ZDS-Booster: System time override.
        if Key = 'system_time' then begin
          Settings.SystemTimeEnable := (Value = '1');
          InitSystemTimeEnable := Settings.SystemTimeEnable;
        end;

        // ZDS-Booster: PostFX render-graph. Each key maps to both a Settings
        // field (for the menu toggles) and the matching Init*Enable global
        // that PostFX.pas reads on the next frame.
        if Key = 'fxaa' then begin
          Settings.FXAAEnable := (Value = '1');
          InitFXAAEnable := Settings.FXAAEnable;
        end;
        if Key = 'bloom' then begin
          Settings.BloomEnable := (Value = '1');
          InitBloomEnable := Settings.BloomEnable;
        end;
        if Key = 'postfx' then begin
          Settings.PostFXEnable := (Value = '1');
          InitPostFXEnable := Settings.PostFXEnable;
        end;
        if Key = 'tonemap' then begin
          Settings.TonemapEnable := (Value = '1');
          InitTonemapEnable := Settings.TonemapEnable;
        end;
        if Key = 'sharpen' then begin
          Settings.SharpenEnable := (Value = '1');
          InitSharpenEnable := Settings.SharpenEnable;
        end;
        if Key = 'vignette' then begin
          Settings.VignetteEnable := (Value = '1');
          InitVignetteEnable := Settings.VignetteEnable;
        end;
        if Key = 'grain' then begin
          Settings.GrainEnable := (Value = '1');
          InitGrainEnable := Settings.GrainEnable;
        end;
        if Key = 'ssao' then begin
          Settings.SSAOEnable := (Value = '1');
          InitSSAOEnable := Settings.SSAOEnable;
        end;
        if Key = 'fog' then begin
          Settings.FogEnable := (Value = '1');
          InitFogEnable := Settings.FogEnable;
        end;
        if Key = 'dof' then begin
          Settings.DOFEnable := (Value = '1');
          InitDOFEnable := Settings.DOFEnable;
        end;
        
        // Значения слайдеров (ParseFloat нормализует запятую/точку)
        if Key = 'basespeed' then Settings.BasespeedSlider.Value := ParseFloat(Value, 1.0);
        if Key = 'fastspeed' then Settings.FastspeedSlider.Value := ParseFloat(Value, 2.0);
        if Key = 'turnspeed' then Settings.TurnspeedSlider.Value := ParseFloat(Value, 1.5);
        if Key = 'stepforward' then Settings.StepForwardSlider.Value := ParseFloat(Value, 0.5);
        if Key = 'maxvisibledistance' then Settings.MaxVisibleDistanceSlider.Value := ParseFloat(Value, 1200);
        if Key = 'view_angle' then Settings.ViewAngleSlider.Value := ParseFloat(Value, 3.0);
        if Key = 'camera_sensitivity_value' then Settings.CameraSensitivitySlider.Value := ParseFloat(Value, 5.0);

        // Глобальный слайдер яркости
        if Key = 'brightness' then Settings.BrightnessSlider.Value := ParseFloat(Value, 0.0);

        // Режим гизмо
        if Key = 'gizmo_mode' then
        begin
          SrcVal := StrToIntDef(Value, 0);
          if (SrcVal >= 0) and (SrcVal <= Integer(High(TGizmoMode))) then
            GizmoMode := TGizmoMode(SrcVal);
        end;

        // --- Кастомные 3D-тексты ---
        // Сначала размер массива:
        if Key = 'custom_text_count' then
        begin
          CTCount := StrToIntDef(Value, 0);
          if CTCount < 0 then CTCount := 0;
          if CTCount > 256 then CTCount := 256; // sanity-cap
          SetLength(CustomTexts, CTCount);
          // Заполняем дефолтами на случай неполного конфига:
          for CTIdx := 0 to CTCount - 1 do
          begin
            CustomTexts[CTIdx].Source  := ksSpeed;
            CustomTexts[CTIdx].X       := 0.0;
            CustomTexts[CTIdx].Y       := 0.0;
            CustomTexts[CTIdx].Z       := 0.2;
            CustomTexts[CTIdx].RX      := -90.0;
            CustomTexts[CTIdx].RY      := 0.0;
            CustomTexts[CTIdx].RZ      := 0.0;
            CustomTexts[CTIdx].Scale   := CUSTOM_TEXT_DEFAULT_SCALE;
            CustomTexts[CTIdx].Visible := True;
            CustomTexts[CTIdx].Color   := $FFFFFF;
          end;
          if CustomTextSelectedIdx >= CTCount then
            CustomTextSelectedIdx := CTCount - 1;
        end
        else if MatchCT(Key, CTIdx, CTPrefix) and (CTIdx < Length(CustomTexts)) then
        begin
          if CTPrefix = 'source' then
          begin
            SrcVal := StrToIntDef(Value, 0);
            if (SrcVal >= 0) and (SrcVal < KLUB_SOURCE_COUNT) then
              CustomTexts[CTIdx].Source := TKlubSource(SrcVal);
          end
          else if CTPrefix = 'color'   then CustomTexts[CTIdx].Color   := Cardinal(StrToIntDef(Value, $FFFFFF))
          else if CTPrefix = 'x'       then CustomTexts[CTIdx].X       := ParseFloat(Value, 0.0)
          else if CTPrefix = 'y'       then CustomTexts[CTIdx].Y       := ParseFloat(Value, 0.0)
          else if CTPrefix = 'z'       then CustomTexts[CTIdx].Z       := ParseFloat(Value, 0.2)
          else if CTPrefix = 'rx'      then CustomTexts[CTIdx].RX      := ParseFloat(Value, -90.0)
          else if CTPrefix = 'ry'      then CustomTexts[CTIdx].RY      := ParseFloat(Value, 0.0)
          else if CTPrefix = 'rz'      then CustomTexts[CTIdx].RZ      := ParseFloat(Value, 0.0)
          else if CTPrefix = 'scale'   then CustomTexts[CTIdx].Scale   := ParseFloat(Value, CUSTOM_TEXT_DEFAULT_SCALE)
          else if CTPrefix = 'visible' then CustomTexts[CTIdx].Visible := (Value = '1');
        end;
      end;
    end;
    CloseFile(F);

    // Если что-то выбрано, синхронизируем слайдеры (значения изменились на диске).
    if (CustomTextSelectedIdx >= 0) and (CustomTextSelectedIdx < Length(CustomTexts)) then
      SyncSlidersFromSelected;

    MenuFreecamBaseSpeed := Settings.BasespeedSlider.Value;
    MenuFreecamFastSpeed := Settings.FastspeedSlider.Value;
    MenuFreecamTurnSpeed := Settings.TurnspeedSlider.Value;
    stepforward := Settings.StepForwardSlider.Value;
    maxvisibledistance := Settings.MaxVisibleDistanceSlider.Value;
    newsky := Settings.NewSky;

    SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
    
  except on E: Exception do
    AddToLogFile(EngineLog, 'Ошибка чтения конфига: ' + E.Message);
  end;
end;

// Оптимизированное чтение конфига
procedure LoadConfigThrottled;
var
  CurrentTime: Cardinal;
begin
  CurrentTime := GetTickCount;
  
  if (CurrentTime - LastConfigReadTime) >= ConfigReadInterval then
  begin
    LoadConfig;
    LastConfigReadTime := CurrentTime;
  end;
end;

// Принудительное чтение конфига
procedure LoadConfigForced;
begin
  LoadConfig;
  LastConfigReadTime := GetTickCount;
end;

procedure SaveConfig;
var
  F: TextFile;
  I_SAVE: Integer;
begin
  try
    AssignFile(F, 'zdbooster.cfg');
    Rewrite(F);
    
    // ДОБАВЛЯЕМ СОХРАНЕНИЕ ЯЗЫКА
    WriteLn(F, 'language: ' + IntToStr(Integer(CurrentLanguage)));
    
    // Состояния модулей (0/1)
    if Settings.Freecam then WriteLn(F, 'freecam: 1') else WriteLn(F, 'freecam: 0');
    if Settings.MainCamera then WriteLn(F, 'main_camera: 1') else WriteLn(F, 'main_camera: 0');
    if Settings.MaxVisibleDistance then WriteLn(F, 'max_distance: 1') else WriteLn(F, 'max_distance: 0');
    if Settings.NewSky then WriteLn(F, 'newsky: 1') else WriteLn(F, 'newsky: 0');
    if Settings.NewClubPositions then WriteLn(F, 'new_club_positions: 1') else WriteLn(F, 'new_club_positions: 0');
    if Settings.RA3Hover then WriteLn(F, 'ra3_hover: 1') else WriteLn(F, 'ra3_hover: 0');
    if Settings.NewViewAngle then WriteLn(F, 'new_view_angle: 1') else WriteLn(F, 'new_view_angle: 0');
    if Settings.CameraSensitivity then WriteLn(F, 'camera_sensitivity: 1') else WriteLn(F, 'camera_sensitivity: 0');
    
    // Чекбоксы дальности
    if Settings.ShowWires then WriteLn(F, 'show_wires: 1') else WriteLn(F, 'show_wires: 0');
    if Settings.ShowDistantModels then WriteLn(F, 'show_distant_models: 1') else WriteLn(F, 'show_distant_models: 0');
    if Settings.ShowTrafficLights then WriteLn(F, 'show_traffic_lights: 1') else WriteLn(F, 'show_traffic_lights: 0');

    // ZDS-Booster: System time override (WORLD-вкладка).
    if Settings.SystemTimeEnable then WriteLn(F, 'system_time: 1') else WriteLn(F, 'system_time: 0');

    // ZDS-Booster: graphics-effects toggles. PostFX re-reads each
    // Init*Enable global every frame, so simply mirroring the menu state
    // here keeps .cfg, in-memory globals and the live pipeline in sync
    // after SaveConfig(). `postfx:` is the master ("Графич. эффекты"); when
    // 0, all individual flags will also be 0 because SetPostFXMasterEnabled
    // forced them off when the user disabled the master.
    if Settings.PostFXEnable    then WriteLn(F, 'postfx: 1')      else WriteLn(F, 'postfx: 0');
    if Settings.FXAAEnable      then WriteLn(F, 'fxaa: 1')        else WriteLn(F, 'fxaa: 0');
    if Settings.BloomEnable     then WriteLn(F, 'bloom: 1')       else WriteLn(F, 'bloom: 0');
    if Settings.TonemapEnable   then WriteLn(F, 'tonemap: 1')     else WriteLn(F, 'tonemap: 0');
    if Settings.SharpenEnable   then WriteLn(F, 'sharpen: 1')     else WriteLn(F, 'sharpen: 0');
    if Settings.VignetteEnable  then WriteLn(F, 'vignette: 1')    else WriteLn(F, 'vignette: 0');
    if Settings.GrainEnable     then WriteLn(F, 'grain: 1')       else WriteLn(F, 'grain: 0');
    if Settings.SSAOEnable      then WriteLn(F, 'ssao: 1')        else WriteLn(F, 'ssao: 0');
    if Settings.FogEnable       then WriteLn(F, 'fog: 1')         else WriteLn(F, 'fog: 0');
    if Settings.DOFEnable       then WriteLn(F, 'dof: 1')         else WriteLn(F, 'dof: 0');
    
    // Значения слайдеров
    WriteLn(F, 'basespeed: ' + FormatValue(Settings.BasespeedSlider.Value));
    WriteLn(F, 'fastspeed: ' + FormatValue(Settings.FastspeedSlider.Value));
    WriteLn(F, 'turnspeed: ' + FormatValue(Settings.TurnspeedSlider.Value));
    WriteLn(F, 'stepforward: ' + FormatValue(Settings.StepForwardSlider.Value));
    WriteLn(F, 'maxvisibledistance: ' + IntToStr(Round(Settings.MaxVisibleDistanceSlider.Value)));
    WriteLn(F, 'view_angle: ' + FormatValue(Settings.ViewAngleSlider.Value));
    WriteLn(F, 'camera_sensitivity_value: ' + FormatValue(Settings.CameraSensitivitySlider.Value));
    
    // Глобальный слайдер яркости
    WriteLn(F, 'brightness: ' + FormatValue(Settings.BrightnessSlider.Value));

    // Режим гизмо
    WriteLn(F, 'gizmo_mode: ' + IntToStr(Integer(GizmoMode)));

    // Кастомные 3D-тексты в кабине РА-3
    WriteLn(F, 'custom_text_count: ' + IntToStr(Length(CustomTexts)));
    for I_SAVE := 0 to Length(CustomTexts) - 1 do
    begin
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_source: ' + IntToStr(Integer(CustomTexts[I_SAVE].Source)));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_x: ' + FormatValue(CustomTexts[I_SAVE].X));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_y: ' + FormatValue(CustomTexts[I_SAVE].Y));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_z: ' + FormatValue(CustomTexts[I_SAVE].Z));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_rx: ' + FormatValue(CustomTexts[I_SAVE].RX));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_ry: ' + FormatValue(CustomTexts[I_SAVE].RY));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_rz: ' + FormatValue(CustomTexts[I_SAVE].RZ));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_scale: ' + FormatValue(CustomTexts[I_SAVE].Scale));
      WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_color: ' + IntToStr(Integer(CustomTexts[I_SAVE].Color)));
      if CustomTexts[I_SAVE].Visible then
        WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_visible: 1')
      else
        WriteLn(F, 'ct' + IntToStr(I_SAVE) + '_visible: 0');
    end;

    CloseFile(F);
  except
    on E: Exception do
    begin
      try CloseFile(F); except end;
      AddToLogFile(EngineLog, 'Ошибка сохранения конфига (CheatMenu): ' + E.Message);
    end;
  end;

  // Зеркалим CustomTexts в per-loco файл data\<folder>\<num>\raildriver\custom_texts.cfg
  // Это основной store: при смене кабины оттуда же подгружается. Глобальный
  // zdbooster.cfg остаётся как fallback / общая конфигурация бустера.
  try
    SaveCustomTextsForCurrentLoco;
  except
    // Не валим SaveConfig из-за per-loco файла.
  end;
end;

// Применение патча КЛУБ
procedure ApplyClubPositionsPatch;
var
  OldProtect: Cardinal;
  i: Integer;
begin
  if ClubPositionsPatched then Exit;
  
  try
    AddToLogFile(EngineLog, 'Применяем КЛУБ патч...');
    
    // Speed X
    if VirtualProtect(Pointer(SpeedXAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(SpeedXAddr + i)^ := NewSpeedXValue[i];
      VirtualProtect(Pointer(SpeedXAddr), 4, OldProtect, OldProtect);
    end;
    
    // Allowed Speed
    if VirtualProtect(Pointer(AllowedSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(AllowedSpeedAddr + i)^ := NewAllowedSpeedValue[i];
      VirtualProtect(Pointer(AllowedSpeedAddr), 4, OldProtect, OldProtect);
    end;
    
    // Shunting Speed
    if VirtualProtect(Pointer(ShuntingSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(ShuntingSpeedAddr + i)^ := NewShuntingSpeedValue[i];
      VirtualProtect(Pointer(ShuntingSpeedAddr), 4, OldProtect, OldProtect);
    end;
    
    // Train Speed
    if VirtualProtect(Pointer(TrainSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(TrainSpeedAddr + i)^ := NewTrainSpeedValue[i];
      VirtualProtect(Pointer(TrainSpeedAddr), 4, OldProtect, OldProtect);
    end;
    
    // Time
    if VirtualProtect(Pointer(TimeAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(TimeAddr + i)^ := NewTimeValue[i];
      VirtualProtect(Pointer(TimeAddr), 4, OldProtect, OldProtect);
    end;
    
    // Number Accel
    if VirtualProtect(Pointer(NumberAccelAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(NumberAccelAddr + i)^ := NewNumberAccelValue[i];
      VirtualProtect(Pointer(NumberAccelAddr), 4, OldProtect, OldProtect);
    end;
    
    // Reverse
    if VirtualProtect(Pointer(ReverseAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(ReverseAddr + i)^ := NewReverseValue[i];
      VirtualProtect(Pointer(ReverseAddr), 4, OldProtect, OldProtect);
    end;
    
    // Additional
    if VirtualProtect(Pointer(AdditionalAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(AdditionalAddr + i)^ := NewAdditionalValue[i];
      VirtualProtect(Pointer(AdditionalAddr), 4, OldProtect, OldProtect);
    end;
    
    // Radius (10 байт)
    if VirtualProtect(Pointer(RadiusAddr), 10, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 9 do
        PByte(RadiusAddr + i)^ := NewRadiusValue[i];
      VirtualProtect(Pointer(RadiusAddr), 10, OldProtect, OldProtect);
    end;
    
    ClubPositionsPatched := True;
    AddToLogFile(EngineLog, 'КЛУБ патч успешно применен');
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Ошибка применения КЛУБ патча: ' + E.Message);
      ClubPositionsPatched := False;
    end;
  end;
end;

// Восстановление КЛУБ патча
procedure RemoveClubPositionsPatch;
var
  OldProtect: Cardinal;
  i: Integer;
begin
  if not ClubPositionsPatched then Exit;
  
  try
    AddToLogFile(EngineLog, 'Восстанавливаем оригинальные значения КЛУБ...');
    
    // Speed X
    if VirtualProtect(Pointer(SpeedXAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(SpeedXAddr + i)^ := OrigSpeedXValue[i];
      VirtualProtect(Pointer(SpeedXAddr), 4, OldProtect, OldProtect);
    end;
    
    // Allowed Speed
    if VirtualProtect(Pointer(AllowedSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(AllowedSpeedAddr + i)^ := OrigAllowedSpeedValue[i];
      VirtualProtect(Pointer(AllowedSpeedAddr), 4, OldProtect, OldProtect);
    end;
    
    // Shunting Speed
    if VirtualProtect(Pointer(ShuntingSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(ShuntingSpeedAddr + i)^ := OrigShuntingSpeedValue[i];
      VirtualProtect(Pointer(ShuntingSpeedAddr), 4, OldProtect, OldProtect);
    end;
    
    // Train Speed
    if VirtualProtect(Pointer(TrainSpeedAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(TrainSpeedAddr + i)^ := OrigTrainSpeedValue[i];
      VirtualProtect(Pointer(TrainSpeedAddr), 4, OldProtect, OldProtect);
    end;
    
    // Time
    if VirtualProtect(Pointer(TimeAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(TimeAddr + i)^ := OrigTimeValue[i];
      VirtualProtect(Pointer(TimeAddr), 4, OldProtect, OldProtect);
    end;
    
    // Number Accel
    if VirtualProtect(Pointer(NumberAccelAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(NumberAccelAddr + i)^ := OrigNumberAccelValue[i];
      VirtualProtect(Pointer(NumberAccelAddr), 4, OldProtect, OldProtect);
    end;
    
    // Reverse
    if VirtualProtect(Pointer(ReverseAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(ReverseAddr + i)^ := OrigReverseValue[i];
      VirtualProtect(Pointer(ReverseAddr), 4, OldProtect, OldProtect);
    end;
    
    // Additional
    if VirtualProtect(Pointer(AdditionalAddr), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 3 do
        PByte(AdditionalAddr + i)^ := OrigAdditionalValue[i];
      VirtualProtect(Pointer(AdditionalAddr), 4, OldProtect, OldProtect);
    end;
    
    // Radius (10 байт)
    if VirtualProtect(Pointer(RadiusAddr), 10, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 9 do
        PByte(RadiusAddr + i)^ := OrigRadiusValue[i];
      VirtualProtect(Pointer(RadiusAddr), 10, OldProtect, OldProtect);
    end;
    
    ClubPositionsPatched := False;
    AddToLogFile(EngineLog, 'КЛУБ патч успешно удален, оригинальные значения восстановлены');
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Ошибка восстановления КЛУБ патча: ' + E.Message);
    end;
  end;
end;

// Применение меню патча
procedure ApplyMenuPatch;
var
  OldProtect: Cardinal;
  BytesWritten: Cardinal;
  NopBytes: array[0..4] of Byte;
begin
  if MenuCallPatched then Exit;
  
  try
    AddToLogFile(EngineLog, 'Применяем меню патч...');
    
    if not ReadProcessMemory(GetCurrentProcess, Pointer(MenuCallAddr), @OrigMenuCallBytes[0], 5, BytesWritten) then
    begin
      AddToLogFile(EngineLog, 'Ошибка чтения оригинальных байтов меню патча');
      Exit;
    end;
    
    NopBytes[0] := $90;
    NopBytes[1] := $90;
    NopBytes[2] := $90;
    NopBytes[3] := $90;
    NopBytes[4] := $90;
    
    if VirtualProtect(Pointer(MenuCallAddr), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      if WriteProcessMemory(GetCurrentProcess, Pointer(MenuCallAddr), @NopBytes[0], 5, BytesWritten) then
      begin
        MenuCallPatched := True;
        AddToLogFile(EngineLog, 'Меню патч успешно применен');
      end
      else
        AddToLogFile(EngineLog, 'Ошибка записи меню патча');
      VirtualProtect(Pointer(MenuCallAddr), 5, OldProtect, OldProtect);
    end
    else
      AddToLogFile(EngineLog, 'Ошибка получения прав на запись меню патча');
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Исключение при применении меню патча: ' + E.Message);
      MenuCallPatched := False;
    end;
  end;
end;

procedure RemoveMenuPatch;
var
  OldProtect: Cardinal;
  BytesWritten: Cardinal;
begin
  if not MenuCallPatched then Exit;
  
  try
    AddToLogFile(EngineLog, 'Восстанавливаем меню патч...');
    
    if VirtualProtect(Pointer(MenuCallAddr), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      if WriteProcessMemory(GetCurrentProcess, Pointer(MenuCallAddr), @OrigMenuCallBytes[0], 5, BytesWritten) then
      begin
        MenuCallPatched := False;
        AddToLogFile(EngineLog, 'Меню патч успешно восстановлен');
      end
      else
        AddToLogFile(EngineLog, 'Ошибка восстановления меню патча');
      VirtualProtect(Pointer(MenuCallAddr), 5, OldProtect, OldProtect);
    end
    else
      AddToLogFile(EngineLog, 'Ошибка получения прав на запись при восстановлении меню патча');
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Исключение при восстановлении меню патча: ' + E.Message);
    end;
  end;
end;

// Функция трансформации координат с поворотом
procedure TransformCoords(var X, Y: Integer; CenterX, CenterY: Integer; Scale, Rotation: Single);
var
  DX, DY: Single;
  NewX, NewY: Single;
begin
  // Смещаем в начало координат
  DX := (X - CenterX) * Scale;
  DY := (Y - CenterY) * Scale;
  
  // Поворачиваем
  NewX := DX * Cos(Rotation) - DY * Sin(Rotation);
  NewY := DX * Sin(Rotation) + DY * Cos(Rotation);
  
  // Возвращаем обратно
  X := Round(CenterX + NewX);
  Y := Round(CenterY + NewY);
end;


procedure AnimateWindow(var Win: TWindow; DeltaTime: Single; CanAnimate: Boolean; Progress: Single; CenterX, CenterY: Integer);
const
  POS_SPEED  = 10.0;
  FADE_SPEED = 7.0;
  SCALE_SPEED = 11.0;
  ROT_SPEED  = 9.0;
  TY_SPEED   = 9.0;
  DRAG_SPEED = 14.0;
begin
  if CanAnimate then
  begin
    if Abs(Win.Alpha - Win.TargetAlpha) > 0.005 then
      Win.Alpha := Win.Alpha + (Win.TargetAlpha - Win.Alpha) * FADE_SPEED * DeltaTime;
    if Abs(Win.Scale - Win.TargetScale) > 0.005 then
      Win.Scale := Win.Scale + (Win.TargetScale - Win.Scale) * SCALE_SPEED * DeltaTime;
    if Abs(Win.Rotation - Win.TargetRotation) > 0.001 then
      Win.Rotation := Win.Rotation + (Win.TargetRotation - Win.Rotation) * ROT_SPEED * DeltaTime;
    if Abs(Win.TranslateY - Win.TargetTranslateY) > 0.5 then
      Win.TranslateY := Win.TranslateY + (Win.TargetTranslateY - Win.TranslateY) * TY_SPEED * DeltaTime;
    if Abs(Win.TransformProgress - Win.TargetTransformProgress) > 0.005 then
      Win.TransformProgress := Win.TransformProgress + (Win.TargetTransformProgress - Win.TransformProgress) * POS_SPEED * DeltaTime;
    if Abs(Win.ShadowIntensity - Win.TargetShadowIntensity) > 0.005 then
      Win.ShadowIntensity := Win.ShadowIntensity + (Win.TargetShadowIntensity - Win.ShadowIntensity) * FADE_SPEED * DeltaTime;
  end;
  // Ширина при перетаскивании анимируется всегда, независимо от SpawnDelay
  if Abs(Win.DragWidthExpansion - Win.TargetDragWidthExpansion) > 0.1 then
    Win.DragWidthExpansion := Win.DragWidthExpansion + (Win.TargetDragWidthExpansion - Win.DragWidthExpansion) * DRAG_SPEED * DeltaTime;
  // Позиция (центр -> финальная) движется по общему MenuAnimationProgress
  if not Win.IsDragging then
  begin
    Win.X := Round(CenterX + (Win.OriginalX - CenterX) * Progress);
    Win.Y := Round(CenterY + (Win.OriginalY - CenterY) * Progress);
  end;
end;

// Forward — реализация ниже у DrawModernCheckbox/Toggle вместе с другими
// helpers; UpdateAnimations должна вызвать PaperDragTick на каждом кадре.
procedure PaperDragTick(var Win: TWindow); forward;

procedure UpdateAnimations;
var
  CurrentTime: Cardinal;
  DeltaTime: Single;
  CenterX, CenterY: Integer;
begin
  CurrentTime := GetTickCount;
  if LastFrameTime = 0 then LastFrameTime := CurrentTime;

  DeltaTime := (CurrentTime - LastFrameTime) / 1000.0;
  if DeltaTime > 0.1 then DeltaTime := 0.1;
  LastFrameTime := CurrentTime;

  if PendingApply and ((CurrentTime - LastApplyTime) >= ApplyInterval) then
    ApplySettingsThrottled;

  // Таймер для каскада появления окон: растёт во время открытия, сбрасывается при закрытии
  if MenuVisible then
  begin
    MenuOpenTimer := MenuOpenTimer + DeltaTime;
    if MenuOpenTimer > 10.0 then MenuOpenTimer := 10.0;
  end
  else
    MenuOpenTimer := 0.0;

  // === АНИМАЦИЯ ОБЩЕГО СОСТОЯНИЯ МЕНЮ ===
  if Abs(MenuAnimationProgress - MenuTargetProgress) > 0.005 then
    MenuAnimationProgress := MenuAnimationProgress + (MenuTargetProgress - MenuAnimationProgress) * MenuAnimationSpeed * DeltaTime;

  CenterX := InitResX div 2;
  CenterY := InitResY div 2;

  // Paper-drag: гасим velocity и обновляем TargetRotation КАЖДЫЙ кадр —
  // даже если курсор не двигается (WM_MOUSEMOVE не приходит). Без этого
  // окно "залипало" под углом до следующего движения.
  PaperDragTick(RenderWindow);
  PaperDragTick(WorldWindow);
  PaperDragTick(LocomotiveWindow);
  PaperDragTick(MenuWindow);

  // При открытии окно начинает анимацию только после своего SpawnDelay; при закрытии — все сразу
  AnimateWindow(RenderWindow,     DeltaTime, (not MenuVisible) or (MenuOpenTimer >= RenderWindow.SpawnDelay),     MenuAnimationProgress, CenterX, CenterY);
  AnimateWindow(WorldWindow,      DeltaTime, (not MenuVisible) or (MenuOpenTimer >= WorldWindow.SpawnDelay),      MenuAnimationProgress, CenterX, CenterY);
  AnimateWindow(LocomotiveWindow, DeltaTime, (not MenuVisible) or (MenuOpenTimer >= LocomotiveWindow.SpawnDelay), MenuAnimationProgress, CenterX, CenterY);
  AnimateWindow(MenuWindow,       DeltaTime, (not MenuVisible) or (MenuOpenTimer >= MenuWindow.SpawnDelay),       MenuAnimationProgress, CenterX, CenterY);
  
  // Анимация секций
  with Settings do
  begin
    // Freecam секция
    if FreecamSection.Expanded then
    begin
      if FreecamSection.AnimProgress < 1.0 then
        FreecamSection.AnimProgress := FreecamSection.AnimProgress + 5.0 * DeltaTime;
    end
    else
    begin
      if FreecamSection.AnimProgress > 0.0 then
        FreecamSection.AnimProgress := FreecamSection.AnimProgress - 5.0 * DeltaTime;
    end;
    if FreecamSection.AnimProgress < 0 then FreecamSection.AnimProgress := 0;
    if FreecamSection.AnimProgress > 1 then FreecamSection.AnimProgress := 1;
    
    // MainCamera секция
    if MainCameraSection.Expanded then
    begin
      if MainCameraSection.AnimProgress < 1.0 then
        MainCameraSection.AnimProgress := MainCameraSection.AnimProgress + 5.0 * DeltaTime;
    end
    else
    begin
      if MainCameraSection.AnimProgress > 0.0 then
        MainCameraSection.AnimProgress := MainCameraSection.AnimProgress - 5.0 * DeltaTime;
    end;
    if MainCameraSection.AnimProgress < 0 then MainCameraSection.AnimProgress := 0;
    if MainCameraSection.AnimProgress > 1 then MainCameraSection.AnimProgress := 1;
    
    // MaxVisibleDistance секция
    if MaxVisibleDistanceSection.Expanded then
    begin
      if MaxVisibleDistanceSection.AnimProgress < 1.0 then
        MaxVisibleDistanceSection.AnimProgress := MaxVisibleDistanceSection.AnimProgress + 5.0 * DeltaTime;
    end
    else
    begin
      if MaxVisibleDistanceSection.AnimProgress > 0.0 then
        MaxVisibleDistanceSection.AnimProgress := MaxVisibleDistanceSection.AnimProgress - 5.0 * DeltaTime;
    end;
    if MaxVisibleDistanceSection.AnimProgress < 0 then MaxVisibleDistanceSection.AnimProgress := 0;
    if MaxVisibleDistanceSection.AnimProgress > 1 then MaxVisibleDistanceSection.AnimProgress := 1;

    // PostFX секция (Phase 1 + Phase 2). Та же 5x exponential glide, что и
    // у остальных раскрывающихся секций — глаз не различает разницы.
    if PostFXSection.Expanded then
    begin
      if PostFXSection.AnimProgress < 1.0 then
        PostFXSection.AnimProgress := PostFXSection.AnimProgress + 5.0 * DeltaTime;
    end
    else
    begin
      if PostFXSection.AnimProgress > 0.0 then
        PostFXSection.AnimProgress := PostFXSection.AnimProgress - 5.0 * DeltaTime;
    end;
    if PostFXSection.AnimProgress < 0 then PostFXSection.AnimProgress := 0;
    if PostFXSection.AnimProgress > 1 then PostFXSection.AnimProgress := 1;

    // Developer секция
    if DeveloperSection.Expanded then
    begin
      if DeveloperSection.AnimProgress < 1.0 then
        DeveloperSection.AnimProgress := DeveloperSection.AnimProgress + 5.0 * DeltaTime;
    end
    else
    begin
      if DeveloperSection.AnimProgress > 0.0 then
        DeveloperSection.AnimProgress := DeveloperSection.AnimProgress - 5.0 * DeltaTime;
    end;
    if DeveloperSection.AnimProgress < 0 then DeveloperSection.AnimProgress := 0;
    if DeveloperSection.AnimProgress > 1 then DeveloperSection.AnimProgress := 1;
  end;
end;


procedure InitCheatMenu; stdcall;
begin
  FillChar(Settings, SizeOf(Settings), 0);

  // ZDS-Booster: seed Visual tab state from engine globals.
  Settings.FXAAEnable := InitFXAAEnable;
  Settings.BloomEnable := InitBloomEnable;
  // Seed system-time toggle from its global.
  Settings.SystemTimeEnable := InitSystemTimeEnable;
  // PostFX (Phase 1 + Phase 2) — seed from Variables.pas defaults.
  Settings.PostFXEnable    := InitPostFXEnable;
  Settings.TonemapEnable   := InitTonemapEnable;
  Settings.SharpenEnable   := InitSharpenEnable;
  Settings.VignetteEnable  := InitVignetteEnable;
  Settings.GrainEnable     := InitGrainEnable;
  Settings.SSAOEnable      := InitSSAOEnable;
  Settings.FogEnable       := InitFogEnable;
  Settings.DOFEnable       := InitDOFEnable;

  // Инициализация ГЛОБАЛЬНОГО слайдера яркости
  Settings.BrightnessSlider.Value := 0.0;
  Settings.BrightnessSlider.MinValue := 0.0;
  Settings.BrightnessSlider.MaxValue := 255.0;
  Settings.BrightnessSlider.HoverProgress := 0.0;
  
  // Инициализация слайдеров FREECAM
  Settings.BasespeedSlider.Value := 0.01;
  Settings.BasespeedSlider.MinValue := 0.01;
  Settings.BasespeedSlider.MaxValue := 1.00;
  Settings.BasespeedSlider.HoverProgress := 0.0;
  
  Settings.FastspeedSlider.Value := 1.5;
  Settings.FastspeedSlider.MinValue := 0.01;
  Settings.FastspeedSlider.MaxValue := 2.0;
  Settings.FastspeedSlider.HoverProgress := 0.0;
  
  Settings.TurnspeedSlider.Value := 1.5;
  Settings.TurnspeedSlider.MinValue := 0.01;
  Settings.TurnspeedSlider.MaxValue := 2.0;
  Settings.TurnspeedSlider.HoverProgress := 0.0;
  
  Settings.StepForwardSlider.Value := 0.5;
  Settings.StepForwardSlider.MinValue := 0.01;
  Settings.StepForwardSlider.MaxValue := 1.0;
  Settings.StepForwardSlider.HoverProgress := 0.0;
  
  Settings.MaxVisibleDistanceSlider.Value := 1200;
  Settings.MaxVisibleDistanceSlider.MinValue := 800;
  Settings.MaxVisibleDistanceSlider.MaxValue := 1600;
  Settings.MaxVisibleDistanceSlider.HoverProgress := 0.0;
  
  // Слайдер угла обзора
  Settings.ViewAngleSlider.Value := 3.0;
  Settings.ViewAngleSlider.MinValue := 0.0;
  Settings.ViewAngleSlider.MaxValue := 7.0;
  Settings.ViewAngleSlider.HoverProgress := 0.0;
  
  // Слайдер чувствительности камеры
  Settings.CameraSensitivitySlider.Value := 5.0;
  Settings.CameraSensitivitySlider.MinValue := 1.0;
  Settings.CameraSensitivitySlider.MaxValue := 9.0;
  Settings.CameraSensitivitySlider.HoverProgress := 0.0;

  // Слайдеры редактора кастомных 3D-текстов
  InitCustomTextSliders;
  SetLength(CustomTexts, 0);
  CustomTextSelectedIdx := -1;

  // Адреса для угла обзора
  ViewAngleNopAddr1 := $723909;
  ViewAngleNopAddr2 := $72384C;
  ViewAngleSliderAddr := $725C1C;

  // Адрес для чувствительности камеры
  CameraSensitivityAddr := $7229F8;

  // Читаем оригинальные значения
  ReadOriginalViewAngleValues;
  ReadOriginalCameraSensitivityValues;

  // Применяем патчи если нужно
  if Settings.MainCamera and Settings.NewViewAngle then
    ApplyViewAnglePatch;

  if Settings.MainCamera and Settings.CameraSensitivity then
    ApplyCameraSensitivity;

  MenuFreecamBaseSpeed := Settings.BasespeedSlider.Value;
  MenuFreecamFastSpeed := Settings.FastspeedSlider.Value;
  MenuFreecamTurnSpeed := Settings.TurnspeedSlider.Value;

  stepforward := Settings.StepForwardSlider.Value;
  maxvisibledistance := Settings.MaxVisibleDistanceSlider.Value;

  // Инициализация адресов
  SpeedXAddr := $00400000 + $84B2B;
  AllowedSpeedAddr := $00400000 + $84D25;
  ShuntingSpeedAddr := $00400000 + $853C6;
  TrainSpeedAddr := $00400000 + $853E0;
  TimeAddr := $00400000 + $854B1;
  NumberAccelAddr := $00400000 + $85979;
  ReverseAddr := $00400000 + $84CA3;
  AdditionalAddr := $00400000 + $85630;
  RadiusAddr := $00400000 + $85F40;
  
  MenuCallAddr := $743B9E;
  
  // Адреса освещения
  MainLightAddr := $4942AC;
  AdditionalLightAddr := $4942B4;
  SunOrbitAddr := $4942CC;
  SunHeightAddr := $4942B8;
  
  // Адреса дальности
  WireAddrs[0] := $494408;
  WireAddrs[1] := $494414;
  DistantModelAddr := $494358;
  TrafficLightAddrs[0] := $48DB9C;
  TrafficLightAddrs[1] := $48DC1C;
  TrafficLightAddrs[2] := $48DBA0;
  
  // Читаем оригинальные значения
  ReadOriginalLightingValues;
  ReadOriginalDistanceValues;
  
  // Применяем патчи если нужно
  if Settings.NewClubPositions then
    ApplyClubPositionsPatch;
    
  if Settings.MaxVisibleDistance then
    ApplyDistanceSettings;

  LoadConfig;

  SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);

  // Инициализация окон с transform анимацией
  RenderWindow.Title := GetText('RenderTitle');
  RenderWindow.X := 50;
  RenderWindow.Y := 50;
  RenderWindow.Width := 280;
  RenderWindow.OriginalWidth := 280;
  RenderWindow.Height := 120;
  RenderWindow.Alpha := 0.0;
  RenderWindow.TargetAlpha := 1.0;
  RenderWindow.OriginalX := 50;
  RenderWindow.OriginalY := 50;
  RenderWindow.Scale := 0.3;
  RenderWindow.TargetScale := 1.0;
  RenderWindow.TransformProgress := 0.0;
  RenderWindow.TargetTransformProgress := 1.0;
  RenderWindow.Rotation := 0.0;
  RenderWindow.TargetRotation := 0.0;
  RenderWindow.TranslateY := -200.0;
  RenderWindow.TargetTranslateY := 0.0;
  RenderWindow.DragWidthExpansion := 0.0;
  RenderWindow.TargetDragWidthExpansion := 0.0;
  RenderWindow.ShadowIntensity := 0.8;
  RenderWindow.TargetShadowIntensity := 0.8;
  RenderWindow.SpawnDelay := 0.0;
  
  WorldWindow.Title := GetText('WorldTitle');
  WorldWindow.X := 350;
  WorldWindow.Y := 50;
  WorldWindow.Width := 280;
  WorldWindow.OriginalWidth := 280;
  WorldWindow.Height := 170;
  WorldWindow.Alpha := 0.0;
  WorldWindow.TargetAlpha := 1.0;
  WorldWindow.OriginalX := 350;
  WorldWindow.OriginalY := 50;
  WorldWindow.Scale := 0.3;
  WorldWindow.TargetScale := 1.0;
  WorldWindow.TransformProgress := 0.0;
  WorldWindow.TargetTransformProgress := 1.0;
  WorldWindow.Rotation := 0.0;
  WorldWindow.TargetRotation := 0.0;
  WorldWindow.TranslateY := -150.0;
  WorldWindow.TargetTranslateY := 0.0;
  WorldWindow.DragWidthExpansion := 0.0;
  WorldWindow.TargetDragWidthExpansion := 0.0;
  WorldWindow.ShadowIntensity := 0.8;
  WorldWindow.TargetShadowIntensity := 0.8;
  WorldWindow.SpawnDelay := 0.1;
  
  LocomotiveWindow.Title := GetText('LocomotiveTitle');
  LocomotiveWindow.X := 650;
  LocomotiveWindow.Y := 50;
  LocomotiveWindow.Width := 280;
  LocomotiveWindow.OriginalWidth := 280;
  LocomotiveWindow.Height := 100;
  LocomotiveWindow.Alpha := 0.0;
  LocomotiveWindow.TargetAlpha := 1.0;
  LocomotiveWindow.OriginalX := 650;
  LocomotiveWindow.OriginalY := 50;
  LocomotiveWindow.Scale := 0.3;
  LocomotiveWindow.TargetScale := 1.0;
  LocomotiveWindow.TransformProgress := 0.0;
  LocomotiveWindow.TargetTransformProgress := 1.0;
  LocomotiveWindow.Rotation := 0.0;
  LocomotiveWindow.TargetRotation := 0.0;
  LocomotiveWindow.TranslateY := -100.0;
  LocomotiveWindow.TargetTranslateY := 0.0;
  LocomotiveWindow.DragWidthExpansion := 0.0;
  LocomotiveWindow.TargetDragWidthExpansion := 0.0;
  LocomotiveWindow.ShadowIntensity := 0.8;
  LocomotiveWindow.TargetShadowIntensity := 0.8;
  LocomotiveWindow.SpawnDelay := 0.2;
  
  MenuWindow.Title := GetText('MenuTitle');
  MenuWindow.X := 950;
  MenuWindow.Y := 50;
  MenuWindow.Width := 320;
  MenuWindow.OriginalWidth := 320;
  MenuWindow.Height := 200;
  MenuWindow.Alpha := 0.0;
  MenuWindow.TargetAlpha := 1.0;
  MenuWindow.OriginalX := 950;
  MenuWindow.OriginalY := 50;
  MenuWindow.Scale := 0.3;
  MenuWindow.TargetScale := 1.0;
  MenuWindow.TransformProgress := 0.0;
  MenuWindow.TargetTransformProgress := 1.0;
  MenuWindow.Rotation := 0.0;
  MenuWindow.TargetRotation := 0.0;
  MenuWindow.TranslateY := -250.0;
  MenuWindow.TargetTranslateY := 0.0;
  MenuWindow.DragWidthExpansion := 0.0;
  MenuWindow.TargetDragWidthExpansion := 0.0;
  MenuWindow.ShadowIntensity := 0.8;
  MenuWindow.TargetShadowIntensity := 0.8;
  MenuWindow.SpawnDelay := 0.3;
end;

// Современная кнопка развертывания
procedure DrawModernExpandButton(X, Y: Integer; Expanded: Boolean; Alpha: Integer; HoverProgress: Single = 0.0);
var
  CenterX, CenterY: Integer;
  ButtonColor: Integer;
  IconColor: Integer;
  Size: Integer;
begin
  CenterX := X + BUTTON_SIZE div 2;
  CenterY := Y + BUTTON_SIZE div 2;
  Size := Round(10 + HoverProgress * 2);
  
  ButtonColor := LerpColor(COLOR_SURFACE_VARIANT, COLOR_PRIMARY, HoverProgress);
  IconColor := LerpColor(COLOR_ON_SURFACE, COLOR_ON_PRIMARY, HoverProgress);
  
  // Фон кнопки
  DrawCircle2D_Fill(CenterX, CenterY, Size, ButtonColor, Alpha);
  DrawCircle2D(CenterX, CenterY, Size, COLOR_BORDER_LIGHT, Alpha div 3);
  
  // Иконка
  DrawLine2D(CenterX - 5, CenterY, CenterX + 5, CenterY, IconColor, Alpha, 2.0);
  if not Expanded then
    DrawLine2D(CenterX, CenterY - 5, CenterX, CenterY + 5, IconColor, Alpha, 2.0);
end;

// Универсальный hover-detection. Возвращает 1.0 если курсор находится
// внутри прямоугольника (X, Y, W, H), иначе 0.0. Координаты курсора берутся
// из глобальных MoveXcoord / MoveYcoord (screen-space, обновляются движком
// каждый кадр), те же что используют гизмо и hover в DrawPostFXToggles.
//
// Бинарный (0/1) вместо плавной анимации: DrawModernToggle/Checkbox внутри
// уже лерпит цвета через LerpColor(...) — на глаз получается мягкий
// переход за счёт сглаживания между кадрами.
function HoverFor(X, Y, W, H: Integer): Single;
var
  MX, MY: Integer;
begin
  MX := Round(MoveXcoord);
  MY := Round(MoveYcoord);
  if (MX >= X) and (MX < X + W) and (MY >= Y) and (MY < Y + H) then
    Result := 1.0
  else
    Result := 0.0;
end;

function ClampF(V, A, B: Single): Single;
begin
  if V < A then Result := A
  else if V > B then Result := B
  else Result := V;
end;

// "Paper drag" rotation: окно отстаёт в наклоне за курсором, как лист
// бумаги, который держишь сверху и тянешь в сторону.
//
// Логика разнесена на два хука:
//
//   * PaperDragOnMouseMove(Win, X) — вызывается на WM_MOUSEMOVE при
//     активном drag'е. Накапливает мгновенную velocity курсора в
//     Win.DragVelX (с инжекцией свежего DeltaX поверх остатка предыдущей
//     велосити, чтобы быстрые рывки сразу давали большой угол).
//
//   * PaperDragTick(Win) — вызывается каждый кадр в UpdateAnimations.
//     Затухает Win.DragVelX (per-frame damping) и переписывает
//     Win.TargetRotation. Это критично: WM_MOUSEMOVE приходит только
//     когда курсор реально двигается, а нам нужно гасить наклон даже
//     при неподвижном курсоре, иначе окно остаётся "залипшим" под
//     углом до следующего движения.
//
// PAPER_VEL_GAIN     — сколько радиан угла на 1 единицу velocity
// PAPER_VEL_DAMP     — per-frame retention (0..1)
// PAPER_ROT_LIMIT    — максимальный угол в радианах (~20°)
const
  PAPER_VEL_GAIN  = 0.02;
  PAPER_VEL_DAMP  = 0.82;
  PAPER_ROT_LIMIT = 0.35;

procedure PaperDragOnMouseMove(var Win: TWindow; CursorX: Integer);
var
  DeltaX: Integer;
begin
  DeltaX := CursorX - Win.PrevDragX;
  Win.PrevDragX := CursorX;
  // Прибавляем новый дельта поверх существующей velocity — рывок сразу
  // даёт большой угол, повторные рывки в одну сторону усиливают эффект.
  Win.DragVelX := Win.DragVelX + DeltaX;
  // Чтобы рывки не уходили в бесконечность, сразу обрезаем по разумному
  // потолку (effectively limits per-event impact).
  if Win.DragVelX >  60.0 then Win.DragVelX :=  60.0;
  if Win.DragVelX < -60.0 then Win.DragVelX := -60.0;
end;

procedure PaperDragTick(var Win: TWindow);
begin
  if Win.IsDragging then
  begin
    Win.DragVelX := Win.DragVelX * PAPER_VEL_DAMP;
    Win.TargetRotation := ClampF(Win.DragVelX * PAPER_VEL_GAIN,
                                 -PAPER_ROT_LIMIT, PAPER_ROT_LIMIT);
  end
  else
  begin
    Win.DragVelX := 0.0;
  end;
end;

// Современный чекбокс
procedure DrawModernCheckbox(X, Y: Integer; Text: string; Checked: Boolean; Alpha: Integer; HoverProgress: Single = 0.0);
var
  BgColor, BorderColor, TextColor, CheckColor: Integer;
begin
  if Checked then
  begin
    BgColor := LerpColor(COLOR_PRIMARY, COLOR_ACCENT, HoverProgress);
    BorderColor := BgColor;
    TextColor := COLOR_ON_SURFACE;
    CheckColor := COLOR_ON_PRIMARY;
  end
  else
  begin
    BgColor := LerpColor(COLOR_SURFACE, COLOR_SURFACE_VARIANT, HoverProgress);
    BorderColor := LerpColor(COLOR_BORDER, COLOR_BORDER_LIGHT, HoverProgress);
    TextColor := COLOR_ON_SURFACE;
    CheckColor := COLOR_ON_PRIMARY;
  end;
  
  // Фон чекбокса
  DrawStyledRect(X, Y, CHECKBOX_SIZE, CHECKBOX_SIZE, BgColor, Alpha, True, BorderColor);
  
  // Галочка
  if Checked then
  begin
    DrawLine2D(X + 4, Y + 9, X + 7, Y + 12, CheckColor, Alpha, 2.0);
    DrawLine2D(X + 7, Y + 12, X + 13, Y + 6, CheckColor, Alpha, 2.0);
  end;
  
  // Текст
  DrawModernText(X + CHECKBOX_SIZE + 10, Y - 1, Text, TextColor, Alpha, 0.75);
end;

// Современный слайдер
procedure DrawModernSlider(X, Y: Integer; var Slider: TSlider; Text: string; Alpha: Integer; WinScale: Single = 1.0);
var
  Progress: Single;
  SliderX: Integer;
  ValueText: string;
  TrackColor, ActiveColor, ThumbColor: Integer;
  ThumbSize: Integer;
  ScaledWidth: Integer;
begin
  if Alpha <= 0 then Exit;
  
  ScaledWidth := Round(SLIDER_WIDTH * WinScale);
  Progress := (Slider.Value - Slider.MinValue) / (Slider.MaxValue - Slider.MinValue);
  SliderX := X + Round(Progress * ScaledWidth);
  
  ValueText := FormatValue(Slider.Value);
  
  // Цвета в зависимости от состояния
  TrackColor := COLOR_SURFACE_VARIANT;
  ActiveColor := LerpColor(COLOR_PRIMARY, COLOR_ACCENT, Slider.HoverProgress);
  ThumbColor := LerpColor(COLOR_PRIMARY, COLOR_ACCENT, Slider.HoverProgress);
  ThumbSize := Round(8 + Slider.HoverProgress * 3);
  
  // Заголовок и значение
  DrawModernText(X, Y - 22, Text + ': ' + ValueText, COLOR_ON_SURFACE, Alpha, 0.7);
  
  // Трек слайдера
  DrawStyledRect(X, Y + 8, ScaledWidth, 6, TrackColor, Alpha, True, COLOR_BORDER);
  
  // Активная часть
  if Progress > 0 then
    DrawStyledRect(X, Y + 8, Round(Progress * ScaledWidth), 6, ActiveColor, Alpha, True, ActiveColor);
  
  // Ползунок
  DrawCircle2D_Fill(SliderX, Y + 11, ThumbSize + 2, COLOR_SURFACE, Alpha);
  DrawCircle2D_Fill(SliderX, Y + 11, ThumbSize, ThumbColor, Alpha);
  DrawCircle2D(SliderX, Y + 11, ThumbSize + 1, COLOR_BORDER_LIGHT, Alpha div 3);
end;

// Современная кнопка переключения
procedure DrawModernToggle(X, Y: Integer; Text: string; Enabled: Boolean; Alpha: Integer; HasExpandButton: Boolean = False; ExpandButtonX: Integer = 0; Expanded: Boolean = False; HoverProgress: Single = 0.0);
var
  BgColor, BorderColor, TextColor: Integer;
  TextX: Integer;
begin
  if Enabled then
  begin
    BgColor := LerpColor(COLOR_PRIMARY, COLOR_ACCENT, HoverProgress);
    BorderColor := BgColor;
    TextColor := COLOR_ON_PRIMARY;
  end
  else
  begin
    BgColor := LerpColor(COLOR_SURFACE, COLOR_SURFACE_VARIANT, HoverProgress);
    BorderColor := LerpColor(COLOR_BORDER, COLOR_BORDER_LIGHT, HoverProgress);
    TextColor := LerpColor(COLOR_ON_SURFACE, COLOR_ON_SURFACE, HoverProgress);
  end;
  
  // Фон кнопки
  DrawStyledRect(X, Y, 240, ITEM_HEIGHT, BgColor, Alpha, True, BorderColor);
  
  // Текст
  TextX := X + 16;
  DrawModernText(TextX, Y + 6, Text, TextColor, Alpha, 0.8);
  
  // Кнопка развертывания
  if HasExpandButton then
    DrawModernExpandButton(ExpandButtonX, Y + 6, Expanded, Alpha, HoverProgress);
end;

// Современная кнопка языка
procedure DrawModernLanguageButton(X, Y, Width, Height: Integer; Text: string; Alpha: Integer; Selected: Boolean = False; HoverProgress: Single = 0.0);
var
  BgColor, BorderColor, TextColor: Integer;
  TextWidth, TextX: Integer;
begin
  if Selected then
  begin
    BgColor := LerpColor(COLOR_PRIMARY, COLOR_ACCENT, HoverProgress);
    BorderColor := BgColor;
    TextColor := COLOR_ON_PRIMARY;
  end
  else
  begin
    BgColor := LerpColor(COLOR_SURFACE, COLOR_SURFACE_VARIANT, HoverProgress);
    BorderColor := LerpColor(COLOR_BORDER, COLOR_BORDER_LIGHT, HoverProgress);
    TextColor := COLOR_ON_SURFACE;
  end;
  
  // Фон кнопки
  DrawStyledRect(X, Y, Width, Height, BgColor, Alpha, True, BorderColor);
  
  // Центрированный текст
  TextWidth := Length(Text) * 8;
  TextX := X + (Width - TextWidth) div 2;
  
  DrawModernText(TextX, Y + (Height - 20) div 2, Text, TextColor, Alpha, 0.75);
end;

// Главная функция отрисовки окна с transform эффектами
// Безопасные обёртки над функциями KlubData: если игра ещё не инициализирована,
// чтение по указателям может упасть — возвращаем '—' вместо падения меню.
function SK_Speed: string;       begin try Result := GetSpeed;       except Result := '—'; end; end;
function SK_Limit: string;       begin try Result := GetLimitSpeed;  except Result := '—'; end; end;
function SK_Distance: string;    begin try Result := GetDistance;    except Result := '—'; end; end;
function SK_Track: string;       begin try Result := GetTrackNumber; except Result := '—'; end; end;
function SK_TM: string;          begin try Result := GetPressureTM;  except Result := '—'; end; end;
function SK_UR: string;          begin try Result := GetPressureUR;  except Result := '—'; end; end;
function SK_TC: string;          begin try Result := GetPressureTC;  except Result := '—'; end; end;
function SK_Accel: string;       begin try Result := GetAcceleration; except Result := '—'; end; end;
function SK_Coord: string;       begin try Result := GetCoordinatesFormatted; except Result := '—'; end; end;
function SK_Signal: string;      begin try Result := GetSvetoforValue; except Result := '—'; end; end;
function SK_Target: string;      begin try Result := IntToStr(GetTargetSpeedValue); except Result := '—'; end; end;
function SK_ALS: string;         begin try Result := IntToStr(GetALS); except Result := '—'; end; end;

procedure DrawDevRow(X, Y: Integer; const Caption, Value: string; Alpha: Integer; WinScale: Single);
begin
  DrawModernText(X, Y, Caption, COLOR_ON_SURFACE, Alpha, 0.65 * WinScale);
  DrawModernText(X + Round(90 * WinScale), Y, Value, COLOR_ACCENT, Alpha, 0.65 * WinScale);
end;

// === Высота Developer-секции ===
// Секция содержит:
//   - 12 строк живых данных KlubData (Speed/Limit/...)
//   - заголовок "Custom Texts" + кнопка [+ Add]
//   - список кастомных текстов (по строке на каждый, с кнопкой удаления)
//   - редактор выбранного текста (источник, Visible, 7 слайдеров)
const
  DEV_LIVE_ROWS       = 12;
  DEV_LIVE_ROW_H      = 18;
  DEV_TOPPAD          = 12;
  DEV_BOTTOMPAD       = 12;
  DEV_HDR_H              = 28;  // "Custom Texts" + Add
  DEV_LIST_ROW_H         = 22;
  DEV_EDITOR_HEADER_H    = 30;  // строка выбора источника <Source>
  DEV_EDITOR_GIZMO_MODE_H = 30; // кнопки режима T/R/S
  DEV_EDITOR_VIS_H       = 26;  // чекбокс Visible
  DEV_EDITOR_SLIDER_H    = 36;  // высота одного слайдера
  DEV_EDITOR_SLIDERS     = 7;
  DEV_EDITOR_DEL_H       = 28;  // кнопка Delete
  DEV_GAP                = 8;
  DEV_GIZMO_BTN_W        = 28;  // ширина кнопки режима

// "Графич. эффекты" section: 9 toggles laid out vertically inside the
// expanded panel. Layout constants are local — kept tight (6 px between
// rows) so the whole panel fits without scroll.
//
// POSTFX_INNER_X = MARGIN means the inner toggles are flush-left under the
// header (the user explicitly asked to remove the +24 px indent). The
// background panel uses POSTFX_BG_X which has just a small visual padding
// so the section is clearly grouped without pushing the controls right.
const
  POSTFX_PAD_TOP    = 10;
  POSTFX_PAD_BOTTOM = 10;
  POSTFX_ROW_GAP    = 6;
  POSTFX_TOGGLE_COUNT = 9;
  POSTFX_INNER_X    = MARGIN;       // inner toggles aligned with the header
  POSTFX_BG_X       = MARGIN - 4;   // background panel — slight outset
  POSTFX_BG_W       = 240 + 24;     // wide enough to wrap the toggles

function GetPostFXContentHeight: Integer;
begin
  // 9 toggles, each ITEM_HEIGHT tall, separated by POSTFX_ROW_GAP, plus
  // top and bottom padding. Final value is at unscaled coords; the caller
  // multiplies by Win.Scale.
  Result := POSTFX_PAD_TOP + POSTFX_PAD_BOTTOM +
            POSTFX_TOGGLE_COUNT * ITEM_HEIGHT +
            (POSTFX_TOGGLE_COUNT - 1) * POSTFX_ROW_GAP;
end;

// Render the 9 effect toggles inside the expanded section.
// X, Y point to the top-left of the inner content area (already accounting
// for section padding); SectionHeight is the currently animated height in
// scaled pixels (so we can fade items in/out as the section expands).
//
// Hover detection: для каждого ряда сравниваем позицию мыши
// (MoveXcoord/MoveYcoord — глобальные курсорные координаты в client-space
// окна игры) с прямоугольником toggle. Совпало — передаём HoverProgress=1
// в DrawModernToggle, что плавно подсветит фон + текст.
procedure DrawPostFXToggles(X, Y: Integer; Alpha: Integer; WinScale: Single; SectionHeight: Integer);
var
  RowH, ToggleW: Integer;
  CurY, MaxY: Integer;
  MX, MY: Integer;

  procedure DrawOne(const TextKey: string; Enabled: Boolean);
  var
    Hover: Single;
  begin
    if CurY + RowH > MaxY then
    begin
      Inc(CurY, RowH);
      Exit;
    end;
    // Простой бинарный hover: 1.0 если курсор внутри toggle-rect, иначе 0.
    // Плавная анимация была бы приятнее, но для этого нужен per-toggle
    // state, а тут toggle'ы безымянные — каждый кадр перерисовываются
    // одной функцией. Бинарного достаточно: DrawModernToggle уже плавно
    // лерпит цвет внутри фрейма.
    if (MX >= X) and (MX < X + ToggleW) and
       (MY >= CurY) and (MY < CurY + Round(ITEM_HEIGHT * WinScale)) then
      Hover := 1.0
    else
      Hover := 0.0;
    DrawModernToggle(X, CurY, GetText(TextKey), Enabled, Alpha,
                     False, 0, False, Hover);
    Inc(CurY, RowH);
  end;
begin
  RowH := Round((ITEM_HEIGHT + POSTFX_ROW_GAP) * WinScale);
  ToggleW := Round(220 * WinScale);
  CurY := Y;
  MaxY := Y + SectionHeight - Round(POSTFX_PAD_BOTTOM * WinScale);

  // Текущая позиция мыши в координатах окна — те же координаты, в которых
  // передан X (т.е. ScaledX-relative).
  MX := Round(MoveXcoord);
  MY := Round(MoveYcoord);

  // Order is intentional: "primitive" toggles (FXAA, Bloom) that exist in
  // legacy fallback too go first, then the new PostFX effects in order of
  // dependency (tonemap is always-on, vignette/grain are pure colour,
  // SSAO/Fog/DOF need depth).
  DrawOne('FXAAText',     Settings.FXAAEnable);
  DrawOne('BloomText',    Settings.BloomEnable);
  DrawOne('TonemapText',  Settings.TonemapEnable);
  DrawOne('SharpenText',  Settings.SharpenEnable);
  DrawOne('VignetteText', Settings.VignetteEnable);
  DrawOne('GrainText',    Settings.GrainEnable);
  DrawOne('SSAOText',     Settings.SSAOEnable);
  DrawOne('FogText',      Settings.FogEnable);
  DrawOne('DOFText',      Settings.DOFEnable);
end;

// Master toggle for "Графич. эффекты". Toggling it OFF saves the current
// per-effect state into PostFXSaved and disables every effect (so the
// section visually reflects "everything off"). Toggling it ON restores
// from PostFXSaved if available, otherwise applies the Variables.pas
// defaults — that way a freshly started session with the master at OFF
// will come back to a sensible "everything visible" preset on first
// re-enable.
//
// Both Settings.* (UI mirror) and Init*Enable (engine globals that PostFX
// reads each frame) are updated together, so the change is live.
procedure SetPostFXMasterEnabled(NewState: Boolean);
begin
  if NewState = Settings.PostFXEnable then Exit;

  if not NewState then
  begin
    // Master OFF — snapshot per-effect states, then disable everything.
    Settings.PostFXSaved.Init     := True;
    Settings.PostFXSaved.FXAA     := Settings.FXAAEnable;
    Settings.PostFXSaved.Bloom    := Settings.BloomEnable;
    Settings.PostFXSaved.Tonemap  := Settings.TonemapEnable;
    Settings.PostFXSaved.Sharpen  := Settings.SharpenEnable;
    Settings.PostFXSaved.Vignette := Settings.VignetteEnable;
    Settings.PostFXSaved.Grain    := Settings.GrainEnable;
    Settings.PostFXSaved.SSAO     := Settings.SSAOEnable;
    Settings.PostFXSaved.Fog      := Settings.FogEnable;
    Settings.PostFXSaved.DOF      := Settings.DOFEnable;

    Settings.FXAAEnable     := False;  InitFXAAEnable     := False;
    Settings.BloomEnable    := False;  InitBloomEnable    := False;
    Settings.TonemapEnable  := False;  InitTonemapEnable  := False;
    Settings.SharpenEnable  := False;  InitSharpenEnable  := False;
    Settings.VignetteEnable := False;  InitVignetteEnable := False;
    Settings.GrainEnable    := False;  InitGrainEnable    := False;
    Settings.SSAOEnable     := False;  InitSSAOEnable     := False;
    Settings.FogEnable      := False;  InitFogEnable      := False;
    Settings.DOFEnable      := False;  InitDOFEnable      := False;
  end
  else
  begin
    // Master ON — restore from shadow, or use Variables.pas defaults.
    if Settings.PostFXSaved.Init then
    begin
      Settings.FXAAEnable     := Settings.PostFXSaved.FXAA;
      Settings.BloomEnable    := Settings.PostFXSaved.Bloom;
      Settings.TonemapEnable  := Settings.PostFXSaved.Tonemap;
      Settings.SharpenEnable  := Settings.PostFXSaved.Sharpen;
      Settings.VignetteEnable := Settings.PostFXSaved.Vignette;
      Settings.GrainEnable    := Settings.PostFXSaved.Grain;
      Settings.SSAOEnable     := Settings.PostFXSaved.SSAO;
      Settings.FogEnable      := Settings.PostFXSaved.Fog;
      Settings.DOFEnable      := Settings.PostFXSaved.DOF;
    end
    else
    begin
      // No previous shadow — turn on the safe set (= Variables.pas defaults).
      // Fog and DOF stay OFF so the user isn't surprised by a heavy DOF blur
      // they never enabled.
      Settings.FXAAEnable     := True;
      Settings.BloomEnable    := True;
      Settings.TonemapEnable  := True;
      Settings.SharpenEnable  := True;
      Settings.VignetteEnable := True;
      Settings.GrainEnable    := True;
      Settings.SSAOEnable     := True;
      Settings.FogEnable      := False;
      Settings.DOFEnable      := False;
    end;
    InitFXAAEnable     := Settings.FXAAEnable;
    InitBloomEnable    := Settings.BloomEnable;
    InitTonemapEnable  := Settings.TonemapEnable;
    InitSharpenEnable  := Settings.SharpenEnable;
    InitVignetteEnable := Settings.VignetteEnable;
    InitGrainEnable    := Settings.GrainEnable;
    InitSSAOEnable     := Settings.SSAOEnable;
    InitFogEnable      := Settings.FogEnable;
    InitDOFEnable      := Settings.DOFEnable;
  end;

  Settings.PostFXEnable := NewState;
  InitPostFXEnable      := NewState;
end;

function GetDeveloperContentHeight: Integer;
var
  H: Integer;
begin
  H := DEV_TOPPAD + DEV_LIVE_ROWS * DEV_LIVE_ROW_H + DEV_GAP;
  H := H + (26 + 8); // кнопка Open Full Dev Editor
  H := H + DEV_HDR_H;
  H := H + Length(CustomTexts) * DEV_LIST_ROW_H;
  if (CustomTextSelectedIdx >= 0) and (CustomTextSelectedIdx < Length(CustomTexts)) then
    H := H + DEV_GAP + DEV_EDITOR_HEADER_H + DEV_EDITOR_GIZMO_MODE_H + DEV_EDITOR_VIS_H +
         DEV_EDITOR_SLIDERS * DEV_EDITOR_SLIDER_H + DEV_EDITOR_DEL_H;
  H := H + DEV_BOTTOMPAD;
  Result := H;
end;

procedure DrawDeveloperValues(X, Y: Integer; Alpha: Integer; WinScale: Single; SectionHeight: Integer);
var
  RowH, CurY, MaxY: Integer;
  i, RowY: Integer;
  SrcName, ValStr: string;
  IsSel: Boolean;
  AddBtnX, AddBtnY: Integer;
begin
  RowH := Round(DEV_LIVE_ROW_H * WinScale);
  CurY := Y;
  MaxY := Y + SectionHeight - Round(8 * WinScale);

  // --- Живые данные KlubData ---
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Speed:',    SK_Speed + ' km/h', Alpha, WinScale);    Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Limit:',    SK_Limit + ' km/h', Alpha, WinScale);    Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Target:',   SK_Target + ' km/h', Alpha, WinScale);   Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Distance:', SK_Distance + ' m', Alpha, WinScale);    Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Track:',    SK_Track, Alpha, WinScale);              Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'TM:',       SK_TM, Alpha, WinScale);                 Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'UR:',       SK_UR, Alpha, WinScale);                 Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'TC:',       SK_TC, Alpha, WinScale);                 Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'ALS:',      SK_ALS, Alpha, WinScale);                Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Accel:',    SK_Accel, Alpha, WinScale);              Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Coords:',   SK_Coord, Alpha, WinScale);              Inc(CurY, RowH); end;
  if CurY <= MaxY then begin DrawDevRow(X, CurY, 'Signal:',   SK_Signal, Alpha, WinScale);             Inc(CurY, RowH); end;

  Inc(CurY, Round(DEV_GAP * WinScale));

  // --- Кнопка Open Dev Editor (фуллскрин) ---
  if CurY > MaxY then Exit;
  DrawStyledRect(X, CurY, Round(190 * WinScale), Round(26 * WinScale),
    COLOR_ACCENT, Alpha, True, COLOR_PRIMARY_VARIANT);
  DrawModernText(X + Round(20 * WinScale), CurY + Round(6 * WinScale),
    'Open Full Dev Editor →', COLOR_ON_PRIMARY, Alpha, 0.7);
  Inc(CurY, Round((26 + 8) * WinScale));

  // --- Заголовок Custom Texts + кнопка [+ Add] ---
  if CurY > MaxY then Exit;
  DrawModernText(X, CurY + Round(6 * WinScale), 'Custom Texts (' + IntToStr(Length(CustomTexts)) + ')',
    COLOR_ON_SURFACE, Alpha, 0.7);
  AddBtnX := X + Round(160 * WinScale);
  AddBtnY := CurY;
  DrawStyledRect(AddBtnX, AddBtnY, Round(36 * WinScale), Round(22 * WinScale),
    COLOR_PRIMARY, Alpha, True, COLOR_PRIMARY_VARIANT);
  DrawModernText(AddBtnX + Round(11 * WinScale), AddBtnY + Round(4 * WinScale),
    '+ Add', COLOR_ON_PRIMARY, Alpha, 0.65);
  Inc(CurY, Round(DEV_HDR_H * WinScale));

  // --- Список текстов ---
  for i := 0 to Length(CustomTexts) - 1 do
  begin
    if CurY > MaxY then Break;
    RowY := CurY;
    IsSel := (i = CustomTextSelectedIdx);
    if IsSel then
      DrawStyledRect(X - Round(4 * WinScale), RowY, Round(196 * WinScale),
        Round(20 * WinScale), COLOR_PRIMARY_VARIANT, Alpha div 2, True, 0);
    if CustomTexts[i].Visible then
      DrawModernText(X, RowY + Round(2 * WinScale), '*', COLOR_ACCENT, Alpha, 0.7)
    else
      DrawModernText(X, RowY + Round(2 * WinScale), '-', COLOR_BORDER_LIGHT, Alpha, 0.7);
    DrawModernText(X + Round(14 * WinScale), RowY + Round(2 * WinScale),
      IntToStr(i + 1) + '. ' + KlubSourceName(CustomTexts[i].Source),
      COLOR_ON_SURFACE, Alpha, 0.65);
    Inc(CurY, Round(DEV_LIST_ROW_H * WinScale));
  end;

  // --- Редактор выбранного текста ---
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then Exit;
  if CurY > MaxY then Exit;
  Inc(CurY, Round(DEV_GAP * WinScale));

  // Строка выбора источника:  [<]   Source: NAME   [>]
  SrcName := KlubSourceName(CustomTexts[CustomTextSelectedIdx].Source);
  ValStr  := GetKlubSourceValue(CustomTexts[CustomTextSelectedIdx].Source);
  // Кнопка <
  DrawStyledRect(X, CurY, Round(22 * WinScale), Round(22 * WinScale),
    COLOR_SURFACE_VARIANT, Alpha, True, COLOR_BORDER_LIGHT);
  DrawModernText(X + Round(8 * WinScale), CurY + Round(4 * WinScale), '<', COLOR_ON_SURFACE, Alpha, 0.7);
  // Имя + текущее значение
  DrawModernText(X + Round(28 * WinScale), CurY + Round(4 * WinScale),
    SrcName + ' = ' + ValStr, COLOR_ACCENT, Alpha, 0.65);
  // Кнопка >
  DrawStyledRect(X + Round(168 * WinScale), CurY, Round(22 * WinScale), Round(22 * WinScale),
    COLOR_SURFACE_VARIANT, Alpha, True, COLOR_BORDER_LIGHT);
  DrawModernText(X + Round(176 * WinScale), CurY + Round(4 * WinScale), '>', COLOR_ON_SURFACE, Alpha, 0.7);
  Inc(CurY, Round(DEV_EDITOR_HEADER_H * WinScale));

  // Кнопки режима гизмо: [T] [R] [S]
  // Активный — primary, неактивные — surface variant.
  if GizmoMode = gmTranslate then
    DrawStyledRect(X, CurY, Round(DEV_GIZMO_BTN_W * WinScale), Round(22 * WinScale), COLOR_PRIMARY, Alpha, True, COLOR_PRIMARY)
  else
    DrawStyledRect(X, CurY, Round(DEV_GIZMO_BTN_W * WinScale), Round(22 * WinScale), COLOR_SURFACE_VARIANT, Alpha, True, COLOR_BORDER_LIGHT);
  DrawModernText(X + Round(10 * WinScale), CurY + Round(4 * WinScale), 'T', COLOR_ON_PRIMARY, Alpha, 0.7);

  if GizmoMode = gmRotate then
    DrawStyledRect(X + Round((DEV_GIZMO_BTN_W + 4) * WinScale), CurY, Round(DEV_GIZMO_BTN_W * WinScale), Round(22 * WinScale), COLOR_PRIMARY, Alpha, True, COLOR_PRIMARY)
  else
    DrawStyledRect(X + Round((DEV_GIZMO_BTN_W + 4) * WinScale), CurY, Round(DEV_GIZMO_BTN_W * WinScale), Round(22 * WinScale), COLOR_SURFACE_VARIANT, Alpha, True, COLOR_BORDER_LIGHT);
  DrawModernText(X + Round((DEV_GIZMO_BTN_W + 14) * WinScale), CurY + Round(4 * WinScale), 'R', COLOR_ON_PRIMARY, Alpha, 0.7);

  if GizmoMode = gmScale then
    DrawStyledRect(X + Round((2 * (DEV_GIZMO_BTN_W + 4)) * WinScale), CurY, Round(DEV_GIZMO_BTN_W * WinScale), Round(22 * WinScale), COLOR_PRIMARY, Alpha, True, COLOR_PRIMARY)
  else
    DrawStyledRect(X + Round((2 * (DEV_GIZMO_BTN_W + 4)) * WinScale), CurY, Round(DEV_GIZMO_BTN_W * WinScale), Round(22 * WinScale), COLOR_SURFACE_VARIANT, Alpha, True, COLOR_BORDER_LIGHT);
  DrawModernText(X + Round((2 * (DEV_GIZMO_BTN_W + 4) + 10) * WinScale), CurY + Round(4 * WinScale), 'S', COLOR_ON_PRIMARY, Alpha, 0.7);

  // Подпись справа от кнопок: текущий режим словом
  case GizmoMode of
    gmTranslate: DrawModernText(X + Round(((3 * (DEV_GIZMO_BTN_W + 4)) + 6) * WinScale), CurY + Round(4 * WinScale), 'Move',     COLOR_ACCENT, Alpha, 0.6);
    gmRotate:    DrawModernText(X + Round(((3 * (DEV_GIZMO_BTN_W + 4)) + 6) * WinScale), CurY + Round(4 * WinScale), 'Rotate',   COLOR_ACCENT, Alpha, 0.6);
    gmScale:     DrawModernText(X + Round(((3 * (DEV_GIZMO_BTN_W + 4)) + 6) * WinScale), CurY + Round(4 * WinScale), 'Scale',    COLOR_ACCENT, Alpha, 0.6);
  end;
  Inc(CurY, Round(DEV_EDITOR_GIZMO_MODE_H * WinScale));

  // Чекбокс Visible
  DrawModernCheckbox(X, CurY, 'Visible', CustomTexts[CustomTextSelectedIdx].Visible, Alpha);
  Inc(CurY, Round(DEV_EDITOR_VIS_H * WinScale));

  // 7 слайдеров. У каждого слайдера занимаем DEV_EDITOR_SLIDER_H высоты.
  // DrawModernSlider пишет заголовок выше Y (Y - 22), так что добавляю отступ.
  DrawModernSlider(X, CurY + Round(20 * WinScale), CustomXSlider,    'X',  Alpha, WinScale);
  Inc(CurY, Round(DEV_EDITOR_SLIDER_H * WinScale));
  DrawModernSlider(X, CurY + Round(20 * WinScale), CustomYSlider,    'Y',  Alpha, WinScale);
  Inc(CurY, Round(DEV_EDITOR_SLIDER_H * WinScale));
  DrawModernSlider(X, CurY + Round(20 * WinScale), CustomZSlider,    'Z',  Alpha, WinScale);
  Inc(CurY, Round(DEV_EDITOR_SLIDER_H * WinScale));
  DrawModernSlider(X, CurY + Round(20 * WinScale), CustomRXSlider,   'RX', Alpha, WinScale);
  Inc(CurY, Round(DEV_EDITOR_SLIDER_H * WinScale));
  DrawModernSlider(X, CurY + Round(20 * WinScale), CustomRYSlider,   'RY', Alpha, WinScale);
  Inc(CurY, Round(DEV_EDITOR_SLIDER_H * WinScale));
  DrawModernSlider(X, CurY + Round(20 * WinScale), CustomRZSlider,   'RZ', Alpha, WinScale);
  Inc(CurY, Round(DEV_EDITOR_SLIDER_H * WinScale));
  DrawModernSlider(X, CurY + Round(20 * WinScale), CustomScaleSlider, 'Scale', Alpha, WinScale);
  Inc(CurY, Round(DEV_EDITOR_SLIDER_H * WinScale));

  // Кнопка Delete
  DrawStyledRect(X, CurY, Round(80 * WinScale), Round(22 * WinScale),
    COLOR_WARNING, Alpha, True, COLOR_WARNING);
  DrawModernText(X + Round(20 * WinScale), CurY + Round(4 * WinScale),
    'Delete', COLOR_ON_PRIMARY, Alpha, 0.65);
end;

procedure DrawTransformWindow(var Win: TWindow; WindowType: Integer);
var
  Alpha: Integer;
  ContentY: Integer;
  SectionHeight: Integer;
  ExpandButtonX: Integer;
  TotalHeight: Integer;
  ScaledWidth, ScaledHeight: Integer;
  ScaledX, ScaledY: Integer;
  OffsetX, OffsetY: Integer;
  HeaderGradientStart, HeaderGradientEnd: Integer;
  // Transform координаты
  TransformX1, TransformY1, TransformX2, TransformY2: Integer;
  TransformX3, TransformY3, TransformX4, TransformY4: Integer;
  CenterX, CenterY: Integer;
begin
  Alpha := Round(Win.Alpha * 255);
  if Alpha <= 0 then Exit;
  
  // === ПРИМЕНЯЕМ МАСШТАБИРОВАНИЕ + АНИМАЦИЮ РАСШИРЕНИЯ ПРИ ПЕРЕТАСКИВАНИИ ===
  ScaledWidth := Round(Win.Width * Win.Scale) + Round(Win.DragWidthExpansion);
  ScaledHeight := Round(Win.Height * Win.Scale);

  // Центрирование масштабированного окна + расширение drag-эффекта по центру
  OffsetX := (Win.Width - Round(Win.Width * Win.Scale)) div 2 - Round(Win.DragWidthExpansion / 2);
  OffsetY := (Win.Height - ScaledHeight) div 2;

  ScaledX := Win.X + OffsetX;
  // TranslateY добавляет вертикальное смещение для анимации "влёта" сверху
  ScaledY := Win.Y + OffsetY + Round(Win.TranslateY);
  
  // Вычисляем динамическую высоту окна
  TotalHeight := HEADER_HEIGHT + MARGIN * 3;
  
  case WindowType of
    0: // RENDER окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT;
      if Settings.FreecamSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(150 * Settings.FreecamSection.AnimProgress) + MARGIN;
      
      TotalHeight := TotalHeight + ITEM_HEIGHT;
      if Settings.MainCameraSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(200 * Settings.MainCameraSection.AnimProgress) + MARGIN;

      // "Графич. эффекты" master toggle + optional expanded section with
      // all 9 post-process toggles. Перенесено сюда из WORLD-окна, чтобы
      // визуально жить рядом с другими render-настройками.
      TotalHeight := TotalHeight + MARGIN + ITEM_HEIGHT;
      if Settings.PostFXSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(GetPostFXContentHeight * Settings.PostFXSection.AnimProgress) + MARGIN;
    end;
    
    1: // WORLD окно
    begin
      // MaxDistance toggle (+ optional expanded slider section).
      TotalHeight := TotalHeight + ITEM_HEIGHT;
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(180 * Settings.MaxVisibleDistanceSection.AnimProgress) + MARGIN;

      // NewSky + SystemTime toggles.
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN;
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN;
    end;
    
    2: // LOCOMOTIVE окно
    begin
      // ClubFixes toggle + Developer toggle
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN + ITEM_HEIGHT + MARGIN;
      // Expanded section — высота зависит от количества кастомных текстов и
      // выбранного редактора (см. GetDeveloperContentHeight).
      if Settings.DeveloperSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(GetDeveloperContentHeight * Settings.DeveloperSection.AnimProgress) + MARGIN;
    end;
    
    3: // MENU окно
    begin
      TotalHeight := 220;
    end;
  end;
  
  Win.Height := TotalHeight;
  ScaledHeight := Round(Win.Height * Win.Scale);
  OffsetY := (Win.Height - ScaledHeight) div 2;
  ScaledY := Win.Y + OffsetY + Round(Win.TranslateY);
  
  // Центр для трансформации
  CenterX := ScaledX + ScaledWidth div 2;
  CenterY := ScaledY + ScaledHeight div 2;
  
  // Вычисляем углы прямоугольника с учетом трансформации
  TransformX1 := ScaledX;
  TransformY1 := ScaledY;
  TransformX2 := ScaledX + ScaledWidth;
  TransformY2 := ScaledY;
  TransformX3 := ScaledX + ScaledWidth;
  TransformY3 := ScaledY + ScaledHeight;
  TransformX4 := ScaledX;
  TransformY4 := ScaledY + ScaledHeight;
  
  // Применяем трансформацию к углам
  TransformCoords(TransformX1, TransformY1, CenterX, CenterY, 1.0, Win.Rotation);
  TransformCoords(TransformX2, TransformY2, CenterX, CenterY, 1.0, Win.Rotation);
  TransformCoords(TransformX3, TransformY3, CenterX, CenterY, 1.0, Win.Rotation);
  TransformCoords(TransformX4, TransformY4, CenterX, CenterY, 1.0, Win.Rotation);
  
  // === СОВРЕМЕННАЯ ТЕНЬ ===
  DrawShadow(ScaledX, ScaledY, ScaledWidth, ScaledHeight, Round(Win.ShadowIntensity * Alpha), Win.ShadowIntensity);
  
//  // === ГРАДИЕНТНЫЙ ЗАГОЛОВОК ===
  HeaderGradientStart := LerpColor(COLOR_PRIMARY, COLOR_ACCENT, 0.3);
  HeaderGradientEnd := LerpColor(COLOR_PRIMARY_VARIANT, COLOR_PRIMARY, 0.2);
  
  // Основной фон заголовка
  DrawStyledRect(ScaledX, ScaledY, ScaledWidth, Round(HEADER_HEIGHT * Win.Scale), HeaderGradientStart, Alpha, True, COLOR_BORDER_LIGHT);
  
  // Имитация градиента через несколько слоев
  //DrawStyledRect(ScaledX, ScaledY + Round(HEADER_HEIGHT * Win.Scale) - 8, ScaledWidth, 8, HeaderGradientEnd, Alpha div 2, True, 0);

  // Текст заголовка
  DrawModernText(ScaledX + Round(16 * Win.Scale), ScaledY + Round(12 * Win.Scale), Win.Title, COLOR_ON_PRIMARY, Alpha, 0.9);
  
  // === ТЕЛО ОКНА ===
  DrawStyledRect(ScaledX, ScaledY + Round(HEADER_HEIGHT * Win.Scale), ScaledWidth, ScaledHeight - Round(HEADER_HEIGHT * Win.Scale), COLOR_BACKGROUND, Alpha, True, COLOR_BORDER);
  
  ContentY := ScaledY + Round(HEADER_HEIGHT * Win.Scale) + Round(MARGIN * Win.Scale);
  
  case WindowType of
    0: // RENDER окно
    begin
      // Freecam
      ExpandButtonX := ScaledX + Round(220 * Win.Scale);
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('FreeCameraText'), Settings.Freecam, Alpha, True, ExpandButtonX, Settings.FreecamSection.Expanded, Settings.FreecamSection.AnimProgress);
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));
      
      // Freecam секция. Контент сдвинут на CONTENT_TEXT_INSET (= 16)
      // вправо от MARGIN, чтобы ТЕКСТ слайдеров стоял точно под текстом
      // header'а (у DrawModernToggle тоже +16 px паддинг до текста).
      if Settings.FreecamSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(150 * Settings.FreecamSection.AnimProgress * Win.Scale);

        DrawStyledRect(ScaledX + Round(POSTFX_BG_X * Win.Scale), ContentY,
          Round(POSTFX_BG_W * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);

        if SectionHeight > Round(40 * Win.Scale) then
        begin
          Settings.BasespeedSlider.HoverProgress :=
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round((25 - 22) * Win.Scale),
                     Round((SLIDER_WIDTH + 30) * Win.Scale), Round(40 * Win.Scale));
          DrawModernSlider(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(25 * Win.Scale), Settings.BasespeedSlider, GetText('BaseSpeedText'), Alpha, Win.Scale);

          Settings.FastspeedSlider.HoverProgress :=
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round((75 - 22) * Win.Scale),
                     Round((SLIDER_WIDTH + 30) * Win.Scale), Round(40 * Win.Scale));
          DrawModernSlider(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(75 * Win.Scale), Settings.FastspeedSlider, GetText('FastSpeedText'), Alpha, Win.Scale);
        end;

        Inc(ContentY, SectionHeight + Round(MARGIN * Win.Scale));
      end;
      
      // Main Camera
      ExpandButtonX := ScaledX + Round(220 * Win.Scale);
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('MainCameraText'), Settings.MainCamera, Alpha, True, ExpandButtonX, Settings.MainCameraSection.Expanded, Settings.MainCameraSection.AnimProgress);
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));
      
      // Main Camera секция — контент сдвинут на +16 чтобы выровнять текст
      // элементов с текстом header'а (у toggle padding 16 px до текста).
      if Settings.MainCameraSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(200 * Settings.MainCameraSection.AnimProgress * Win.Scale);

        DrawStyledRect(ScaledX + Round(POSTFX_BG_X * Win.Scale), ContentY,
          Round(POSTFX_BG_W * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);

        if SectionHeight > Round(40 * Win.Scale) then
        begin
          Settings.StepForwardSlider.HoverProgress :=
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round((25 - 22) * Win.Scale),
                     Round((SLIDER_WIDTH + 30) * Win.Scale), Round(40 * Win.Scale));
          DrawModernSlider(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(25 * Win.Scale), Settings.StepForwardSlider, GetText('StepForwardText'), Alpha, Win.Scale);
        end;

        if SectionHeight > Round(70 * Win.Scale) then
          DrawModernCheckbox(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(60 * Win.Scale), GetText('NewZoomText'), Settings.NewViewAngle, Alpha,
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(60 * Win.Scale), Round(200 * Win.Scale), Round(20 * Win.Scale)));

        if (SectionHeight > Round(110 * Win.Scale)) and Settings.NewViewAngle then
        begin
          Settings.ViewAngleSlider.HoverProgress :=
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round((95 - 22) * Win.Scale),
                     Round((SLIDER_WIDTH + 30) * Win.Scale), Round(40 * Win.Scale));
          DrawModernSlider(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(95 * Win.Scale), Settings.ViewAngleSlider, GetText('ValueText'), Alpha, Win.Scale);
        end;

        if SectionHeight > Round(140 * Win.Scale) then
          DrawModernCheckbox(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(130 * Win.Scale), GetText('SensitivityText'), Settings.CameraSensitivity, Alpha,
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(130 * Win.Scale), Round(200 * Win.Scale), Round(20 * Win.Scale)));

        if (SectionHeight > Round(170 * Win.Scale)) and Settings.CameraSensitivity then
        begin
          Settings.CameraSensitivitySlider.HoverProgress :=
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round((165 - 22) * Win.Scale),
                     Round((SLIDER_WIDTH + 30) * Win.Scale), Round(40 * Win.Scale));
          DrawModernSlider(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(165 * Win.Scale), Settings.CameraSensitivitySlider, GetText('ValueText'), Alpha, Win.Scale);
        end;

        Inc(ContentY, SectionHeight + Round(MARGIN * Win.Scale));
      end;

      // === "Графич. эффекты" — master toggle + раскрывающаяся секция ===
      // Перенесено сюда (RENDER) из WORLD-окна. Левая часть toggle-а
      // переключает все 9 эффектов разом (с запоминанием в PostFXSaved
      // для последующего восстановления); правый шеврон — независимое
      // expand/collapse секции.
      ExpandButtonX := ScaledX + Round(220 * Win.Scale);
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY,
        GetText('PostFXText'), Settings.PostFXEnable, Alpha,
        True, ExpandButtonX,
        Settings.PostFXSection.Expanded, Settings.PostFXSection.AnimProgress);
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));

      // Развёрнутая секция с 9 toggle (FXAA, Bloom, Tonemap, Sharpen,
      // Vignette, Grain, SSAO, Fog, DOF). Контент выровнен под header
      // (POSTFX_INNER_X = MARGIN), фон секции лишь чуть-чуть выходит за
      // края (POSTFX_BG_X = MARGIN-4) — без огромного отступа вправо.
      if Settings.PostFXSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(GetPostFXContentHeight * Settings.PostFXSection.AnimProgress * Win.Scale);

        DrawStyledRect(ScaledX + Round(POSTFX_BG_X * Win.Scale), ContentY,
          Round(POSTFX_BG_W * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);

        if SectionHeight > Round(POSTFX_PAD_TOP * Win.Scale) then
          DrawPostFXToggles(
            ScaledX + Round(POSTFX_INNER_X * Win.Scale),
            ContentY + Round(POSTFX_PAD_TOP * Win.Scale),
            Alpha, Win.Scale, SectionHeight);

        Inc(ContentY, SectionHeight + Round(MARGIN * Win.Scale));
      end;
    end;
    
    1: // WORLD окно
    begin
      // Max Visible Distance
      ExpandButtonX := ScaledX + Round(220 * Win.Scale);
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('MaxDistanceText'), Settings.MaxVisibleDistance, Alpha, True, ExpandButtonX, Settings.MaxVisibleDistanceSection.Expanded, Settings.MaxVisibleDistanceSection.AnimProgress);
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));
      
      // Max Visible Distance секция — контент сдвинут на +16 для
      // вертикального выравнивания текста с header.
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(180 * Settings.MaxVisibleDistanceSection.AnimProgress * Win.Scale);

        DrawStyledRect(ScaledX + Round(POSTFX_BG_X * Win.Scale), ContentY,
          Round(POSTFX_BG_W * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);

        if SectionHeight > Round(40 * Win.Scale) then
        begin
          Settings.MaxVisibleDistanceSlider.HoverProgress :=
            HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round((25 - 22) * Win.Scale),
                     Round((SLIDER_WIDTH + 30) * Win.Scale), Round(40 * Win.Scale));
          DrawModernSlider(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(25 * Win.Scale), Settings.MaxVisibleDistanceSlider, GetText('DistanceText'), Alpha, Win.Scale);

          if SectionHeight > Round(70 * Win.Scale) then
          begin
            DrawModernCheckbox(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(60 * Win.Scale), GetText('WiresText'), Settings.ShowWires, Alpha,
              HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(60 * Win.Scale), Round(200 * Win.Scale), Round(20 * Win.Scale)));
            DrawModernCheckbox(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(90 * Win.Scale), GetText('DistantModelsText'), Settings.ShowDistantModels, Alpha,
              HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(90 * Win.Scale), Round(200 * Win.Scale), Round(20 * Win.Scale)));
          end;
          if SectionHeight > Round(120 * Win.Scale) then
            DrawModernCheckbox(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(120 * Win.Scale), GetText('TrafficLightsText'), Settings.ShowTrafficLights, Alpha,
              HoverFor(ScaledX + Round((MARGIN + 16) * Win.Scale), ContentY + Round(120 * Win.Scale), Round(200 * Win.Scale), Round(20 * Win.Scale)));
        end;

        Inc(ContentY, SectionHeight + Round(MARGIN * Win.Scale));
      end;
      
      // New Sky
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('NewSkyText'), Settings.NewSky, Alpha,
        False, 0, False,
        HoverFor(ScaledX + Round(MARGIN * Win.Scale), ContentY, Round(240 * Win.Scale), Round(ITEM_HEIGHT * Win.Scale)));
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));

      // System Time — подмена AbsoluteTime симулятора на Windows time.
      // SystemTime.pas читает InitSystemTimeEnable каждый кадр, так что
      // переключение сразу применяется без рестарта.
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('SystemTimeText'), Settings.SystemTimeEnable, Alpha,
        False, 0, False,
        HoverFor(ScaledX + Round(MARGIN * Win.Scale), ContentY, Round(240 * Win.Scale), Round(ITEM_HEIGHT * Win.Scale)));
    end;
    
    2: // LOCOMOTIVE окно
    begin
      // Исправления КЛУБ
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('ClubFixesText'), Settings.NewClubPositions, Alpha,
        False, 0, False,
        HoverFor(ScaledX + Round(MARGIN * Win.Scale), ContentY, Round(240 * Win.Scale), Round(ITEM_HEIGHT * Win.Scale)));
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));

      // RA3 подсветка
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('RA3HoverText'), Settings.RA3Hover, Alpha,
        False, 0, False,
        HoverFor(ScaledX + Round(MARGIN * Win.Scale), ContentY, Round(240 * Win.Scale), Round(ITEM_HEIGHT * Win.Scale)));
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));

      // Меню разработчика (toggle + expand)
      ExpandButtonX := ScaledX + Round(220 * Win.Scale);
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('DeveloperMenuText'), Settings.DeveloperMenu, Alpha, True, ExpandButtonX, Settings.DeveloperSection.Expanded, Settings.DeveloperSection.AnimProgress);
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));

      if Settings.DeveloperSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(GetDeveloperContentHeight * Settings.DeveloperSection.AnimProgress * Win.Scale);

        // Фон секции
        DrawStyledRect(ScaledX + Round((MARGIN + 12) * Win.Scale), ContentY, Round(240 * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);

        if Settings.DeveloperMenu and (SectionHeight > Round(30 * Win.Scale)) then
          DrawDeveloperValues(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(12 * Win.Scale), Alpha, Win.Scale, SectionHeight);
      end;
    end;
    
    3: // MENU окно
    begin
      // Заголовок языка
      DrawModernText(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(15 * Win.Scale), GetText('LanguageText'), COLOR_ON_SURFACE, Alpha, 0.85);
      Inc(ContentY, Round(50 * Win.Scale));
      
      // Кнопки языков в ряд (с hover-подсветкой)
      DrawModernLanguageButton(ScaledX + Round(MARGIN * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale), GetText('RussianText'), Alpha, CurrentLanguage = langRussian,
        HoverFor(ScaledX + Round(MARGIN * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale)));
      DrawModernLanguageButton(ScaledX + Round((MARGIN + 100) * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale), GetText('UkrainianText'), Alpha, CurrentLanguage = langUkrainian,
        HoverFor(ScaledX + Round((MARGIN + 100) * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale)));
      DrawModernLanguageButton(ScaledX + Round((MARGIN + 200) * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale), GetText('EnglishText'), Alpha, CurrentLanguage = langEnglish,
        HoverFor(ScaledX + Round((MARGIN + 200) * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale)));
    end;
  end;
end;

// Современная информационная панель
procedure DrawModernInfoBar;
var
  Alpha, BarWidth, BarHeight, BarX, BarY: Integer;
  InfoText: string;
  GradientColor1, GradientColor2: Integer;
begin
  Alpha := Round(RenderWindow.Alpha * 200); // Немного прозрачнее чем окна
  if Alpha <= 0 then Exit;
  
  InfoText := GetText('InfoText');
  BarWidth := 400;
  BarHeight := 38;
  BarX := 16;
  BarY := InitResY - BarHeight - 16;
  
  GradientColor1 := COLOR_SURFACE;
  GradientColor2 := COLOR_SURFACE_VARIANT;
  
  // Тень
  DrawShadow(BarX, BarY, BarWidth, BarHeight, Alpha div 2, 0.6);
  
  // Основной фон с градиентом
  DrawStyledRect(BarX, BarY, BarWidth, BarHeight, GradientColor1, Alpha, True, COLOR_BORDER_LIGHT);
  DrawStyledRect(BarX, BarY + BarHeight - 8, BarWidth, 8, GradientColor2, Alpha div 2, True, 0);
  
  // Иконка (простая точка)
  DrawCircle2D_Fill(BarX + 16, BarY + BarHeight div 2, 5, COLOR_ACCENT, Alpha);
  
  // Текст
  DrawModernText(BarX + 32, BarY + 9, InfoText, COLOR_ON_SURFACE, Alpha, 0.8);
end;

procedure DrawCheatMenu; stdcall;
var
  BackgroundAlpha: Integer;
  OverlayColor: Integer;
  AnyWindowVisible: Boolean;
begin
  // Сбрасываем per-frame флаг рендера кастомных текстов СРАЗУ, до early exit.
  // Иначе при закрытом меню тексты отрендерятся один раз и потом флаг
  // больше не сбросится — texts исчезнут.
  // DrawCheatMenu вызывается раз в кадр в конце glDraw — отличное место.
  CustomTextsRenderedThisFrame := False;

  // Проверяем смену локо/состава и подгружаем per-loco custom_texts.cfg.
  // Внутри throttle на 2 секунды — реально файл читается только при смене кабины.
  CheckLocoChangeAndReloadTexts;

  // Продолжаем рисовать во время анимации закрытия, пока альфа окон не угаснет
  AnyWindowVisible := (RenderWindow.Alpha > 0.01) or (WorldWindow.Alpha > 0.01) or
                      (LocomotiveWindow.Alpha > 0.01) or (MenuWindow.Alpha > 0.01);
  // Dev Editor живёт независимо от MenuVisible — его собственный тумблер.
  if (not MenuVisible) and (not AnyWindowVisible) and (not DevEditorVisible) then Exit;

  if MenuVisible then
    LoadConfigThrottled;

  UpdateAnimations;

  // Per-frame обновление dev editor'а — hover, scroll, auto-repeat. Работает
  // даже если основное меню закрыто (когда DevEditorVisible).
  UpdateDevEditorPerFrame;

  // Если открыт фуллскрин dev editor — рисуем его ВМЕСТО 4 окон. Сам Begin2D
  // делает внутри, ничего общего с состоянием обычного меню.
  if DevEditorVisible then
  begin
    DrawDevEditor;
    Exit;
  end;

  Begin2D;
  try
    // Современное затемнение фона
    if Settings.BrightnessSlider.Value > 0 then
    begin
      BackgroundAlpha := Round(Settings.BrightnessSlider.Value * RenderWindow.Alpha * 0.7);
      OverlayColor := LerpColor(COLOR_BACKGROUND, COLOR_SHADOW, 0.3);
      DrawRectangle2D(0, 0, InitResX, InitResY, OverlayColor, BackgroundAlpha, True);
    end;
    
    // Отрисовка окон с transform эффектами
    DrawTransformWindow(RenderWindow, 0);      // RENDER окно
    DrawTransformWindow(WorldWindow, 1);       // WORLD окно  
    DrawTransformWindow(LocomotiveWindow, 2);  // LOCOMOTIVE окно
    DrawTransformWindow(MenuWindow, 3);        // MENU окно
    DrawModernInfoBar;
  finally
    End2D;
  end;
end;

// Per-drag throttle для SaveConfig.
// SaveConfig — это write всего .cfg на диск; в горячем пути drag'а это
// убивало FPS до однозначных цифр на быстрых движениях слайдера. Теперь
// помечаем "грязный" флаг, на диск пишем не чаще раза в N миллисекунд,
// и финальный гарантированный save делается в HandleMouseUp (см. ниже).
const
  SLIDER_SAVE_THROTTLE_MS = 250;
var
  SliderDirty: Boolean = False;
  LastSliderSaveTime: Cardinal = 0;

// Оптимизированная функция перетаскивания слайдера
procedure HandleSliderDrag(X: Integer; var Slider: TSlider; SliderX: Integer);
var
  NewProgress: Single;
  OldValue: Single;
  CurrentTime: Cardinal;
begin
  if not Slider.IsDragging then Exit;

  OldValue := Slider.Value;

  NewProgress := (X - SliderX) / SLIDER_WIDTH;
  if NewProgress < 0 then NewProgress := 0;
  if NewProgress > 1 then NewProgress := 1;

  Slider.Value := Slider.MinValue + NewProgress * (Slider.MaxValue - Slider.MinValue);

  if Abs(Slider.Value - OldValue) > 0.001 then
  begin
    // Синхронизация с глобальными переменными — это live-эффект, должен
    // обновляться каждый кадр без throttle.
    if @Slider = @Settings.BasespeedSlider then
      MenuFreecamBaseSpeed := Settings.BasespeedSlider.Value;
    if @Slider = @Settings.FastspeedSlider then
      MenuFreecamFastSpeed := Settings.FastspeedSlider.Value;
    if @Slider = @Settings.TurnspeedSlider then
      MenuFreecamTurnSpeed := Settings.TurnspeedSlider.Value;
    if @Slider = @Settings.StepForwardSlider then
      stepforward := Settings.StepForwardSlider.Value;
    if @Slider = @Settings.MaxVisibleDistanceSlider then
      maxvisibledistance := Settings.MaxVisibleDistanceSlider.Value;
    if @Slider = @Settings.ViewAngleSlider then
    begin
      if Settings.MainCamera and Settings.NewViewAngle then
        WriteFloatToMemory(ViewAngleSliderAddr, Settings.ViewAngleSlider.Value);
    end;
    if @Slider = @Settings.CameraSensitivitySlider then
    begin
      if Settings.MainCamera and Settings.CameraSensitivity then
        WriteFloatToMemory(CameraSensitivityAddr, Settings.CameraSensitivitySlider.Value);
    end;

    // SaveConfig теперь throttled — на диск пишем максимум раз в 250 мс
    // во время drag, а финальный save точно случится в HandleMouseUp.
    SliderDirty := True;
    CurrentTime := GetTickCount;
    if (CurrentTime - LastSliderSaveTime) > SLIDER_SAVE_THROTTLE_MS then
    begin
      SaveConfig;
      LastSliderSaveTime := CurrentTime;
      SliderDirty := False;
    end;

    if (@Slider <> @Settings.BrightnessSlider) then
      ApplySettingsThrottled;
  end;
end;

procedure HandleMenuHover(X, Y: Integer); stdcall;
begin
  if not MenuVisible then Exit;
  
  // Обработка драггинга окон с анимацией "подхвата":
  //   - расширение по ширине, усиление тени, "бумажный" наклон по
  //     velocity курсора (см. UpdatePaperDrag). Никакого scale-bump'а:
  //     окно не "подпрыгивает", только плавно наклоняется в сторону
  //     движения и выпрямляется при остановке.
  if RenderWindow.IsDragging then
  begin
    RenderWindow.X := X - RenderWindow.DragOffsetX;
    RenderWindow.Y := Y - RenderWindow.DragOffsetY;
    RenderWindow.OriginalX := RenderWindow.X;
    RenderWindow.OriginalY := RenderWindow.Y;
    RenderWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    RenderWindow.TargetShadowIntensity := 1.8;
    PaperDragOnMouseMove(RenderWindow, X);
    RenderWindow.TargetScale := 1.0;       // без scale-bump
  end
  else
  begin
    RenderWindow.TargetDragWidthExpansion := 0.0;
    RenderWindow.TargetShadowIntensity := 0.8;
    if MenuVisible then
    begin
      RenderWindow.TargetRotation := 0.0;
      RenderWindow.TargetScale := 1.0;
    end;
  end;

  if WorldWindow.IsDragging then
  begin
    WorldWindow.X := X - WorldWindow.DragOffsetX;
    WorldWindow.Y := Y - WorldWindow.DragOffsetY;
    WorldWindow.OriginalX := WorldWindow.X;
    WorldWindow.OriginalY := WorldWindow.Y;
    WorldWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    WorldWindow.TargetShadowIntensity := 1.8;
    PaperDragOnMouseMove(WorldWindow, X);
    WorldWindow.TargetScale := 1.0;
  end
  else
  begin
    WorldWindow.TargetDragWidthExpansion := 0.0;
    WorldWindow.TargetShadowIntensity := 0.8;
    if MenuVisible then
    begin
      WorldWindow.TargetRotation := 0.0;
      WorldWindow.TargetScale := 1.0;
    end;
  end;

  if LocomotiveWindow.IsDragging then
  begin
    LocomotiveWindow.X := X - LocomotiveWindow.DragOffsetX;
    LocomotiveWindow.Y := Y - LocomotiveWindow.DragOffsetY;
    LocomotiveWindow.OriginalX := LocomotiveWindow.X;
    LocomotiveWindow.OriginalY := LocomotiveWindow.Y;
    LocomotiveWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    LocomotiveWindow.TargetShadowIntensity := 1.8;
    PaperDragOnMouseMove(LocomotiveWindow, X);
    LocomotiveWindow.TargetScale := 1.0;
  end
  else
  begin
    LocomotiveWindow.TargetDragWidthExpansion := 0.0;
    LocomotiveWindow.TargetShadowIntensity := 0.8;
    if MenuVisible then
    begin
      LocomotiveWindow.TargetRotation := 0.0;
      LocomotiveWindow.TargetScale := 1.0;
    end;
  end;

  if MenuWindow.IsDragging then
  begin
    MenuWindow.X := X - MenuWindow.DragOffsetX;
    MenuWindow.Y := Y - MenuWindow.DragOffsetY;
    MenuWindow.OriginalX := MenuWindow.X;
    MenuWindow.OriginalY := MenuWindow.Y;
    MenuWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    MenuWindow.TargetShadowIntensity := 1.8;
    PaperDragOnMouseMove(MenuWindow, X);
    MenuWindow.TargetScale := 1.0;
  end
  else
  begin
    MenuWindow.TargetDragWidthExpansion := 0.0;
    MenuWindow.TargetShadowIntensity := 0.8;
    if MenuVisible then
    begin
      MenuWindow.TargetRotation := 0.0;
      MenuWindow.TargetScale := 1.0;
    end;
  end;
  
  // Обработка драггинга слайдеров. Drag-origin совпадает с X, в котором
  // слайдер реально отрисовывается (= MARGIN + 16 внутри expanded секций),
  // иначе при перетаскивании thumb прыгает.
  if Settings.BrightnessSlider.IsDragging then
    HandleSliderDrag(X, Settings.BrightnessSlider, RenderWindow.X + MARGIN + 16);
  if Settings.BasespeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.BasespeedSlider, RenderWindow.X + MARGIN + 16);
  if Settings.FastspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.FastspeedSlider, RenderWindow.X + MARGIN + 16);
  if Settings.TurnspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.TurnspeedSlider, RenderWindow.X + MARGIN + 16);
  if Settings.StepForwardSlider.IsDragging then
    HandleSliderDrag(X, Settings.StepForwardSlider, RenderWindow.X + MARGIN + 16);
  if Settings.MaxVisibleDistanceSlider.IsDragging then
    HandleSliderDrag(X, Settings.MaxVisibleDistanceSlider, WorldWindow.X + MARGIN + 16);
  if Settings.ViewAngleSlider.IsDragging then
    HandleSliderDrag(X, Settings.ViewAngleSlider, RenderWindow.X + MARGIN + 16);
  if Settings.CameraSensitivitySlider.IsDragging then
    HandleSliderDrag(X, Settings.CameraSensitivitySlider, RenderWindow.X + MARGIN + 16);

  // Слайдеры редактора кастомных текстов — после изменения пишем в выбранный текст.
  if CustomXSlider.IsDragging then
    HandleSliderDrag(X, CustomXSlider, LocomotiveWindow.X + MARGIN + 24);
  if CustomYSlider.IsDragging then
    HandleSliderDrag(X, CustomYSlider, LocomotiveWindow.X + MARGIN + 24);
  if CustomZSlider.IsDragging then
    HandleSliderDrag(X, CustomZSlider, LocomotiveWindow.X + MARGIN + 24);
  if CustomRXSlider.IsDragging then
    HandleSliderDrag(X, CustomRXSlider, LocomotiveWindow.X + MARGIN + 24);
  if CustomRYSlider.IsDragging then
    HandleSliderDrag(X, CustomRYSlider, LocomotiveWindow.X + MARGIN + 24);
  if CustomRZSlider.IsDragging then
    HandleSliderDrag(X, CustomRZSlider, LocomotiveWindow.X + MARGIN + 24);
  if CustomScaleSlider.IsDragging then
    HandleSliderDrag(X, CustomScaleSlider, LocomotiveWindow.X + MARGIN + 24);
  if AnyCustomSliderDragging then
    WriteSlidersToSelected;
  // Гизмо живёт в UpdateGizmoFrameRA3 (per-frame), здесь ничего не делаем.
end;

// Клики внутри Developer-секции (когда она раскрыта).
// SectionStartY — Y начала фона секции (он же ContentY в LOCOMOTIVE-блоке после
// прохождения трёх toggle'ов).
procedure HandleDeveloperSectionClick(X, Y, SectionStartY: Integer);
var
  ContentX, CurY: Integer;
  i: Integer;
begin
  ContentX := LocomotiveWindow.X + MARGIN + 24;
  CurY := SectionStartY + DEV_TOPPAD;
  Inc(CurY, DEV_LIVE_ROWS * DEV_LIVE_ROW_H);
  Inc(CurY, DEV_GAP);

  // [Open Full Dev Editor] — переключаемся в фуллскрин-режим
  if InRect(X, Y, ContentX, CurY, 190, 26) then
  begin
    DevEditorVisible := True;
    DevEditorScrollY := 0;
    Exit;
  end;
  Inc(CurY, 26 + 8);

  // [+ Add] — добавить новый текст
  if InRect(X, Y, ContentX + 160, CurY, 36, 22) then
  begin
    AddCustomText;
    SaveConfig;
    Exit;
  end;
  Inc(CurY, DEV_HDR_H);

  // Список — клик по строке выбирает её для редактирования
  for i := 0 to Length(CustomTexts) - 1 do
  begin
    if InRect(X, Y, ContentX - 4, CurY, 196, 20) then
    begin
      CustomTextSelectedIdx := i;
      SyncSlidersFromSelected;
      Exit;
    end;
    Inc(CurY, DEV_LIST_ROW_H);
  end;

  // Редактор — только если что-то выбрано
  if (CustomTextSelectedIdx < 0) or (CustomTextSelectedIdx >= Length(CustomTexts)) then Exit;
  Inc(CurY, DEV_GAP);

  // [<] — предыдущий источник
  if InRect(X, Y, ContentX, CurY, 22, 22) then
  begin
    CycleSelectedSource(-1);
    SaveConfig;
    Exit;
  end;
  // [>] — следующий источник
  if InRect(X, Y, ContentX + 168, CurY, 22, 22) then
  begin
    CycleSelectedSource(+1);
    SaveConfig;
    Exit;
  end;
  Inc(CurY, DEV_EDITOR_HEADER_H);

  // Кнопки режима гизмо: T / R / S
  if InRect(X, Y, ContentX, CurY, DEV_GIZMO_BTN_W, 22) then
  begin GizmoMode := gmTranslate; SaveConfig; Exit; end;
  if InRect(X, Y, ContentX + DEV_GIZMO_BTN_W + 4, CurY, DEV_GIZMO_BTN_W, 22) then
  begin GizmoMode := gmRotate; SaveConfig; Exit; end;
  if InRect(X, Y, ContentX + 2 * (DEV_GIZMO_BTN_W + 4), CurY, DEV_GIZMO_BTN_W, 22) then
  begin GizmoMode := gmScale; SaveConfig; Exit; end;
  Inc(CurY, DEV_EDITOR_GIZMO_MODE_H);

  // Visible checkbox
  if InRect(X, Y, ContentX, CurY, 120, CHECKBOX_SIZE + 4) then
  begin
    CustomTexts[CustomTextSelectedIdx].Visible := not CustomTexts[CustomTextSelectedIdx].Visible;
    SaveConfig;
    Exit;
  end;
  Inc(CurY, DEV_EDITOR_VIS_H);

  // 7 слайдеров. Клик в любую часть слота инициирует drag.
  if InRect(X, Y, ContentX, CurY + 15, SLIDER_WIDTH + 30, 30) then
  begin CustomXSlider.IsDragging := True; Exit; end;
  Inc(CurY, DEV_EDITOR_SLIDER_H);
  if InRect(X, Y, ContentX, CurY + 15, SLIDER_WIDTH + 30, 30) then
  begin CustomYSlider.IsDragging := True; Exit; end;
  Inc(CurY, DEV_EDITOR_SLIDER_H);
  if InRect(X, Y, ContentX, CurY + 15, SLIDER_WIDTH + 30, 30) then
  begin CustomZSlider.IsDragging := True; Exit; end;
  Inc(CurY, DEV_EDITOR_SLIDER_H);
  if InRect(X, Y, ContentX, CurY + 15, SLIDER_WIDTH + 30, 30) then
  begin CustomRXSlider.IsDragging := True; Exit; end;
  Inc(CurY, DEV_EDITOR_SLIDER_H);
  if InRect(X, Y, ContentX, CurY + 15, SLIDER_WIDTH + 30, 30) then
  begin CustomRYSlider.IsDragging := True; Exit; end;
  Inc(CurY, DEV_EDITOR_SLIDER_H);
  if InRect(X, Y, ContentX, CurY + 15, SLIDER_WIDTH + 30, 30) then
  begin CustomRZSlider.IsDragging := True; Exit; end;
  Inc(CurY, DEV_EDITOR_SLIDER_H);
  if InRect(X, Y, ContentX, CurY + 15, SLIDER_WIDTH + 30, 30) then
  begin CustomScaleSlider.IsDragging := True; Exit; end;
  Inc(CurY, DEV_EDITOR_SLIDER_H);

  // Delete
  if InRect(X, Y, ContentX, CurY, 80, 22) then
  begin
    DeleteSelectedCustomText;
    SaveConfig;
    Exit;
  end;
end;

// True если клик попал внутрь любого из четырёх окон меню — тогда гизмо
// игнорируем, чтобы не воровать клики у UI.
function PointInAnyMenuWindow(X, Y: Integer): Boolean;
begin
  Result :=
    InRect(X, Y, RenderWindow.X,     RenderWindow.Y,     RenderWindow.Width,     RenderWindow.Height) or
    InRect(X, Y, WorldWindow.X,      WorldWindow.Y,      WorldWindow.Width,      WorldWindow.Height) or
    InRect(X, Y, LocomotiveWindow.X, LocomotiveWindow.Y, LocomotiveWindow.Width, LocomotiveWindow.Height) or
    InRect(X, Y, MenuWindow.X,       MenuWindow.Y,       MenuWindow.Width,       MenuWindow.Height);
end;

procedure HandleMenuClick(X, Y: Integer); stdcall;
var
  ContentY: Integer;
  FreecamSectionY, MainCameraSectionY, MaxVisibleDistanceSectionY: Integer;
  SectionHeight: Integer;
  I_PFX: Integer;  // PostFX section row dispatch index
begin
  // Если открыт фуллскрин Dev Editor — все клики перехватывает он, обычное
  // меню в этом режиме спрятано.
  if DevEditorVisible then
  begin
    HandleDevEditorClick(X, Y);
    Exit;
  end;

  if not MenuVisible then Exit;

  if (RenderWindow.Alpha < 0.1) and (WorldWindow.Alpha < 0.1) and (LocomotiveWindow.Alpha < 0.1) and (MenuWindow.Alpha < 0.1) then Exit;

  // 3D-гизмо обрабатывается в UpdateGizmoFrameRA3 (per-frame), так что здесь
  // ничего не делаем — клик вне окон меню провалится в обычное игровое поведение
  // (или будет перехвачен per-frame state-machine).
  
  // MENU WINDOW
  if MenuWindow.Alpha > 0.1 then
  begin
    ContentY := MenuWindow.Y + HEADER_HEIGHT + MARGIN;
    
    // Заголовок для драггинга
    if InRect(X, Y, MenuWindow.X, MenuWindow.Y, MenuWindow.Width, HEADER_HEIGHT) then
    begin
      MenuWindow.IsDragging := True;
      MenuWindow.DragOffsetX := X - MenuWindow.X;
      MenuWindow.DragOffsetY := Y - MenuWindow.Y;
      MenuWindow.PrevDragX  := X;     // seed paper-drag velocity tracking
      MenuWindow.DragVelX   := 0.0;
      Exit;
    end;
    
    // Кнопки языков
    Inc(ContentY, 50); // Пропускаем заголовок
    
    // Русский
    if InRect(X, Y, MenuWindow.X + MARGIN, ContentY, 90, 35) then
    begin
      CurrentLanguage := langRussian;
      SaveConfig;
      Exit;
    end;
    
    // Украинский
    if InRect(X, Y, MenuWindow.X + MARGIN + 100, ContentY, 90, 35) then
    begin
      CurrentLanguage := langUkrainian;
      SaveConfig;
      Exit;
    end;
    
    // Английский
    if InRect(X, Y, MenuWindow.X + MARGIN + 200, ContentY, 90, 35) then
    begin
      CurrentLanguage := langEnglish;
      SaveConfig;
      Exit;
    end;
  end;
  
  // RENDER WINDOW
  if RenderWindow.Alpha > 0.1 then
  begin
    ContentY := RenderWindow.Y + HEADER_HEIGHT + MARGIN;
    
    // Заголовок для драггинга
    if InRect(X, Y, RenderWindow.X, RenderWindow.Y, RenderWindow.Width, HEADER_HEIGHT) then
    begin
      RenderWindow.IsDragging := True;
      RenderWindow.DragOffsetX := X - RenderWindow.X;
      RenderWindow.DragOffsetY := Y - RenderWindow.Y;
      RenderWindow.PrevDragX  := X;
      RenderWindow.DragVelX   := 0.0;
      Exit;
    end;
    
    // Freecam expand button
    if InRect(X, Y, RenderWindow.X + 220, ContentY + 6, BUTTON_SIZE, BUTTON_SIZE) then
    begin
      Settings.FreecamSection.Expanded := not Settings.FreecamSection.Expanded;
      Exit;
    end;
    
    // Freecam toggle
    if InRect(X, Y, RenderWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.Freecam := not Settings.Freecam;
      if Settings.Freecam then Settings.FreecamSection.Expanded := True;
  
      SaveConfig;
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
      LoadConfigForced;
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);
    
    // Freecam sliders
    FreecamSectionY := ContentY;
    if Settings.FreecamSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(150 * Settings.FreecamSection.AnimProgress);
      if SectionHeight > 40 then
      begin
        if InRect(X, Y, RenderWindow.X + MARGIN + 16, FreecamSectionY + 15, SLIDER_WIDTH + 30, 30) then
        begin
          Settings.BasespeedSlider.IsDragging := True;
          Exit;
        end;
        if InRect(X, Y, RenderWindow.X + MARGIN + 16, FreecamSectionY + 65, SLIDER_WIDTH + 30, 30) then
        begin
          Settings.FastspeedSlider.IsDragging := True;
          Exit;
        end;
      end;
      Inc(ContentY, SectionHeight + MARGIN);
    end;
    
    // Main Camera expand button
    if InRect(X, Y, RenderWindow.X + 220, ContentY + 6, BUTTON_SIZE, BUTTON_SIZE) then
    begin
      Settings.MainCameraSection.Expanded := not Settings.MainCameraSection.Expanded;
      Exit;
    end;
    
    // Main Camera toggle
    if InRect(X, Y, RenderWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.MainCamera := not Settings.MainCamera;
      if Settings.MainCamera then 
        Settings.MainCameraSection.Expanded := True
      else
      begin
        if ViewAnglePatched then
          RemoveViewAnglePatch;
        RestoreCameraSensitivity;
      end;
      
      if Settings.MainCamera then
      begin
        if Settings.NewViewAngle then
          ApplyViewAnglePatch;
        if Settings.CameraSensitivity then
          ApplyCameraSensitivity;
      end;
        
      SaveConfig;
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);
    
    // Main Camera section
    MainCameraSectionY := ContentY;
    if Settings.MainCameraSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(200 * Settings.MainCameraSection.AnimProgress);
      
      // Клик по слайдеру StepForward
      if (SectionHeight > 40) and InRect(X, Y, RenderWindow.X + MARGIN + 16, MainCameraSectionY + 15, SLIDER_WIDTH + 30, 30) then
      begin
        Settings.StepForwardSlider.IsDragging := True;
        Exit;
      end;
      
      // Клик по галочке "Новый угол обзора"
      if (SectionHeight > 70) and InRect(X, Y, RenderWindow.X + MARGIN + 16, MainCameraSectionY + 60, 200, 20) then
      begin
        Settings.NewViewAngle := not Settings.NewViewAngle;
        
        if Settings.MainCamera and Settings.NewViewAngle then
          ApplyViewAnglePatch
        else
          RemoveViewAnglePatch;
          
        SaveConfig;
        Exit;
      end;
      
      // Клик по слайдеру угла обзора
      if (SectionHeight > 110) and Settings.NewViewAngle and InRect(X, Y, RenderWindow.X + MARGIN + 16, MainCameraSectionY + 85, SLIDER_WIDTH + 30, 30) then
      begin
        Settings.ViewAngleSlider.IsDragging := True;
        Exit;
      end;
      
      // Галочка чувствительности камеры
      if (SectionHeight > 140) and InRect(X, Y, RenderWindow.X + MARGIN + 16, MainCameraSectionY + 130, 200, 20) then
      begin
        Settings.CameraSensitivity := not Settings.CameraSensitivity;
        
        if Settings.MainCamera and Settings.CameraSensitivity then
          ApplyCameraSensitivity
        else
          RestoreCameraSensitivity;
          
        SaveConfig;
        Exit;
      end;
      
      // Слайдер чувствительности камеры
      if (SectionHeight > 170) and Settings.CameraSensitivity and InRect(X, Y, RenderWindow.X + MARGIN + 16, MainCameraSectionY + 155, SLIDER_WIDTH + 30, 30) then
      begin
        Settings.CameraSensitivitySlider.IsDragging := True;
        Exit;
      end;
      
      Inc(ContentY, SectionHeight + MARGIN);
    end;

    // === "Графич. эффекты" header (in RENDER tab) ===
    // Раздельная семантика для двух интерактивных областей:
    //   * шеврон  → expand/collapse секции, не трогает enable;
    //   * label    → master ON/OFF (через SetPostFXMasterEnabled, что
    //                 синхронизирует все 9 индивидуальных toggle).
    if InRect(X, Y, RenderWindow.X + 220, ContentY + 6, BUTTON_SIZE, BUTTON_SIZE) then
    begin
      Settings.PostFXSection.Expanded := not Settings.PostFXSection.Expanded;
      Exit;
    end;

    if InRect(X, Y, RenderWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      SetPostFXMasterEnabled(not Settings.PostFXEnable);
      if Settings.PostFXEnable then
        Settings.PostFXSection.Expanded := True;
      SaveConfig;
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);

    // Внутри секции: диспетчер на 9 toggle. Layout совпадает с
    // DrawPostFXToggles — PAD_TOP сверху, потом 9 рядов высотой
    // (ITEM_HEIGHT + ROW_GAP). Координаты теперь относительно RenderWindow,
    // не WorldWindow (PostFX переехал в RENDER tab).
    if Settings.PostFXSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(GetPostFXContentHeight * Settings.PostFXSection.AnimProgress);
      if SectionHeight > POSTFX_PAD_TOP then
      begin
        for I_PFX := 0 to POSTFX_TOGGLE_COUNT - 1 do
        begin
          if InRect(X, Y,
                    RenderWindow.X + POSTFX_INNER_X,
                    ContentY + POSTFX_PAD_TOP + I_PFX * (ITEM_HEIGHT + POSTFX_ROW_GAP),
                    220, ITEM_HEIGHT) then
          begin
            case I_PFX of
              0: begin Settings.FXAAEnable     := not Settings.FXAAEnable;     InitFXAAEnable     := Settings.FXAAEnable;     end;
              1: begin Settings.BloomEnable    := not Settings.BloomEnable;    InitBloomEnable    := Settings.BloomEnable;    end;
              2: begin Settings.TonemapEnable  := not Settings.TonemapEnable;  InitTonemapEnable  := Settings.TonemapEnable;  end;
              3: begin Settings.SharpenEnable  := not Settings.SharpenEnable;  InitSharpenEnable  := Settings.SharpenEnable;  end;
              4: begin Settings.VignetteEnable := not Settings.VignetteEnable; InitVignetteEnable := Settings.VignetteEnable; end;
              5: begin Settings.GrainEnable    := not Settings.GrainEnable;    InitGrainEnable    := Settings.GrainEnable;    end;
              6: begin Settings.SSAOEnable     := not Settings.SSAOEnable;     InitSSAOEnable     := Settings.SSAOEnable;     end;
              7: begin Settings.FogEnable      := not Settings.FogEnable;      InitFogEnable      := Settings.FogEnable;      end;
              8: begin Settings.DOFEnable      := not Settings.DOFEnable;      InitDOFEnable      := Settings.DOFEnable;      end;
            end;
            SaveConfig;
            Exit;
          end;
        end;
      end;
      Inc(ContentY, SectionHeight + MARGIN);
    end;
  end;
  
  // WORLD WINDOW
  if WorldWindow.Alpha > 0.1 then
  begin
    ContentY := WorldWindow.Y + HEADER_HEIGHT + MARGIN;
    
    // Заголовок для драггинга
    if InRect(X, Y, WorldWindow.X, WorldWindow.Y, WorldWindow.Width, HEADER_HEIGHT) then
    begin
      WorldWindow.IsDragging := True;
      WorldWindow.DragOffsetX := X - WorldWindow.X;
      WorldWindow.DragOffsetY := Y - WorldWindow.Y;
      WorldWindow.PrevDragX  := X;
      WorldWindow.DragVelX   := 0.0;
      Exit;
    end;
    
    // Max Visible Distance expand button
    if InRect(X, Y, WorldWindow.X + 220, ContentY + 6, BUTTON_SIZE, BUTTON_SIZE) then
    begin
      Settings.MaxVisibleDistanceSection.Expanded := not Settings.MaxVisibleDistanceSection.Expanded;
      Exit;
    end;
    
    // Max Visible Distance toggle
    if InRect(X, Y, WorldWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.MaxVisibleDistance := not Settings.MaxVisibleDistance;
      if Settings.MaxVisibleDistance then 
      begin
        Settings.MaxVisibleDistanceSection.Expanded := True;
        ApplyDistanceSettings;
      end
      else
      begin
        RestoreOriginalDistanceValues;
      end;
      
      SaveConfig;
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
      LastConfigReadTime := GetTickCount;
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);
    
    // Max Visible Distance секция
    MaxVisibleDistanceSectionY := ContentY;
    if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(180 * Settings.MaxVisibleDistanceSection.AnimProgress);
      if SectionHeight > 40 then
      begin
        // Слайдер дальности
        if InRect(X, Y, WorldWindow.X + MARGIN + 16, MaxVisibleDistanceSectionY + 15, SLIDER_WIDTH + 30, 30) then
        begin
          Settings.MaxVisibleDistanceSlider.IsDragging := True;
          Exit;
        end;
        
        // Чекбоксы
        if (SectionHeight > 70) and InRect(X, Y, WorldWindow.X + MARGIN + 16, MaxVisibleDistanceSectionY + 60, 200, 20) then
        begin
          Settings.ShowWires := not Settings.ShowWires;
          if Settings.MaxVisibleDistance then 
            ApplyDistanceSettings;
          SaveConfig;
          Exit;
        end;
        if (SectionHeight > 100) and InRect(X, Y, WorldWindow.X + MARGIN + 16, MaxVisibleDistanceSectionY + 90, 200, 20) then
        begin
          Settings.ShowDistantModels := not Settings.ShowDistantModels;
          if Settings.MaxVisibleDistance then 
            ApplyDistanceSettings;
          SaveConfig;
          Exit;
        end;
        if (SectionHeight > 130) and InRect(X, Y, WorldWindow.X + MARGIN + 16, MaxVisibleDistanceSectionY + 120, 200, 20) then
        begin
          Settings.ShowTrafficLights := not Settings.ShowTrafficLights;
          if Settings.MaxVisibleDistance then 
            ApplyDistanceSettings;
          SaveConfig;
          Exit;
        end;
      end;
      Inc(ContentY, SectionHeight + MARGIN);
    end;
    
    // New Sky toggle
    if InRect(X, Y, WorldWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.NewSky := not Settings.NewSky;
      SaveConfig;
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);

    // System Time toggle — пишет в InitSystemTimeEnable, который
    // SystemTime.pas читает на каждый кадр.
    if InRect(X, Y, WorldWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.SystemTimeEnable := not Settings.SystemTimeEnable;
      InitSystemTimeEnable := Settings.SystemTimeEnable;
      SaveConfig;
      Exit;
    end;
    // Note: PostFX header + section dispatcher переехали в RENDER click
    // handler выше — здесь только NewSky + SystemTime.
  end;

  // LOCOMOTIVE WINDOW
  if LocomotiveWindow.Alpha > 0.1 then
  begin
    ContentY := LocomotiveWindow.Y + HEADER_HEIGHT + MARGIN;

    // Заголовок для драггинга
    if InRect(X, Y, LocomotiveWindow.X, LocomotiveWindow.Y, LocomotiveWindow.Width, HEADER_HEIGHT) then
    begin
      LocomotiveWindow.IsDragging := True;
      LocomotiveWindow.DragOffsetX := X - LocomotiveWindow.X;
      LocomotiveWindow.DragOffsetY := Y - LocomotiveWindow.Y;
      LocomotiveWindow.PrevDragX  := X;
      LocomotiveWindow.DragVelX   := 0.0;
      Exit;
    end;

    // Исправления КЛУБ
    if InRect(X, Y, LocomotiveWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.NewClubPositions := not Settings.NewClubPositions;
      if Settings.NewClubPositions then
        ApplyClubPositionsPatch
      else
        RemoveClubPositionsPatch;
      SaveConfig;
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);

    // RA3 подсветка
    if InRect(X, Y, LocomotiveWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.RA3Hover := not Settings.RA3Hover;
      SaveConfig;
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);

    // Меню разработчика — клик по кнопке expand ИЛИ по toggle:
    //   обе превратились в одно действие — «открыть фуллскрин Dev Editor».
    //   Inline-панель (Speed/Limit/Target/Distance + Custom texts) больше не
    //   разворачиваем — пользователь хочет сразу редактор.
    if InRect(X, Y, LocomotiveWindow.X + 220, ContentY + 6, BUTTON_SIZE, BUTTON_SIZE) or
       InRect(X, Y, LocomotiveWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.DeveloperMenu := True;
      Settings.DeveloperSection.Expanded := False;
      DevEditorVisible := True;
      DevEditorScrollY := 0;
      SaveConfig;
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);

    // === Кликабельные зоны внутри Developer-секции — оставлены на случай,
    // если кто-то вручную развернёт inline-панель из конфига. В нормальной
    // работе мы её больше не показываем ===
    if Settings.DeveloperMenu and (Settings.DeveloperSection.AnimProgress > 0.5) then
      HandleDeveloperSectionClick(X, Y, ContentY);
  end;
end;

function IsRA3HoverEnabled: Boolean;
begin
  Result := Settings.RA3Hover;
end;

// === ОПТИМИЗИРОВАННАЯ ФУНКЦИЯ ОТПУСКАНИЯ МЫШИ ===
procedure HandleMenuMouseUp; stdcall;
begin
  // Dev editor всегда обрабатывает свой mouseUp (release scrollbar drag/auto-repeat),
  // даже если основное меню спрятано.
  if DevEditorVisible then
    HandleDevEditorMouseUp;

  if not MenuVisible then Exit;
  
  // Останавливаем драггинг окон. Сбрасываем paper-drag velocity, иначе
  // при следующем нажатии rotation начнёт с inherit-значения.
  RenderWindow.IsDragging := False;     RenderWindow.DragVelX := 0.0;
  WorldWindow.IsDragging := False;      WorldWindow.DragVelX := 0.0;
  LocomotiveWindow.IsDragging := False; LocomotiveWindow.DragVelX := 0.0;
  MenuWindow.IsDragging := False;       MenuWindow.DragVelX := 0.0;
  
  // Возвращаем размеры окон к нормальным
  RenderWindow.TargetDragWidthExpansion := 0.0;
  WorldWindow.TargetDragWidthExpansion := 0.0;
  LocomotiveWindow.TargetDragWidthExpansion := 0.0;
  MenuWindow.TargetDragWidthExpansion := 0.0;

  // Возвращаем тени к нормальным
  RenderWindow.TargetShadowIntensity := 0.8;
  WorldWindow.TargetShadowIntensity := 0.8;
  LocomotiveWindow.TargetShadowIntensity := 0.8;
  MenuWindow.TargetShadowIntensity := 0.8;

  // Сбрасываем наклон и подъём от перетаскивания — плавный возврат к норме
  RenderWindow.TargetRotation := 0.0;
  WorldWindow.TargetRotation := 0.0;
  LocomotiveWindow.TargetRotation := 0.0;
  MenuWindow.TargetRotation := 0.0;

  RenderWindow.TargetScale := 1.0;
  WorldWindow.TargetScale := 1.0;
  LocomotiveWindow.TargetScale := 1.0;
  MenuWindow.TargetScale := 1.0;
  
  // Останавливаем драггинг слайдеров
  Settings.BrightnessSlider.IsDragging := False;
  Settings.BasespeedSlider.IsDragging := False;
  Settings.FastspeedSlider.IsDragging := False;
  Settings.TurnspeedSlider.IsDragging := False;
  Settings.StepForwardSlider.IsDragging := False;
  Settings.MaxVisibleDistanceSlider.IsDragging := False;
  Settings.ViewAngleSlider.IsDragging := False;
  Settings.CameraSensitivitySlider.IsDragging := False;

  // Если drag слайдера успел запачкать настройки, но throttle не дал
  // записать на диск — финальный гарантированный save при отпускании.
  if SliderDirty then
  begin
    SaveConfig;
    SliderDirty := False;
    LastSliderSaveTime := GetTickCount;
  end;

  // Слайдеры редактора кастомных текстов
  if AnyCustomSliderDragging then
  begin
    WriteSlidersToSelected;
    SaveConfig;
  end;
  CustomXSlider.IsDragging := False;
  CustomYSlider.IsDragging := False;
  CustomZSlider.IsDragging := False;
  CustomRXSlider.IsDragging := False;
  CustomRYSlider.IsDragging := False;
  CustomRZSlider.IsDragging := False;
  CustomScaleSlider.IsDragging := False;
  // Гизмо обрабатывает MouseUp сам в UpdateGizmoFrameRA3.

  // Применяем все настройки при отпускании мыши
  if Settings.MaxVisibleDistance then
    ApplyDistanceSettings;
  if Settings.MainCamera and Settings.CameraSensitivity then
    ApplyCameraSensitivity;
end;

procedure ToggleMenu; stdcall;
var
  CenterX, CenterY: Integer;
begin
  MenuVisible := not MenuVisible;

  // F12 открывает обычное меню (4 окна). Dev Editor включается только из
  // секции «Меню разработчика → Open Full Dev Editor». При закрытии меню
  // редактор тоже закрываем — чтобы не остался поверх кабины.
  if not MenuVisible then
    DevEditorVisible := False;

  if MenuVisible then
  begin
    ShowCursor(True);
    LoadConfigForced;
    
    LastFrameTime := GetTickCount;
    
    CenterX := InitResX div 2;
    CenterY := InitResY div 2;
    
    MenuAnimationProgress := 0.0;
    MenuTargetProgress := 1.0;
    
    // === RENDER WINDOW ===
    // НЕ ТРОГАЕМ OriginalX и OriginalY!
    RenderWindow.X := CenterX;
    RenderWindow.Y := CenterY;
    RenderWindow.Alpha := 0.0;
    RenderWindow.TargetAlpha := 1.0;
    RenderWindow.Scale := 0.3;
    RenderWindow.TargetScale := 1.0;
    RenderWindow.TransformProgress := 0.0;
    RenderWindow.TargetTransformProgress := 1.0;
    RenderWindow.Rotation := 15.0 * PI / 180.0;
    RenderWindow.TargetRotation := 0.0;
    RenderWindow.TranslateY := -200.0;
    RenderWindow.TargetTranslateY := 0.0;
    RenderWindow.DragWidthExpansion := 0.0;
    RenderWindow.TargetDragWidthExpansion := 0.0;
    RenderWindow.ShadowIntensity := 0.0;
    RenderWindow.TargetShadowIntensity := 0.8;
    RenderWindow.IsDragging := False;
    
    // === WORLD WINDOW ===
    WorldWindow.X := CenterX;
    WorldWindow.Y := CenterY;
    WorldWindow.Alpha := 0.0;
    WorldWindow.TargetAlpha := 1.0;
    WorldWindow.Scale := 0.3;
    WorldWindow.TargetScale := 1.0;
    WorldWindow.TransformProgress := 0.0;
    WorldWindow.TargetTransformProgress := 1.0;
    WorldWindow.Rotation := -10.0 * PI / 180.0;
    WorldWindow.TargetRotation := 0.0;
    WorldWindow.TranslateY := -150.0;
    WorldWindow.TargetTranslateY := 0.0;
    WorldWindow.DragWidthExpansion := 0.0;
    WorldWindow.TargetDragWidthExpansion := 0.0;
    WorldWindow.ShadowIntensity := 0.0;
    WorldWindow.TargetShadowIntensity := 0.8;
    WorldWindow.IsDragging := False;
    
    // === LOCOMOTIVE WINDOW ===
    LocomotiveWindow.X := CenterX;
    LocomotiveWindow.Y := CenterY;
    LocomotiveWindow.Alpha := 0.0;
    LocomotiveWindow.TargetAlpha := 1.0;
    LocomotiveWindow.Scale := 0.3;
    LocomotiveWindow.TargetScale := 1.0;
    LocomotiveWindow.TransformProgress := 0.0;
    LocomotiveWindow.TargetTransformProgress := 1.0;
    LocomotiveWindow.Rotation := 20.0 * PI / 180.0;
    LocomotiveWindow.TargetRotation := 0.0;
    LocomotiveWindow.TranslateY := -100.0;
    LocomotiveWindow.TargetTranslateY := 0.0;
    LocomotiveWindow.DragWidthExpansion := 0.0;
    LocomotiveWindow.TargetDragWidthExpansion := 0.0;
    LocomotiveWindow.ShadowIntensity := 0.0;
    LocomotiveWindow.TargetShadowIntensity := 0.8;
    LocomotiveWindow.IsDragging := False;
    
    // === MENU WINDOW ===
    MenuWindow.X := CenterX;
    MenuWindow.Y := CenterY;
    MenuWindow.Alpha := 0.0;
    MenuWindow.TargetAlpha := 1.0;
    MenuWindow.Scale := 0.3;
    MenuWindow.TargetScale := 1.0;
    MenuWindow.TransformProgress := 0.0;
    MenuWindow.TargetTransformProgress := 1.0;
    MenuWindow.Rotation := -25.0 * PI / 180.0;
    MenuWindow.TargetRotation := 0.0;
    MenuWindow.TranslateY := -250.0;
    MenuWindow.TargetTranslateY := 0.0;
    MenuWindow.DragWidthExpansion := 0.0;
    MenuWindow.TargetDragWidthExpansion := 0.0;
    MenuWindow.ShadowIntensity := 0.0;
    MenuWindow.TargetShadowIntensity := 0.8;
    MenuWindow.IsDragging := False;
    
    ApplyMenuPatch;
  end
  else
  begin
    ShowCursor(False);

    MenuTargetProgress := 0.0;

    // Отпускаем перетаскивание, чтобы окна корректно «улетели» обратно к центру
    RenderWindow.IsDragging := False;
    WorldWindow.IsDragging := False;
    LocomotiveWindow.IsDragging := False;
    MenuWindow.IsDragging := False;

    RenderWindow.TargetAlpha := 0.0;
    RenderWindow.TargetScale := 0.3;
    RenderWindow.TargetTransformProgress := 0.0;
    RenderWindow.TargetRotation := -15.0 * PI / 180.0;
    RenderWindow.TargetTranslateY := -200.0;
    RenderWindow.TargetShadowIntensity := 0.0;
    
    WorldWindow.TargetAlpha := 0.0;
    WorldWindow.TargetScale := 0.3;
    WorldWindow.TargetTransformProgress := 0.0;
    WorldWindow.TargetRotation := 10.0 * PI / 180.0;
    WorldWindow.TargetTranslateY := -150.0;
    WorldWindow.TargetShadowIntensity := 0.0;
    
    LocomotiveWindow.TargetAlpha := 0.0;
    LocomotiveWindow.TargetScale := 0.3;
    LocomotiveWindow.TargetTransformProgress := 0.0;
    LocomotiveWindow.TargetRotation := -20.0 * PI / 180.0;
    LocomotiveWindow.TargetTranslateY := -100.0;
    LocomotiveWindow.TargetShadowIntensity := 0.0;
    
    MenuWindow.TargetAlpha := 0.0;
    MenuWindow.TargetScale := 0.3;
    MenuWindow.TargetTransformProgress := 0.0;
    MenuWindow.TargetRotation := 25.0 * PI / 180.0;
    MenuWindow.TargetTranslateY := -250.0;
    MenuWindow.TargetShadowIntensity := 0.0;
    
    RemoveMenuPatch;
  end;
end;

// ===========================================================================
//  DEV EDITOR — реализация
// ===========================================================================

// Помощник: True, если точка (X,Y) внутри прямоугольника RX..RX+RW × RY..RY+RH.
function DEInRect(X, Y, RX, RY, RW, RH: Integer): Boolean;
begin
  Result := (X >= RX) and (X <= RX + RW) and (Y >= RY) and (Y <= RY + RH);
end;

// Заголовок столбца над ячейкой — отцентрирован над полем значения,
// крупнее и контрастнее, чтобы было читаемо.
procedure DEDrawHdrLabel(CenterX, Y: Integer; const Lbl: string; Alpha: Integer);
var
  W: Integer;
begin
  // Грубая оценка ширины текста (px на символ при size=0.6 ≈ 7).
  W := Length(Lbl) * 7;
  DrawText2D(0, CenterX - W div 2 + 1, Y + 1, Lbl, COLOR_SHADOW, Alpha div 3, 0.6);
  DrawText2D(0, CenterX - W div 2, Y, Lbl, COLOR_ACCENT, Alpha, 0.6);
end;

// Рисуем «[−] value [+]» ячейку с заголовком над полем значения.
// IsHotMinus/IsHotPlus — для подсветки кнопок при hover.
procedure DEDrawValueCell(X, Y: Integer; const Lbl, ValStr: string;
  Alpha: Integer; IsHotMinus, IsHotPlus: Boolean);
var
  BtnY, ValX, ValTextX, ValTextY: Integer;
  CMinus, CPlus: Integer;
  TextW: Integer;
begin
  BtnY := Y;
  ValX := X + DE_BTN_SMALL + 2;

  // Заголовок столбца (X / Y / Z / RX / RY / RZ / Scale) — над value-полем.
  // Подняли на 3 px ближе к верху карточки.
  DEDrawHdrLabel(ValX + DE_VAL_W div 2, Y - 17, Lbl, Alpha);

  // Кнопка [-] ---------------------------------------------------------------
  if IsHotMinus then CMinus := COLOR_PRIMARY else CMinus := COLOR_SURFACE_VARIANT;
  DrawStyledRect(X, BtnY, DE_BTN_SMALL, DE_BTN_SMALL, CMinus, Alpha, True, COLOR_BORDER);
  // «−» — текст чуть выше центра кнопки.
  DrawText2D(0, X + (DE_BTN_SMALL - 6) div 2, BtnY + 1,
    '-', COLOR_ON_SURFACE, Alpha, 0.7);

  // Поле значения -----------------------------------------------------------
  DrawStyledRect(ValX, BtnY, DE_VAL_W, DE_BTN_SMALL,
    COLOR_SURFACE, Alpha, True, COLOR_BORDER);
  // Центрируем числовой текст (оценка по 7 px/символ при size=0.6).
  TextW := Length(ValStr) * 7;
  if TextW > DE_VAL_W - 4 then TextW := DE_VAL_W - 4;
  ValTextX := ValX + (DE_VAL_W - TextW) div 2;
  // Подняли значения чуть выше — ближе к верху поля (было +5).
  ValTextY := BtnY + 2;
  DrawText2D(0, ValTextX + 1, ValTextY + 1, ValStr, COLOR_SHADOW, Alpha div 3, 0.6);
  DrawText2D(0, ValTextX,     ValTextY,     ValStr, COLOR_ON_SURFACE, Alpha, 0.6);

  // Кнопка [+] --------------------------------------------------------------
  if IsHotPlus then CPlus := COLOR_PRIMARY else CPlus := COLOR_SURFACE_VARIANT;
  DrawStyledRect(ValX + DE_VAL_W + 2, BtnY, DE_BTN_SMALL, DE_BTN_SMALL,
    CPlus, Alpha, True, COLOR_BORDER);
  DrawText2D(0, ValX + DE_VAL_W + 2 + (DE_BTN_SMALL - 6) div 2, BtnY + 1,
    '+', COLOR_ON_SURFACE, Alpha, 0.7);
end;

// Тестируем, в каком hot-spot'е находится точка (MX,MY) относительно карточки
// с верхним левым углом (CardX, CardY) и шириной CardW. Карточка имеет
// фиксированную раскладку — должна совпадать с DrawDevEditor.
// Возвращает DE_HOT_* код или DE_HOT_NONE.
function DEHotspotAtPoint(MX, MY, CardX, CardY, CardW: Integer): Integer;
var
  CellX, CellY: Integer;
  i: Integer;
  Steps: array[0..6] of Integer;
  StepHotMinus: array[0..6] of Integer;
  StepHotPlus:  array[0..6] of Integer;
begin
  Result := DE_HOT_NONE;
  // Все интерактивные элементы лежат в одной горизонтальной полосе на CardY+30,
  // высотой DE_BTN_SMALL (22 px). Над ними — column-labels.
  CellY := CardY + 30;

  // Source-combobox
  if DEInRect(MX, MY, CardX + DE_IDX_W, CellY, DE_SOURCE_W, DE_BTN_SMALL) then
  begin Result := DE_HOT_SOURCE; Exit; end;

  // Семь -/value/+ ячеек по списку
  Steps[0] := DE_HOT_X_MINUS;  StepHotMinus[0] := DE_HOT_X_MINUS;  StepHotPlus[0] := DE_HOT_X_PLUS;
  Steps[1] := DE_HOT_Y_MINUS;  StepHotMinus[1] := DE_HOT_Y_MINUS;  StepHotPlus[1] := DE_HOT_Y_PLUS;
  Steps[2] := DE_HOT_Z_MINUS;  StepHotMinus[2] := DE_HOT_Z_MINUS;  StepHotPlus[2] := DE_HOT_Z_PLUS;
  Steps[3] := DE_HOT_RX_MINUS; StepHotMinus[3] := DE_HOT_RX_MINUS; StepHotPlus[3] := DE_HOT_RX_PLUS;
  Steps[4] := DE_HOT_RY_MINUS; StepHotMinus[4] := DE_HOT_RY_MINUS; StepHotPlus[4] := DE_HOT_RY_PLUS;
  Steps[5] := DE_HOT_RZ_MINUS; StepHotMinus[5] := DE_HOT_RZ_MINUS; StepHotPlus[5] := DE_HOT_RZ_PLUS;
  Steps[6] := DE_HOT_S_MINUS;  StepHotMinus[6] := DE_HOT_S_MINUS;  StepHotPlus[6] := DE_HOT_S_PLUS;

  CellX := CardX + DE_IDX_W + DE_SOURCE_W + 8;
  for i := 0 to 6 do
  begin
    // [-]
    if DEInRect(MX, MY, CellX, CellY, DE_BTN_SMALL, DE_BTN_SMALL) then
    begin Result := StepHotMinus[i]; Exit; end;
    // [+]
    if DEInRect(MX, MY, CellX + DE_BTN_SMALL + 2 + DE_VAL_W + 2, CellY,
      DE_BTN_SMALL, DE_BTN_SMALL) then
    begin Result := StepHotPlus[i]; Exit; end;
    Inc(CellX, DE_CELL_W);
  end;

  // Color swatch — по той же горизонтальной полосе.
  if DEInRect(MX, MY, CellX, CellY, DE_SWATCH_W - 8, DE_BTN_SMALL) then
  begin Result := DE_HOT_COLOR; Exit; end;
  Inc(CellX, DE_SWATCH_W);

  // Visible
  if DEInRect(MX, MY, CellX, CellY, DE_BTN_SMALL, DE_BTN_SMALL) then
  begin Result := DE_HOT_VISIBLE; Exit; end;
  Inc(CellX, DE_VISIBLE_W);

  // Delete
  if DEInRect(MX, MY, CellX, CellY, DE_DELETE_W - 4, DE_BTN_SMALL) then
  begin Result := DE_HOT_DELETE; Exit; end;
end;

// Применить шаг +/− к выбранной карточке.
procedure DEApplyStep(CardIdx, Hotspot: Integer; Sign: Integer);
begin
  if (CardIdx < 0) or (CardIdx >= Length(CustomTexts)) then Exit;
  case Hotspot of
    DE_HOT_X_MINUS, DE_HOT_X_PLUS:   CustomTexts[CardIdx].X  := CustomTexts[CardIdx].X  + Sign * DE_STEP_POS;
    DE_HOT_Y_MINUS, DE_HOT_Y_PLUS:   CustomTexts[CardIdx].Y  := CustomTexts[CardIdx].Y  + Sign * DE_STEP_POS;
    DE_HOT_Z_MINUS, DE_HOT_Z_PLUS:   CustomTexts[CardIdx].Z  := CustomTexts[CardIdx].Z  + Sign * DE_STEP_POS;
    DE_HOT_RX_MINUS, DE_HOT_RX_PLUS: CustomTexts[CardIdx].RX := CustomTexts[CardIdx].RX + Sign * DE_STEP_ANG;
    DE_HOT_RY_MINUS, DE_HOT_RY_PLUS: CustomTexts[CardIdx].RY := CustomTexts[CardIdx].RY + Sign * DE_STEP_ANG;
    DE_HOT_RZ_MINUS, DE_HOT_RZ_PLUS: CustomTexts[CardIdx].RZ := CustomTexts[CardIdx].RZ + Sign * DE_STEP_ANG;
    DE_HOT_S_MINUS, DE_HOT_S_PLUS:
    begin
      CustomTexts[CardIdx].Scale := CustomTexts[CardIdx].Scale + Sign * DE_STEP_SCALE;
      if CustomTexts[CardIdx].Scale < 0.0005 then CustomTexts[CardIdx].Scale := 0.0005;
    end;
  end;
  if CardIdx = CustomTextSelectedIdx then SyncSlidersFromSelected;
end;

// True если хот-спот это +/- кнопка (поддерживает auto-repeat при удержании).
function DEIsRepeatableHotspot(H: Integer): Boolean;
begin
  Result :=
    (H = DE_HOT_X_MINUS) or (H = DE_HOT_X_PLUS) or
    (H = DE_HOT_Y_MINUS) or (H = DE_HOT_Y_PLUS) or
    (H = DE_HOT_Z_MINUS) or (H = DE_HOT_Z_PLUS) or
    (H = DE_HOT_RX_MINUS) or (H = DE_HOT_RX_PLUS) or
    (H = DE_HOT_RY_MINUS) or (H = DE_HOT_RY_PLUS) or
    (H = DE_HOT_RZ_MINUS) or (H = DE_HOT_RZ_PLUS) or
    (H = DE_HOT_S_MINUS) or (H = DE_HOT_S_PLUS);
end;

function DEHotspotSign(H: Integer): Integer;
begin
  case H of
    DE_HOT_X_MINUS, DE_HOT_Y_MINUS, DE_HOT_Z_MINUS,
    DE_HOT_RX_MINUS, DE_HOT_RY_MINUS, DE_HOT_RZ_MINUS,
    DE_HOT_S_MINUS: Result := -1;
  else
    Result := 1;
  end;
end;

procedure DrawDevEditor;
var
  ScrW, ScrH: Integer;
  HeaderY, ToolbarY, ListY, ListBottom: Integer;
  CardX, CardY, CardW, CardInnerW: Integer;
  i, k: Integer;
  T: TCustomText3D;
  Idx: Integer;
  CellX: Integer;
  IsSel, IsHover: Boolean;
  CardBg, CardBorder: Integer;
  ValStr: string;
  HotMinus, HotPlus: Boolean;
  ScrollbarY, ScrollbarH: Integer;
  ContentH: Integer;
  // Combobox dropdown
  CmbX, CmbY, CmbW, CmbH: Integer;
  // Color picker
  PkrX, PkrY, PkrW, PkrH: Integer;
  Col, Row: Integer;
  PaletteX, PaletteY: Integer;
begin
  if not DevEditorVisible then Exit;
  ScrW := InitResX;
  ScrH := InitResY;

  Begin2D;
  try
    // Полупрозрачный фон, чтобы кабину было слегка видно — но достаточно тёмно
    // для контраста с UI.
    DrawRectangle2D(0, 0, ScrW, ScrH, COLOR_BACKGROUND, 230, True);

    // === HEADER ===
    HeaderY := DE_PAD_Y;
    DrawRectangle2D(DE_PAD_X, HeaderY, ScrW - 2 * DE_PAD_X, DE_HEADER_H,
      COLOR_PRIMARY, 255, True);
    DrawRectangle2D(DE_PAD_X, HeaderY, ScrW - 2 * DE_PAD_X, DE_HEADER_H,
      COLOR_PRIMARY_VARIANT, 255, False);

    // Кнопка Back СЛЕВА — рядом с краем, чтобы не уходить в правый угол.
    if DevEditorHoveredHotspot = DE_HOT_BACK then
      DrawStyledRect(DE_PAD_X + 12, HeaderY + 12, 110, DE_HEADER_H - 24,
        COLOR_ACCENT, 255, True, COLOR_ACCENT)
    else
      DrawStyledRect(DE_PAD_X + 12, HeaderY + 12, 110, DE_HEADER_H - 24,
        COLOR_PRIMARY_VARIANT, 255, True, COLOR_BORDER_LIGHT);
    DrawText2D(0, DE_PAD_X + 12 + 18, HeaderY + 16,
      '< BACK TO MENU', COLOR_ON_PRIMARY, 255, 0.55);

    // Заголовок — справа от Back-кнопки.
    DrawText2D(0, DE_PAD_X + 12 + 110 + 17, HeaderY + 14,
      'ZDSIMULATOR BOOSTER  —  CUSTOM TEXT EDITOR',
      COLOR_SHADOW, 100, 0.95);
    DrawText2D(0, DE_PAD_X + 12 + 110 + 16, HeaderY + 13,
      'ZDSIMULATOR BOOSTER  —  CUSTOM TEXT EDITOR',
      COLOR_ON_PRIMARY, 255, 0.95);

    // === TOOLBAR ===
    ToolbarY := HeaderY + DE_HEADER_H + 6;
    DrawRectangle2D(DE_PAD_X, ToolbarY, ScrW - 2 * DE_PAD_X, DE_TOOLBAR_H,
      COLOR_SURFACE, 240, True);
    DrawRectangle2D(DE_PAD_X, ToolbarY, ScrW - 2 * DE_PAD_X, DE_TOOLBAR_H,
      COLOR_BORDER, 240, False);

    // [+ Add Text] кнопка — текст поднят на 3 px.
    if DevEditorHoveredHotspot = DE_HOT_ADD then
      DrawStyledRect(DE_PAD_X + 12, ToolbarY + 10, 110, 24,
        COLOR_ACCENT, 255, True, COLOR_ACCENT)
    else
      DrawStyledRect(DE_PAD_X + 12, ToolbarY + 10, 110, 24,
        COLOR_PRIMARY, 255, True, COLOR_PRIMARY_VARIANT);
    DrawText2D(0, DE_PAD_X + 28, ToolbarY + 13, '+ Add Text',
      COLOR_ON_PRIMARY, 255, 0.65);

    // === Mode selector: [Move XYZ] [Rotate XYZ] [Scale] ===
    // Сразу после Add Text. Меняет глобальный GizmoMode — при выбранной
    // карточке гизмо в кабине переключается на нужный режим.
    DrawText2D(0, DE_PAD_X + 134, ToolbarY + 13,
      'Gizmo:', COLOR_BORDER_LIGHT, 220, 0.55);

    // Кнопка Move (gmTranslate)
    if (GizmoMode = gmTranslate) then
      DrawStyledRect(DE_PAD_X + 184, ToolbarY + 10, 90, 24,
        COLOR_PRIMARY, 255, True, COLOR_PRIMARY_VARIANT)
    else if DevEditorHoveredHotspot = DE_HOT_MODE_T then
      DrawStyledRect(DE_PAD_X + 184, ToolbarY + 10, 90, 24,
        COLOR_ACCENT, 255, True, COLOR_ACCENT)
    else
      DrawStyledRect(DE_PAD_X + 184, ToolbarY + 10, 90, 24,
        COLOR_SURFACE_VARIANT, 255, True, COLOR_BORDER);
    DrawText2D(0, DE_PAD_X + 200, ToolbarY + 13,
      'Move XYZ', COLOR_ON_PRIMARY, 255, 0.6);

    // Кнопка Rotate (gmRotate)
    if (GizmoMode = gmRotate) then
      DrawStyledRect(DE_PAD_X + 278, ToolbarY + 10, 100, 24,
        COLOR_PRIMARY, 255, True, COLOR_PRIMARY_VARIANT)
    else if DevEditorHoveredHotspot = DE_HOT_MODE_R then
      DrawStyledRect(DE_PAD_X + 278, ToolbarY + 10, 100, 24,
        COLOR_ACCENT, 255, True, COLOR_ACCENT)
    else
      DrawStyledRect(DE_PAD_X + 278, ToolbarY + 10, 100, 24,
        COLOR_SURFACE_VARIANT, 255, True, COLOR_BORDER);
    DrawText2D(0, DE_PAD_X + 290, ToolbarY + 13,
      'Rotate XYZ', COLOR_ON_PRIMARY, 255, 0.6);

    // Кнопка Scale (gmScale)
    if (GizmoMode = gmScale) then
      DrawStyledRect(DE_PAD_X + 382, ToolbarY + 10, 70, 24,
        COLOR_PRIMARY, 255, True, COLOR_PRIMARY_VARIANT)
    else if DevEditorHoveredHotspot = DE_HOT_MODE_S then
      DrawStyledRect(DE_PAD_X + 382, ToolbarY + 10, 70, 24,
        COLOR_ACCENT, 255, True, COLOR_ACCENT)
    else
      DrawStyledRect(DE_PAD_X + 382, ToolbarY + 10, 70, 24,
        COLOR_SURFACE_VARIANT, 255, True, COLOR_BORDER);
    DrawText2D(0, DE_PAD_X + 401, ToolbarY + 13,
      'Scale', COLOR_ON_PRIMARY, 255, 0.6);

    // Counter (сдвинут вправо за mode-кнопки).
    if (CustomTextSelectedIdx >= 0) and (CustomTextSelectedIdx < Length(CustomTexts)) then
      ValStr := '#' + IntToStr(CustomTextSelectedIdx)
    else
      ValStr := '—';
    DrawText2D(0, DE_PAD_X + 470, ToolbarY + 13,
      'Total: ' + IntToStr(Length(CustomTexts)) +
      '   |   Sel: ' + ValStr,
      COLOR_ON_SURFACE, 220, 0.6);

    // Подсказка справа — короткая, чтобы не упиралась в кнопки.
    DrawText2D(0, ScrW - DE_PAD_X - 280, ToolbarY + 13,
      'Click row to select  |  Wheel = scroll  |  Esc = cancel gizmo drag',
      COLOR_BORDER_LIGHT, 200, 0.55);

    // === LIST AREA ===
    ListY := ToolbarY + DE_TOOLBAR_H + 6;
    ListBottom := ScrH - DE_PAD_Y;
    DevEditorViewportTop := ListY;
    DevEditorViewportBottom := ListBottom;

    // Фон списка
    DrawRectangle2D(DE_PAD_X, ListY, ScrW - 2 * DE_PAD_X, ListBottom - ListY,
      COLOR_BACKGROUND, 200, True);
    DrawRectangle2D(DE_PAD_X, ListY, ScrW - 2 * DE_PAD_X, ListBottom - ListY,
      COLOR_BORDER, 200, False);

    CardX := DE_PAD_X + 8;
    CardW := ScrW - 2 * DE_PAD_X - 16 - DE_SCROLLBAR_W - 4;
    CardInnerW := CardW;

    // Высота всего контента и ограничение скролла
    ContentH := Length(CustomTexts) * (DE_CARD_H + DE_CARD_GAP);
    if ContentH > (ListBottom - ListY - 8) then
      DevEditorMaxScroll := ContentH - (ListBottom - ListY - 8)
    else
      DevEditorMaxScroll := 0;
    if DevEditorScrollY > DevEditorMaxScroll then DevEditorScrollY := DevEditorMaxScroll;
    if DevEditorScrollY < 0 then DevEditorScrollY := 0;

    // Карточки
    for i := 0 to Length(CustomTexts) - 1 do
    begin
      CardY := ListY + 6 + i * (DE_CARD_H + DE_CARD_GAP) - DevEditorScrollY;
      // Off-screen — скип.
      if (CardY + DE_CARD_H < ListY) or (CardY > ListBottom) then Continue;

      T := CustomTexts[i];
      IsSel := (i = CustomTextSelectedIdx);
      IsHover := (i = DevEditorHoveredCard);

      if IsSel then
      begin
        CardBg := COLOR_PRIMARY_VARIANT;
        CardBorder := COLOR_ACCENT;
      end
      else if IsHover then
      begin
        CardBg := COLOR_SURFACE_VARIANT;
        CardBorder := COLOR_BORDER_LIGHT;
      end
      else
      begin
        CardBg := COLOR_SURFACE;
        CardBorder := COLOR_BORDER;
      end;
      DrawRectangle2D(CardX, CardY, CardInnerW, DE_CARD_H, CardBg, 240, True);
      DrawRectangle2D(CardX, CardY, CardInnerW, DE_CARD_H, CardBorder, 240, False);

      // Все интерактивные элементы — на горизонтальной полосе CardY+30, h=22.
      // Над ними — column-labels (X/Y/Z/...) на высоте CardY+13 (на 3 px выше).

      // Index label (#N) — отцентрирован вертикально по полосе controls (-3 px).
      DrawText2D(0, CardX + 10, CardY + 30,
        '#' + IntToStr(i), COLOR_ACCENT, 255, 0.7);

      // "Source" заголовок над combobox'ом — поднят на 3 px.
      DEDrawHdrLabel(CardX + DE_IDX_W + DE_SOURCE_W div 2, CardY + 13,
        'Source', 240);

      // Source combobox — высота совпадает с кнопками, центр совпадает с кнопками.
      DrawStyledRect(CardX + DE_IDX_W, CardY + 30, DE_SOURCE_W, DE_BTN_SMALL,
        COLOR_SURFACE_VARIANT, 240, True, COLOR_BORDER);
      DrawText2D(0, CardX + DE_IDX_W + 8, CardY + 30,
        KlubSourceName(T.Source), COLOR_ON_SURFACE, 240, 0.6);
      DrawText2D(0, CardX + DE_IDX_W + DE_SOURCE_W - 14, CardY + 30,
        'v', COLOR_BORDER_LIGHT, 240, 0.6);
      // Под combobox'ом — текущее живое значение источника (-3 px).
      DrawText2D(0, CardX + DE_IDX_W + 8, CardY + 53,
        '= ' + GetKlubSourceValue(T.Source), COLOR_ACCENT, 220, 0.55);

      // Семь -/value/+ ячеек
      CellX := CardX + DE_IDX_W + DE_SOURCE_W + 12;
      // X
      HotMinus := IsHover and (DevEditorHoveredHotspot = DE_HOT_X_MINUS);
      HotPlus  := IsHover and (DevEditorHoveredHotspot = DE_HOT_X_PLUS);
      ValStr := FormatGizmoFloat(T.X, 3);
      DEDrawValueCell(CellX, CardY + 30, 'X', ValStr, 240, HotMinus, HotPlus);
      Inc(CellX, DE_CELL_W);
      // Y
      HotMinus := IsHover and (DevEditorHoveredHotspot = DE_HOT_Y_MINUS);
      HotPlus  := IsHover and (DevEditorHoveredHotspot = DE_HOT_Y_PLUS);
      ValStr := FormatGizmoFloat(T.Y, 3);
      DEDrawValueCell(CellX, CardY + 30, 'Y', ValStr, 240, HotMinus, HotPlus);
      Inc(CellX, DE_CELL_W);
      // Z
      HotMinus := IsHover and (DevEditorHoveredHotspot = DE_HOT_Z_MINUS);
      HotPlus  := IsHover and (DevEditorHoveredHotspot = DE_HOT_Z_PLUS);
      ValStr := FormatGizmoFloat(T.Z, 3);
      DEDrawValueCell(CellX, CardY + 30, 'Z', ValStr, 240, HotMinus, HotPlus);
      Inc(CellX, DE_CELL_W);
      // RX
      HotMinus := IsHover and (DevEditorHoveredHotspot = DE_HOT_RX_MINUS);
      HotPlus  := IsHover and (DevEditorHoveredHotspot = DE_HOT_RX_PLUS);
      ValStr := FormatGizmoFloat(T.RX, 1);
      DEDrawValueCell(CellX, CardY + 30, 'RX', ValStr, 240, HotMinus, HotPlus);
      Inc(CellX, DE_CELL_W);
      // RY
      HotMinus := IsHover and (DevEditorHoveredHotspot = DE_HOT_RY_MINUS);
      HotPlus  := IsHover and (DevEditorHoveredHotspot = DE_HOT_RY_PLUS);
      ValStr := FormatGizmoFloat(T.RY, 1);
      DEDrawValueCell(CellX, CardY + 30, 'RY', ValStr, 240, HotMinus, HotPlus);
      Inc(CellX, DE_CELL_W);
      // RZ
      HotMinus := IsHover and (DevEditorHoveredHotspot = DE_HOT_RZ_MINUS);
      HotPlus  := IsHover and (DevEditorHoveredHotspot = DE_HOT_RZ_PLUS);
      ValStr := FormatGizmoFloat(T.RZ, 1);
      DEDrawValueCell(CellX, CardY + 30, 'RZ', ValStr, 240, HotMinus, HotPlus);
      Inc(CellX, DE_CELL_W);
      // Scale
      HotMinus := IsHover and (DevEditorHoveredHotspot = DE_HOT_S_MINUS);
      HotPlus  := IsHover and (DevEditorHoveredHotspot = DE_HOT_S_PLUS);
      ValStr := FormatGizmoFloat(T.Scale, 4);
      DEDrawValueCell(CellX, CardY + 30, 'Scale', ValStr, 240, HotMinus, HotPlus);
      Inc(CellX, DE_CELL_W);

      // Color swatch — на той же горизонтальной полосе.
      DEDrawHdrLabel(CellX + (DE_SWATCH_W - 8) div 2, CardY + 13, 'Color', 240);
      DrawStyledRect(CellX, CardY + 30, DE_SWATCH_W - 8, DE_BTN_SMALL,
        Integer(T.Color), 255, True, COLOR_BORDER_LIGHT);
      if IsHover and (DevEditorHoveredHotspot = DE_HOT_COLOR) then
        DrawRectangle2D(CellX, CardY + 30, DE_SWATCH_W - 8, DE_BTN_SMALL,
          COLOR_ACCENT, 255, False);
      Inc(CellX, DE_SWATCH_W);

      // Visible checkbox
      DEDrawHdrLabel(CellX + DE_BTN_SMALL div 2, CardY + 13, 'Vis', 240);
      if T.Visible then
        DrawStyledRect(CellX, CardY + 30, DE_BTN_SMALL, DE_BTN_SMALL,
          COLOR_PRIMARY, 255, True, COLOR_PRIMARY_VARIANT)
      else
        DrawStyledRect(CellX, CardY + 30, DE_BTN_SMALL, DE_BTN_SMALL,
          COLOR_SURFACE_VARIANT, 255, True, COLOR_BORDER);
      if T.Visible then
        DrawText2D(0, CellX + (DE_BTN_SMALL - 6) div 2, CardY + 30,
          'V', COLOR_ON_PRIMARY, 255, 0.6);
      if IsHover and (DevEditorHoveredHotspot = DE_HOT_VISIBLE) then
        DrawRectangle2D(CellX, CardY + 30, DE_BTN_SMALL, DE_BTN_SMALL,
          COLOR_ACCENT, 255, False);
      Inc(CellX, DE_VISIBLE_W);

      // Delete button [×]
      DEDrawHdrLabel(CellX + (DE_DELETE_W - 4) div 2, CardY + 13, 'Del', 240);
      if IsHover and (DevEditorHoveredHotspot = DE_HOT_DELETE) then
        DrawStyledRect(CellX, CardY + 30, DE_DELETE_W - 4, DE_BTN_SMALL,
          COLOR_WARNING, 255, True, COLOR_WARNING)
      else
        DrawStyledRect(CellX, CardY + 30, DE_DELETE_W - 4, DE_BTN_SMALL,
          COLOR_SURFACE_VARIANT, 255, True, COLOR_BORDER);
      DrawText2D(0, CellX + (DE_DELETE_W - 4 - 7) div 2, CardY + 30,
        'X', COLOR_ON_SURFACE, 255, 0.65);
    end;

    // === SCROLLBAR ===
    if DevEditorMaxScroll > 0 then
    begin
      ScrollbarY := ListY + 4;
      ScrollbarH := ListBottom - ListY - 8;
      DrawRectangle2D(ScrW - DE_PAD_X - 4 - DE_SCROLLBAR_W, ScrollbarY,
        DE_SCROLLBAR_W, ScrollbarH, COLOR_SURFACE, 220, True);
      // Ползунок
      k := Round(ScrollbarH * (DevEditorScrollY / (DevEditorScrollY + ScrollbarH * 0.5)));
      // Высота ползунка пропорциональна видимой части.
      ScrollbarH := Round((ListBottom - ListY - 8) *
        (ListBottom - ListY - 8) / (ContentH));
      if ScrollbarH < 24 then ScrollbarH := 24;
      k := Round(DevEditorScrollY *
        ((ListBottom - ListY - 8) - ScrollbarH) / DevEditorMaxScroll);
      DrawRectangle2D(ScrW - DE_PAD_X - 4 - DE_SCROLLBAR_W,
        ListY + 4 + k, DE_SCROLLBAR_W, ScrollbarH,
        COLOR_PRIMARY, 240, True);
    end;

    // === COMBOBOX DROPDOWN OVERLAY ===
    if DevEditorComboboxOpen and
       (DevEditorComboboxCardIdx >= 0) and
       (DevEditorComboboxCardIdx < Length(CustomTexts)) then
    begin
      Idx := DevEditorComboboxCardIdx;
      CardY := ListY + 6 + Idx * (DE_CARD_H + DE_CARD_GAP) - DevEditorScrollY;
      CmbX := CardX + DE_IDX_W;
      // Дропдаун выпадает СРАЗУ под combobox'ом (combobox: CardY+30, h=DE_BTN_SMALL).
      CmbY := CardY + 30 + DE_BTN_SMALL + 2;
      CmbW := DE_SOURCE_W;
      CmbH := KLUB_SOURCE_COUNT * DE_COMBO_DROP_H + 4;
      // Если выпадает за низ — рисуем над карточкой.
      if CmbY + CmbH > ScrH - DE_PAD_Y then
        CmbY := CardY - CmbH - 2;

      DrawRectangle2D(CmbX, CmbY, CmbW, CmbH, COLOR_SURFACE, 245, True);
      DrawRectangle2D(CmbX, CmbY, CmbW, CmbH, COLOR_ACCENT, 255, False);
      for k := 0 to KLUB_SOURCE_COUNT - 1 do
      begin
        DrawText2D(0, CmbX + 8, CmbY + 4 + k * DE_COMBO_DROP_H + 4,
          KlubSourceName(TKlubSource(k)),
          COLOR_ON_SURFACE, 240, 0.55);
        if Integer(CustomTexts[Idx].Source) = k then
          DrawText2D(0, CmbX + CmbW - 18, CmbY + 4 + k * DE_COMBO_DROP_H + 4,
            'V', COLOR_ACCENT, 240, 0.55);
      end;
    end;

    // === COLOR PICKER OVERLAY ===
    if DevEditorColorPickerOpen and
       (DevEditorColorPickerCardIdx >= 0) and
       (DevEditorColorPickerCardIdx < Length(CustomTexts)) then
    begin
      Idx := DevEditorColorPickerCardIdx;
      CardY := ListY + 6 + Idx * (DE_CARD_H + DE_CARD_GAP) - DevEditorScrollY;
      // Расположение palette: справа от swatch'а. Считаем X swatch'а.
      CellX := CardX + DE_IDX_W + DE_SOURCE_W + 8 + 7 * DE_CELL_W;
      PkrW := DE_PICKER_COLS * (DE_PICKER_SIZE + 2) + 8;
      PkrH := DE_PICKER_ROWS * (DE_PICKER_SIZE + 2) + 8;
      PkrX := CellX + DE_SWATCH_W - PkrW;
      if PkrX < DE_PAD_X then PkrX := DE_PAD_X;
      PkrY := CardY + DE_CARD_H + 4;
      if PkrY + PkrH > ScrH - DE_PAD_Y then PkrY := CardY - PkrH - 4;

      DrawRectangle2D(PkrX, PkrY, PkrW, PkrH, COLOR_SURFACE, 245, True);
      DrawRectangle2D(PkrX, PkrY, PkrW, PkrH, COLOR_ACCENT, 255, False);
      for Row := 0 to DE_PICKER_ROWS - 1 do
        for Col := 0 to DE_PICKER_COLS - 1 do
        begin
          k := Row * DE_PICKER_COLS + Col;
          if k >= 18 then Break;
          PaletteX := PkrX + 4 + Col * (DE_PICKER_SIZE + 2);
          PaletteY := PkrY + 4 + Row * (DE_PICKER_SIZE + 2);
          DrawRectangle2D(PaletteX, PaletteY, DE_PICKER_SIZE, DE_PICKER_SIZE,
            Integer(DevEditorPalette[k]), 255, True);
          if DevEditorPalette[k] = CustomTexts[Idx].Color then
            DrawRectangle2D(PaletteX, PaletteY, DE_PICKER_SIZE, DE_PICKER_SIZE,
              COLOR_ACCENT, 255, False);
        end;
    end;
  finally
    End2D;
  end;
end;

// True если точка попадает в любой UI-элемент Dev Editor'а (для гейтинга гизмо).
function DevEditorPointInUI(MX, MY: Integer): Boolean;
begin
  Result := DevEditorVisible and
    (MX >= 0) and (MX <= InitResX) and (MY >= 0) and (MY <= InitResY);
end;

procedure HandleDevEditorClick(MX, MY: Integer);
var
  ScrW, ScrH: Integer;
  HeaderY, ToolbarY, ListY, ListBottom: Integer;
  CardX, CardY, CardW: Integer;
  i, k: Integer;
  HitCard, HitHotspot: Integer;
  CmbX, CmbY, CmbW, CmbH: Integer;
  PkrX, PkrY, PkrW, PkrH: Integer;
  CellX: Integer;
  Col, Row: Integer;
  Idx: Integer;
  PaletteX, PaletteY: Integer;
  ScrollbarX, ScrollbarTop, ScrollbarHandleH, ScrollbarHandleY: Integer;
  ContentH, VisibleH: Integer;
begin
  if not DevEditorVisible then Exit;
  ScrW := InitResX;
  ScrH := InitResY;
  HeaderY := DE_PAD_Y;
  ToolbarY := HeaderY + DE_HEADER_H + 6;
  ListY := ToolbarY + DE_TOOLBAR_H + 6;
  ListBottom := ScrH - DE_PAD_Y;
  CardX := DE_PAD_X + 8;
  CardW := ScrW - 2 * DE_PAD_X - 16 - DE_SCROLLBAR_W - 4;

  // 1) Открытый combobox — клик на пункт или вне → закрыть.
  if DevEditorComboboxOpen and
     (DevEditorComboboxCardIdx >= 0) and
     (DevEditorComboboxCardIdx < Length(CustomTexts)) then
  begin
    Idx := DevEditorComboboxCardIdx;
    CardY := ListY + 6 + Idx * (DE_CARD_H + DE_CARD_GAP) - DevEditorScrollY;
    CmbX := CardX + DE_IDX_W;
    CmbY := CardY + 30 + DE_BTN_SMALL + 2;
    CmbW := DE_SOURCE_W;
    CmbH := KLUB_SOURCE_COUNT * DE_COMBO_DROP_H + 4;
    if CmbY + CmbH > ScrH - DE_PAD_Y then CmbY := CardY - CmbH - 2;
    if DEInRect(MX, MY, CmbX, CmbY, CmbW, CmbH) then
    begin
      k := (MY - CmbY - 4) div DE_COMBO_DROP_H;
      if (k >= 0) and (k < KLUB_SOURCE_COUNT) then
      begin
        CustomTexts[Idx].Source := TKlubSource(k);
        SaveConfig;
      end;
    end;
    DevEditorComboboxOpen := False;
    DevEditorComboboxCardIdx := -1;
    Exit;
  end;

  // 2) Открытый color picker — клик на пункт или вне → закрыть.
  if DevEditorColorPickerOpen and
     (DevEditorColorPickerCardIdx >= 0) and
     (DevEditorColorPickerCardIdx < Length(CustomTexts)) then
  begin
    Idx := DevEditorColorPickerCardIdx;
    CardY := ListY + 6 + Idx * (DE_CARD_H + DE_CARD_GAP) - DevEditorScrollY;
    CellX := CardX + DE_IDX_W + DE_SOURCE_W + 8 + 7 * DE_CELL_W;
    PkrW := DE_PICKER_COLS * (DE_PICKER_SIZE + 2) + 8;
    PkrH := DE_PICKER_ROWS * (DE_PICKER_SIZE + 2) + 8;
    PkrX := CellX + DE_SWATCH_W - PkrW;
    if PkrX < DE_PAD_X then PkrX := DE_PAD_X;
    PkrY := CardY + DE_CARD_H + 4;
    if PkrY + PkrH > ScrH - DE_PAD_Y then PkrY := CardY - PkrH - 4;
    if DEInRect(MX, MY, PkrX, PkrY, PkrW, PkrH) then
    begin
      for Row := 0 to DE_PICKER_ROWS - 1 do
        for Col := 0 to DE_PICKER_COLS - 1 do
        begin
          k := Row * DE_PICKER_COLS + Col;
          if k >= 18 then Break;
          PaletteX := PkrX + 4 + Col * (DE_PICKER_SIZE + 2);
          PaletteY := PkrY + 4 + Row * (DE_PICKER_SIZE + 2);
          if DEInRect(MX, MY, PaletteX, PaletteY, DE_PICKER_SIZE, DE_PICKER_SIZE) then
          begin
            CustomTexts[Idx].Color := DevEditorPalette[k];
            SaveConfig;
            Break;
          end;
        end;
    end;
    DevEditorColorPickerOpen := False;
    DevEditorColorPickerCardIdx := -1;
    Exit;
  end;

  // 3) Header buttons
  if DEInRect(MX, MY, DE_PAD_X + 12, HeaderY + 12, 110, DE_HEADER_H - 24) then
  begin
    DevEditorVisible := False;
    DevEditorScrollY := 0;
    Exit;
  end;

  // 4) Toolbar — Add Text
  if DEInRect(MX, MY, DE_PAD_X + 12, ToolbarY + 10, 110, 24) then
  begin
    AddCustomText;
    SaveConfig;
    Exit;
  end;

  // 4b) Toolbar — Mode selector (Move / Rotate / Scale)
  if DEInRect(MX, MY, DE_PAD_X + 184, ToolbarY + 10, 90, 24) then
  begin
    GizmoMode := gmTranslate;
    SaveConfig;
    Exit;
  end;
  if DEInRect(MX, MY, DE_PAD_X + 278, ToolbarY + 10, 100, 24) then
  begin
    GizmoMode := gmRotate;
    SaveConfig;
    Exit;
  end;
  if DEInRect(MX, MY, DE_PAD_X + 382, ToolbarY + 10, 70, 24) then
  begin
    GizmoMode := gmScale;
    SaveConfig;
    Exit;
  end;

  // 5) Scrollbar drag
  if DevEditorMaxScroll > 0 then
  begin
    ScrollbarX := ScrW - DE_PAD_X - 4 - DE_SCROLLBAR_W;
    VisibleH := ListBottom - ListY - 8;
    ContentH := Length(CustomTexts) * (DE_CARD_H + DE_CARD_GAP);
    if ContentH < VisibleH then ContentH := VisibleH;
    ScrollbarHandleH := Round(VisibleH * VisibleH / ContentH);
    if ScrollbarHandleH < 24 then ScrollbarHandleH := 24;
    ScrollbarHandleY := ListY + 4 + Round(DevEditorScrollY *
      (VisibleH - ScrollbarHandleH) / DevEditorMaxScroll);
    ScrollbarTop := ListY + 4;
    if DEInRect(MX, MY, ScrollbarX, ScrollbarHandleY, DE_SCROLLBAR_W, ScrollbarHandleH) then
    begin
      DevEditorScrollbarDragging := True;
      DevEditorScrollbarDragOffsetY := MY - ScrollbarHandleY;
      Exit;
    end;
    // Клик в пустую часть полосы — page-scroll.
    if DEInRect(MX, MY, ScrollbarX, ScrollbarTop, DE_SCROLLBAR_W, VisibleH) then
    begin
      if MY < ScrollbarHandleY then DevEditorScrollY := DevEditorScrollY - VisibleH div 2
      else DevEditorScrollY := DevEditorScrollY + VisibleH div 2;
      if DevEditorScrollY < 0 then DevEditorScrollY := 0;
      if DevEditorScrollY > DevEditorMaxScroll then DevEditorScrollY := DevEditorMaxScroll;
      Exit;
    end;
  end;

  // 6) Карточки — селект + хотспоты
  if (MY < ListY) or (MY > ListBottom) then Exit;
  HitCard := -1;
  HitHotspot := DE_HOT_NONE;
  for i := 0 to Length(CustomTexts) - 1 do
  begin
    CardY := ListY + 6 + i * (DE_CARD_H + DE_CARD_GAP) - DevEditorScrollY;
    if (CardY + DE_CARD_H < ListY) or (CardY > ListBottom) then Continue;
    if DEInRect(MX, MY, CardX, CardY, CardW, DE_CARD_H) then
    begin
      HitCard := i;
      HitHotspot := DEHotspotAtPoint(MX, MY, CardX, CardY, CardW);
      Break;
    end;
  end;
  if HitCard < 0 then Exit;

  // Любой клик внутри карточки выделяет её, плюс выполняет hotspot-действие.
  CustomTextSelectedIdx := HitCard;
  SyncSlidersFromSelected;

  case HitHotspot of
    DE_HOT_SOURCE:
    begin
      DevEditorComboboxOpen := True;
      DevEditorComboboxCardIdx := HitCard;
    end;
    DE_HOT_X_MINUS, DE_HOT_X_PLUS,
    DE_HOT_Y_MINUS, DE_HOT_Y_PLUS,
    DE_HOT_Z_MINUS, DE_HOT_Z_PLUS,
    DE_HOT_RX_MINUS, DE_HOT_RX_PLUS,
    DE_HOT_RY_MINUS, DE_HOT_RY_PLUS,
    DE_HOT_RZ_MINUS, DE_HOT_RZ_PLUS,
    DE_HOT_S_MINUS, DE_HOT_S_PLUS:
    begin
      DEApplyStep(HitCard, HitHotspot, DEHotspotSign(HitHotspot));
      // Запускаем auto-repeat — продолжается, пока ЛКМ зажата.
      DevEditorBtnHeldHotspot := HitHotspot;
      DevEditorBtnHeldCardIdx := HitCard;
      DevEditorBtnHeldStartTime := GetTickCount;
      DevEditorBtnLastFireTime := DevEditorBtnHeldStartTime;
      SaveConfig;
    end;
    DE_HOT_COLOR:
    begin
      DevEditorColorPickerOpen := True;
      DevEditorColorPickerCardIdx := HitCard;
    end;
    DE_HOT_VISIBLE:
    begin
      CustomTexts[HitCard].Visible := not CustomTexts[HitCard].Visible;
      SaveConfig;
    end;
    DE_HOT_DELETE:
    begin
      CustomTextSelectedIdx := HitCard;
      DeleteSelectedCustomText;
      SaveConfig;
    end;
  end;
end;

procedure HandleDevEditorMouseUp;
begin
  DevEditorScrollbarDragging := False;
  DevEditorBtnHeldHotspot := DE_HOT_NONE;
  DevEditorBtnHeldCardIdx := -1;
end;

// Per-frame обновление: hover state, scrollbar drag, auto-repeat, mouse wheel.
procedure UpdateDevEditorPerFrame;
var
  MX, MY: Integer;
  i, ScrW, ScrH: Integer;
  ListY, ListBottom: Integer;
  CardX, CardY, CardW, ToolbarY, HeaderY: Integer;
  WheelDelta: Integer;
  TickNow: Cardinal;
  VisibleH, ScrollbarHandleH, NewHandleY: Integer;
  ContentH: Integer;
begin
  if not DevEditorVisible then
  begin
    DevEditorHoveredCard := -1;
    DevEditorHoveredHotspot := DE_HOT_NONE;
    Exit;
  end;
  MX := Round(MoveXcoord);
  MY := Round(MoveYcoord);
  ScrW := InitResX;
  ScrH := InitResY;
  HeaderY := DE_PAD_Y;
  ToolbarY := HeaderY + DE_HEADER_H + 6;
  ListY := ToolbarY + DE_TOOLBAR_H + 6;
  ListBottom := ScrH - DE_PAD_Y;
  CardX := DE_PAD_X + 8;
  CardW := ScrW - 2 * DE_PAD_X - 16 - DE_SCROLLBAR_W - 4;

  // Hover на header кнопке Back
  if DEInRect(MX, MY, DE_PAD_X + 12, HeaderY + 12, 110, DE_HEADER_H - 24) then
  begin
    DevEditorHoveredHotspot := DE_HOT_BACK;
    DevEditorHoveredCard := -1;
  end
  // Hover на Add Text
  else if DEInRect(MX, MY, DE_PAD_X + 12, ToolbarY + 10, 110, 24) then
  begin
    DevEditorHoveredHotspot := DE_HOT_ADD;
    DevEditorHoveredCard := -1;
  end
  // Hover на Mode-кнопки
  else if DEInRect(MX, MY, DE_PAD_X + 184, ToolbarY + 10, 90, 24) then
  begin
    DevEditorHoveredHotspot := DE_HOT_MODE_T;
    DevEditorHoveredCard := -1;
  end
  else if DEInRect(MX, MY, DE_PAD_X + 278, ToolbarY + 10, 100, 24) then
  begin
    DevEditorHoveredHotspot := DE_HOT_MODE_R;
    DevEditorHoveredCard := -1;
  end
  else if DEInRect(MX, MY, DE_PAD_X + 382, ToolbarY + 10, 70, 24) then
  begin
    DevEditorHoveredHotspot := DE_HOT_MODE_S;
    DevEditorHoveredCard := -1;
  end
  else
  begin
    // Hover на карточках
    DevEditorHoveredCard := -1;
    DevEditorHoveredHotspot := DE_HOT_NONE;
    if (MY >= ListY) and (MY <= ListBottom) then
    begin
      for i := 0 to Length(CustomTexts) - 1 do
      begin
        CardY := ListY + 6 + i * (DE_CARD_H + DE_CARD_GAP) - DevEditorScrollY;
        if (CardY + DE_CARD_H < ListY) or (CardY > ListBottom) then Continue;
        if DEInRect(MX, MY, CardX, CardY, CardW, DE_CARD_H) then
        begin
          DevEditorHoveredCard := i;
          DevEditorHoveredHotspot := DEHotspotAtPoint(MX, MY, CardX, CardY, CardW);
          Break;
        end;
      end;
    end;
  end;

  // Mouse wheel — прокрутка списка.
  WheelDelta := MouseWheelDelta;
  if (WheelDelta <> 0) and (DevEditorMaxScroll > 0) then
  begin
    DevEditorScrollY := DevEditorScrollY - WheelDelta * (DE_CARD_H + DE_CARD_GAP);
    if DevEditorScrollY < 0 then DevEditorScrollY := 0;
    if DevEditorScrollY > DevEditorMaxScroll then DevEditorScrollY := DevEditorMaxScroll;
  end;

  // Drag-скроллбара
  if DevEditorScrollbarDragging and ((GetAsyncKeyState(VK_LBUTTON) and $8000) <> 0) then
  begin
    if DevEditorMaxScroll > 0 then
    begin
      VisibleH := ListBottom - ListY - 8;
      ContentH := Length(CustomTexts) * (DE_CARD_H + DE_CARD_GAP);
      if ContentH < VisibleH then ContentH := VisibleH;
      ScrollbarHandleH := Round(VisibleH * VisibleH / ContentH);
      if ScrollbarHandleH < 24 then ScrollbarHandleH := 24;
      NewHandleY := MY - DevEditorScrollbarDragOffsetY - (ListY + 4);
      if NewHandleY < 0 then NewHandleY := 0;
      if NewHandleY > VisibleH - ScrollbarHandleH then NewHandleY := VisibleH - ScrollbarHandleH;
      DevEditorScrollY := Round(NewHandleY * DevEditorMaxScroll /
        (VisibleH - ScrollbarHandleH));
    end;
  end
  else
    DevEditorScrollbarDragging := False;

  // Auto-repeat для +/- кнопок
  if DevEditorBtnHeldHotspot <> DE_HOT_NONE then
  begin
    if (GetAsyncKeyState(VK_LBUTTON) and $8000) = 0 then
    begin
      DevEditorBtnHeldHotspot := DE_HOT_NONE;
      DevEditorBtnHeldCardIdx := -1;
    end
    else
    begin
      TickNow := GetTickCount;
      if TickNow - DevEditorBtnHeldStartTime > DE_AUTO_REPEAT_DELAY then
      begin
        if TickNow - DevEditorBtnLastFireTime > DE_AUTO_REPEAT_RATE then
        begin
          DEApplyStep(DevEditorBtnHeldCardIdx, DevEditorBtnHeldHotspot,
            DEHotspotSign(DevEditorBtnHeldHotspot));
          DevEditorBtnLastFireTime := TickNow;
        end;
      end;
    end;
  end;
end;

end.
