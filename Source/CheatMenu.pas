unit CheatMenu;

interface
uses 
  Windows, SysUtils, Classes, Variables, DrawFunc2D, DrawFunc3D, EngineUtils, ShellAPI, Math;

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
    
    // Locomotive
    NewClubPositions: Boolean;
  end;

procedure InitCheatMenu; stdcall;
procedure DrawCheatMenu; stdcall;
procedure HandleMenuClick(X, Y: Integer); stdcall;
procedure HandleMenuHover(X, Y: Integer); stdcall;
procedure HandleMenuMouseUp; stdcall;
procedure ToggleMenu; stdcall;

function GetFreecamBasespeed: Single; stdcall;
function GetFreecamFastspeed: Single; stdcall;
function GetFreecamTurnspeed: Single; stdcall;

procedure LoadConfig; stdcall;

implementation

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
    ClubFixesText: string;
    
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
      ClubFixesText: 'Исправления БИЛ-В';
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
      InfoText: 'ZDS-Booster v1.1 | vk.com/raildriver';
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
      ClubFixesText: 'Виправлення БІЛ-В';
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
      InfoText: 'ZDS-Booster v1.1 | t.me/raildrive';
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
      ClubFixesText: 'BIL-V Fixes';
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
      InfoText: 'ZDS-Booster v1.1 | t.me/raildrive';
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
  else if TextType = 'ClubFixesText' then Result := LanguageTexts[CurrentLanguage].ClubFixesText
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
begin
  if Value >= 1000 then
    Result := IntToStr(Round(Value))
  else if Value = Round(Value) then
    Result := IntToStr(Round(Value))
  else
    Result := Format('%.2f', [Value]);
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
        if Key = 'new_view_angle' then Settings.NewViewAngle := (Value = '1');
        if Key = 'camera_sensitivity' then Settings.CameraSensitivity := (Value = '1');

        // Чекбоксы дальности
        if Key = 'show_wires' then Settings.ShowWires := (Value = '1');
        if Key = 'show_distant_models' then Settings.ShowDistantModels := (Value = '1');
        if Key = 'show_traffic_lights' then Settings.ShowTrafficLights := (Value = '1');
        
        // Значения слайдеров
        if Key = 'basespeed' then Settings.BasespeedSlider.Value := StrToFloatDef(Value, 1.0);
        if Key = 'fastspeed' then Settings.FastspeedSlider.Value := StrToFloatDef(Value, 2.0);
        if Key = 'turnspeed' then Settings.TurnspeedSlider.Value := StrToFloatDef(Value, 1.5);
        if Key = 'stepforward' then Settings.StepForwardSlider.Value := StrToFloatDef(Value, 0.5);
        if Key = 'maxvisibledistance' then Settings.MaxVisibleDistanceSlider.Value := StrToFloatDef(Value, 1200);
        if Key = 'view_angle' then Settings.ViewAngleSlider.Value := StrToFloatDef(Value, 3.0);
        if Key = 'camera_sensitivity_value' then Settings.CameraSensitivitySlider.Value := StrToFloatDef(Value, 5.0);
        
        // Глобальный слайдер яркости
        if Key = 'brightness' then Settings.BrightnessSlider.Value := StrToFloatDef(Value, 0.0);
      end;
    end;
    CloseFile(F);

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
    if Settings.NewViewAngle then WriteLn(F, 'new_view_angle: 1') else WriteLn(F, 'new_view_angle: 0');
    if Settings.CameraSensitivity then WriteLn(F, 'camera_sensitivity: 1') else WriteLn(F, 'camera_sensitivity: 0');
    
    // Чекбоксы дальности
    if Settings.ShowWires then WriteLn(F, 'show_wires: 1') else WriteLn(F, 'show_wires: 0');
    if Settings.ShowDistantModels then WriteLn(F, 'show_distant_models: 1') else WriteLn(F, 'show_distant_models: 0');
    if Settings.ShowTrafficLights then WriteLn(F, 'show_traffic_lights: 1') else WriteLn(F, 'show_traffic_lights: 0');
    
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

    CloseFile(F);
  except
    // Игнорируем ошибки записи конфига
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
  
  // === АНИМАЦИЯ ОБЩЕГО СОСТОЯНИЯ МЕНЮ ===
  if Abs(MenuAnimationProgress - MenuTargetProgress) > 0.01 then
    MenuAnimationProgress := MenuAnimationProgress + (MenuTargetProgress - MenuAnimationProgress) * MenuAnimationSpeed * DeltaTime;
  
  // Вычисляем центр экрана для анимации
  CenterX := InitResX div 2;
  CenterY := InitResY div 2;
  
  // Анимация окон с трансформацией
  if Abs(RenderWindow.Alpha - RenderWindow.TargetAlpha) > 0.01 then
    RenderWindow.Alpha := RenderWindow.Alpha + (RenderWindow.TargetAlpha - RenderWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(WorldWindow.Alpha - WorldWindow.TargetAlpha) > 0.01 then
    WorldWindow.Alpha := WorldWindow.Alpha + (WorldWindow.TargetAlpha - WorldWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(LocomotiveWindow.Alpha - LocomotiveWindow.TargetAlpha) > 0.01 then
    LocomotiveWindow.Alpha := LocomotiveWindow.Alpha + (LocomotiveWindow.TargetAlpha - LocomotiveWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(MenuWindow.Alpha - MenuWindow.TargetAlpha) > 0.01 then
    MenuWindow.Alpha := MenuWindow.Alpha + (MenuWindow.TargetAlpha - MenuWindow.Alpha) * 5.0 * DeltaTime;
  
  // === АНИМАЦИЯ МАСШТАБА И ПОЗИЦИИ ОКОН ===
  with RenderWindow do
  begin
    if Abs(Scale - TargetScale) > 0.01 then
      Scale := Scale + (TargetScale - Scale) * MenuAnimationSpeed * DeltaTime;
    // Интерполяция позиции от центра к оригинальной позиции
    if not IsDragging then
    begin
      X := Round(CenterX + (OriginalX - CenterX) * MenuAnimationProgress);
      Y := Round(CenterY + (OriginalY - CenterY) * MenuAnimationProgress);
    end;
  end;
  
  with WorldWindow do
  begin
    if Abs(Scale - TargetScale) > 0.01 then
      Scale := Scale + (TargetScale - Scale) * MenuAnimationSpeed * DeltaTime;
    if not IsDragging then
    begin
      X := Round(CenterX + (OriginalX - CenterX) * MenuAnimationProgress);
      Y := Round(CenterY + (OriginalY - CenterY) * MenuAnimationProgress);
    end;
  end;
  
  with LocomotiveWindow do
  begin
    if Abs(Scale - TargetScale) > 0.01 then
      Scale := Scale + (TargetScale - Scale) * MenuAnimationSpeed * DeltaTime;
    if not IsDragging then
    begin
      X := Round(CenterX + (OriginalX - CenterX) * MenuAnimationProgress);
      Y := Round(CenterY + (OriginalY - CenterY) * MenuAnimationProgress);
    end;
  end;
  
  with MenuWindow do
  begin
    if Abs(Scale - TargetScale) > 0.01 then
      Scale := Scale + (TargetScale - Scale) * MenuAnimationSpeed * DeltaTime;
    if not IsDragging then
    begin
      X := Round(CenterX + (OriginalX - CenterX) * MenuAnimationProgress);
      Y := Round(CenterY + (OriginalY - CenterY) * MenuAnimationProgress);
    end;
  end;
  
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
  end;
end;


procedure InitCheatMenu; stdcall;
begin
  FillChar(Settings, SizeOf(Settings), 0);
  
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
  WorldWindow.Height := 100;
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
  
  // === ПРИМЕНЯЕМ МАСШТАБИРОВАНИЕ ===
  ScaledWidth := Round(Win.Width * Win.Scale);
  ScaledHeight := Round(Win.Height * Win.Scale);
  
  // Вычисляем смещение для центрирования масштабированного окна
  OffsetX := (Win.Width - ScaledWidth) div 2;
  OffsetY := (Win.Height - ScaledHeight) div 2;
  
  ScaledX := Win.X + OffsetX;
  ScaledY := Win.Y + OffsetY;
  
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
    end;
    
    1: // WORLD окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT;
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(180 * Settings.MaxVisibleDistanceSection.AnimProgress) + MARGIN;
      
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN;
    end;
    
    2: // LOCOMOTIVE окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN;
    end;
    
    3: // MENU окно
    begin
      TotalHeight := 220;
    end;
  end;
  
  Win.Height := TotalHeight;
  ScaledHeight := Round(Win.Height * Win.Scale);
  OffsetY := (Win.Height - ScaledHeight) div 2;
  ScaledY := Win.Y + OffsetY;
  
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
      
      // Freecam секция
      if Settings.FreecamSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(150 * Settings.FreecamSection.AnimProgress * Win.Scale);
        
        // Фон секции
        DrawStyledRect(ScaledX + Round((MARGIN + 12) * Win.Scale), ContentY, Round(240 * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);
        
        if SectionHeight > Round(40 * Win.Scale) then
        begin
          DrawModernSlider(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(25 * Win.Scale), Settings.BasespeedSlider, GetText('BaseSpeedText'), Alpha, Win.Scale);
          DrawModernSlider(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(75 * Win.Scale), Settings.FastspeedSlider, GetText('FastSpeedText'), Alpha, Win.Scale);
        end;
        
        Inc(ContentY, SectionHeight + Round(MARGIN * Win.Scale));
      end;
      
      // Main Camera
      ExpandButtonX := ScaledX + Round(220 * Win.Scale);
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('MainCameraText'), Settings.MainCamera, Alpha, True, ExpandButtonX, Settings.MainCameraSection.Expanded, Settings.MainCameraSection.AnimProgress);
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));
      
      // Main Camera секция
      if Settings.MainCameraSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(200 * Settings.MainCameraSection.AnimProgress * Win.Scale);
        
        // Фон секции
        DrawStyledRect(ScaledX + Round((MARGIN + 12) * Win.Scale), ContentY, Round(240 * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);
        
        if SectionHeight > Round(40 * Win.Scale) then
          DrawModernSlider(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(25 * Win.Scale), Settings.StepForwardSlider, GetText('StepForwardText'), Alpha, Win.Scale);
        
        if SectionHeight > Round(70 * Win.Scale) then
          DrawModernCheckbox(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(60 * Win.Scale), GetText('NewZoomText'), Settings.NewViewAngle, Alpha);
        
        if (SectionHeight > Round(110 * Win.Scale)) and Settings.NewViewAngle then
          DrawModernSlider(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(95 * Win.Scale), Settings.ViewAngleSlider, GetText('ValueText'), Alpha, Win.Scale);
        
        if SectionHeight > Round(140 * Win.Scale) then
          DrawModernCheckbox(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(130 * Win.Scale), GetText('SensitivityText'), Settings.CameraSensitivity, Alpha);
        
        if (SectionHeight > Round(170 * Win.Scale)) and Settings.CameraSensitivity then
          DrawModernSlider(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(165 * Win.Scale), Settings.CameraSensitivitySlider, GetText('ValueText'), Alpha, Win.Scale);
        
        Inc(ContentY, SectionHeight + Round(MARGIN * Win.Scale));
      end;
    end;
    
    1: // WORLD окно
    begin
      // Max Visible Distance
      ExpandButtonX := ScaledX + Round(220 * Win.Scale);
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('MaxDistanceText'), Settings.MaxVisibleDistance, Alpha, True, ExpandButtonX, Settings.MaxVisibleDistanceSection.Expanded, Settings.MaxVisibleDistanceSection.AnimProgress);
      Inc(ContentY, Round((ITEM_HEIGHT + MARGIN) * Win.Scale));
      
      // Max Visible Distance секция
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(180 * Settings.MaxVisibleDistanceSection.AnimProgress * Win.Scale);
        
        // Фон секции
        DrawStyledRect(ScaledX + Round((MARGIN + 12) * Win.Scale), ContentY, Round(240 * Win.Scale), SectionHeight, COLOR_SURFACE, Alpha, True, COLOR_BORDER);
        
        if SectionHeight > Round(40 * Win.Scale) then
        begin
          DrawModernSlider(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(25 * Win.Scale), Settings.MaxVisibleDistanceSlider, GetText('DistanceText'), Alpha, Win.Scale);
          
          if SectionHeight > Round(70 * Win.Scale) then
          begin
            DrawModernCheckbox(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(60 * Win.Scale), GetText('WiresText'), Settings.ShowWires, Alpha);
            DrawModernCheckbox(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(90 * Win.Scale), GetText('DistantModelsText'), Settings.ShowDistantModels, Alpha);
          end;
          if SectionHeight > Round(120 * Win.Scale) then
            DrawModernCheckbox(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(120 * Win.Scale), GetText('TrafficLightsText'), Settings.ShowTrafficLights, Alpha);
        end;
        
        Inc(ContentY, SectionHeight + Round(MARGIN * Win.Scale));
      end;
      
      // New Sky
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('NewSkyText'), Settings.NewSky, Alpha);
    end;
    
    2: // LOCOMOTIVE окно
    begin
      // Исправления КЛУБ
      DrawModernToggle(ScaledX + Round(MARGIN * Win.Scale), ContentY, GetText('ClubFixesText'), Settings.NewClubPositions, Alpha);
    end;
    
    3: // MENU окно
    begin
      // Заголовок языка
      DrawModernText(ScaledX + Round((MARGIN + 24) * Win.Scale), ContentY + Round(15 * Win.Scale), GetText('LanguageText'), COLOR_ON_SURFACE, Alpha, 0.85);
      Inc(ContentY, Round(50 * Win.Scale));
      
      // Кнопки языков в ряд
      DrawModernLanguageButton(ScaledX + Round(MARGIN * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale), GetText('RussianText'), Alpha, CurrentLanguage = langRussian);
      DrawModernLanguageButton(ScaledX + Round((MARGIN + 100) * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale), GetText('UkrainianText'), Alpha, CurrentLanguage = langUkrainian);
      DrawModernLanguageButton(ScaledX + Round((MARGIN + 200) * Win.Scale), ContentY, Round(90 * Win.Scale), Round(35 * Win.Scale), GetText('EnglishText'), Alpha, CurrentLanguage = langEnglish);
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
begin
  if not MenuVisible then Exit;
  
  LoadConfigThrottled;
  
  UpdateAnimations;

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

// Оптимизированная функция перетаскивания слайдера
procedure HandleSliderDrag(X: Integer; var Slider: TSlider; SliderX: Integer);
var
  NewProgress: Single;
  OldValue: Single;
begin
  if not Slider.IsDragging then Exit;
  
  OldValue := Slider.Value;
  
  NewProgress := (X - SliderX) / SLIDER_WIDTH;
  if NewProgress < 0 then NewProgress := 0;
  if NewProgress > 1 then NewProgress := 1;
  
  Slider.Value := Slider.MinValue + NewProgress * (Slider.MaxValue - Slider.MinValue);
  
  if Abs(Slider.Value - OldValue) > 0.001 then
  begin
    // Синхронизация с глобальными переменными
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

    SaveConfig;
  
    if (@Slider <> @Settings.BrightnessSlider) then
      ApplySettingsThrottled;
  end;
end;

procedure HandleMenuHover(X, Y: Integer); stdcall;
begin
  if not MenuVisible then Exit;
  
  // Обработка драггинга окон с анимацией расширения
  if RenderWindow.IsDragging then
  begin
    RenderWindow.X := X - RenderWindow.DragOffsetX;
    RenderWindow.Y := Y - RenderWindow.DragOffsetY;
    RenderWindow.OriginalX := RenderWindow.X;
    RenderWindow.OriginalY := RenderWindow.Y;
    // Анимация расширения при перетаскивании
    RenderWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    RenderWindow.TargetShadowIntensity := 1.5;
  end
  else
  begin
    RenderWindow.TargetDragWidthExpansion := 0.0;
    RenderWindow.TargetShadowIntensity := 0.8;
  end;
  
  if WorldWindow.IsDragging then
  begin
    WorldWindow.X := X - WorldWindow.DragOffsetX;
    WorldWindow.Y := Y - WorldWindow.DragOffsetY;
    WorldWindow.OriginalX := WorldWindow.X;
    WorldWindow.OriginalY := WorldWindow.Y;
    WorldWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    WorldWindow.TargetShadowIntensity := 1.5;
  end
  else
  begin
    WorldWindow.TargetDragWidthExpansion := 0.0;
    WorldWindow.TargetShadowIntensity := 0.8;
  end;
  
  if LocomotiveWindow.IsDragging then
  begin
    LocomotiveWindow.X := X - LocomotiveWindow.DragOffsetX;
    LocomotiveWindow.Y := Y - LocomotiveWindow.DragOffsetY;
    LocomotiveWindow.OriginalX := LocomotiveWindow.X;
    LocomotiveWindow.OriginalY := LocomotiveWindow.Y;
    LocomotiveWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    LocomotiveWindow.TargetShadowIntensity := 1.5;
  end
  else
  begin
    LocomotiveWindow.TargetDragWidthExpansion := 0.0;
    LocomotiveWindow.TargetShadowIntensity := 0.8;
  end;
  
  if MenuWindow.IsDragging then
  begin
    MenuWindow.X := X - MenuWindow.DragOffsetX;
    MenuWindow.Y := Y - MenuWindow.DragOffsetY;
    MenuWindow.OriginalX := MenuWindow.X;
    MenuWindow.OriginalY := MenuWindow.Y;
    MenuWindow.TargetDragWidthExpansion := DRAG_EXPANSION;
    MenuWindow.TargetShadowIntensity := 1.5;
  end
  else
  begin
    MenuWindow.TargetDragWidthExpansion := 0.0;
    MenuWindow.TargetShadowIntensity := 0.8;
  end;
  
  // Обработка драггинга слайдеров
  if Settings.BrightnessSlider.IsDragging then
    HandleSliderDrag(X, Settings.BrightnessSlider, RenderWindow.X + MARGIN + 24);
  if Settings.BasespeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.BasespeedSlider, RenderWindow.X + MARGIN + 24);
  if Settings.FastspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.FastspeedSlider, RenderWindow.X + MARGIN + 24);
  if Settings.TurnspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.TurnspeedSlider, RenderWindow.X + MARGIN + 24);
  if Settings.StepForwardSlider.IsDragging then
    HandleSliderDrag(X, Settings.StepForwardSlider, RenderWindow.X + MARGIN + 24);
  if Settings.MaxVisibleDistanceSlider.IsDragging then
    HandleSliderDrag(X, Settings.MaxVisibleDistanceSlider, WorldWindow.X + MARGIN + 24);
  if Settings.ViewAngleSlider.IsDragging then
    HandleSliderDrag(X, Settings.ViewAngleSlider, RenderWindow.X + MARGIN + 24);
  if Settings.CameraSensitivitySlider.IsDragging then
    HandleSliderDrag(X, Settings.CameraSensitivitySlider, RenderWindow.X + MARGIN + 24);
end;

procedure HandleMenuClick(X, Y: Integer); stdcall;
var
  ContentY: Integer;
  FreecamSectionY, MainCameraSectionY, MaxVisibleDistanceSectionY: Integer;
  SectionHeight: Integer;
begin
  if not MenuVisible then Exit;
  
  if (RenderWindow.Alpha < 0.1) and (WorldWindow.Alpha < 0.1) and (LocomotiveWindow.Alpha < 0.1) and (MenuWindow.Alpha < 0.1) then Exit;
  
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
        if InRect(X, Y, RenderWindow.X + MARGIN + 24, FreecamSectionY + 15, SLIDER_WIDTH + 30, 30) then
        begin
          Settings.BasespeedSlider.IsDragging := True;
          Exit;
        end;
        if InRect(X, Y, RenderWindow.X + MARGIN + 24, FreecamSectionY + 65, SLIDER_WIDTH + 30, 30) then
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
      if (SectionHeight > 40) and InRect(X, Y, RenderWindow.X + MARGIN + 24, MainCameraSectionY + 15, SLIDER_WIDTH + 30, 30) then
      begin
        Settings.StepForwardSlider.IsDragging := True;
        Exit;
      end;
      
      // Клик по галочке "Новый угол обзора"
      if (SectionHeight > 70) and InRect(X, Y, RenderWindow.X + MARGIN + 24, MainCameraSectionY + 60, 200, 20) then
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
      if (SectionHeight > 110) and Settings.NewViewAngle and InRect(X, Y, RenderWindow.X + MARGIN + 24, MainCameraSectionY + 85, SLIDER_WIDTH + 30, 30) then
      begin
        Settings.ViewAngleSlider.IsDragging := True;
        Exit;
      end;
      
      // Галочка чувствительности камеры
      if (SectionHeight > 140) and InRect(X, Y, RenderWindow.X + MARGIN + 24, MainCameraSectionY + 130, 200, 20) then
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
      if (SectionHeight > 170) and Settings.CameraSensitivity and InRect(X, Y, RenderWindow.X + MARGIN + 24, MainCameraSectionY + 155, SLIDER_WIDTH + 30, 30) then
      begin
        Settings.CameraSensitivitySlider.IsDragging := True;
        Exit;
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
        if InRect(X, Y, WorldWindow.X + MARGIN + 24, MaxVisibleDistanceSectionY + 15, SLIDER_WIDTH + 30, 30) then
        begin
          Settings.MaxVisibleDistanceSlider.IsDragging := True;
          Exit;
        end;
        
        // Чекбоксы
        if (SectionHeight > 70) and InRect(X, Y, WorldWindow.X + MARGIN + 24, MaxVisibleDistanceSectionY + 60, 200, 20) then
        begin
          Settings.ShowWires := not Settings.ShowWires;
          if Settings.MaxVisibleDistance then 
            ApplyDistanceSettings;
          SaveConfig;
          Exit;
        end;
        if (SectionHeight > 100) and InRect(X, Y, WorldWindow.X + MARGIN + 24, MaxVisibleDistanceSectionY + 90, 200, 20) then
        begin
          Settings.ShowDistantModels := not Settings.ShowDistantModels;
          if Settings.MaxVisibleDistance then 
            ApplyDistanceSettings;
          SaveConfig;
          Exit;
        end;
        if (SectionHeight > 130) and InRect(X, Y, WorldWindow.X + MARGIN + 24, MaxVisibleDistanceSectionY + 120, 200, 20) then
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
  end;
end;

// === ОПТИМИЗИРОВАННАЯ ФУНКЦИЯ ОТПУСКАНИЯ МЫШИ ===
procedure HandleMenuMouseUp; stdcall;
begin
  if not MenuVisible then Exit;
  
  // Останавливаем драггинг окон
  RenderWindow.IsDragging := False;
  WorldWindow.IsDragging := False;
  LocomotiveWindow.IsDragging := False;
  MenuWindow.IsDragging := False;
  
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
  
  // Останавливаем драггинг слайдеров
  Settings.BrightnessSlider.IsDragging := False;
  Settings.BasespeedSlider.IsDragging := False;
  Settings.FastspeedSlider.IsDragging := False;
  Settings.TurnspeedSlider.IsDragging := False;
  Settings.StepForwardSlider.IsDragging := False;
  Settings.MaxVisibleDistanceSlider.IsDragging := False;
  Settings.ViewAngleSlider.IsDragging := False;
  Settings.CameraSensitivitySlider.IsDragging := False;

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

end.
