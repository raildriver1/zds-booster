unit CheatMenu;

interface
uses 
  Windows, SysUtils, Classes, Variables, DrawFunc2D, DrawFunc3D, EngineUtils, ShellAPI;

type
  TSlider = record
    Value: Single;
    MinValue: Single;
    MaxValue: Single;
    IsDragging: Boolean;
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
  RenderWindow, WorldWindow, LocomotiveWindow, MenuWindow: TWindow;  // ПЕРЕИМЕНОВАЛИ DonateWindow в MenuWindow
  LastFrameTime: Cardinal = 0;
  
  // === ПЕРЕМЕННЫЕ ДЛЯ ЯЗЫКА ===
  CurrentLanguage: TLanguage = langRussian;
  
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
  ITEM_HEIGHT = 28;
  MARGIN = 12;
  HEADER_HEIGHT = 35;
  SLIDER_WIDTH = 190;
  BUTTON_SIZE = 20;
  CHECKBOX_SIZE = 16;

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
      RenderTitle: 'РЕНДЕР';
      WorldTitle: 'МИР';
      LocomotiveTitle: 'ЛОКОМОТИВ';
      MenuTitle: 'МЕНЮ';
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
      LanguageText: 'Язык:';
      RussianText: 'Русский';
      UkrainianText: 'Украинский';
      EnglishText: 'Английский';
      InfoText: 'ZDS-Booster v1.1 | vk.com/raildriver';
    ),
    // Украинский
    (
      RenderTitle: 'РЕНДЕР';
      WorldTitle: 'СВІТ';
      LocomotiveTitle: 'ЛОКОМОТИВ';
      MenuTitle: 'МЕНЮ';
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
      LanguageText: 'Мова:';
      RussianText: 'Російська';
      UkrainianText: 'Українська';
      EnglishText: 'Англійська';
      InfoText: 'ZDS-Booster v1.1 | vk.com/raildriver';
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
      LanguageText: 'Language:';
      RussianText: 'Russian';
      UkrainianText: 'Ukrainian';
      EnglishText: 'English';
      InfoText: 'ZDS-Booster v1.1 | vk.com/raildriver';
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
  else Result := TextType; // Возвращаем оригинал если не найден
end;

// === ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ===
function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
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

// Простая отрисовка текста
procedure DrawText(X, Y: Integer; Text: string; Color: Integer; Alpha: Integer = 255);
begin
  DrawText2D(0, X + 1, Y + 1, Text, $000000, Alpha div 3, 0.8);
  DrawText2D(0, X, Y, Text, Color, Alpha, 0.8);
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

procedure UpdateAnimations;
var
  CurrentTime: Cardinal;
  DeltaTime: Single;
begin
  CurrentTime := GetTickCount;
  if LastFrameTime = 0 then LastFrameTime := CurrentTime;
  
  DeltaTime := (CurrentTime - LastFrameTime) / 1000.0;
  if DeltaTime > 0.1 then DeltaTime := 0.1;
  LastFrameTime := CurrentTime;
  
  if PendingApply and ((CurrentTime - LastApplyTime) >= ApplyInterval) then
    ApplySettingsThrottled;
  
  // Анимация окон
  if Abs(RenderWindow.Alpha - RenderWindow.TargetAlpha) > 0.01 then
    RenderWindow.Alpha := RenderWindow.Alpha + (RenderWindow.TargetAlpha - RenderWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(WorldWindow.Alpha - WorldWindow.TargetAlpha) > 0.01 then
    WorldWindow.Alpha := WorldWindow.Alpha + (WorldWindow.TargetAlpha - WorldWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(LocomotiveWindow.Alpha - LocomotiveWindow.TargetAlpha) > 0.01 then
    LocomotiveWindow.Alpha := LocomotiveWindow.Alpha + (LocomotiveWindow.TargetAlpha - LocomotiveWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(MenuWindow.Alpha - MenuWindow.TargetAlpha) > 0.01 then
    MenuWindow.Alpha := MenuWindow.Alpha + (MenuWindow.TargetAlpha - MenuWindow.Alpha) * 5.0 * DeltaTime;
  
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
  
  // Инициализация слайдеров FREECAM
  Settings.BasespeedSlider.Value := 0.01;
  Settings.BasespeedSlider.MinValue := 0.01;
  Settings.BasespeedSlider.MaxValue := 1.00;
  
  Settings.FastspeedSlider.Value := 1.5;
  Settings.FastspeedSlider.MinValue := 0.01;
  Settings.FastspeedSlider.MaxValue := 2.0;
  
  Settings.TurnspeedSlider.Value := 1.5;
  Settings.TurnspeedSlider.MinValue := 0.01;
  Settings.TurnspeedSlider.MaxValue := 2.0;
  
  Settings.StepForwardSlider.Value := 0.5;
  Settings.StepForwardSlider.MinValue := 0.01;
  Settings.StepForwardSlider.MaxValue := 1.0;
  
  Settings.MaxVisibleDistanceSlider.Value := 1200;
  Settings.MaxVisibleDistanceSlider.MinValue := 800;
  Settings.MaxVisibleDistanceSlider.MaxValue := 1600;
  
  // Слайдер угла обзора
  Settings.ViewAngleSlider.Value := 3.0;
  Settings.ViewAngleSlider.MinValue := 0.0;
  Settings.ViewAngleSlider.MaxValue := 7.0;
  
  // Слайдер чувствительности камеры
  Settings.CameraSensitivitySlider.Value := 5.0;
  Settings.CameraSensitivitySlider.MinValue := 1.0;
  Settings.CameraSensitivitySlider.MaxValue := 9.0;

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

  // Инициализация окон
  RenderWindow.Title := GetText('RenderTitle');
  RenderWindow.X := 50;
  RenderWindow.Y := 40;
  RenderWindow.Width := 240;
  RenderWindow.Height := 120;
  RenderWindow.Alpha := 0.0;
  RenderWindow.TargetAlpha := 1.0;
  
  WorldWindow.Title := GetText('WorldTitle');
  WorldWindow.X := 310;
  WorldWindow.Y := 40;
  WorldWindow.Width := 240;
  WorldWindow.Height := 100;
  WorldWindow.Alpha := 0.0;
  WorldWindow.TargetAlpha := 1.0;
  
  LocomotiveWindow.Title := GetText('LocomotiveTitle');
  LocomotiveWindow.X := 570;
  LocomotiveWindow.Y := 40;
  LocomotiveWindow.Width := 240;
  LocomotiveWindow.Height := 100;
  LocomotiveWindow.Alpha := 0.0;
  LocomotiveWindow.TargetAlpha := 1.0;
  
  // ОКНО МЕНЮ ВМЕСТО ДОНАТОВ
  MenuWindow.Title := GetText('MenuTitle');
  MenuWindow.X := 830;
  MenuWindow.Y := 40;
  MenuWindow.Width := 280;
  MenuWindow.Height := 180; // Уменьшили высоту для простого меню
  MenuWindow.Alpha := 0.0;
  MenuWindow.TargetAlpha := 1.0;
end;

procedure DrawExpandButton(X, Y: Integer; Expanded: Boolean; Alpha: Integer);
var
  CenterX, CenterY: Integer;
begin
  CenterX := X + BUTTON_SIZE div 2;
  CenterY := Y + BUTTON_SIZE div 2;
  
  DrawCircle2D_Fill(CenterX, CenterY, 8, $404040, Alpha);
  DrawCircle2D(CenterX, CenterY, 8, $CCCCCC, Alpha div 2);
  
  DrawLine2D(CenterX - 4, CenterY, CenterX + 4, CenterY, $CCCCCC, Alpha, 2.0);
  if not Expanded then
    DrawLine2D(CenterX, CenterY - 4, CenterX, CenterY + 4, $CCCCCC, Alpha, 2.0);
end;

procedure DrawCheckbox(X, Y: Integer; Text: string; Checked: Boolean; Alpha: Integer);
var
  BgColor, TextColor: Integer;
begin
  if Checked then
  begin
    BgColor := $0066FF;
    TextColor := $FFFFFF;
  end
  else
  begin
    BgColor := $2A2A2A;
    TextColor := $BBBBBB;
  end;
  
  DrawRectangle2D(X, Y, CHECKBOX_SIZE, CHECKBOX_SIZE, BgColor, Alpha, True);
  DrawRectangle2D(X, Y, CHECKBOX_SIZE, CHECKBOX_SIZE, $444444, Alpha, False);
  
  if Checked then
  begin
    DrawLine2D(X + 4, Y + 8, X + 7, Y + 11, $FFFFFF, Alpha, 2.0);
    DrawLine2D(X + 7, Y + 11, X + 12, Y + 5, $FFFFFF, Alpha, 2.0);
  end;
  
  DrawText(X + CHECKBOX_SIZE + 8, Y - 1, Text, TextColor, Alpha);
end;

procedure DrawSlider(X, Y: Integer; var Slider: TSlider; Text: string; Alpha: Integer);
var
  Progress: Single;
  SliderX: Integer;
  ValueText: string;
begin
  if Alpha <= 0 then Exit;
  
  Progress := (Slider.Value - Slider.MinValue) / (Slider.MaxValue - Slider.MinValue);
  SliderX := X + Round(Progress * SLIDER_WIDTH);
  
  ValueText := FormatValue(Slider.Value);
  
  DrawText2D(0, X, Y - 20, Text + ': ' + ValueText, $FFFFFF, Alpha, 0.55);
  
  // Трек
  DrawRectangle2D(X, Y + 8, SLIDER_WIDTH, 6, $1A1A1A, Alpha, True);
  
  // Активная часть
  if Progress > 0 then
    DrawRectangle2D(X, Y + 8, Round(Progress * SLIDER_WIDTH), 6, $0066FF, Alpha, True);
  
  // Ползунок
  DrawCircle2D_Fill(SliderX, Y + 11, 12, $0088FF, Alpha);
  DrawCircle2D_Fill(SliderX, Y + 11, 8, $00AAFF, Alpha);
end;

procedure DrawToggle(X, Y: Integer; Text: string; Enabled: Boolean; Alpha: Integer; HasExpandButton: Boolean = False; ExpandButtonX: Integer = 0; Expanded: Boolean = False);
var
  BgColor, TextColor: Integer;
begin
  if Enabled then
  begin
    BgColor := $0066FF;
    TextColor := $FFFFFF;
  end
  else
  begin
    BgColor := $2A2A2A;
    TextColor := $BBBBBB;
  end;
  
  DrawRectangle2D(X, Y, 220, ITEM_HEIGHT, BgColor, Alpha, True);
  DrawRectangle2D(X, Y, 220, ITEM_HEIGHT, $444444, Alpha, False);
  
  DrawText(X + 12, Y, Text, TextColor, Alpha);
  
  if HasExpandButton then
    DrawExpandButton(ExpandButtonX, Y + 4, Expanded, Alpha);
end;

// === НОВАЯ ФУНКЦИЯ: Отрисовка кнопки языка ===
procedure DrawLanguageButton(X, Y, Width, Height: Integer; Text: string; Alpha: Integer; Selected: Boolean = False);
var
  BgColor, TextColor: Integer;
  TextWidth, TextX: Integer;
begin
  if Selected then
  begin
    BgColor := $0066FF;
    TextColor := $FFFFFF;
  end
  else
  begin
    BgColor := $404040;
    TextColor := $CCCCCC;
  end;
  
  DrawRectangle2D(X, Y, Width, Height, BgColor, Alpha, True);
  DrawRectangle2D(X, Y, Width, Height, $666666, Alpha, False);
  
  // Центрируем текст
  TextWidth := Length(Text) * 7;
  TextX := X + (Width - TextWidth) div 2;
  
  DrawText(TextX, Y + (Height - 20) div 2, Text, TextColor, Alpha);
end;

procedure DrawWindow(var Win: TWindow; WindowType: Integer);
var
  Alpha: Integer;
  ContentY: Integer;
  SectionHeight: Integer;
  ExpandButtonX: Integer;
  TotalHeight: Integer;
begin
  Alpha := Round(Win.Alpha * 255);
  if Alpha <= 0 then Exit;
  
  // Вычисляем динамическую высоту окна в зависимости от типа
  TotalHeight := HEADER_HEIGHT + MARGIN * 3;
  
  case WindowType of
    0: // RENDER окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT;
      if Settings.FreecamSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(140 * Settings.FreecamSection.AnimProgress) + MARGIN;
      
      TotalHeight := TotalHeight + ITEM_HEIGHT;
      if Settings.MainCameraSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(180 * Settings.MainCameraSection.AnimProgress) + MARGIN;
    end;
    
    1: // WORLD окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT;
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(150 * Settings.MaxVisibleDistanceSection.AnimProgress) + MARGIN;
      
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN;
    end;
    
    2: // LOCOMOTIVE окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN;
    end;
    
    3: // MENU окно
    begin
      TotalHeight := 180; // Фиксированная высота для окна меню
    end;
  end;
  
  // Обновляем высоту окна
  Win.Height := TotalHeight;
  
  // Тень
  DrawRectangle2D(Win.X + 4, Win.Y + 4, Win.Width, Win.Height, $000000, Alpha div 4, True);
  
  // Заголовок
  DrawRectangle2D(Win.X, Win.Y, Win.Width, HEADER_HEIGHT, $404080, Alpha, True);
  DrawRectangle2D(Win.X, Win.Y, Win.Width, HEADER_HEIGHT, $6060A0, Alpha, False);
  DrawText(Win.X + 12, Win.Y + 11, Win.Title, $FFFFFF, Alpha);
  
  // Тело
  DrawRectangle2D(Win.X, Win.Y + HEADER_HEIGHT, Win.Width, Win.Height - HEADER_HEIGHT, $2A2A2A, Alpha, True);
  DrawRectangle2D(Win.X, Win.Y + HEADER_HEIGHT, Win.Width, Win.Height - HEADER_HEIGHT, $404040, Alpha, False);
  
  ContentY := Win.Y + HEADER_HEIGHT + MARGIN;
  
  case WindowType of
    0: // RENDER окно
    begin
      // Freecam
      ExpandButtonX := Win.X + 200;
      DrawToggle(Win.X + MARGIN, ContentY, GetText('FreeCameraText'), Settings.Freecam, Alpha, True, ExpandButtonX, Settings.FreecamSection.Expanded);
      Inc(ContentY, ITEM_HEIGHT + MARGIN);
      
      // Freecam секция
      if Settings.FreecamSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(140 * Settings.FreecamSection.AnimProgress);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $202020, Alpha, True);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $353535, Alpha, False);
        
        if SectionHeight > 30 then
        begin
          DrawSlider(Win.X + MARGIN + 20, ContentY + 20, Settings.BasespeedSlider, GetText('BaseSpeedText'), Alpha);
          DrawSlider(Win.X + MARGIN + 20, ContentY + 60, Settings.FastspeedSlider, GetText('FastSpeedText'), Alpha);
        end;
        
        Inc(ContentY, SectionHeight + MARGIN);
      end;
      
      // Main Camera
      ExpandButtonX := Win.X + 200;
      DrawToggle(Win.X + MARGIN, ContentY, GetText('MainCameraText'), Settings.MainCamera, Alpha, True, ExpandButtonX, Settings.MainCameraSection.Expanded);
      Inc(ContentY, ITEM_HEIGHT + MARGIN);
      
      // Main Camera секция
      if Settings.MainCameraSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(180 * Settings.MainCameraSection.AnimProgress);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $202020, Alpha, True);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $353535, Alpha, False);
        
        if SectionHeight > 30 then
          DrawSlider(Win.X + MARGIN + 20, ContentY + 20, Settings.StepForwardSlider, GetText('StepForwardText'), Alpha);
        
        if SectionHeight > 60 then
          DrawCheckbox(Win.X + MARGIN + 20, ContentY + 50, GetText('NewZoomText'), Settings.NewViewAngle, Alpha);
        
        if (SectionHeight > 90) and Settings.NewViewAngle then
          DrawSlider(Win.X + MARGIN + 20, ContentY + 80, Settings.ViewAngleSlider, GetText('ValueText'), Alpha);
        
        if SectionHeight > 120 then
          DrawCheckbox(Win.X + MARGIN + 20, ContentY + 110, GetText('SensitivityText'), Settings.CameraSensitivity, Alpha);
        
        if (SectionHeight > 150) and Settings.CameraSensitivity then
          DrawSlider(Win.X + MARGIN + 20, ContentY + 140, Settings.CameraSensitivitySlider, GetText('ValueText'), Alpha);
        
        Inc(ContentY, SectionHeight + MARGIN);
      end;
    end;
    
    1: // WORLD окно
    begin
      // Max Visible Distance
      ExpandButtonX := Win.X + 200;
      DrawToggle(Win.X + MARGIN, ContentY, GetText('MaxDistanceText'), Settings.MaxVisibleDistance, Alpha, True, ExpandButtonX, Settings.MaxVisibleDistanceSection.Expanded);
      Inc(ContentY, ITEM_HEIGHT + MARGIN);
      
      // Max Visible Distance секция
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(150 * Settings.MaxVisibleDistanceSection.AnimProgress);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $202020, Alpha, True);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $353535, Alpha, False);
        
        if SectionHeight > 30 then
        begin
          DrawSlider(Win.X + MARGIN + 20, ContentY + 20, Settings.MaxVisibleDistanceSlider, GetText('DistanceText'), Alpha);
          
          if SectionHeight > 60 then
          begin
            DrawCheckbox(Win.X + MARGIN + 20, ContentY + 50, GetText('WiresText'), Settings.ShowWires, Alpha);
            DrawCheckbox(Win.X + MARGIN + 20, ContentY + 75, GetText('DistantModelsText'), Settings.ShowDistantModels, Alpha);
          end;
          if SectionHeight > 100 then
            DrawCheckbox(Win.X + MARGIN + 20, ContentY + 100, GetText('TrafficLightsText'), Settings.ShowTrafficLights, Alpha);
        end;
        
        Inc(ContentY, SectionHeight + MARGIN);
      end;
      
      // New Sky
      DrawToggle(Win.X + MARGIN, ContentY, GetText('NewSkyText'), Settings.NewSky, Alpha);
    end;
    
    2: // LOCOMOTIVE окно
    begin
      // Исправления КЛУБ
      DrawToggle(Win.X + MARGIN, ContentY, GetText('ClubFixesText'), Settings.NewClubPositions, Alpha);
    end;
    
    3: // MENU окно
    begin
      // Заголовок языка
      DrawText(Win.X + MARGIN + 20, ContentY + 10, GetText('LanguageText'), $FFFFFF, Alpha);
      Inc(ContentY, 40);
      
      // Кнопки языков
      DrawLanguageButton(Win.X + MARGIN, ContentY, 80, 30, GetText('RussianText'), Alpha, CurrentLanguage = langRussian);
      DrawLanguageButton(Win.X + MARGIN + 90, ContentY, 80, 30, GetText('UkrainianText'), Alpha, CurrentLanguage = langUkrainian);
      DrawLanguageButton(Win.X + MARGIN + 180, ContentY, 80, 30, GetText('EnglishText'), Alpha, CurrentLanguage = langEnglish);
    end;
  end;
end;

procedure DrawInfoBar;
var
  Alpha, BarWidth, BarHeight, BarX, BarY: Integer;
  InfoText: string;
begin
  Alpha := Round(RenderWindow.Alpha * 255);
  if Alpha <= 0 then Exit;
  
  InfoText := GetText('InfoText');
  BarWidth := 320;
  BarHeight := 30;
  BarX := 10;
  BarY := InitResY - BarHeight - 10;
  
  // Тень
  DrawRectangle2D(BarX + 3, BarY + 3, BarWidth, BarHeight, $000000, Alpha div 4, True);
  
  // Основной прямоугольник в стиле заголовка окна
  DrawRectangle2D(BarX, BarY, BarWidth, BarHeight, $404080, Alpha, True);
  DrawRectangle2D(BarX, BarY, BarWidth, BarHeight, $6060A0, Alpha, False);
  
  // Текст чуть повыше
  DrawText(BarX + 8, BarY + 3, InfoText, $FFFFFF, Alpha);
end;

procedure DrawCheatMenu; stdcall;
var
  BackgroundAlpha: Integer;
begin
  if not MenuVisible then Exit;
  
  LoadConfigThrottled;
  
  UpdateAnimations;

  Begin2D;
  try
    // Затемнение фона
    if Settings.BrightnessSlider.Value > 0 then
    begin
      BackgroundAlpha := Round(Settings.BrightnessSlider.Value * RenderWindow.Alpha);
      DrawRectangle2D(0, 0, InitResX, InitResY, $000011, BackgroundAlpha, True);
    end;
    
    DrawWindow(RenderWindow, 0);      // RENDER окно
    DrawWindow(WorldWindow, 1);       // WORLD окно  
    DrawWindow(LocomotiveWindow, 2);  // LOCOMOTIVE окно
    DrawWindow(MenuWindow, 3);        // MENU окно
    DrawInfoBar;
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
  
  // Обработка драггинга окон
  if RenderWindow.IsDragging then
  begin
    RenderWindow.X := X - RenderWindow.DragOffsetX;
    RenderWindow.Y := Y - RenderWindow.DragOffsetY;
  end;
  
  if WorldWindow.IsDragging then
  begin
    WorldWindow.X := X - WorldWindow.DragOffsetX;
    WorldWindow.Y := Y - WorldWindow.DragOffsetY;
  end;
  
  if LocomotiveWindow.IsDragging then
  begin
    LocomotiveWindow.X := X - LocomotiveWindow.DragOffsetX;
    LocomotiveWindow.Y := Y - LocomotiveWindow.DragOffsetY;
  end;
  
  if MenuWindow.IsDragging then
  begin
    MenuWindow.X := X - MenuWindow.DragOffsetX;
    MenuWindow.Y := Y - MenuWindow.DragOffsetY;
  end;
  
  // Обработка драггинга слайдеров
  if Settings.BrightnessSlider.IsDragging then
    HandleSliderDrag(X, Settings.BrightnessSlider, RenderWindow.X + MARGIN + 20);
  if Settings.BasespeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.BasespeedSlider, RenderWindow.X + MARGIN + 20);
  if Settings.FastspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.FastspeedSlider, RenderWindow.X + MARGIN + 20);
  if Settings.TurnspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.TurnspeedSlider, RenderWindow.X + MARGIN + 20);
  if Settings.StepForwardSlider.IsDragging then
    HandleSliderDrag(X, Settings.StepForwardSlider, RenderWindow.X + MARGIN + 20);
  if Settings.MaxVisibleDistanceSlider.IsDragging then
    HandleSliderDrag(X, Settings.MaxVisibleDistanceSlider, WorldWindow.X + MARGIN + 20);
  if Settings.ViewAngleSlider.IsDragging then
    HandleSliderDrag(X, Settings.ViewAngleSlider, RenderWindow.X + MARGIN + 20);
  if Settings.CameraSensitivitySlider.IsDragging then
    HandleSliderDrag(X, Settings.CameraSensitivitySlider, RenderWindow.X + MARGIN + 20);
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
    Inc(ContentY, 40); // Пропускаем заголовок
    
    // Русский
    if InRect(X, Y, MenuWindow.X + MARGIN, ContentY, 80, 30) then
    begin
      CurrentLanguage := langRussian;
      SaveConfig;
      Exit;
    end;
    
    // Украинский
    if InRect(X, Y, MenuWindow.X + MARGIN + 90, ContentY, 80, 30) then
    begin
      CurrentLanguage := langUkrainian;
      SaveConfig;
      Exit;
    end;
    
    // Английский
    if InRect(X, Y, MenuWindow.X + MARGIN + 180, ContentY, 80, 30) then
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
    if InRect(X, Y, RenderWindow.X + 200, ContentY + 4, BUTTON_SIZE, BUTTON_SIZE) then
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
  
      // ИСПРАВЛЯЕМ: вызываем синхронизацию ПОСЛЕ сохранения
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
  
      // ДОБАВЛЯЕМ ПРИНУДИТЕЛЬНОЕ ОБНОВЛЕНИЕ
      LoadConfigForced; // Принудительно перечитываем конфиг
  
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);
    
    // Freecam sliders
    FreecamSectionY := ContentY;
    if Settings.FreecamSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(140 * Settings.FreecamSection.AnimProgress);
      if SectionHeight > 30 then
      begin
        if InRect(X, Y, RenderWindow.X + MARGIN + 20, FreecamSectionY + 10, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.BasespeedSlider.IsDragging := True;
          Exit;
        end;
        if InRect(X, Y, RenderWindow.X + MARGIN + 20, FreecamSectionY + 50, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.FastspeedSlider.IsDragging := True;
          Exit;
        end;
      end;
      Inc(ContentY, SectionHeight + MARGIN);
    end;
    
    // Main Camera expand button
    if InRect(X, Y, RenderWindow.X + 200, ContentY + 4, BUTTON_SIZE, BUTTON_SIZE) then
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
        // При выключении MainCamera - отключаем патч угла обзора и чувствительности
        if ViewAnglePatched then
          RemoveViewAnglePatch;
        RestoreCameraSensitivity;
      end;
      
      // При включении MainCamera - проверяем нужно ли включить патчи
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
    
    // Main Camera section (УВЕЛИЧЕНА СО 120 ДО 180)
    MainCameraSectionY := ContentY;
    if Settings.MainCameraSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(180 * Settings.MainCameraSection.AnimProgress);
      
      // Клик по слайдеру StepForward
      if (SectionHeight > 30) and InRect(X, Y, RenderWindow.X + MARGIN + 20, MainCameraSectionY + 10, SLIDER_WIDTH + 25, 40) then
      begin
        Settings.StepForwardSlider.IsDragging := True;
        Exit;
      end;
      
      // НОВАЯ ГАЛОЧКА - клик по галочке "Новый угол обзора"
      if (SectionHeight > 60) and InRect(X, Y, RenderWindow.X + MARGIN + 20, MainCameraSectionY + 50, 180, CHECKBOX_SIZE) then
      begin
        Settings.NewViewAngle := not Settings.NewViewAngle;
        
        // Применяем или убираем патч в зависимости от состояния MainCamera И NewViewAngle
        if Settings.MainCamera and Settings.NewViewAngle then
          ApplyViewAnglePatch
        else
          RemoveViewAnglePatch;
          
        SaveConfig;
        Exit;
      end;
      
      // НОВЫЙ СЛАЙДЕР - клик по слайдеру угла обзора
      if (SectionHeight > 90) and Settings.NewViewAngle and InRect(X, Y, RenderWindow.X + MARGIN + 20, MainCameraSectionY + 70, SLIDER_WIDTH + 25, 40) then
      begin
        Settings.ViewAngleSlider.IsDragging := True;
        Exit;
      end;
      
      // ГАЛОЧКА ЧУВСТВИТЕЛЬНОСТИ КАМЕРЫ
      if (SectionHeight > 120) and InRect(X, Y, RenderWindow.X + MARGIN + 20, MainCameraSectionY + 110, 180, CHECKBOX_SIZE) then
      begin
        Settings.CameraSensitivity := not Settings.CameraSensitivity;
        
        // Применяем или убираем чувствительность в зависимости от состояния MainCamera И CameraSensitivity
        if Settings.MainCamera and Settings.CameraSensitivity then
          ApplyCameraSensitivity
        else
          RestoreCameraSensitivity;
          
        SaveConfig;
        Exit;
      end;
      
      // СЛАЙДЕР ЧУВСТВИТЕЛЬНОСТИ КАМЕРЫ
      if (SectionHeight > 150) and Settings.CameraSensitivity and InRect(X, Y, RenderWindow.X + MARGIN + 20, MainCameraSectionY + 130, SLIDER_WIDTH + 25, 40) then
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
    if InRect(X, Y, WorldWindow.X + 200, ContentY + 4, BUTTON_SIZE, BUTTON_SIZE) then
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
        // При выключении восстанавливаем оригинальные значения
        RestoreOriginalDistanceValues;
      end;
      
      // Сначала сохраняем, потом синхронизируем
      SaveConfig;
      
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
      
      // Принудительно обновляем время последнего чтения конфига
      LastConfigReadTime := GetTickCount;
      
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);
    
    // Max Visible Distance секция
    MaxVisibleDistanceSectionY := ContentY;
    if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(150 * Settings.MaxVisibleDistanceSection.AnimProgress);
      if SectionHeight > 30 then
      begin
        // Слайдер дальности
        if InRect(X, Y, WorldWindow.X + MARGIN + 20, MaxVisibleDistanceSectionY + 10, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.MaxVisibleDistanceSlider.IsDragging := True;
          Exit;
        end;
        
        // Чекбоксы с исправленной логикой
        if (SectionHeight > 60) and InRect(X, Y, WorldWindow.X + MARGIN + 20, MaxVisibleDistanceSectionY + 50, 180, CHECKBOX_SIZE) then
        begin
          Settings.ShowWires := not Settings.ShowWires;
          // Применяем настройки только если главный переключатель включен
          if Settings.MaxVisibleDistance then 
            ApplyDistanceSettings;
          SaveConfig;
          Exit;
        end;
        if (SectionHeight > 85) and InRect(X, Y, WorldWindow.X + MARGIN + 20, MaxVisibleDistanceSectionY + 75, 180, CHECKBOX_SIZE) then
        begin
          Settings.ShowDistantModels := not Settings.ShowDistantModels;
          // Применяем настройки только если главный переключатель включен
          if Settings.MaxVisibleDistance then 
            ApplyDistanceSettings;
          SaveConfig;
          Exit;
        end;
        if (SectionHeight > 110) and InRect(X, Y, WorldWindow.X + MARGIN + 20, MaxVisibleDistanceSectionY + 100, 180, CHECKBOX_SIZE) then
        begin
          Settings.ShowTrafficLights := not Settings.ShowTrafficLights;
          // Применяем настройки только если главный переключатель включен
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
    
    // ПРЯМО "Исправления КЛУБ" (без секции)
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
  
  RenderWindow.IsDragging := False;
  WorldWindow.IsDragging := False;
  LocomotiveWindow.IsDragging := False;
  MenuWindow.IsDragging := False;
  
  Settings.BrightnessSlider.IsDragging := False;
  Settings.BasespeedSlider.IsDragging := False;
  Settings.FastspeedSlider.IsDragging := False;
  Settings.TurnspeedSlider.IsDragging := False;
  Settings.StepForwardSlider.IsDragging := False;
  Settings.MaxVisibleDistanceSlider.IsDragging := False;
  Settings.ViewAngleSlider.IsDragging := False;      // НОВЫЙ СЛАЙДЕР
  Settings.CameraSensitivitySlider.IsDragging := False; // СЛАЙДЕР ЧУВСТВИТЕЛЬНОСТИ

  // ПРИНУДИТЕЛЬНО применяем все настройки при отпускании мыши
  if Settings.MaxVisibleDistance then
    ApplyDistanceSettings;
  if Settings.MainCamera and Settings.CameraSensitivity then
    ApplyCameraSensitivity;
  
  // Конфиг уже сохранен в HandleSliderDrag, дублировать не нужно
end;

procedure ToggleMenu; stdcall;
begin
  MenuVisible := not MenuVisible;
  
  if MenuVisible then
  begin
    ShowCursor(True);
    
    // === ПРИНУДИТЕЛЬНО ЧИТАЕМ КОНФИГ ПРИ ОТКРЫТИИ МЕНЮ ===
    LoadConfigForced;
    
    RenderWindow.Alpha := 0.0;
    RenderWindow.TargetAlpha := 1.0;
    WorldWindow.Alpha := 0.0;
    WorldWindow.TargetAlpha := 1.0;
    LocomotiveWindow.Alpha := 0.0;
    LocomotiveWindow.TargetAlpha := 1.0;
    MenuWindow.Alpha := 0.0;
    MenuWindow.TargetAlpha := 1.0;
    
    // Патчим вызов при открытии меню
    ApplyMenuPatch;
  end
  else
  begin
    ShowCursor(False);
    RenderWindow.TargetAlpha := 0.0;
    WorldWindow.TargetAlpha := 0.0;
    LocomotiveWindow.TargetAlpha := 0.0;
    MenuWindow.TargetAlpha := 0.0;
    
    // Восстанавливаем вызов при закрытии меню
    RemoveMenuPatch;
  end;
end;

end.
