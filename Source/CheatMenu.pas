﻿//----------------------------------------------------------------------------//
//CheatMenu.pas - Простое и рабочее чит-меню (ИСПРАВЛЕНО)                   //
//----------------------------------------------------------------------------//
unit CheatMenu;

interface
uses 
  Windows, SysUtils, Variables, DrawFunc2D, DrawFunc3D, EngineUtils;

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
    
    // Освещение
    Lighting: Boolean;
    LightingSection: TExpandableSection;
    MainLightIntensitySlider: TSlider;      // 4942AC - Яркость рельс
    AdditionalLightIntensitySlider: TSlider; // 4942B4 - Контрастность рельс
    CabinBrightnessSlider: TSlider;         // 482824 - Яркость кабины
    CabinContrastSlider: TSlider;           // 48282C - Контрастность кабины
    SunOrbitRadiusSlider: TSlider;          // 4942CC
    SunHeightSlider: TSlider;               // 4942B8
    
    // World
    MaxVisibleDistance: Boolean;
    MaxVisibleDistanceSection: TExpandableSection;
    MaxVisibleDistanceSlider: TSlider;
    ShowWires: Boolean;        // Провода (494408 + 494414)
    ShowDistantModels: Boolean; // Дальние модели (494358)  
    ShowTrafficLights: Boolean; // Светофоры (48DB9C + 48DC1C + 48DBA0)
    
    NewSky: Boolean;
    
    // Locomotive - ПРЯМО исправления КЛУБ без секции
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

implementation

var
  MenuVisible: Boolean = False;
  Settings: TCheatSettings;
  RenderWindow, WorldWindow, LocomotiveWindow: TWindow;
  LastFrameTime: Cardinal = 0;
  
  // === ОПТИМИЗАЦИЯ: Ограничение частоты применения настроек ===
  LastApplyTime: Cardinal = 0;
  ApplyInterval: Cardinal = 20; // Применять настройки не чаще чем раз в 20ms (было 50ms)
  PendingApply: Boolean = False;
  
  // === ОПТИМИЗАЦИЯ: Ограничение частоты чтения конфига ===
  LastConfigReadTime: Cardinal = 0;
  ConfigReadInterval: Cardinal = 50; // Читать конфиг не чаще чем раз в 50ms
  
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
  MainLightAddr: Cardinal;        // 4942AC
  AdditionalLightAddr: Cardinal;  // 4942B4
  SunOrbitAddr: Cardinal;         // 4942CC
  SunHeightAddr: Cardinal;        // 4942B8
  
  // Оригинальные значения освещения
  OrigMainLightValue: Single;
  OrigAdditionalLightValue: Single;
  OrigSunOrbitValue: Single;
  OrigSunHeightValue: Single;
  LightingValuesRead: Boolean = False;
  
  // Адреса для дальности (3 группы)
  WireAddrs: array[0..1] of Cardinal;        // 494408, 494414
  DistantModelAddr: Cardinal;                // 494358
  TrafficLightAddrs: array[0..2] of Cardinal; // 48DB9C, 48DC1C, 48DBA0
  
  // Оригинальные значения дальности (ТЕПЕРЬ ВОССТАНАВЛИВАЮТСЯ!)
  OrigWireValues: array[0..1] of Single;
  OrigDistantModelValue: Single;
  OrigTrafficLightValues: array[0..2] of Single;
  DistanceValuesRead: Boolean = False;
  
 // ОРИГИНАЛЬНЫЕ ЗНАЧЕНИЯ (ИЗ ВАШЕГО ДАМПА)
  OrigSpeedXValue: array[0..3] of Byte = ($58, $39, $34, $BC);
  OrigAllowedSpeedValue: array[0..3] of Byte = ($58, $39, $34, $BC);
  OrigShuntingSpeedValue: array[0..3] of Byte = ($96, $43, $8B, $3D);
  OrigTrainSpeedValue: array[0..3] of Byte = ($96, $43, $8B, $3D);
  OrigTimeValue: array[0..3] of Byte = ($31, $08, $AC, $3C);
  OrigNumberAccelValue: array[0..3] of Byte = ($0A, $D7, $A3, $3C);
  OrigReverseValue: array[0..3] of Byte = ($0A, $D7, $A3, $BB);
  OrigAdditionalValue: array[0..3] of Byte = ($7F, $6A, $3C, $3D);
  OrigRadiusValue: array[0..9] of Byte = ($0A, $D7, $A3, $70, $3D, $0A, $D7, $A3, $F7, $BF);
  
  // НОВЫЕ ЗНАЧЕНИЯ ДЛЯ ЗАМЕНЫ (ВАШИ ТЕКУЩИЕ)
  NewSpeedXValue: array[0..3] of Byte = ($7B, $12, $83, $BB);
  NewAllowedSpeedValue: array[0..3] of Byte = ($7B, $12, $83, $BB);
  NewShuntingSpeedValue: array[0..3] of Byte = ($BD, $CA, $A1, $3D);
  NewTrainSpeedValue: array[0..3] of Byte = ($5A, $E3, $A5, $3D);
  NewTimeValue: array[0..3] of Byte = ($46, $60, $E5, $3C);
  NewNumberAccelValue: array[0..3] of Byte = ($91, $C2, $F5, $3C);
  NewReverseValue: array[0..3] of Byte = ($74, $12, $03, $3B);
  NewAdditionalValue: array[0..3] of Byte = ($E9, $FD, $54, $3D);
  NewRadiusValue: array[0..9] of Byte = ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00); // Обнуляем радиус

  
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

// Чтение оригинальных значений дальности (ТЕПЕРЬ ВОССТАНАВЛИВАЮТСЯ!)
procedure ReadOriginalDistanceValues;
var
  i: Integer;
begin
  if DistanceValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Читаем оригинальные значения дальности...');
  
  // Читаем провода
  for i := 0 to 1 do
    OrigWireValues[i] := ReadFloatFromMemory(WireAddrs[i]);
  
  // Читаем дальние модели
  OrigDistantModelValue := ReadFloatFromMemory(DistantModelAddr);
  
  // Читаем светофоры
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
  
  // Восстанавливаем оригинальные значения рельс
  WriteFloatToMemory(MainLightAddr, OrigMainLightValue);
  WriteFloatToMemory(AdditionalLightAddr, OrigAdditionalLightValue);
  
  // Устанавливаем дефолтные значения кабины
  // Яркость кабины = 9000
  WriteFloatToMemory($482824, 9000.0);
  WriteFloatToMemory($48A7A8, 9000.0);
  
  // Контрастность кабины = 8000
  WriteFloatToMemory($48282C, 8000.0);
  WriteFloatToMemory($48A7B0, 8000.0);
  WriteFloatToMemory($48DBC4, 8000.0);
  
  // Восстанавливаем оригинальные значения солнца
  WriteFloatToMemory(SunOrbitAddr, OrigSunOrbitValue);
  WriteFloatToMemory(SunHeightAddr, OrigSunHeightValue);
  
  AddToLogFile(EngineLog, 'Дефолтные значения освещения восстановлены');
end;

// НОВАЯ ФУНКЦИЯ: Восстановление оригинальных значений дальности
procedure RestoreOriginalDistanceValues;
var
  i: Integer;
begin
  if not DistanceValuesRead then Exit;
  
  AddToLogFile(EngineLog, 'Восстанавливаем оригинальные значения дальности...');
  
  // Восстанавливаем провода
  for i := 0 to 1 do
    WriteFloatToMemory(WireAddrs[i], OrigWireValues[i]);
  
  // Восстанавливаем дальние модели
  WriteFloatToMemory(DistantModelAddr, OrigDistantModelValue);
  
  // Восстанавливаем светофоры
  for i := 0 to 2 do
    WriteFloatToMemory(TrafficLightAddrs[i], OrigTrafficLightValues[i]);
  
  AddToLogFile(EngineLog, 'Оригинальные значения дальности восстановлены');
end;

// === ОПТИМИЗИРОВАННАЯ ФУНКЦИЯ ПРИМЕНЕНИЯ НАСТРОЕК ===
procedure ApplyLightingSettings;
begin
  if not Settings.Lighting then Exit;
  
  AddToLogFile(EngineLog, 'Применяем настройки освещения...');
  
  // Яркость рельс
  WriteFloatToMemory(MainLightAddr, Settings.MainLightIntensitySlider.Value);
  
  // Контрастность рельс
  WriteFloatToMemory(AdditionalLightAddr, Settings.AdditionalLightIntensitySlider.Value);
  
  // Яркость кабины
  WriteFloatToMemory($482824, Settings.CabinBrightnessSlider.Value);
  WriteFloatToMemory($48A7A8, Settings.CabinBrightnessSlider.Value);
  
  // Контрастность кабины
  WriteFloatToMemory($48282C, Settings.CabinContrastSlider.Value);
  WriteFloatToMemory($48A7B0, Settings.CabinContrastSlider.Value);
  WriteFloatToMemory($48DBC4, Settings.CabinContrastSlider.Value);
  
  // Остальные настройки (закомментированы)
  {
  WriteFloatToMemory($48B9BC, Settings.MainLightIntensitySlider.Value);
  WriteFloatToMemory($48DBBC, Settings.MainLightIntensitySlider.Value);
  WriteFloatToMemory($7396D4, Settings.MainLightIntensitySlider.Value);
  WriteFloatToMemory($7396D8, Settings.AdditionalLightIntensitySlider.Value);
  }
  
  WriteFloatToMemory(SunOrbitAddr, Settings.SunOrbitRadiusSlider.Value);
  WriteFloatToMemory(SunHeightAddr, Settings.SunHeightSlider.Value);
  
  AddToLogFile(EngineLog, 'Настройки освещения применены');
end;

// ИСПРАВЛЕННАЯ ФУНКЦИЯ: Применение настроек дальности с восстановлением
procedure ApplyDistanceSettings;
var
  WireDistance, DistantModelDistance, TrafficLightDistance: Single;
  i: Integer;
begin
  if not Settings.MaxVisibleDistance then 
  begin
    // Если MaxVisibleDistance выключен, восстанавливаем все оригинальные значения
    RestoreOriginalDistanceValues;
    Exit;
  end;
  
  // Устанавливаем дефолтные значения для каждой группы
  WireDistance := 250.0;         // Провода
  DistantModelDistance := 300.0; // Дальние модели  
  TrafficLightDistance := 1000.0; // Светофоры
  
  AddToLogFile(EngineLog, 'Применяем настройки дальности...');
  
  // Провода - применяем или восстанавливаем
  if Settings.ShowWires then
  begin
    for i := 0 to 1 do
      WriteFloatToMemory(WireAddrs[i], WireDistance);
  end
  else
  begin
    for i := 0 to 1 do
      WriteFloatToMemory(WireAddrs[i], OrigWireValues[i]);
  end;
  
  // Дальние модели - применяем или восстанавливаем
  if Settings.ShowDistantModels then
    WriteFloatToMemory(DistantModelAddr, DistantModelDistance)
  else
    WriteFloatToMemory(DistantModelAddr, OrigDistantModelValue);
  
  // Светофоры - применяем или восстанавливаем
  if Settings.ShowTrafficLights then
  begin
    for i := 0 to 2 do
      WriteFloatToMemory(TrafficLightAddrs[i], TrafficLightDistance);
  end
  else
  begin
    for i := 0 to 2 do
      WriteFloatToMemory(TrafficLightAddrs[i], OrigTrafficLightValues[i]);
  end;
  
  AddToLogFile(EngineLog, 'Настройки дальности применены');
end;

// === НОВАЯ ФУНКЦИЯ: Отложенное применение настроек ===
procedure ApplySettingsThrottled;
var
  CurrentTime: Cardinal;
begin
  CurrentTime := GetTickCount;
  
  // Применяем настройки только если прошло достаточно времени
  if (CurrentTime - LastApplyTime) >= ApplyInterval then
  begin
    // Применяем только включенные настройки
    if Settings.Lighting then
      ApplyLightingSettings;
    if Settings.MaxVisibleDistance then
      ApplyDistanceSettings;
    
    LastApplyTime := CurrentTime;
    PendingApply := False;
  end
  else
  begin
    PendingApply := True; // Запомнить, что нужно применить позже
  end;
end;

procedure LoadConfig;
var
  F: TextFile;
  Line, Key, Value: string;
  ColonPos: Integer;
  OldFreecamState: Boolean;
begin
  if not FileExists('zdbooster.cfg') then Exit;
  
  // Сохраняем старое состояние для отладки
  OldFreecamState := Settings.Freecam;
  
  try
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
        
        // Состояния модулей (0/1)
        if Key = 'freecam' then Settings.Freecam := (Value = '1');
        if Key = 'main_camera' then Settings.MainCamera := (Value = '1');
        if Key = 'lighting' then Settings.Lighting := (Value = '1');
        if Key = 'max_distance' then Settings.MaxVisibleDistance := (Value = '1');
        if Key = 'newsky' then Settings.NewSky := (Value = '1');
        if Key = 'new_club_positions' then Settings.NewClubPositions := (Value = '1');
        
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
        
        // Глобальный слайдер яркости
        if Key = 'brightness' then Settings.BrightnessSlider.Value := StrToFloatDef(Value, 0.0);
        
        // Слайдеры освещения
        if Key = 'main_light_intensity' then Settings.MainLightIntensitySlider.Value := StrToFloatDef(Value, 5000.0);
        if Key = 'additional_light_intensity' then Settings.AdditionalLightIntensitySlider.Value := StrToFloatDef(Value, 5000.0);
        if Key = 'cabin_brightness' then Settings.CabinBrightnessSlider.Value := StrToFloatDef(Value, 5000.0);
        if Key = 'cabin_contrast' then Settings.CabinContrastSlider.Value := StrToFloatDef(Value, 5000.0);
        if Key = 'sun_orbit_radius' then Settings.SunOrbitRadiusSlider.Value := StrToFloatDef(Value, 700.0);
        if Key = 'sun_height' then Settings.SunHeightSlider.Value := StrToFloatDef(Value, 200.0);
      end;
    end;
    CloseFile(F);

    MenuFreecamBaseSpeed := Settings.BasespeedSlider.Value;
    MenuFreecamFastSpeed := Settings.FastspeedSlider.Value;
    MenuFreecamTurnSpeed := Settings.TurnspeedSlider.Value;

    stepforward := Settings.StepForwardSlider.Value;
    maxvisibledistance := Settings.MaxVisibleDistanceSlider.Value;

    newsky := Settings.NewSky;

    // Логируем изменения состояния freecam
    if Settings.Freecam <> OldFreecamState then
    begin
      AddToLogFile(EngineLog, Format('FREECAM состояние изменилось внешне: %s -> %s', [BoolToStr(OldFreecamState), BoolToStr(Settings.Freecam)]));
    end;
    
  except
    // Игнорируем ошибки чтения конфига
  end;
end;


// === ОПТИМИЗИРОВАННОЕ ЧТЕНИЕ КОНФИГА ===
procedure LoadConfigThrottled;
var
  CurrentTime: Cardinal;
begin
  CurrentTime := GetTickCount;
  
  // Читаем конфиг только если прошло достаточно времени
  if (CurrentTime - LastConfigReadTime) >= ConfigReadInterval then
  begin
    LoadConfig;
    LastConfigReadTime := CurrentTime;
  end;
end;

// === ПРИНУДИТЕЛЬНОЕ ЧТЕНИЕ КОНФИГА ===
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
    
    // Состояния модулей (0/1)
    if Settings.Freecam then WriteLn(F, 'freecam: 1') else WriteLn(F, 'freecam: 0');
    if Settings.MainCamera then WriteLn(F, 'main_camera: 1') else WriteLn(F, 'main_camera: 0');
    if Settings.Lighting then WriteLn(F, 'lighting: 1') else WriteLn(F, 'lighting: 0');
    if Settings.MaxVisibleDistance then WriteLn(F, 'max_distance: 1') else WriteLn(F, 'max_distance: 0');
    if Settings.NewSky then WriteLn(F, 'newsky: 1') else WriteLn(F, 'newsky: 0');
    if Settings.NewClubPositions then WriteLn(F, 'new_club_positions: 1') else WriteLn(F, 'new_club_positions: 0');
    
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
    
    // Глобальный слайдер яркости
    WriteLn(F, 'brightness: ' + FormatValue(Settings.BrightnessSlider.Value));
    
    // Слайдеры освещения
    WriteLn(F, 'main_light_intensity: ' + FormatValue(Settings.MainLightIntensitySlider.Value));
    WriteLn(F, 'additional_light_intensity: ' + FormatValue(Settings.AdditionalLightIntensitySlider.Value));
    WriteLn(F, 'cabin_brightness: ' + FormatValue(Settings.CabinBrightnessSlider.Value));
    WriteLn(F, 'cabin_contrast: ' + FormatValue(Settings.CabinContrastSlider.Value));
    WriteLn(F, 'sun_orbit_radius: ' + FormatValue(Settings.SunOrbitRadiusSlider.Value));
    WriteLn(F, 'sun_height: ' + FormatValue(Settings.SunHeightSlider.Value));
    
    CloseFile(F);
  except
    // Игнорируем ошибки записи конфига
  end;
end;

// ===== ФУНКЦИЯ ПРИМЕНЕНИЯ ПАТЧА =====
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

// ===== ФУНКЦИЯ ВОССТАНОВЛЕНИЯ =====
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

// ИСПРАВЛЕННАЯ ФУНКЦИЯ МЕНЮ ПАТЧА
procedure ApplyMenuPatch;
var
  OldProtect: Cardinal;
  BytesWritten: Cardinal;
  NopBytes: array[0..4] of Byte;
begin
  if MenuCallPatched then Exit;
  
  try
    AddToLogFile(EngineLog, 'Применяем меню патч...');
    
    // Сохраняем оригинальные байты
    if not ReadProcessMemory(GetCurrentProcess, Pointer(MenuCallAddr), @OrigMenuCallBytes[0], 5, BytesWritten) then
    begin
      AddToLogFile(EngineLog, 'Ошибка чтения оригинальных байтов меню патча');
      Exit;
    end;
    
    // Подготавливаем NOP инструкции (90 90 90 90 90)
    NopBytes[0] := $90;
    NopBytes[1] := $90;
    NopBytes[2] := $90;
    NopBytes[3] := $90;
    NopBytes[4] := $90;
    
    // Применяем патч
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
    
    // Восстанавливаем оригинальные байты
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
  
  // === ПРОВЕРЯЕМ ОТЛОЖЕННОЕ ПРИМЕНЕНИЕ НАСТРОЕК (не каждый кадр) ===
  if PendingApply and ((CurrentTime - LastApplyTime) >= ApplyInterval) then
    ApplySettingsThrottled;
  
  // Анимация окон
  if Abs(RenderWindow.Alpha - RenderWindow.TargetAlpha) > 0.01 then
    RenderWindow.Alpha := RenderWindow.Alpha + (RenderWindow.TargetAlpha - RenderWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(WorldWindow.Alpha - WorldWindow.TargetAlpha) > 0.01 then
    WorldWindow.Alpha := WorldWindow.Alpha + (WorldWindow.TargetAlpha - WorldWindow.Alpha) * 5.0 * DeltaTime;
  if Abs(LocomotiveWindow.Alpha - LocomotiveWindow.TargetAlpha) > 0.01 then
    LocomotiveWindow.Alpha := LocomotiveWindow.Alpha + (LocomotiveWindow.TargetAlpha - LocomotiveWindow.Alpha) * 5.0 * DeltaTime;
  
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
    
    // Lighting секция
    if LightingSection.Expanded then
    begin
      if LightingSection.AnimProgress < 1.0 then
        LightingSection.AnimProgress := LightingSection.AnimProgress + 5.0 * DeltaTime;
    end
    else
    begin
      if LightingSection.AnimProgress > 0.0 then
        LightingSection.AnimProgress := LightingSection.AnimProgress - 5.0 * DeltaTime;
    end;
    if LightingSection.AnimProgress < 0 then LightingSection.AnimProgress := 0;
    if LightingSection.AnimProgress > 1 then LightingSection.AnimProgress := 1;
    
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
  
  // ИСПРАВЛЕННАЯ ИНИЦИАЛИЗАЦИЯ СЛАЙДЕРОВ FREECAM (шаг 0.01, максимум 2)
  Settings.BasespeedSlider.Value := 1.0;
  Settings.BasespeedSlider.MinValue := 0.01;
  Settings.BasespeedSlider.MaxValue := 2.0;
  
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
  
  // Инициализация слайдеров освещения
  Settings.MainLightIntensitySlider.Value := 5000.0;
  Settings.MainLightIntensitySlider.MinValue := 0.0;
  Settings.MainLightIntensitySlider.MaxValue := 10000.0;
  
  Settings.AdditionalLightIntensitySlider.Value := 5000.0;
  Settings.AdditionalLightIntensitySlider.MinValue := 0.0;
  Settings.AdditionalLightIntensitySlider.MaxValue := 10000.0;
  
  Settings.CabinBrightnessSlider.Value := 5000.0;
  Settings.CabinBrightnessSlider.MinValue := 0.0;
  Settings.CabinBrightnessSlider.MaxValue := 10000.0;
  
  Settings.CabinContrastSlider.Value := 5000.0;
  Settings.CabinContrastSlider.MinValue := 0.0;
  Settings.CabinContrastSlider.MaxValue := 10000.0;
  
  Settings.SunOrbitRadiusSlider.Value := 700.0;
  Settings.SunOrbitRadiusSlider.MinValue := 100.0;
  Settings.SunOrbitRadiusSlider.MaxValue := 5000.0;
  
  Settings.SunHeightSlider.Value := 200.0;
  Settings.SunHeightSlider.MinValue := -500.0;
  Settings.SunHeightSlider.MaxValue := 2000.0;

  LoadConfig;

  MenuFreecamBaseSpeed := Settings.BasespeedSlider.Value;
  MenuFreecamFastSpeed := Settings.FastspeedSlider.Value;
  MenuFreecamTurnSpeed := Settings.TurnspeedSlider.Value;

  stepforward := Settings.StepForwardSlider.Value;
  maxvisibledistance := Settings.MaxVisibleDistanceSlider.Value;

  // ИСПРАВЛЕННАЯ ИНИЦИАЛИЗАЦИЯ АДРЕСОВ (БЕЗ +1!)
  SpeedXAddr := $00400000 + $84B2B;
  AllowedSpeedAddr := $00400000 + $84D25;
  ShuntingSpeedAddr := $00400000 + $853C6;
  TrainSpeedAddr := $00400000 + $853E0;
  TimeAddr := $00400000 + $854B1;
  NumberAccelAddr := $00400000 + $85979;
  ReverseAddr := $00400000 + $84CA3;
  AdditionalAddr := $00400000 + $85630;
  RadiusAddr := $00400000 + $85F40;
  
  // ИНИЦИАЛИЗАЦИЯ АДРЕСА МЕНЮ ПАТЧА
  MenuCallAddr := $743B9E; // ЗАМЕНИТЕ НА ПРАВИЛЬНЫЙ АДРЕС!
  
  // Адреса освещения (БЕЗ +400000!)
  MainLightAddr := $4942AC;
  AdditionalLightAddr := $4942B4;
  SunOrbitAddr := $4942CC;
  SunHeightAddr := $4942B8;
  
  // Адреса дальности (3 группы, БЕЗ +400000!)
  WireAddrs[0] := $494408;        // Провода 1
  WireAddrs[1] := $494414;        // Провода 2
  DistantModelAddr := $494358;    // Дальние модели
  TrafficLightAddrs[0] := $48DB9C; // Светофоры 1
  TrafficLightAddrs[1] := $48DC1C; // Светофоры 2
  TrafficLightAddrs[2] := $48DBA0; // Светофоры 3
  
  // Читаем оригинальные значения освещения
  ReadOriginalLightingValues;
  
  // Читаем оригинальные значения дальности
  ReadOriginalDistanceValues;
  
  // Применяем патч если он был включен в конфиге
  if Settings.NewClubPositions then
    ApplyClubPositionsPatch;
    
  // Применяем освещение если оно было включено в конфиге
  if Settings.Lighting then
    ApplyLightingSettings;
    
  // Применяем дальность если она была включена в конфиге
  if Settings.MaxVisibleDistance then
    ApplyDistanceSettings;

  // Остальная инициализация окон...
  RenderWindow.Title := 'RENDER';
  RenderWindow.X := 50;
  RenderWindow.Y := 40;
  RenderWindow.Width := 240;
  RenderWindow.Height := 120;
  RenderWindow.Alpha := 0.0;
  RenderWindow.TargetAlpha := 1.0;
  
  WorldWindow.Title := 'WORLD';
  WorldWindow.X := 310;
  WorldWindow.Y := 40;
  WorldWindow.Width := 240;
  WorldWindow.Height := 100;
  WorldWindow.Alpha := 0.0;
  WorldWindow.TargetAlpha := 1.0;
  
  LocomotiveWindow.Title := 'LOCOMOTIVE';
  LocomotiveWindow.X := 570;
  LocomotiveWindow.Y := 40;
  LocomotiveWindow.Width := 240;
  LocomotiveWindow.Height := 100;
  LocomotiveWindow.Alpha := 0.0;
  LocomotiveWindow.TargetAlpha := 1.0;
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
  
  // Поднял текст ещё выше и уменьшил размер шрифта в 2 раза
  DrawText2D(0, X, Y - 20, Text + ': ' + ValueText, $FFFFFF, Alpha, 0.55);
  
  // Трек
  DrawRectangle2D(X, Y + 8, SLIDER_WIDTH, 6, $1A1A1A, Alpha, True);
  
  // Активная часть
  if Progress > 0 then
    DrawRectangle2D(X, Y + 8, Round(Progress * SLIDER_WIDTH), 6, $0066FF, Alpha, True);
  
  // Ползунок с увеличенной областью клика
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
      TotalHeight := TotalHeight + ITEM_HEIGHT; // Freecam
      if Settings.FreecamSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(140 * Settings.FreecamSection.AnimProgress) + MARGIN;
      
      TotalHeight := TotalHeight + ITEM_HEIGHT; // Main Camera
      if Settings.MainCameraSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(75 * Settings.MainCameraSection.AnimProgress) + MARGIN;
      
      TotalHeight := TotalHeight + ITEM_HEIGHT; // Lighting
      if Settings.LightingSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(320 * Settings.LightingSection.AnimProgress) + MARGIN;
    end;
    
    1: // WORLD окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT; // Max Visible Distance
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
        TotalHeight := TotalHeight + Round(150 * Settings.MaxVisibleDistanceSection.AnimProgress) + MARGIN;
      
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN; // New Sky
    end;
    
    2: // LOCOMOTIVE окно
    begin
      TotalHeight := TotalHeight + ITEM_HEIGHT + MARGIN; // Прямо "Исправления КЛУБ"
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
      DrawToggle(Win.X + MARGIN, ContentY, 'Свободная Камера', Settings.Freecam, Alpha, True, ExpandButtonX, Settings.FreecamSection.Expanded);
      Inc(ContentY, ITEM_HEIGHT + MARGIN);
      
      // Freecam секция
      if Settings.FreecamSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(140 * Settings.FreecamSection.AnimProgress);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $202020, Alpha, True);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $353535, Alpha, False);
        
        if SectionHeight > 30 then
        begin
          DrawSlider(Win.X + MARGIN + 20, ContentY + 20, Settings.BasespeedSlider, 'Базовая скорость', Alpha);
          DrawSlider(Win.X + MARGIN + 20, ContentY + 60, Settings.FastspeedSlider, 'Скорость с Shift', Alpha);
        end;
        
        Inc(ContentY, SectionHeight + MARGIN);
      end;
      
      // Main Camera
      ExpandButtonX := Win.X + 200;
      DrawToggle(Win.X + MARGIN, ContentY, 'Основная Камера', Settings.MainCamera, Alpha, True, ExpandButtonX, Settings.MainCameraSection.Expanded);
      Inc(ContentY, ITEM_HEIGHT + MARGIN);
      
      // Main Camera секция
      if Settings.MainCameraSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(75 * Settings.MainCameraSection.AnimProgress);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $202020, Alpha, True);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $353535, Alpha, False);
        
        if SectionHeight > 30 then
          DrawSlider(Win.X + MARGIN + 20, ContentY + 20, Settings.StepForwardSlider, 'Шаг вперёд', Alpha);
        
        Inc(ContentY, SectionHeight + MARGIN);
      end;
      
      // Lighting
//      ExpandButtonX := Win.X + 200;
//      DrawToggle(Win.X + MARGIN, ContentY, 'Освещение (alpha)', Settings.Lighting, Alpha, True, ExpandButtonX, Settings.LightingSection.Expanded);
//      Inc(ContentY, ITEM_HEIGHT + MARGIN);
      
      // Lighting секция (7 слайдеров)
//      if Settings.LightingSection.AnimProgress > 0.01 then
//      begin
//        SectionHeight := Round(320 * Settings.LightingSection.AnimProgress);
//        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $202020, Alpha, True);
//        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $353535, Alpha, False);
//        
//        if SectionHeight > 30 then
//        begin
//          DrawSlider(Win.X + MARGIN + 20, ContentY + 20, Settings.MainLightIntensitySlider, 'Яркость рельс', Alpha);
//          if SectionHeight > 70 then
//            DrawSlider(Win.X + MARGIN + 20, ContentY + 60, Settings.AdditionalLightIntensitySlider, 'Контрастность рельс', Alpha);
//          if SectionHeight > 110 then
//            DrawSlider(Win.X + MARGIN + 20, ContentY + 100, Settings.CabinBrightnessSlider, 'Яркость кабины', Alpha);
//          if SectionHeight > 150 then
//            DrawSlider(Win.X + MARGIN + 20, ContentY + 140, Settings.CabinContrastSlider, 'Контрастность кабины', Alpha);
//          if SectionHeight > 190 then
//            DrawSlider(Win.X + MARGIN + 20, ContentY + 180, Settings.BrightnessSlider, 'Яркость меню', Alpha);
//          if SectionHeight > 230 then
//            DrawSlider(Win.X + MARGIN + 20, ContentY + 220, Settings.SunOrbitRadiusSlider, 'Радиус орбиты', Alpha);
//          if SectionHeight > 270 then
//            DrawSlider(Win.X + MARGIN + 20, ContentY + 260, Settings.SunHeightSlider, 'Высота солнца', Alpha);
//        end;
//        
//        Inc(ContentY, SectionHeight + MARGIN);
//      end;
    end;
    
    1: // WORLD окно
    begin
      // Max Visible Distance
      ExpandButtonX := Win.X + 200;
      DrawToggle(Win.X + MARGIN, ContentY, 'Макс. дальность', Settings.MaxVisibleDistance, Alpha, True, ExpandButtonX, Settings.MaxVisibleDistanceSection.Expanded);
      Inc(ContentY, ITEM_HEIGHT + MARGIN);
      
      // Max Visible Distance секция
      if Settings.MaxVisibleDistanceSection.AnimProgress > 0.01 then
      begin
        SectionHeight := Round(150 * Settings.MaxVisibleDistanceSection.AnimProgress);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $202020, Alpha, True);
        DrawRectangle2D(Win.X + MARGIN + 10, ContentY, 210, SectionHeight, $353535, Alpha, False);
        
        if SectionHeight > 30 then
        begin
          DrawSlider(Win.X + MARGIN + 20, ContentY + 20, Settings.MaxVisibleDistanceSlider, 'Дальность (м.)', Alpha);
          
          if SectionHeight > 60 then
          begin
            DrawCheckbox(Win.X + MARGIN + 20, ContentY + 50, 'Провода', Settings.ShowWires, Alpha);
            DrawCheckbox(Win.X + MARGIN + 20, ContentY + 75, 'Дальние модели', Settings.ShowDistantModels, Alpha);
          end;
          if SectionHeight > 100 then
            DrawCheckbox(Win.X + MARGIN + 20, ContentY + 100, 'Светофоры', Settings.ShowTrafficLights, Alpha);
        end;
        
        Inc(ContentY, SectionHeight + MARGIN);
      end;
      
      // New Sky
      DrawToggle(Win.X + MARGIN, ContentY, 'Новая логика неба', Settings.NewSky, Alpha);
    end;
    
    2: // LOCOMOTIVE окно
    begin
      // ПРЯМО "Исправления КЛУБ" без секции
      DrawToggle(Win.X + MARGIN, ContentY, 'Исправления БИЛ-В', Settings.NewClubPositions, Alpha);
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
  
  InfoText := 'ZDS-Booster v1 | vk.com/raildriver';
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
  DrawText(BarX + 8, BarY + 6, InfoText, $FFFFFF, Alpha);
end;

procedure DrawCheatMenu; stdcall;
var
  BackgroundAlpha: Integer;
begin
  if not MenuVisible then Exit;
  
  // === ОПТИМИЗИРОВАННОЕ ЧТЕНИЕ КОНФИГА ДЛЯ СИНХРОНИЗАЦИИ ===
  LoadConfigThrottled;
  
  UpdateAnimations;

  Begin2D;
  try
    // Затемнение фона ТОЛЬКО если ГЛОБАЛЬНЫЙ слайдер больше 0
    if Settings.BrightnessSlider.Value > 0 then
    begin
      BackgroundAlpha := Round(Settings.BrightnessSlider.Value * RenderWindow.Alpha);
      DrawRectangle2D(0, 0, InitResX, InitResY, $000011, BackgroundAlpha, True);
    end;
    
    DrawWindow(RenderWindow, 0);      // RENDER окно
    DrawWindow(WorldWindow, 1);       // WORLD окно  
    DrawWindow(LocomotiveWindow, 2);  // LOCOMOTIVE окно
    DrawInfoBar;
  finally
    End2D;
  end;
end;

// === ОПТИМИЗИРОВАННАЯ ФУНКЦИЯ ПЕРЕТАСКИВАНИЯ СЛАЙДЕРА ===
procedure HandleSliderDrag(X: Integer; var Slider: TSlider; SliderX: Integer);
var
  NewProgress: Single;
  OldValue: Single;
begin
  if not Slider.IsDragging then Exit;
  
  OldValue := Slider.Value; // Сохраняем старое значение
  
  NewProgress := (X - SliderX) / SLIDER_WIDTH;
  if NewProgress < 0 then NewProgress := 0;
  if NewProgress > 1 then NewProgress := 1;
  
  Slider.Value := Slider.MinValue + NewProgress * (Slider.MaxValue - Slider.MinValue);
  
  // Применяем настройки только если значение действительно изменилось
if Abs(Slider.Value - OldValue) > 0.001 then
begin
  // === СИНХРОНИЗАЦИЯ С ГЛОБАЛЬНЫМИ ПЕРЕМЕННЫМИ ===
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

  // МГНОВЕННОЕ сохранение конфига для отзывчивости интерфейса
  SaveConfig;
  
  // ОТЛОЖЕННОЕ применение настроек к памяти (только для освещения и дальности)
  // Слайдер яркости меню не нуждается в throttling, так как не записывает в память игры
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
  
  // Обработка драггинга слайдеров
  if Settings.BrightnessSlider.IsDragging then // Слайдер яркости меню (теперь в секции освещения)
    HandleSliderDrag(X, Settings.BrightnessSlider, RenderWindow.X + MARGIN + 20);
  if Settings.BasespeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.BasespeedSlider, RenderWindow.X + MARGIN + 20);
  if Settings.FastspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.FastspeedSlider, RenderWindow.X + MARGIN + 20);
  if Settings.TurnspeedSlider.IsDragging then
    HandleSliderDrag(X, Settings.TurnspeedSlider, RenderWindow.X + MARGIN + 20);
  if Settings.StepForwardSlider.IsDragging then
    HandleSliderDrag(X, Settings.StepForwardSlider, RenderWindow.X + MARGIN + 20);
  if Settings.MainLightIntensitySlider.IsDragging then
    HandleSliderDrag(X, Settings.MainLightIntensitySlider, RenderWindow.X + MARGIN + 20);
  if Settings.AdditionalLightIntensitySlider.IsDragging then
    HandleSliderDrag(X, Settings.AdditionalLightIntensitySlider, RenderWindow.X + MARGIN + 20);
  if Settings.CabinBrightnessSlider.IsDragging then
    HandleSliderDrag(X, Settings.CabinBrightnessSlider, RenderWindow.X + MARGIN + 20);
  if Settings.CabinContrastSlider.IsDragging then
    HandleSliderDrag(X, Settings.CabinContrastSlider, RenderWindow.X + MARGIN + 20);
  if Settings.SunOrbitRadiusSlider.IsDragging then
    HandleSliderDrag(X, Settings.SunOrbitRadiusSlider, RenderWindow.X + MARGIN + 20);
  if Settings.SunHeightSlider.IsDragging then
    HandleSliderDrag(X, Settings.SunHeightSlider, RenderWindow.X + MARGIN + 20);
  if Settings.MaxVisibleDistanceSlider.IsDragging then
    HandleSliderDrag(X, Settings.MaxVisibleDistanceSlider, WorldWindow.X + MARGIN + 20);
end;

procedure HandleMenuClick(X, Y: Integer); stdcall;
var
  ContentY: Integer;
  FreecamSectionY, MainCameraSectionY, LightingSectionY, MaxVisibleDistanceSectionY: Integer;
  SectionHeight: Integer;
begin
  if not MenuVisible then Exit;
  
  if (RenderWindow.Alpha < 0.1) and (WorldWindow.Alpha < 0.1) and (LocomotiveWindow.Alpha < 0.1) then Exit;
  
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
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
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
      if Settings.MainCamera then Settings.MainCameraSection.Expanded := True;
      SaveConfig;
      SyncConfigFromMenu(Settings.Freecam, Settings.MainCamera, Settings.MaxVisibleDistance, Settings.NewSky);
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);
    
    // Main Camera slider
    MainCameraSectionY := ContentY;
    if Settings.MainCameraSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(75 * Settings.MainCameraSection.AnimProgress);
      if (SectionHeight > 30) and InRect(X, Y, RenderWindow.X + MARGIN + 20, MainCameraSectionY + 10, SLIDER_WIDTH + 25, 40) then
      begin
        Settings.StepForwardSlider.IsDragging := True;
        Exit;
      end;
      Inc(ContentY, SectionHeight + MARGIN);
    end;
    
    // Lighting expand button
    if InRect(X, Y, RenderWindow.X + 200, ContentY + 4, BUTTON_SIZE, BUTTON_SIZE) then
    begin
      Settings.LightingSection.Expanded := not Settings.LightingSection.Expanded;
      Exit;
    end;
    
    // Lighting toggle
    if InRect(X, Y, RenderWindow.X + MARGIN, ContentY, 220, ITEM_HEIGHT) then
    begin
      Settings.Lighting := not Settings.Lighting;
      if Settings.Lighting then 
      begin
        Settings.LightingSection.Expanded := True;
        ApplyLightingSettings;
      end
      else
      begin
        // При выключении восстанавливаем дефолтные значения
        RestoreOriginalLightingValues;
      end;
      SaveConfig;
      Exit;
    end;
    Inc(ContentY, ITEM_HEIGHT + MARGIN);
    
    // Lighting sliders (7 слайдеров)
    LightingSectionY := ContentY;
    if Settings.LightingSection.AnimProgress > 0.01 then
    begin
      SectionHeight := Round(320 * Settings.LightingSection.AnimProgress);
      if SectionHeight > 30 then
      begin
        if InRect(X, Y, RenderWindow.X + MARGIN + 20, LightingSectionY + 10, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.MainLightIntensitySlider.IsDragging := True;
          Exit;
        end;
        if (SectionHeight > 70) and InRect(X, Y, RenderWindow.X + MARGIN + 20, LightingSectionY + 50, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.AdditionalLightIntensitySlider.IsDragging := True;
          Exit;
        end;
        if (SectionHeight > 110) and InRect(X, Y, RenderWindow.X + MARGIN + 20, LightingSectionY + 90, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.CabinBrightnessSlider.IsDragging := True;
          Exit;
        end;
        if (SectionHeight > 150) and InRect(X, Y, RenderWindow.X + MARGIN + 20, LightingSectionY + 130, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.CabinContrastSlider.IsDragging := True;
          Exit;
        end;
        if (SectionHeight > 190) and InRect(X, Y, RenderWindow.X + MARGIN + 20, LightingSectionY + 170, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.BrightnessSlider.IsDragging := True;
          Exit;
        end;
        if (SectionHeight > 230) and InRect(X, Y, RenderWindow.X + MARGIN + 20, LightingSectionY + 210, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.SunOrbitRadiusSlider.IsDragging := True;
          Exit;
        end;
        if (SectionHeight > 270) and InRect(X, Y, RenderWindow.X + MARGIN + 20, LightingSectionY + 250, SLIDER_WIDTH + 25, 40) then
        begin
          Settings.SunHeightSlider.IsDragging := True;
          Exit;
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
  
  Settings.BrightnessSlider.IsDragging := False;
  Settings.BasespeedSlider.IsDragging := False;
  Settings.FastspeedSlider.IsDragging := False;
  Settings.TurnspeedSlider.IsDragging := False;
  Settings.StepForwardSlider.IsDragging := False;
  Settings.MainLightIntensitySlider.IsDragging := False;
  Settings.AdditionalLightIntensitySlider.IsDragging := False;
  Settings.CabinBrightnessSlider.IsDragging := False;
  Settings.CabinContrastSlider.IsDragging := False;
  Settings.SunOrbitRadiusSlider.IsDragging := False;
  Settings.SunHeightSlider.IsDragging := False;
  Settings.MaxVisibleDistanceSlider.IsDragging := False;
  
  // ПРИНУДИТЕЛЬНО применяем все настройки при отпускании мыши
  if Settings.Lighting then
    ApplyLightingSettings;
  if Settings.MaxVisibleDistance then
    ApplyDistanceSettings;
  
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
    
    // Патчим вызов при открытии меню
    ApplyMenuPatch;
  end
  else
  begin
    ShowCursor(False);
    RenderWindow.TargetAlpha := 0.0;
    WorldWindow.TargetAlpha := 0.0;
    LocomotiveWindow.TargetAlpha := 0.0;
    
    // Восстанавливаем вызов при закрытии меню
    RemoveMenuPatch;
  end;
end;

end.
