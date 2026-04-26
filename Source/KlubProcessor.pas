unit KlubProcessor;

interface

uses
  Windows, SysUtils, Addresses;

// === КОНСТАНТЫ И КОНФИГУРАЦИЯ ===
const
  // Версия
  HOOK_VERSION_MAJOR = 1;
  HOOK_VERSION_MINOR = 0;
  HOOK_VERSION_BUILD = 1;
  HOOK_VERSION_STRING = '1.0.1';

  // Memory addresses and button offsets are defined in Addresses.pas

// === ТИПЫ ===
type
  TKlubTime = record
    hour, min, sec: Integer;
  end;

var
  // Глобальные переменные
  EnableLogging: Boolean = True;
  LogFileName: string = 'klub_processor.log';

// === ОСНОВНЫЕ ФУНКЦИИ ===
procedure WORLD_PROCESSKLUBKEYS(); stdcall;
procedure WORLD_PROCESSKLUBKEYS_HOOK(); stdcall;

// === ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ===
function SafeReadByte(Address: Cardinal): Byte;
function SafeReadWord(Address: Cardinal): Word;
function SafeReadInteger(Address: Cardinal): Integer;
function SafeReadSingle(Address: Cardinal): Single;
function SafeReadDouble(Address: Cardinal): Double;

procedure SafeWriteByte(Address: Cardinal; Value: Byte);
procedure SafeWriteWord(Address: Cardinal; Value: Word);
procedure SafeWriteInteger(Address: Cardinal; Value: Integer);

function IsButtonPressed(CurrentOffset, PrevOffset: Integer): Boolean;
function IsButtonHeld(Offset: Integer): Boolean;

procedure LogMessage(const Msg: string);
procedure SetLogging(Enable: Boolean);
function GetHookVersion: string;

implementation

// === БЕЗОПАСНОЕ ЧТЕНИЕ ПАМЯТИ ===
function SafeReadByte(Address: Cardinal): Byte;
begin
  try
    Result := PByte(Address)^;
  except
    Result := 0;
  end;
end;

function SafeReadWord(Address: Cardinal): Word;
begin
  try
    Result := PWord(Address)^;
  except
    Result := 0;
  end;
end;

function SafeReadInteger(Address: Cardinal): Integer;
begin
  try
    Result := PInteger(Address)^;
  except
    Result := 0;
  end;
end;

function SafeReadSingle(Address: Cardinal): Single;
begin
  try
    Result := PSingle(Address)^;
  except
    Result := 0.0;
  end;
end;

function SafeReadDouble(Address: Cardinal): Double;
begin
  try
    Result := PDouble(Address)^;
  except
    Result := 0.0;
  end;
end;

procedure SafeWriteByte(Address: Cardinal; Value: Byte);
begin
  try
    PByte(Address)^ := Value;
  except
    // Игнорируем ошибки записи
  end;
end;

procedure SafeWriteWord(Address: Cardinal; Value: Word);
begin
  try
    PWord(Address)^ := Value;
  except
    // Игнорируем ошибки записи
  end;
end;

procedure SafeWriteInteger(Address: Cardinal; Value: Integer);
begin
  try
    PInteger(Address)^ := Value;
  except
    // Игнорируем ошибки записи
  end;
end;

// === ЛОГИРОВАНИЕ ===
procedure LogMessage(const Msg: string);
var
  F: TextFile;
  TimeStr: string;
begin
  if not EnableLogging then Exit;
  
  try
    AssignFile(F, LogFileName);
    if FileExists(LogFileName) then
      Append(F)
    else
      Rewrite(F);
      
    TimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    WriteLn(F, '[' + TimeStr + '] ' + Msg);
    CloseFile(F);
  except
    // Игнорируем ошибки логирования
  end;
end;

procedure SetLogging(Enable: Boolean);
begin
  EnableLogging := Enable;
  if Enable then
    LogMessage('KLUB: Logging enabled')
  else
    LogMessage('KLUB: Logging disabled');
end;

function GetHookVersion: string;
begin
  Result := HOOK_VERSION_STRING;
end;

// === РАБОТА С КНОПКАМИ ===
function IsButtonPressed(CurrentOffset, PrevOffset: Integer): Boolean;
var
  CurrentState, PrevState: Byte;
  ButtonBase: Cardinal;
begin
  Result := False;
  try
    ButtonBase := ZBUTTONS_BASE;
    CurrentState := SafeReadByte(ButtonBase + CurrentOffset);
    PrevState := SafeReadByte(ButtonBase + PrevOffset);
    
    Result := (CurrentState = 1) and (CurrentState <> PrevState);
    
    // Обновляем предыдущее состояние
    if CurrentState <> PrevState then
      SafeWriteByte(ButtonBase + PrevOffset, CurrentState);
  except
    Result := False;
  end;
end;

function IsButtonHeld(Offset: Integer): Boolean;
var
  ButtonBase: Cardinal;
begin
  Result := False;
  try
    ButtonBase := ZBUTTONS_BASE;
    Result := SafeReadByte(ButtonBase + Offset) = 1;
  except
    Result := False;
  end;
end;

// === ОСНОВНАЯ ЛОГИКА ОБРАБОТКИ KLUB ===
procedure ProcessKlubKeys();
var
  wheelsetSpeed, absSpeedKph: Single;
  ls, vkStatus, rbStatus: Word;
  klubRmp, klubVvodMode, klubBrightness: Byte;
  alsnFreq: Byte;
  traversedPath: Double;
  sautPodtDist, sautOtpravDist: Integer;
  sautAimDist: Word;
  myTrack, maxTracks, route: Integer;
  trackSpeedLimit, klubSpeedLimit, sautSSSpeed: Word;
  zTimeHour, zTimeMin, zTimeSec: Integer;
  currSignalDist: Word;
  klubMessage: Integer;
  timeCheck: Integer;
  trackDataBase: Cardinal;
  trackRecord: Cardinal;
begin
  try
    LogMessage('KLUB: Processing keys...');
    
    // Читаем основные переменные
    wheelsetSpeed := SafeReadSingle(WHEELSETS_SPEED_ADDR);
    ls := SafeReadWord(LS_ADDR);
    vkStatus := SafeReadInteger(VK_STATUS_ADDR);
    rbStatus := SafeReadInteger(RB_STATUS_ADDR);
    absSpeedKph := SafeReadDouble(ABS_SPEED_KPH_ADDR);
    traversedPath := SafeReadDouble(TRAVERSED_PATH_ADDR);
    
    // === ОБРАБОТКА КНОПКИ RMP ===
    if IsButtonPressed(BTN_RMP_CURR, BTN_RMP_PREV) then
    begin
      if (Abs(wheelsetSpeed) < 1.0) and (ls <> 5) and (ls <> 4) then
      begin
        klubRmp := SafeReadByte(KLUB_RMP_ADDR);
        Inc(klubRmp);
        if klubRmp > 2 then
          klubRmp := 0;
        SafeWriteByte(KLUB_RMP_ADDR, klubRmp);
        LogMessage('RMP changed to: ' + IntToStr(klubRmp));
      end;
    end;
    
    // === УПРАВЛЕНИЕ ALSN ===
    if (vkStatus = 1) and (rbStatus = 1) and 
       IsButtonHeld($0D) and (ls = 2) then
    begin
      SafeWriteWord(ALSN_MODE_ADDR, 1);
      LogMessage('ALSN Mode activated');
    end;
    
    if ((vkStatus = 1) and (rbStatus = 1) or IsButtonHeld($0D)) and
       (ls = 3) and (SafeReadWord(KLUB_SPEED_LIMIT_ADDR) < 20) and
       (Abs(wheelsetSpeed) < 1.0) then
    begin
      SafeWriteWord(ALSN_MODE_ADDR, 0);
      LogMessage('ALSN Mode deactivated');
    end;
    
    // === ОБРАБОТКА ЧАСТОТЫ ALSN ===
    if IsButtonPressed(BTN_FREQ_CURR, BTN_FREQ_PREV) then
    begin
      alsnFreq := SafeReadByte(ALSN_FREQ_ADDR);
      alsnFreq := alsnFreq + 25;
      if alsnFreq > 75 then alsnFreq := 25;
      if alsnFreq < 25 then alsnFreq := 25;
      SafeWriteByte(ALSN_FREQ_ADDR, alsnFreq);
      LogMessage('ALSN Frequency changed to: ' + IntToStr(alsnFreq));
    end;
    
    // === РЕЖИМЫ ВВОДА ===
    if IsButtonHeld(BTN_MODE10) then
    begin
      SafeWriteByte(KLUB_VVOD_MODE_ADDR, 10);
      SafeWriteInteger(KLUB_STRING3_ADDR, 0);
      LogMessage('Entered mode 10 (Train Length)');
    end;
    
    if IsButtonHeld(BTN_MODE20) then
    begin
      SafeWriteByte(KLUB_VVOD_MODE_ADDR, 20);
      SafeWriteInteger(KLUB_STRING3_ADDR, 0);
      LogMessage('Entered mode 20 (Track Number)');
    end;
    
    if IsButtonHeld(BTN_MODE30) then
    begin
      SafeWriteByte(KLUB_VVOD_MODE_ADDR, 30);
      SafeWriteInteger(KLUB_STRING3_ADDR, 0);
      LogMessage('Entered mode 30 (Commands)');
    end;
    
    // === ЯРКОСТЬ ===
    if IsButtonPressed(BTN_BRIGHT_CURR, BTN_BRIGHT_PREV) then
    begin
      klubBrightness := SafeReadWord(KLUB_BRIGHTNESS_ADDR);
      Inc(klubBrightness, 64);
      if klubBrightness > 255 then
        klubBrightness := 63;
      SafeWriteWord(KLUB_BRIGHTNESS_ADDR, klubBrightness);
      LogMessage('Brightness changed to: ' + IntToStr(klubBrightness));
    end;
    
    // === ПРОВЕРКА ВРЕМЕНИ ===
    zTimeSec := SafeReadInteger(ZTIME_SEC_ADDR);
    timeCheck := zTimeSec mod 10;
    if (timeCheck = 0) and (SafeReadByte(KLUB_K4_ADDR) = 0) then
    begin
      SafeWriteInteger(KLUB_MESSAGE_ADDR, 0);
    end;
    
    // === ОБРАБОТКА ENTER ===
    klubVvodMode := SafeReadByte(KLUB_VVOD_MODE_ADDR);
    
    if IsButtonPressed(BTN_ENTER_CURR, BTN_ENTER_PREV) then
    begin
      case klubVvodMode of
        13: // Длина поезда
        begin
          SafeWriteWord(KLUB_TRAIN_LENGTH_ADDR, 24 * 10);
          LogMessage('Train length set');
        end;
        
        20: // Номер пути
        begin
          SafeWriteByte(KLUB_TRACK_ADDR, 1);
          LogMessage('Track number set');
        end;
        
        21: // Неисправность
        begin
          SafeWriteByte(KLUB_ISPRAV_ADDR, 1);
          LogMessage('Malfunction code set');
        end;
        
        30: // Команды
        begin
          // Обработка команд KLUB
          SafeWriteByte(KLUB_K71_ADDR, 0);
          SafeWriteByte(KLUB_K799_ADDR, 1);
          SafeWriteByte(KLUB_K809_ADDR, 1);
          SafeWriteWord(KLUB_WHITE_SPEED_ADDR, 40);
          LogMessage('Command processed');
        end;
        
        31: // Белая скорость
        begin
          if (SafeReadByte(KLUB_K809_ADDR) = 1) or 
             (SafeReadByte(KLUB_K799_ADDR) = 1) then
          begin
            SafeWriteWord(KLUB_WHITE_SPEED_ADDR, 52);
            LogMessage('White speed set');
          end;
        end;
      end;
      
      // Очистка строки ввода
      SafeWriteInteger(KLUB_STRING3_ADDR, 0);
      
      // Переход к следующему режиму
      Inc(klubVvodMode);
      
      // Проверки завершения режимов
      if (klubVvodMode = 15) or (klubVvodMode = 22) or (klubVvodMode = 32) then
        klubVvodMode := 0;
        
      if (klubVvodMode = 31) and 
         (SafeReadByte(KLUB_K809_ADDR) = 0) and
         (SafeReadByte(KLUB_K799_ADDR) = 0) then
        klubVvodMode := 0;
        
      if (klubVvodMode = 21) and 
         (SafeReadByte(KLUB_TRACK_ADDR) = 0) then
      begin
        klubVvodMode := 0;
        SafeWriteByte(KLUB_ISPRAV_ADDR, $FF); // -1
      end;
      
      SafeWriteByte(KLUB_VVOD_MODE_ADDR, klubVvodMode);
    end;
    
    // === КНОПКА ОТМЕНЫ ===
    if IsButtonHeld(BTN_CANCEL) and (klubVvodMode > 0) then
    begin
      SafeWriteInteger(KLUB_STRING3_ADDR, 0);
      LogMessage('Input cancelled');
    end;
    
    // === ЦИФРОВЫЕ КНОПКИ ===
    if (klubVvodMode > 0) then
    begin
      if IsButtonPressed(BTN_DIGIT0_CURR, BTN_DIGIT0_PREV) then LogMessage('Digit 0 pressed');
      if IsButtonPressed(BTN_DIGIT1_CURR, BTN_DIGIT1_PREV) then LogMessage('Digit 1 pressed');
      if IsButtonPressed(BTN_DIGIT2_CURR, BTN_DIGIT2_PREV) then LogMessage('Digit 2 pressed');
      if IsButtonPressed(BTN_DIGIT3_CURR, BTN_DIGIT3_PREV) then LogMessage('Digit 3 pressed');
      if IsButtonPressed(BTN_DIGIT4_CURR, BTN_DIGIT4_PREV) then LogMessage('Digit 4 pressed');
      if IsButtonPressed(BTN_DIGIT5_CURR, BTN_DIGIT5_PREV) then LogMessage('Digit 5 pressed');
      if IsButtonPressed(BTN_DIGIT6_CURR, BTN_DIGIT6_PREV) then LogMessage('Digit 6 pressed');
      if IsButtonPressed(BTN_DIGIT7_CURR, BTN_DIGIT7_PREV) then LogMessage('Digit 7 pressed');
      if IsButtonPressed(BTN_DIGIT8_CURR, BTN_DIGIT8_PREV) then LogMessage('Digit 8 pressed');
      if IsButtonPressed(BTN_DIGIT9_CURR, BTN_DIGIT9_PREV) then LogMessage('Digit 9 pressed');
    end;
    
    // === SAUT ФУНКЦИИ ===
    
    // Копирование SAUTAIMDIST
    currSignalDist := SafeReadWord(CURR_SIGNAL_DIST_ADDR);
    SafeWriteWord(SAUT_AIM_DIST_ADDR, currSignalDist);
    
    // Обработка SAUT distances
    sautPodtDist := SafeReadInteger(SAUT_PODT_DIST_ADDR);
    if sautPodtDist > 0 then
    begin
      sautAimDist := SafeReadWord(SAUT_AIM_DIST_ADDR);
      SafeWriteWord(SAUT_AIM_DIST_ADDR, sautPodtDist - Round(traversedPath));
    end;
    
    sautOtpravDist := SafeReadInteger(SAUT_OTPRAV_DIST_ADDR);
    if sautOtpravDist > 0 then
    begin
      sautAimDist := SafeReadWord(SAUT_AIM_DIST_ADDR);
      SafeWriteWord(SAUT_AIM_DIST_ADDR, sautOtpravDist - Round(traversedPath));
    end;
    
    // Установка SAUT SPEED LIMIT
    sautSSSpeed := SafeReadWord(SAUT_SS_SPEED_ADDR);
    if sautSSSpeed <> 0 then
      SafeWriteWord(SAUT_SPEED_LIMIT_ADDR, sautSSSpeed)
    else
    begin
      klubSpeedLimit := SafeReadWord(KLUB_SPEED_LIMIT_ADDR);
      SafeWriteWord(SAUT_SPEED_LIMIT_ADDR, klubSpeedLimit);
    end;
    
    // === КНОПКА SAUT PODT ===
    if IsButtonPressed(BTN_SAUT_PODT_CURR, BTN_SAUT_PODT_PREV) then
    begin
      sautAimDist := SafeReadWord(SAUT_AIM_DIST_ADDR);
      sautPodtDist := Round(traversedPath) + sautAimDist + 300;
      SafeWriteInteger(SAUT_PODT_DIST_ADDR, sautPodtDist);
      LogMessage('SAUT PODT set to: ' + IntToStr(sautPodtDist));
    end;
    
    // Проверка превышения пройденного пути
    if traversedPath > sautPodtDist then
      SafeWriteInteger(SAUT_PODT_DIST_ADDR, 0);
    
    // === КНОПКА SAUT OTPRAV ===
    if IsButtonPressed(BTN_SAUT_OTPR_CURR, BTN_SAUT_OTPR_PREV) and (ls = 1) then
    begin
      sautAimDist := SafeReadWord(SAUT_AIM_DIST_ADDR);
      sautOtpravDist := Round(traversedPath) + sautAimDist + 600;
      SafeWriteInteger(SAUT_OTPRAV_DIST_ADDR, sautOtpravDist);
      SafeWriteWord(KLUB_WHITE_SPEED_ADDR, 52);
      LogMessage('SAUT OTPRAV activated');
    end;
    
    // Проверка отмены SAUT OTPRAV
    if (traversedPath > sautOtpravDist) or (ls <> 1) then
    begin
      if SafeReadInteger(SAUT_OTPRAV_DIST_ADDR) > 0 then
      begin
        SafeWriteInteger(SAUT_OTPRAV_DIST_ADDR, 0);
        SafeWriteWord(KLUB_WHITE_SPEED_ADDR, 40);
      end;
    end;
    
    // === КНОПКА ЧТЕНИЯ СКОРОСТИ ТРЕКА ===
    if IsButtonPressed(BTN_TRACK_SPD_CURR, BTN_TRACK_SPD_PREV) then
    begin
      myTrack := SafeReadInteger(MY_TRACK_ADDR);
      maxTracks := SafeReadInteger(MAX_TRACKS_ADDR);
      route := SafeReadInteger(ROUTE_ADDR);
      
      if (myTrack > 0) and (maxTracks > 0) then
      begin
        trackDataBase := TRACK_DATA_BASE_ADDR;
        trackRecord := SafeReadInteger(trackDataBase + route * 4);
        
        if trackRecord <> 0 then
        begin
          trackSpeedLimit := SafeReadWord(trackRecord + myTrack * 76 + 36);
          
          if (trackSpeedLimit > 0) and 
             (trackSpeedLimit > SafeReadWord(SAUT_SPEED_LIMIT_ADDR)) then
          begin
            SafeWriteWord(SAUT_SS_SPEED_ADDR, trackSpeedLimit);
            LogMessage('Track speed limit read: ' + IntToStr(trackSpeedLimit));
          end;
        end;
      end;
    end;
    
    // === КНОПКА ТОРМОЖЕНИЯ ALSN ===
    if IsButtonPressed(BTN_BRAKE_CURR, BTN_BRAKE_PREV) and
       (ls = 3) and (SafeReadWord(KLUB_SPEED_LIMIT_ADDR) < 20) then
    begin
      sautAimDist := SafeReadWord(SAUT_AIM_DIST_ADDR);
      sautPodtDist := Round(traversedPath) + sautAimDist + 600;
      SafeWriteInteger(SAUT_PODT_DIST_ADDR, sautPodtDist);
      SafeWriteWord(ALSN_MODE_ADDR, 0);
      LogMessage('ALSN brake activated');
    end;
    
    // === СБРОС СКОРОСТИ ТРЕКА ===
    if (myTrack > 0) and (maxTracks > 0) then
    begin
      trackDataBase := TRACK_DATA_BASE_ADDR;
      trackRecord := SafeReadInteger(trackDataBase + route * 4);
      
      if trackRecord <> 0 then
      begin
        trackSpeedLimit := SafeReadWord(trackRecord + myTrack * 76 + 36);
        sautSSSpeed := SafeReadWord(SAUT_SS_SPEED_ADDR);
        
        if (trackSpeedLimit > 0) and (trackSpeedLimit <> sautSSSpeed) and (sautSSSpeed > 0) then
        begin
          SafeWriteWord(SAUT_SS_SPEED_ADDR, 0);
        end;
      end;
    end;
    
    // === УСТАНОВКА ФЛАГОВ ===
    
    // Флаг ЭК
    if SafeReadByte(KLUB_TRACK_ADDR) > 0 then
      SafeWriteByte(KLUB_EK_ADDR, 1)
    else
      SafeWriteByte(KLUB_EK_ADDR, 0);
    
    // Сброс K4 при движении
    if absSpeedKph > 2.0 then
      SafeWriteByte(KLUB_K4_ADDR, 0);
    
    LogMessage('KLUB: Keys processed successfully');
    
  except
    on E: Exception do
      LogMessage('KLUB: Error processing keys: ' + E.Message);
  end;
end;

// === ГЛАВНЫЕ ФУНКЦИИ ===
procedure WORLD_PROCESSKLUBKEYS(); stdcall;
begin
  ProcessKlubKeys();
end;

procedure WORLD_PROCESSKLUBKEYS_HOOK(); stdcall;
asm
  // Сохраняем ВСЕ регистры и флаги процессора
  pushfd                    // Сохраняем флаги EFLAGS
  pushad                    // Сохраняем EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI
  
  // Сохраняем дополнительные регистры FPU (если используются)
  sub esp, 108              // Место для FPU state
  fsave [esp]               // Сохраняем состояние FPU
  
  // Вызываем нашу Pascal функцию
  call WORLD_PROCESSKLUBKEYS
  
  // Восстанавливаем состояние FPU
  frstor [esp]              // Восстанавливаем состояние FPU
  add esp, 108              // Освобождаем место
  
  // Восстанавливаем регистры и флаги
  popad                     // Восстанавливаем все регистры
  popfd                     // Восстанавливаем флаги EFLAGS
  
  // Функция завершена, ret выполнится автоматически
end;

// === ИНИЦИАЛИЗАЦИЯ ===
initialization
  LogMessage('KLUB Processor Unit: Initialized v' + HOOK_VERSION_STRING);
  LogMessage('KLUB Processor Unit: Ready for hook installation');

finalization
  LogMessage('KLUB Processor Unit: Finalized');

end.
