unit BoosterBLOKRuntime;

interface

uses Windows;

var
  BLOCKModelID: Integer;
  BLOCKDisplayModelID: Integer;
  BLOCKTextureID: Integer;
  BLOCKPSSModelID: Integer;
  BLOCKInitialized: Boolean = False;
  BLOCKPatchApplied: Boolean = False;
  BlockKeyboardTexture: Cardinal = 0;
  BlockKeyboardCurrentOffset: Single = 210;
  BlockKeyboardTargetOffset: Single = 210;
  BlockKeyboardInitialized: Boolean = False;
  BlockKeyboardFileExists: Boolean = False;
  BlockKeyboardSoundID: Integer = -1;
  ScreenWidth: Integer = 1920;
  ScreenHeight: Integer = 1080;
  ButtonHovered: array[0..23] of Boolean;
  ButtonPositions: array[0..23] of record X, Y: Integer; end;
  ButtonPState: Integer = 0;
  InputBuffer: string = '';
  K123State: Integer = 0;
  K123Timer: Cardinal = 0;
  K123Active: Boolean = False;
  RMPState: Byte = 0;
  BlinkTimer: Cardinal = 0;
  BlinkVisible: Boolean = True;

function ApplyBLOKPatchRuntime: Boolean;
function CheckBLOKFilesExistRuntime(LocType: Integer; const LocNumber: string): Boolean;
function GetStateBLOCKRuntime: Byte;
procedure UpdateK123TimerRuntime;
procedure ProcessBlockClearRuntime;
procedure ProcessBlockPRuntime;
procedure ProcessBlockNumberRuntime(ButtonIndex: Integer);
procedure ProcessBlockEnterRuntime;
procedure ProcessBlockPCycleRuntime(ActionType: Integer; ButtonIndex: Integer = -1);
procedure ProcessBlockVKRuntime;
procedure InitializeBlockButtonPositions;
procedure UpdateBlockButtonHoverStates(MouseX, MouseY, KeyboardX, KeyboardY: Integer);
procedure DrawBlockTransparentButtons(KeyboardX, KeyboardY: Integer);
procedure DrawBlockKeyboard;
procedure DrawBLOCKRuntime(x: Single; y: Single; z: Single; AngZ: Single);
procedure ReinitializeBLOCKRuntime;
procedure ProcessButtonRMP;
function ShouldShowRMPText: Boolean;
function GetRezim: string;
function HandleBlockKeyboardClickRuntime(mouseX, mouseY: Integer): Boolean;

implementation

uses DrawFunc3D, DrawFunc2D, RA3, CheatMenu, OpenGL, MMSystem, Variables, Textures, KlubData, BoosterMemory, EngineUtils, LocomotiveHookRegistry, SysUtils;

function ApplyBLOKPatchRuntime: Boolean;
var
  CurrentLocType: Integer;
  PatchAddress, DrawBLOKAddress: Cardinal;
  NewOffset: Integer;
  OldProtect: DWORD;
begin
  Result := False;
  try
    CurrentLocType := GetLocomotiveTypeFromMemory;
    PatchAddress := BLOKPatchOffset(CurrentLocType);
    if (PatchAddress = 0) and ((CurrentLocType = 822) or
      (CurrentLocType = 812) or (CurrentLocType = 3154)) then Exit;
    if not CheckBLOKFilesExist(CurrentLocType, LocNum) then
    begin
      AddToLogFile(EngineLog, 'BLOK файлы не найдены, патч не применяется');
      Exit;
    end;
    DrawBLOKAddress := Cardinal(@DrawBLOCK);
    NewOffset := Integer(DrawBLOKAddress) - Integer(PatchAddress + 5);
    if VirtualProtect(Pointer(PatchAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      PInteger(PatchAddress + 1)^ := NewOffset;
      VirtualProtect(Pointer(PatchAddress + 1), 4, OldProtect, OldProtect);
      Result := True;
      AddToLogFile(EngineLog, 'BLOK патч применен успешно');
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при применении BLOK патча: ' + E.Message);
  end;
end;

function CheckBLOKFilesExistRuntime(LocType: Integer; const LocNumber: string): Boolean;
var
  LocoPath: string;
begin
  Result := False;
  try
    LocoPath := 'data\' + GetLocomotiveFolder(LocType) + '\' + LocNumber + '\blok\';
    Result := FileExists(LocoPath + 'BI-BLOK.dmd') and
      FileExists(LocoPath + 'blok.bmp') and
      FileExists(LocoPath + 'BI-blok-displ.dmd');
  except
    Result := False;
  end;
end;

function GetStateBLOCKRuntime: Byte;
begin
  Result := PByte(Pointer($00400000 + $34988C))^;
end;

procedure UpdateK123TimerRuntime;
var CurrentTime: Cardinal;
begin
  if not K123Active then Exit;
  CurrentTime := GetTickCount;
  if (CurrentTime - K123Timer) >= 4000 then
  begin
    WriteByteToMemory(Pointer($0074988C), 0);
    K123Active := False;
    K123Timer := 0;
    K123State := 0;
    InputBuffer := '';
  end;
end;

procedure ProcessBlockClearRuntime;
begin
  InputBuffer := '';
end;

procedure ProcessBlockPRuntime;
begin
  if ButtonPState = 0 then
  begin
    WriteByteToMemory(Pointer($0074988C), 20);
    ButtonPState := 1;
    InputBuffer := '';
  end;
end;

procedure ProcessBlockNumberRuntime(ButtonIndex: Integer);
var NumberStr: string;
begin
  if ButtonPState = 0 then Exit;
  case ButtonIndex of
    1: NumberStr := '1'; 2: NumberStr := '2'; 3: NumberStr := '3';
    7: NumberStr := '4'; 8: NumberStr := '5'; 9: NumberStr := '6';
    13: NumberStr := '7'; 14: NumberStr := '8'; 15: NumberStr := '9';
    20: NumberStr := '0';
    else Exit;
  end;
  if Length(InputBuffer) < 3 then InputBuffer := InputBuffer + NumberStr;
end;

procedure ProcessBlockEnterRuntime;
var Number: Integer;
begin
  if (ButtonPState = 0) or (InputBuffer = '') then Exit;
  Number := StrToIntDef(InputBuffer, 0);
  if ButtonPState = 1 then
  begin
    if (Number >= 0) and (Number <= 127) then
    begin
      WriteByteToMemory(Pointer(_dlgPathNumber), Byte(Number));
      WriteByteToMemory(Pointer(_dlgCommandBlock), 21);
      ButtonPState := 2;
      InputBuffer := '';
    end
    else InputBuffer := '';
  end
  else if ButtonPState = 2 then
  begin
    if (Number = 0) or (Number = 1) then
    begin
      WriteByteToMemory(Pointer(_dlgPathDirection), Byte(Number));
      WriteByteToMemory(Pointer(_dlgCommandBlock), 0);
      ButtonPState := 0;
    end;
    InputBuffer := '';
  end;
end;

procedure ProcessBlockPCycleRuntime(ActionType: Integer; ButtonIndex: Integer = -1);
var
  NumberStr: string;
  Number: Integer;
  currentState: Byte;
  NumberValue: LongWord;
begin
  // ActionType: 0 = нажатие кнопки К, 1 = ввод цифры, 2 = нажатие ВВОД
  
  // Читаем текущее состояние из памяти
  currentState := PByte($0074988C)^;
  
  case ActionType of
    0: begin // Нажатие кнопки "К"
         AddToLogFile(EngineLog, '[КНОПКА К] Обработка нажатия кнопки К');
         try
           // Записываем специальное состояние 30 = ожидание кода
           WriteByteToMemory(Pointer($0074988C), 30);
           InputBuffer := '';
           AddToLogFile(EngineLog, '[КНОПКА К] ✓ Установлено состояние 30 (ожидание кода), буфер сброшен');
         except
           on E: Exception do
             AddToLogFile(EngineLog, '[КНОПКА К] ✗ ОШИБКА: ' + E.Message);
         end;
       end;
       
    1: begin // Ввод цифры
         AddToLogFile(EngineLog, '[КНОПКА К - ЧИСЛО] Обработка ввода числа, индекс кнопки: ' + IntToStr(ButtonIndex) + ', состояние памяти: ' + IntToStr(currentState));
         
         // Проверяем что находимся в подходящем состоянии
         if not ((currentState = 30) or ((currentState >= 10) and (currentState <= 19)) or (currentState = 31) or (currentState = 52) or (currentState = 70) or (currentState = 71)) then
         begin
           AddToLogFile(EngineLog, '[КНОПКА К - ЧИСЛО] Игнорируем - неподходящее состояние: ' + IntToStr(currentState));
           Exit;
         end;
         
         // Определяем какая цифра нажата
         case ButtonIndex of
           1: NumberStr := '1';   // Кнопка 1
           2: NumberStr := '2';   // Кнопка 2  
           3: NumberStr := '3';   // Кнопка 3
           7: NumberStr := '4';   // Кнопка 4
           8: NumberStr := '5';   // Кнопка 5
           9: NumberStr := '6';   // Кнопка 6
           13: NumberStr := '7';  // Кнопка 7
           14: NumberStr := '8';  // Кнопка 8
           15: NumberStr := '9';  // Кнопка 9
           20: NumberStr := '0';  // Кнопка 0
           else
           begin
             AddToLogFile(EngineLog, '[КНОПКА К - ЧИСЛО] Неизвестная кнопка: ' + IntToStr(ButtonIndex));
             Exit;
           end;
         end;
         
         // Ограничиваем длину буфера до 6 цифр
         if Length(InputBuffer) >= 6 then
         begin
           AddToLogFile(EngineLog, '[КНОПКА К - ЧИСЛО] Буфер полный (6 символов), игнорируем ввод');
           Exit;
         end;
         
         // Добавляем цифру к буферу
         InputBuffer := InputBuffer + NumberStr;
         AddToLogFile(EngineLog, '[КНОПКА К - ЧИСЛО] Введена цифра: ' + NumberStr + ', буфер: "' + InputBuffer + '"');
       end;
       
    2: begin // Нажатие ВВОД
         AddToLogFile(EngineLog, '[КНОПКА К - ВВОД] Обработка кнопки ВВОД, состояние памяти: ' + IntToStr(currentState) + ', буфер: "' + InputBuffer + '"');
         
         // Обработка состояния 30 (ожидание кода)
         if currentState = 30 then
         begin
           AddToLogFile(EngineLog, '[К-ВВОД] Обработка команды К, буфер: "' + InputBuffer + '"');
           
           if InputBuffer = '7' then
           begin
             // Команда К7 - переход в состояние 10 (НОМЕР МАШИНИСТА)
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К7 - НОМЕР МАШИНИСТА');
             WriteByteToMemory(Pointer($0074988C), 10);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 10');
           end
           else if InputBuffer = '70' then
           begin
             // Команда К70 - переход в состояние 70 + запись в память
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К70');
             WriteByteToMemory(Pointer($0074988C), 70);
             WriteByteToMemory(Pointer($0538D95A), 0);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 70, записан 0 в 0x0538D95A');
           end
           else if InputBuffer = '71' then
           begin
             // КОМАНДА К71 - переход в состояние 71
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К71');
             WriteByteToMemory(Pointer($0074988C), 71);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 71');
           end
           else if InputBuffer = '122' then
           begin
             // КОМАНДА К122 - устанавливает 0538D95F в 1
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К122 - установка 0538D95F в 1');
             WriteByteToMemory(Pointer($0538D95F), 1);   // записываем 1 байт со значением 1
             WriteByteToMemory(Pointer($0074988C), 0);   // сразу возвращаемся в состояние 0
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ К122 выполнена: записана 1 в 0x0538D95F, возврат в состояние 0');
           end
           else if InputBuffer = '123' then
           begin
             // КОМАНДА К123 - сбрасывает 0538D95F в 0
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К123 - сброс 0538D95F в 0');
             WriteByteToMemory(Pointer($0538D95F), 0);   // записываем 1 байт со значением 0
             WriteByteToMemory(Pointer($0074988C), 0);   // сразу возвращаемся в состояние 0
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ К123 выполнена: записан 0 в 0x0538D95F, возврат в состояние 0');
           end
           else if InputBuffer = '137' then
           begin
             // НОВАЯ КОМАНДА К137 - переход в состояние 52
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К137 - управление АЛС');
             WriteByteToMemory(Pointer($0074988C), 52);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 52 (К137)');
           end
           else if InputBuffer = '799' then
           begin
             // КОМАНДА К799 - переход в состояние 31
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К799');
             WriteByteToMemory(Pointer($0074988C), 31);
             WriteDWordToMemory(Pointer($0538D960), 1);  // записываем 4 байта со значением 1
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 31, записана 1 в 0x0538D960');
           end
           else if InputBuffer = '800' then
           begin
             // КОМАНДА К800 - сброс 0538D960 в 0
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К800 - сброс 0538D960');
             WriteDWordToMemory(Pointer($0538D960), 0);  // записываем 4 байта со значением 0
             WriteByteToMemory(Pointer($0074988C), 0);   // сразу возвращаемся в состояние 0
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ К800 выполнена: записан 0 в 0x0538D960, возврат в состояние 0');
           end
           else
           begin
             // Неизвестная команда - сбрасываем в состояние 0
             AddToLogFile(EngineLog, '[К-ВВОД] ✗ Неизвестная команда: "' + InputBuffer + '", сброс');
             WriteByteToMemory(Pointer($0074988C), 0);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Сброс в состояние 0');
           end;
           Exit;
         end;
         
         // ОБРАБОТКА состояния 31 (ожидание числа для записи в 00749894) - К799
         if currentState = 31 then
         begin
           AddToLogFile(EngineLog, '[К-ВВОД] Обработка состояния 31 (К799), буфер: "' + InputBuffer + '"');
           
           if InputBuffer <> '' then
           begin
             NumberValue := StrToIntDef(InputBuffer, 0);
             AddToLogFile(EngineLog, '[К-ВВОД] Записываем число ' + IntToStr(NumberValue) + ' в 0x00749894');
             
             // Записываем введенное число как 4 байта в адрес 00749894
             WriteDWordToMemory(Pointer($00749894), NumberValue);
             
             // Сбрасываем состояние и буфер
             WriteByteToMemory(Pointer($0074988C), 0);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Состояние 31 завершено, сброс в 0');
           end
           else
           begin
             AddToLogFile(EngineLog, '[К-ВВОД] ✗ Буфер пустой для состояния 31');
             WriteByteToMemory(Pointer($0074988C), 0);
             InputBuffer := '';
           end;
           Exit;
         end;
         
         // НОВАЯ ОБРАБОТКА состояния 52 (К137 - управление АЛС)
         if currentState = 52 then
         begin
           AddToLogFile(EngineLog, '[К-ВВОД] Обработка состояния 52 (К137), буфер: "' + InputBuffer + '"');
           
           if InputBuffer <> '' then
           begin
             NumberValue := StrToIntDef(InputBuffer, 0);
             AddToLogFile(EngineLog, '[К-ВВОД] К137 - введенное число: ' + IntToStr(NumberValue));
             
             if NumberValue = 0 then
             begin
               // Ввели 0 - отключаем АЛС
               statek137 := False;
               als_en_state := False;
               AddToLogFile(EngineLog, '[К-ВВОД] ✓ К137(0): statek137=False, als_en_state=False');
             end
             else if (NumberValue >= 1) and (NumberValue <= 3) then
             begin
               // Ввели 1, 2 или 3 - включаем АЛС
               statek137 := True;
               als_en_state := True;
               AddToLogFile(EngineLog, '[К-ВВОД] ✓ К137(' + IntToStr(NumberValue) + '): statek137=True, als_en_state=True');
             end
             else
             begin
               AddToLogFile(EngineLog, '[К-ВВОД] ✗ К137: неверное число ' + IntToStr(NumberValue) + ', ожидается 0-3');
             end;
             
             // Сбрасываем состояние и буфер в любом случае
             WriteByteToMemory(Pointer($0074988C), 0);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Состояние 52 завершено, сброс в 0');
           end
           else
           begin
             AddToLogFile(EngineLog, '[К-ВВОД] ✗ Буфер пустой для состояния 52');
             WriteByteToMemory(Pointer($0074988C), 0);
             InputBuffer := '';
           end;
           Exit;
         end;
         
         // Обработка состояний 10-19 (продолжение цикла)
         if (currentState >= 10) and (currentState <= 19) then
         begin
           AddToLogFile(EngineLog, '[КНОПКА К - ВВОД] Продолжение цикла, текущее состояние: ' + IntToStr(currentState));
           
           // СПЕЦИАЛЬНАЯ ОБРАБОТКА ДЛЯ СОСТОЯНИЯ 13 (ДЛИНА В ВАГОНАХ)
           if currentState = 13 then
           begin
             if InputBuffer <> '' then
             begin
               NumberValue := StrToIntDef(InputBuffer, 0);
               // Умножаем на 4 и записываем по адресу 0x538D95C
               NumberValue := NumberValue * 4;
               AddToLogFile(EngineLog, '[СОСТОЯНИЕ 13] Записываем (число * 4): ' + IntToStr(NumberValue) + ' в 0x538D95C');
               WriteDWordToMemory(Pointer($538D95C), NumberValue);
             end
             else
             begin
               AddToLogFile(EngineLog, '[СОСТОЯНИЕ 13] Буфер пустой, записываем 0');
               WriteDWordToMemory(Pointer($538D95C), 0);
             end;
           end;
           
           // Очищаем буфер и переходим к следующему состоянию
           InputBuffer := '';
           
           if currentState = 19 then
           begin
             // После 19 переходим к 0
             WriteByteToMemory(Pointer($0074988C), 0);
             AddToLogFile(EngineLog, '[КНОПКА К - ВВОД] ✓ Переход с 19 на 0');
           end
           else
           begin
             // Увеличиваем состояние на 1
             WriteByteToMemory(Pointer($0074988C), currentState + 1);
             AddToLogFile(EngineLog, '[КНОПКА К - ВВОД] ✓ Переход с ' + IntToStr(currentState) + ' на ' + IntToStr(currentState + 1));
           end;
           Exit;
         end;
         
         // Обработка состояния 70 - К70
         if currentState = 70 then
         begin
           AddToLogFile(EngineLog, '[К-ВВОД] Обработка состояния 70 (К70), буфер: "' + InputBuffer + '"');
           InputBuffer := '';
           WriteByteToMemory(Pointer($0538D95A), 0);
           AddToLogFile(EngineLog, '[К-ВВОД] ✓ Записан 0 в 0x0538D95A для состояния 70');
           Exit;
         end;
         
         // ОБРАБОТКА состояния 71 - К71
         if currentState = 71 then
         begin
           AddToLogFile(EngineLog, '[К-ВВОД] Обработка состояния 71 (К71), буфер: "' + InputBuffer + '"');
           
           if InputBuffer <> '' then
           begin
             NumberValue := StrToIntDef(InputBuffer, 0);
             AddToLogFile(EngineLog, '[К-ВВОД] К71 - введенное число: ' + IntToStr(NumberValue));
             
             // Здесь можно добавить специальную логику для К71 если нужно
             // Например, записать число в определенный адрес памяти
             // WriteDWordToMemory(Pointer($АДРЕС_ДЛЯ_К71), NumberValue);
             
             // Сбрасываем состояние и буфер
             WriteByteToMemory(Pointer($0074988C), 0);
             InputBuffer := '';
             AddToLogFile(EngineLog, '[К-ВВОД] ✓ Состояние 71 завершено, сброс в 0');
           end
           else
           begin
             AddToLogFile(EngineLog, '[К-ВВОД] ✗ Буфер пустой для состояния 71');
             WriteByteToMemory(Pointer($0074988C), 0);
             InputBuffer := '';
           end;
           Exit;
         end;
         
         AddToLogFile(EngineLog, '[КНОПКА К - ВВОД] Игнорируем - неподходящее состояние: ' + IntToStr(currentState));
       end;
       
    else
      AddToLogFile(EngineLog, '[КНОПКА К] Неизвестный тип действия: ' + IntToStr(ActionType));
  end;
end;

// Обработка ввода цифр для команд К
procedure ProcessKNumberInput(ButtonIndex: Integer);
var
  NumberStr: string;
  currentState: Byte;
begin
  currentState := GetStateBLOCKRuntime;
  AddToLogFile(EngineLog, '[К-ЧИСЛО] Обработка ввода числа, индекс кнопки: ' + IntToStr(ButtonIndex) + ', состояние: ' + IntToStr(currentState));

  // Проверяем режим К123
  if K123State = 1 then
  begin
    // Определяем какая цифра нажата
    case ButtonIndex of
      1: NumberStr := '1';   // Кнопка 1
      2: NumberStr := '2';   // Кнопка 2  
      3: NumberStr := '3';   // Кнопка 3
      7: NumberStr := '4';   // Кнопка 4
      8: NumberStr := '5';   // Кнопка 5
      9: NumberStr := '6';   // Кнопка 6
      13: NumberStr := '7';  // Кнопка 7
      14: NumberStr := '8';  // Кнопка 8
      15: NumberStr := '9';  // Кнопка 9
      20: NumberStr := '0';  // Кнопка 0
      else
      begin
        AddToLogFile(EngineLog, '[К-ЧИСЛО] Неизвестная кнопка: ' + IntToStr(ButtonIndex));
        Exit;
      end;
    end;
    
    // Ограничиваем длину буфера до 3 цифр для К123
    if Length(InputBuffer) >= 3 then
    begin
      AddToLogFile(EngineLog, '[К-ЧИСЛО] Буфер К123 полный, игнорируем ввод');
      Exit;
    end;
    
    // Добавляем цифру к буферу
    InputBuffer := InputBuffer + NumberStr;
    AddToLogFile(EngineLog, '[К-ЧИСЛО] К123 - введена цифра: ' + NumberStr + ', буфер: "' + InputBuffer + '"');
    Exit;
  end;
  
  // Принимаем ввод только в состояниях 30, 10-19 и 31
  if not ((currentState = 30) or ((currentState >= 10) and (currentState <= 19)) or (currentState = 31)) then
  begin
    AddToLogFile(EngineLog, '[К-ЧИСЛО] Игнорируем - неподходящее состояние: ' + IntToStr(currentState));
    Exit;
  end;
  
  // Определяем какая цифра нажата
  case ButtonIndex of
    1: NumberStr := '1';   // Кнопка 1
    2: NumberStr := '2';   // Кнопка 2  
    3: NumberStr := '3';   // Кнопка 3
    7: NumberStr := '4';   // Кнопка 4
    8: NumberStr := '5';   // Кнопка 5
    9: NumberStr := '6';   // Кнопка 6
    13: NumberStr := '7';  // Кнопка 7
    14: NumberStr := '8';  // Кнопка 8
    15: NumberStr := '9';  // Кнопка 9
    20: NumberStr := '0';  // Кнопка 0
    else
    begin
      AddToLogFile(EngineLog, '[К-ЧИСЛО] Неизвестная кнопка: ' + IntToStr(ButtonIndex));
      Exit;
    end;
  end;
  
  // Ограничиваем длину буфера до 6 цифр
  if Length(InputBuffer) >= 6 then
  begin
    AddToLogFile(EngineLog, '[К-ЧИСЛО] Буфер полный (6 символов), игнорируем ввод');
    Exit;
  end;
  
  // Добавляем цифру к буферу
  InputBuffer := InputBuffer + NumberStr;
  AddToLogFile(EngineLog, '[К-ЧИСЛО] Введена цифра: ' + NumberStr + ', буфер: "' + InputBuffer + '"');
end;

// Обработка кнопки "ВВОД" для команд К
procedure ProcessKEnter;
var
  currentState: Byte;
  NumberValue: LongWord;
begin
  currentState := GetStateBLOCKRuntime;
  
  // Обработка состояния 30 (ожидание команды К)
  if currentState = 30 then
  begin
    AddToLogFile(EngineLog, '[К-ВВОД] Обработка команды К, буфер: "' + InputBuffer + '"');
    
    if InputBuffer = '7' then
    begin
      // Команда К7 - переход в состояние 10 (НОМЕР МАШИНИСТА)
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К7 - НОМЕР МАШИНИСТА');
      WriteByteToMemory(Pointer(_dlgCommandBlock), 10);
      InputBuffer := '';
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 10');
    end
    else if InputBuffer = '70' then
    begin
      // Команда К70 - переход в состояние 70 + запись в память
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К70');
      WriteByteToMemory(Pointer(_dlgCommandBlock), 70);
      WriteByteToMemory(Pointer($0538D95A), 0);
      InputBuffer := '';
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 70, записан 0 в 0x0538D95A');
    end
    else if InputBuffer = '799' then
    begin
      // НОВАЯ КОМАНДА К799 - переход в состояние 31
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Команда К799');
      WriteByteToMemory(Pointer($0074988C), 31);
      WriteDWordToMemory(Pointer($0538D960), 1);  // записываем 4 байта со значением 1
      InputBuffer := '';
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход в состояние 31, записана 1 в 0x0538D960');
    end
    else
    begin
      // Неизвестная команда - сбрасываем в состояние 0
      AddToLogFile(EngineLog, '[К-ВВОД] ✗ Неизвестная команда: "' + InputBuffer + '", сброс');
      WriteByteToMemory(Pointer(_dlgCommandBlock), 0);
      InputBuffer := '';
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Сброс в состояние 0');
    end;
    Exit;
  end;
  
  // НОВАЯ ОБРАБОТКА состояния 31 (ожидание числа для записи в 00749894)
  if currentState = 31 then
  begin
    AddToLogFile(EngineLog, '[К-ВВОД] Обработка состояния 31 (К799), буфер: "' + InputBuffer + '"');
    
    if InputBuffer <> '' then
    begin
      NumberValue := StrToIntDef(InputBuffer, 0);
      AddToLogFile(EngineLog, '[К-ВВОД] Записываем число ' + IntToStr(NumberValue) + ' в 0x00749894');
      
      // Записываем введенное число как 4 байта в адрес 00749894
      WriteDWordToMemory(Pointer($00749894), NumberValue);
      
      // Сбрасываем состояние и буфер
      WriteByteToMemory(Pointer($0074988C), 0);
      InputBuffer := '';
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Состояние 31 завершено, сброс в 0');
    end
    else
    begin
      AddToLogFile(EngineLog, '[К-ВВОД] ✗ Буфер пустой для состояния 31');
      WriteByteToMemory(Pointer($0074988C), 0);
      InputBuffer := '';
    end;
    Exit;
  end;
  
  // Обработка состояний 10-19 (цикл с переключением)
  if (currentState >= 10) and (currentState <= 19) then
  begin
    AddToLogFile(EngineLog, '[К-ВВОД] Цикл состояний, текущее: ' + IntToStr(currentState));
    InputBuffer := '';
    
    if currentState = 19 then
    begin
      // После 19 переходим к 0
      WriteByteToMemory(Pointer(_dlgCommandBlock), 0);
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход с 19 на 0');
    end
    else
    begin
      // Увеличиваем состояние на 1
      WriteByteToMemory(Pointer(_dlgCommandBlock), currentState + 1);
      AddToLogFile(EngineLog, '[К-ВВОД] ✓ Переход с ' + IntToStr(currentState) + ' на ' + IntToStr(currentState + 1));
    end;
    Exit;
  end;
  
  // Обработка состояния 70
  if currentState = 70 then
  begin
    AddToLogFile(EngineLog, '[К-ВВОД] Обработка состояния 70, буфер: "' + InputBuffer + '"');
    InputBuffer := '';
    WriteByteToMemory(Pointer($0538D95A), 0);
    AddToLogFile(EngineLog, '[К-ВВОД] ✓ Записан 0 в 0x0538D95A для состояния 70');
    Exit;
  end;
  
  AddToLogFile(EngineLog, '[К-ВВОД] Игнорируем - неподходящее состояние: ' + IntToStr(currentState));
end;

// Обработка нажатия кнопки ВК

procedure ProcessBlockVKRuntime;
var
  AlsValue: Integer;
begin
  try
    WriteByteToMemory(Pointer(_dlgVk), 0);
    AlsValue := GetALS;
    if (AlsValue = 2) and (PByte(Pointer(_dlgRb))^ = 1) and
      (PByte(Pointer(_dlgRbs))^ = 1) then
      WriteByteToMemory(Pointer(_dlgVk), 1);
  except
  end;
end;

procedure InitializeBlockButtonPositions;
begin
  // Ряд 1: П 1 2 3 К К20 (Y = 9)
  ButtonPositions[0].X := 29;   ButtonPositions[0].Y := 9;   // П
  ButtonPositions[1].X := 62;   ButtonPositions[1].Y := 9;   // 1
  ButtonPositions[2].X := 95;   ButtonPositions[2].Y := 9;   // 2
  ButtonPositions[3].X := 128;  ButtonPositions[3].Y := 9;   // 3
  ButtonPositions[4].X := 161;  ButtonPositions[4].Y := 9;   // К
  ButtonPositions[5].X := 194;  ButtonPositions[5].Y := 9;   // К20
  
  // Ряд 2: ВК 4 5 6 P OC (Y = 44)
  ButtonPositions[6].X := 29;   ButtonPositions[6].Y := 41;  // ВК
  ButtonPositions[7].X := 62;   ButtonPositions[7].Y := 41;  // 4
  ButtonPositions[8].X := 95;   ButtonPositions[8].Y := 41;  // 5
  ButtonPositions[9].X := 128;  ButtonPositions[9].Y := 41;  // 6
  ButtonPositions[10].X := 161; ButtonPositions[10].Y := 41; // P
  ButtonPositions[11].X := 194; ButtonPositions[11].Y := 41; // OC
  
  // Ряд 3: РМП 7 8 9 ОТМ ОТПР (Y = 79)
  ButtonPositions[12].X := 29;  ButtonPositions[12].Y := 75; // РМП
  ButtonPositions[13].X := 62;  ButtonPositions[13].Y := 75; // 7
  ButtonPositions[14].X := 95;  ButtonPositions[14].Y := 75; // 8
  ButtonPositions[15].X := 128; ButtonPositions[15].Y := 75; // 9
  ButtonPositions[16].X := 161; ButtonPositions[16].Y := 75; // ОТМ
  ButtonPositions[17].X := 194; ButtonPositions[17].Y := 75; // ОТПР
  
  // Ряд 4: F СТР 0 ВВОД о подтяг (Y = 105)
  ButtonPositions[18].X := 29;  ButtonPositions[18].Y := 105; // F
  ButtonPositions[19].X := 62;  ButtonPositions[19].Y := 105; // СТР
  ButtonPositions[20].X := 95;  ButtonPositions[20].Y := 105; // 0
  ButtonPositions[21].X := 128; ButtonPositions[21].Y := 105; // ВВОД
  ButtonPositions[22].X := 161; ButtonPositions[22].Y := 105; // о
  ButtonPositions[23].X := 194; ButtonPositions[23].Y := 105; // подтяг
end;

// Обновление hover состояний кнопок

procedure UpdateBlockButtonHoverStates(mouseX, mouseY: Integer; keyboardX, keyboardY: Integer);
var
  i: Integer;
  buttonX, buttonY: Integer;
  relativeMouseX, relativeMouseY: Integer;
begin
  // Обновляем таймер К123
  UpdateK123TimerRuntime;
  
  relativeMouseX := mouseX - keyboardX;
  relativeMouseY := mouseY - keyboardY;
  
  for i := 0 to 23 do
  begin
    buttonX := ButtonPositions[i].X;
    buttonY := ButtonPositions[i].Y;
    
    ButtonHovered[i] := (relativeMouseX >= buttonX) and 
                        (relativeMouseX <= buttonX + 24) and
                        (relativeMouseY >= buttonY) and 
                        (relativeMouseY <= buttonY + 24);
  end;
end;

// Отрисовка прозрачных кнопок с hover эффектом

procedure DrawBlockTransparentButtons(keyboardX, keyboardY: Integer);
var
  i: Integer;
  buttonX, buttonY: Integer;
  alpha: Byte;
  color: Cardinal;
begin
  for i := 0 to 23 do
  begin
    if not ButtonHovered[i] then
      Continue;
    
    buttonX := keyboardX + ButtonPositions[i].X;
    buttonY := keyboardY + ButtonPositions[i].Y;
    
    alpha := 140;
    color := $4080FF;
    
    Begin2D;
    try
      glDisable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      
      glColor4f(
        ((color shr 16) and $FF) / 255.0,
        ((color shr 8) and $FF) / 255.0,
        (color and $FF) / 255.0,
        alpha / 255.0
      );
      
      glBegin(GL_QUADS);
        glVertex2f(buttonX, buttonY);
        glVertex2f(buttonX + 24, buttonY);
        glVertex2f(buttonX + 24, buttonY + 24);
        glVertex2f(buttonX, buttonY + 24);
      glEnd;
      
      glColor4f(
        ((color shr 16) and $FF) / 255.0,
        ((color shr 8) and $FF) / 255.0,
        (color and $FF) / 255.0,
        (alpha + 60) / 255.0
      );
      
      glLineWidth(1.5);
      glBegin(GL_LINE_LOOP);
        glVertex2f(buttonX, buttonY);
        glVertex2f(buttonX + 24, buttonY);
        glVertex2f(buttonX + 24, buttonY + 24);
        glVertex2f(buttonX, buttonY + 24);
      glEnd;
      
      glDisable(GL_BLEND);
      glEnable(GL_TEXTURE_2D);
      glColor4f(1.0, 1.0, 1.0, 1.0);
      
    finally
      End2D;
    end;
  end;
end;

procedure DrawBlockKeyboard;
var
  keyboardX, keyboardY: Integer;
  isMouseOver: Boolean;
  mousePos: TPoint;
  texturePath: string;
  settingsPath: string;
  difference: Single;
  triggerX, triggerY: Integer;
  settingsFile: TextFile;
  line: string;
  equalPos: Integer;
  paramName, paramValue: string;
begin
  // Обновляем таймер К123 при каждой отрисовке
  UpdateK123TimerRuntime;
  
  if not BlockKeyboardInitialized then
  begin
    NopMemory(Pointer($00738844), 3);

    try
      InitializeBlockButtonPositions;
      FillChar(ButtonHovered, SizeOf(ButtonHovered), 0);

      settingsPath := ExtractFilePath(ParamStr(0)) + 'settings.ini';
      if FileExists(settingsPath) then
      begin
        AssignFile(settingsFile, settingsPath);
        Reset(settingsFile);
        try
          while not Eof(settingsFile) do
          begin
            ReadLn(settingsFile, line);
            line := Trim(line);
            
            if (line <> '') and (line[1] <> ';') and (line[1] <> '#') then
            begin
              equalPos := Pos('=', line);
              if equalPos > 0 then
              begin
                paramName := Trim(Copy(line, 1, equalPos - 1));
                paramValue := Trim(Copy(line, equalPos + 1, Length(line)));
                
                if paramName = 'ScreenWidth' then
                  ScreenWidth := StrToIntDef(paramValue, 1920);
                if paramName = 'ScreenHeight' then
                  ScreenHeight := StrToIntDef(paramValue, 1080);
              end;
            end;
          end;
        finally
          CloseFile(settingsFile);
        end;
        
        AddToLogFile(EngineLog, '[ИНИТ] Парсинг settings.ini: ScreenWidth=' + IntToStr(ScreenWidth) + ', ScreenHeight=' + IntToStr(ScreenHeight));
      end;
      
      texturePath := 'booster\blok_buttons.bmp';
      
      if FileExists(texturePath) then
      begin
        BlockKeyboardTexture := LoadTextureFromFile(texturePath, 0, -1);
        if BlockKeyboardTexture > 0 then
        begin
          BlockKeyboardFileExists := True;
          AddToLogFile(EngineLog, '[ИНИТ] ✓ Текстура клавиатуры БЛОК загружена');
        end
        else
        begin
          BlockKeyboardFileExists := False;
          AddToLogFile(EngineLog, '[ИНИТ] ✗ Ошибка загрузки текстуры');
        end;
      end
      else
      begin
        BlockKeyboardFileExists := False;
        AddToLogFile(EngineLog, '[ИНИТ] ✗ Файл текстуры не найден: ' + texturePath);
      end;
      
      // Проверяем звуковой файл
      if FileExists('booster\blok_pick.wav') then
      begin
        BlockKeyboardSoundID := 1;
        AddToLogFile(EngineLog, '[ИНИТ] ✓ Звуковой файл найден: booster\blok_pick.wav');
      end
      else
      begin
        BlockKeyboardSoundID := -1;
        AddToLogFile(EngineLog, '[ИНИТ] ✗ Файл звука клавиш не найден: booster\blok_pick.wav');
      end;
      
      BlockKeyboardInitialized := True;
      
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, '[ИНИТ] ✗ КРИТИЧЕСКАЯ ОШИБКА: ' + E.Message);
        BlockKeyboardInitialized := True;
        BlockKeyboardFileExists := False;
      end;
    end;
  end;
  
  if not BlockKeyboardFileExists then
    Exit;
    
  try
    if GetCursorPos(mousePos) then
    begin
      if ScreenToClient(GetActiveWindow(), mousePos) then
      begin
        keyboardX := ScreenWidth - 340 + Round(BlockKeyboardCurrentOffset);
        triggerX := ScreenWidth - 233 + Round(BlockKeyboardCurrentOffset) - 5;
        triggerY := ScreenHeight - 250;
        
        isMouseOver := (mousePos.X >= triggerX) and 
                       (mousePos.X <= ScreenWidth) and
                       (mousePos.Y >= triggerY) and 
                       (mousePos.Y <= triggerY + 136);
      end
      else
        isMouseOver := False;
    end
    else
      isMouseOver := False;
    
    if isMouseOver then
      BlockKeyboardTargetOffset := 0
    else
      BlockKeyboardTargetOffset := 210;
    
    difference := BlockKeyboardTargetOffset - BlockKeyboardCurrentOffset;
    if Abs(difference) > 1.0 then
      BlockKeyboardCurrentOffset := BlockKeyboardCurrentOffset + (difference * 0.12)
    else
      BlockKeyboardCurrentOffset := BlockKeyboardTargetOffset;
    
    keyboardX := ScreenWidth - 230 + Round(BlockKeyboardCurrentOffset);
    keyboardY := ScreenHeight - 250;
    
    if GetCursorPos(mousePos) and ScreenToClient(GetActiveWindow(), mousePos) then
      UpdateBlockButtonHoverStates(mousePos.X, mousePos.Y, keyboardX, keyboardY);
    
    Begin2D;
    try
      DrawTexture2D(
        BlockKeyboardTexture,
        keyboardX,
        keyboardY,
        340, 136, 0, 255, $FFFFFF, False
      );
    finally
      End2D;
    end;
    
    DrawBlockTransparentButtons(keyboardX, keyboardY);
    
  except
    on E: Exception do
      AddToLogFile(EngineLog, '[РИСОВАНИЕ] ✗ Ошибка: ' + E.Message);
  end;
end;

procedure DrawBLOCKRuntime(x: Single; y: Single; z: Single; AngZ: Single);
var
  pressureMode: Byte;
  scaleFactor: Single;
  maxScaleValue: Single;
  scaleStep: Single;

  // Переменные для барграфов
  barWidth: Single;
  barTop: Single;
  barBottom: Single;
  scaleStepZ: Single;

  function CheckBLOCKFiles: Boolean;
  begin
    Result := CheckBLOKFilesExistRuntime(GetLocomotiveTypeFromMemory, LocNum);
  end;
  
  // Внутренняя процедура инициализации моделей
  procedure InitBLOCKModels;
  var
    currentLocType: Integer;
    locFolder, blockPath: string;
    blockModelPath, blockDisplayModelPath, blockTexturePath: string;
  begin
    if BLOCKInitialized then Exit;
    
    try
      currentLocType := GetLocomotiveTypeFromMemory;
      locFolder := GetLocomotiveFolder(currentLocType);
      blockPath := 'data\' + locFolder + '\' + GetLocNum + '\blok\';
      
      AddToLogFile(EngineLog, '=== ИНИЦИАЛИЗАЦИЯ BLOCK ===');
      AddToLogFile(EngineLog, 'Тип локомотива: ' + IntToStr(currentLocType));
      AddToLogFile(EngineLog, 'Папка локомотива: ' + locFolder);
      AddToLogFile(EngineLog, 'Номер: ' + GetLocNum);
      AddToLogFile(EngineLog, 'Путь BLOCK: ' + blockPath);
      
      blockModelPath := blockPath + 'BI-BLOK.dmd';
      blockDisplayModelPath := blockPath + 'BI-blok-displ.dmd';
      blockTexturePath := blockPath + 'blok.bmp';
      
      if not CheckBLOCKFiles then
      begin
        AddToLogFile(EngineLog, 'BLOCK файлы не найдены, инициализация отменена');
        Exit;
      end;

      //NopMemory(Pointer($0073880D), 5);

      BLOCKModelID := LoadModel(blockModelPath, 0, False);
      if BLOCKModelID > 0 then
        AddToLogFile(EngineLog, 'BLOCK модель загружена, ID: ' + IntToStr(BLOCKModelID))
      else
      begin
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить BLOCK модель: ' + blockModelPath);
        Exit;
      end;



      BLOCKPSSModelID := LoadModel(blockPath + 'blok_displ_pss.dmd', 0, False);
      if BLOCKModelID > 0 then
        AddToLogFile(EngineLog, 'BLOCKPSSModelID модель загружена, ID: ' + IntToStr(BLOCKModelID))
      else
      begin
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить BLOCKPSSModelID модель: ' + blockModelPath);
        Exit;
      end;

      BLOCKDisplayModelID := LoadModel(blockDisplayModelPath, 0, False);
      if BLOCKDisplayModelID > 0 then
        AddToLogFile(EngineLog, 'BLOCK модель дисплея загружена, ID: ' + IntToStr(BLOCKDisplayModelID))
      else
      begin
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить BLOCK модель дисплея: ' + blockDisplayModelPath);
        Exit;
      end;
      
      BLOCKTextureID := LoadTextureFromFile(blockTexturePath, 0, -1);
      if BLOCKTextureID > 0 then
        AddToLogFile(EngineLog, 'BLOCK текстура загружена, ID: ' + IntToStr(BLOCKTextureID))
      else
      begin
        AddToLogFile(EngineLog, 'ОШИБКА: Не удалось загрузить BLOCK текстуру: ' + blockTexturePath);
        Exit;
      end;
      
      BLOCKInitialized := True;
      AddToLogFile(EngineLog, 'BLOCK инициализация завершена успешно');
      
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'КРИТИЧЕСКАЯ ОШИБКА инициализации BLOCK: ' + E.Message);
        BLOCKInitialized := False;
      end;
    end;
  end;

  // Функция применения NOP патча
  function ApplyNOPPatch(patchAddress: Cardinal; size: Integer): Boolean;
  var
    OldProtect: DWORD;
    i: Integer;
  begin
    Result := False;
    
    if VirtualProtect(Pointer(patchAddress), size, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      try
        for i := 0 to size - 1 do
          PByte(patchAddress + i)^ := $90;
          
        VirtualProtect(Pointer(patchAddress), size, OldProtect, OldProtect);
        Result := True;
        AddToLogFile(EngineLog, 'NOP патч применен успешно');
      except
        on E: Exception do
        begin
          AddToLogFile(EngineLog, 'ОШИБКА применения NOP патча: ' + E.Message);
          Result := False;
        end;
      end;
    end
    else
      AddToLogFile(EngineLog, 'ОШИБКА: Не удалось изменить защиту памяти для NOP патча');
  end;

  // Процедура применения патча
  procedure ApplyBLOCKPatch;
  var
    currentLocType: Integer;
    patchAddress: Cardinal;
    OldProtect: DWORD;
  begin
    if BLOCKPatchApplied then Exit;
    
    try
      currentLocType := GetLocomotiveTypeFromMemory;

      ApplyNOPPatch($00738588, 3);

  VirtualProtect(Pointer($00484AF5 + 2), 1, PAGE_EXECUTE_READWRITE, OldProtect);
  PByte($00484AF5 + 2)^ := $02;
  FlushInstructionCache(GetCurrentProcess, Pointer($00484AF5 + 2), 1);
  VirtualProtect(Pointer($00484AF5 + 2), 1, OldProtect, OldProtect);

//      case currentLocType of
//        822: // ЧС7
//        begin
//          patchAddress := $00677AB3;
//          AddToLogFile(EngineLog, '=== ПРИМЕНЕНИЕ BLOCK ПАТЧА ===');
//          AddToLogFile(EngineLog, 'Тип локомотива: ЧС7 (822)');
//          AddToLogFile(EngineLog, 'Адрес патча: $' + IntToHex(patchAddress, 8));
//          
//          if ApplyNOPPatch(patchAddress, 5) then
//          begin
//            BLOCKPatchApplied := True;
//            AddToLogFile(EngineLog, 'BLOCK патч для ЧС7 применен успешно');
//          end
//          else
//            AddToLogFile(EngineLog, 'ОШИБКА применения BLOCK патча для ЧС7');
//        end;
//        812: // ЧС8
//        begin
//          if ApplyNOPPatch($4D835F, 5) then
//          begin
//            BLOCKPatchApplied := True;
//            AddToLogFile(EngineLog, 'BLOCK патч для ЧС8 применен успешно');
//          end;
//        end;
//        3154: // ЭД4М
//        begin
//          if ApplyNOPPatch($6297EF, 5) then
//          begin
//            BLOCKPatchApplied := True;
//            AddToLogFile(EngineLog, 'BLOCK патч для ЭД4М применен успешно');
//          end;
//        end;
//        621: // ЧС4Т
//        begin
//          if ApplyNOPPatch($5DF68A, 5) then
//          begin
//            BLOCKPatchApplied := True;
//            AddToLogFile(EngineLog, 'BLOCK патч для ЭД4М применен успешно');
//          end;
//        end;
//        880:
//        begin // ВЛ80Т
//          if ApplyNOPPatch($58E8D2, 5) then
//          begin
//            BLOCKPatchApplied := True;
//            AddToLogFile(EngineLog, 'BLOCK патч для ЭД4М применен успешно');
//          end;
//        end;
//        2070: // ТЭП70
//        begin
//          if ApplyNOPPatch($681B04, 5) then
//          begin
//            BLOCKPatchApplied := True;
//            AddToLogFile(EngineLog, 'BLOCK патч для ЭД4М применен успешно');
//          end;
//        end;
//        885: // ВЛ85
//        begin
//          if ApplyNOPPatch($6C41FE, 5) then
//          begin
//            BLOCKPatchApplied := True;
//            AddToLogFile(EngineLog, 'BLOCK патч для ЭД4М применен успешно');
//          end;
//        end;
//        else
//          AddToLogFile(EngineLog, 'BLOCK патч не поддерживается для типа локомотива: ' + IntToStr(currentLocType));
//      end;
      
    except
      on E: Exception do
        AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при применении BLOCK патча: ' + E.Message);
    end;
  end;

  // Процедура отрисовки текста
  procedure DrawTextSimple(posX, posY, posZ: Single; scale: Single; text: string);
  begin
    BeginObj3D;
    glDisable(GL_LIGHTING);
    Position3D(posX, posY, posZ);
    RotateX(-90);
    Scale3D(scale);
    Color3D($FFFFFF, 255, False, 0.0);
    SetTexture(0);
    DrawText3D(0, text);
    glEnable(GL_LIGHTING);
    EndObj3D;
  end;

  // Процедура отрисовки шкалы давления с правильными координатами
  procedure DrawPressureScale(barX: Single);
  var
    i: Integer;
    val: Single;
    posZ, posX: Single;
  begin
    val := maxScaleValue;
    posZ := barTop;
    
    for i := 0 to 5 do
    begin
      // Используем точные координаты из оригинала для каждого барграфа
      if barX = 0.0676 then // ТЦ
      begin
        if val = maxScaleValue then
          posX := 0.055
        else
          posX := 0.057;
      end
      else if barX = 0.0896 then // ТМ
      begin
        if val = maxScaleValue then
          posX := 0.077
        else
          posX := 0.079;
      end
      else if barX = 0.111 then // УР
      begin
        if val = maxScaleValue then
          posX := 0.099
        else
          posX := 0.101;
      end
      else
      begin
        // Резервный вариант для других координат
        if val = maxScaleValue then
          posX := barX - 0.0126
        else
          posX := barX - 0.0106;
      end;
      
      DrawTextSimple(posX, 0, posZ, 0.0044, FormatFloat('0.00', val));
      
      val := val - scaleStep;
      posZ := posZ - scaleStepZ;
    end;
  end;

  // Процедура отрисовки барграфа
  procedure DrawPressureBarGraph(barX: Single; barValue: Single);
  var
    barCurrentHeight: Single;
  begin
    if barValue > maxScaleValue then 
      barValue := maxScaleValue;

    if barValue > 0 then
    begin
      barCurrentHeight := barBottom + ((barTop - barBottom) * (barValue / maxScaleValue));
      
      SetTexture(0);
      glDisable(GL_LIGHTING);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glColor4f(0.0, 0.0, 1.0, 0.9);

      glBegin(GL_QUADS);
        glVertex3f(barX - barWidth/2, 0, barBottom);
        glVertex3f(barX + barWidth/2, 0, barBottom);
        glVertex3f(barX + barWidth/2, 0, barCurrentHeight);
        glVertex3f(barX - barWidth/2, 0, barCurrentHeight);
      glEnd;

      glDisable(GL_BLEND);
      glEnable(GL_LIGHTING);
      glColor4f(1.0, 1.0, 1.0, 1.0);
    end;
  end;

  // Процедура отрисовки подписей барграфа
  procedure DrawPressureLabels(barX: Single; barLabel: string; barValue: Single);
  begin
    DrawTextSimple(barX - 0.008, 0, barBottom - 0.007, 0.005, barLabel);

    if pressureMode = 0 then
      DrawTextSimple(barX - 0.008, 0, barBottom - 0.0135, 0.005, FormatFloat('0.0', barValue * scaleFactor))
    else
      DrawTextSimple(barX - 0.009, 0, barBottom - 0.0135, 0.005, FormatFloat('0.00', barValue));

    DrawTextSimple(barX - 0.009, 0, barBottom - 0.02, 0.005, 'КГС');
  end;

  // Полная процедура для одного барграфа
  procedure DrawCompletePressureBar(barX: Single; barLabel: string; barValue: Single);
  begin
    DrawPressureScale(barX);
    DrawPressureBarGraph(barX, barValue);
    DrawPressureLabels(barX, barLabel, barValue);
  end;

  // Процедура отрисовки всех информационных полей
  procedure DrawAllInfoFields;
  var
    inputText: string;
    currentTrackNumber: Integer;
    patchValue: Byte;
  begin
    // Основная информация
    DrawTextSimple(-0.11, 0, 0.247, 0.007, GetCoordinatesFormatted);
    DrawTextSimple(-0.07, 0, 0.247, 0.007, Copy(GetCurrentStation, 1, 8));
    DrawTextSimple(-0.022, 0, 0.247, 0.007, GetCurrentTime);
    DrawTextSimple(0.01, 0, 0.247, 0.007, GetRezim);
    if GetTrackNumberInt > 0 then
      DrawTextSimple(-0.11, 0, 0.233, 0.007, 'ЭК')
    else
      DrawTextSimple(-0.11, 0, 0.233, 0.007, GetChannel);
    if GetTrackNumberInt > 0 then
      DrawTextSimple(-0.095, 0, 0.233, 0.007, GetTrackWithDirection)
    else
      DrawTextSimple(-0.095, 0, 0.233, 0.007, '0');
    DrawTextSimple(-0.105, 0, 0.216, 0.007, GetAcceleration);
    DrawTextSimple(-0.105, 0, 0.199, 0.007, GetDistance);
    DrawTextSimple(-0.105, 0, 0.182, 0.007, '0.67');
    DrawTextSimple(-0.110, 0, 0.092, 0.0055, GetTargetType);
    if GetTrackNumberInt > 0 then
    begin
      DrawTextSimple(0.035, 0, 0.092, 0.006, GetDistance + ' м');
      DrawTextSimple(0.002, 0, 0.092, 0.006, GetSvetoforValue);
    end;




begin
  currentTrackNumber := GetTrackNumberInt;
  
  // Статическая переменная для отслеживания последнего значения
  if currentTrackNumber <> LastTrackDirection then
  begin
    if currentTrackNumber > 0 then
      patchValue := 1
    else
      patchValue := 0;
      
    WriteByteToMemory(Pointer($400000 + $83F12), patchValue);
    LastTrackDirection := currentTrackNumber;
  end;
end;

try
  if PByte($0074AC58)^ = 0 then
  begin
    BeginObj3D;
    glDisable(GL_LIGHTING);
    Position3D(0.00, 0.00, 0.00);
    SetTexture(0);
    Color3D($0000FF, 255, False, 0);
    DrawModel(BLOCKPSSModelID, 0, True);
    glEnable(GL_LIGHTING);
    EndObj3D;
  end;
except
  // если будет ошибка доступа к памяти — игнорируем
end;

    // Поле ввода
    case GetStateBLOCKRuntime of
      10: inputText := 'НОМЕР МАШИНИСТА ' + InputBuffer + '_';
      11: inputText := 'НОМЕР ПОЕЗДА ' + InputBuffer + '_';
      12: inputText := 'ДЛИНА В ОСЯХ ' + InputBuffer + '_';
      13: inputText := 'ДЛИНА В ВАГОНАХ ' + InputBuffer + '_';
      14: inputText := 'МАССА ПОЕЗДА (Т) ' + InputBuffer + '_';
      15: inputText := 'СМЕЩЕНИЕ ЧАСОВ ' + InputBuffer + '_';
      16: inputText := 'ЗАМЕДЛЕНИЕ ПТ ' + InputBuffer + '_';
      17: inputText := 'ЗАМЕДЛЕНИЕ ЭПТ ' + InputBuffer + '_';
      18: inputText := 'НАЛИЧ.ПОМ.МАШ. ' + InputBuffer + '_';
      20: inputText := 'НОМЕР ПУТИ ' + InputBuffer + '_';
      21: inputText := 'ПРИЗНАК ПРАВ. ' + InputBuffer + '_';
      30: inputText := 'ВВЕДИТЕ КОМАНДУ ' + InputBuffer + '_';
      31: inputText := 'СКОРОСТЬ НА БЕЛЫЙ ' + InputBuffer + '_';
      52: inputText := 'ТАБЛИЦА АЛС-ЕН  ' + InputBuffer + '_';
      71: inputText := '123456789АВ';
      else
      begin
        if GetTrackNumberInt = 0 then
          inputText := 'РЕЖИМ БЕЗ ЭК'
        else
          inputText := '';
      end;
    end;
    
    if inputText <> '' then
      DrawTextSimple(-0.11, 0, 0.081, 0.007, inputText);
  end;

begin
  // Применяем патч при первом вызове
  if not BLOCKInitialized then
    ApplyBLOCKPatch;

  // Инициализируем модели
  if not BLOCKInitialized then
  begin
    InitBLOCKModels;
    if not BLOCKInitialized then
    begin
      AddToLogFile(EngineLog, 'BLOCK не инициализирован, отрисовка отменена');
      Exit;
    end;
  end;

  // Настройка параметров в зависимости от режима давления
  pressureMode := PByte($0538D95F)^;
  if pressureMode = 1 then
  begin
    scaleFactor := 0.1;
    maxScaleValue := 1.0;
    scaleStep := 0.2;
  end
  else
  begin
    scaleFactor := 1.0;
    maxScaleValue := 10.0;
    scaleStep := 2.0;
  end;

  // Константы для барграфов
  barWidth := 0.003;
  barTop := 0.191;
  barBottom := 0.101;
  scaleStepZ := 0.018;

  // Основная отрисовка
  try
    BeginObj3D();
    //Position3D(AngZ, z, y);
    //RotateZ(x);

    // RA3 block
    if IsRA3Active then
      ApplyRA3BlockTransform(x, y, z, AngZ)
    else
    begin
      Position3D(AngZ, z, y);
      RotateZ(x);
    end;
    SetTexture(BLOCKTextureID);



    // Отрисовываем модели
    DrawModel(BLOCKModelID, 0, True);
    
    glDisable(GL_LIGHTING);
    DrawModel(BLOCKDisplayModelID, 0, True);
    glEnable(GL_LIGHTING);

    // Отрисовываем все информационные поля
    DrawAllInfoFields;
    
    // Отрисовываем барграфы давления
    DrawCompletePressureBar(0.0676, 'ТЦ', GetPressureTCf * scaleFactor);
    DrawCompletePressureBar(0.0896, 'ТМ', GetPressureTMf * scaleFactor);
    DrawCompletePressureBar(0.111, 'УР', GetPressureURf * scaleFactor);
    
    // Дополнительные элементы
    DrawSpeedometer3D;
    DrawBlockKeyboard;

    EndObj3D();
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'Ошибка отрисовки BLOCK: ' + E.Message);
  end;

  // Кастомные 3D-тексты + гизмо. Один раз за кадр (внутренний дедуплекс).
  RenderCustomTextsAndGizmoForFrame;
end;

// Дополнительная функция для принудительной переинициализации (опционально)

procedure ReinitializeBLOCKRuntime;
begin
  BLOCKInitialized := False;
  BLOCKPatchApplied := False;
  BLOCKModelID := 0;
  BLOCKTextureID := 0;
  AddToLogFile(EngineLog, 'BLOCK система сброшена для переинициализации');
end;

procedure UpdateRMPBlink;
var
  currentTime: Cardinal;
  currentRMPState: Byte;
begin
  try
    // Читаем текущее состояние из памяти
    currentRMPState := PByte(_dlgMode)^;
    
    if currentRMPState = 2 then
    begin
      currentTime := GetTickCount;
      
      // Инициализируем таймер при первом входе в режим 2
      if BlinkTimer = 0 then
      begin
        BlinkTimer := currentTime;
        BlinkVisible := True;
      end;
      
      // Проверяем прошло ли 500 мс
      if (currentTime - BlinkTimer) >= 500 then
      begin
        BlinkVisible := not BlinkVisible; // Переключаем видимость
        BlinkTimer := currentTime; // Сбрасываем таймер
        // logging removed — called every 500ms
      end;
    end
    else
    begin
      // Если не в режиме 2, сбрасываем таймер и делаем видимым
      BlinkTimer := 0;
      BlinkVisible := True;
    end;
  except
    // В случае ошибки сбрасываем состояние
    BlinkTimer := 0;
    BlinkVisible := True;
  end;
end;

// ОБНОВЛЕННАЯ функция GetRezim с поддержкой мигания
function GetRezim: string;
var
  b: Byte;
begin
  try
    // Обновляем состояние мигания
    UpdateRMPBlink;
    
    // Читаем байт по адресу BaseAddress + $349888
    b := PByte(_dlgMode)^;
    case b of
      0: Result := 'П';
      1: Result := 'М';
      2: begin
           // При состоянии 2 мигаем
           if BlinkVisible then
             Result := 'П'
           else
             Result := '';  // Пустая строка когда не видим
         end;
    else
      Result := 'П';
    end;
  except
    // В случае ошибки возвращаем значение по умолчанию 'П'
    Result := 'П';
  end;
end;

function ShouldShowRMPText: Boolean;
begin
  UpdateRMPBlink; // Обновляем состояние мигания
  
  case RMPState of
    0: Result := False;        // Не показываем при состоянии 0
    1: Result := True;         // Всегда показываем при состоянии 1
    2: Result := BlinkVisible; // Мигаем при состоянии 2
    else Result := False;
  end;
end;

// Обработка нажатия кнопки РМП
procedure ProcessButtonRMP;
var
  currentSpeed: Single;
begin
  AddToLogFile(EngineLog, '[КНОПКА РМП] Обработка нажатия кнопки РМП');
  
  try
    // Проверяем текущую скорость
    currentSpeed := GetSpeedValue;
    AddToLogFile(EngineLog, '[КНОПКА РМП] Текущая скорость: ' + FormatFloat('0.00', currentSpeed));
    
    // Проверяем условие: скорость должна быть равна 0
    if currentSpeed <> 0.0 then
    begin
      AddToLogFile(EngineLog, '[КНОПКА РМП] ✗ РМП недоступна при движении (скорость: ' + FormatFloat('0.00', currentSpeed) + ')');
      Exit;
    end;
    
    // Циклическое переключение: 0 -> 1 -> 2 -> 0
    case RMPState of
      0: RMPState := 1;
      1: RMPState := 2;
      2: RMPState := 0;
      else RMPState := 1; // На всякий случай сброс в 1
    end;
    
    AddToLogFile(EngineLog, '[КНОПКА РМП] Переключение на состояние: ' + IntToStr(RMPState));
    
    // Записываем новое значение в память
    WriteByteToMemory(Pointer($00749888), RMPState);
    AddToLogFile(EngineLog, '[КНОПКА РМП] ✓ Записано значение ' + IntToStr(RMPState) + ' по адресу 0x00749888');
    
  except
    on E: Exception do
      AddToLogFile(EngineLog, '[КНОПКА РМП] ✗ ОШИБКА: ' + E.Message);
  end;
end;

// Функция для обработки кликов по кнопкам клавиатуры

function HandleBlockKeyboardClickRuntime(mouseX, mouseY: Integer): Boolean;
var
  keyboardX, keyboardY: Integer;
  relativeX, relativeY: Integer;
  i: Integer;
begin
  Result := False;
  
  AddToLogFile(EngineLog, '[КЛИК] Обработка клика: X=' + IntToStr(mouseX) + ', Y=' + IntToStr(mouseY));
  
  if not BlockKeyboardFileExists then
  begin
    AddToLogFile(EngineLog, '[КЛИК] Файл клавиатуры отсутствует');
    Exit;
  end;
    
  if BlockKeyboardCurrentOffset > 155 then
  begin
    AddToLogFile(EngineLog, '[КЛИК] Панель скрыта (offset: ' + FloatToStr(BlockKeyboardCurrentOffset) + ')');
    Exit;
  end;
    
  keyboardX := ScreenWidth - 230 + Round(BlockKeyboardCurrentOffset);
  keyboardY := ScreenHeight - 250;
  
  AddToLogFile(EngineLog, '[КЛИК] Позиция панели: X=' + IntToStr(keyboardX) + ', Y=' + IntToStr(keyboardY));
  
  if (mouseX >= keyboardX) and (mouseX <= keyboardX + 340) and
     (mouseY >= keyboardY) and (mouseY <= keyboardY + 136) then
  begin
    relativeX := mouseX - keyboardX;
    relativeY := mouseY - keyboardY;
    
    AddToLogFile(EngineLog, '[КЛИК] ✓ Попадание в панель, относительные координаты: X=' + IntToStr(relativeX) + ', Y=' + IntToStr(relativeY));

    // Проверяем кнопки
    for i := 0 to 23 do
    begin
      if (relativeX >= ButtonPositions[i].X) and 
         (relativeX <= ButtonPositions[i].X + 24) and
         (relativeY >= ButtonPositions[i].Y) and 
         (relativeY <= ButtonPositions[i].Y + 24) then
      begin
        AddToLogFile(EngineLog, '[КЛИК] ✓ Попадание в кнопку ' + IntToStr(i));
        
        // Проигрываем звук нажатия кнопки
        if BlockKeyboardSoundID > 0 then
        begin
          try
            AddToLogFile(EngineLog, '[ЗВУК] Проигрываем звук через PlaySound');
            if PlaySound('booster\blok_pick.wav', 0, SND_FILENAME or SND_ASYNC) then
              AddToLogFile(EngineLog, '[ЗВУК] ✓ Звук успешно проигран')
            else
              AddToLogFile(EngineLog, '[ЗВУК] ✗ Ошибка PlaySound');
          except
            on E: Exception do
              AddToLogFile(EngineLog, '[ЗВУК] ✗ Исключение при воспроизведении: ' + E.Message);
          end;
        end;
        
case i of
  0: ProcessBlockPRuntime;  // Кнопка П
  1, 2, 3, 7, 8, 9, 13, 14, 15, 20: begin
       // Сначала проверяем логику кнопки "П"
       if ButtonPState > 0 then
         ProcessBlockNumberRuntime(i)  // Кнопка "П"
       else
         ProcessBlockPCycleRuntime(1, i);  // Кнопка "К"
     end;
  4: ProcessBlockPCycleRuntime(0);  // Кнопка К
  6: ProcessBlockVKRuntime;  // Кнопка ВК
  12: ProcessButtonRMP;  // Кнопка РМП
  21: begin
        // Сначала проверяем логику кнопки "П"  
        if ButtonPState > 0 then
          ProcessBlockEnterRuntime  // Кнопка "П"
        else
          ProcessBlockPCycleRuntime(2);  // Кнопка "К"
      end;
  else
    AddToLogFile(EngineLog, '[КЛИК] Кнопка ' + IntToStr(i) + ' - функция не реализована');
end;

        Result := True;
        Exit;
      end;
    end;
    
    AddToLogFile(EngineLog, '[КЛИК] Клик мимо всех кнопок');
    Result := True;
  end
  else
  begin
    AddToLogFile(EngineLog, '[КЛИК] Клик мимо панели');
  end;
end;

end.
