unit KlubData;
interface
uses
  SysUtils, Windows, Classes;

function GetSpeed: string;
function GetDistance: string;
function GetCurrentDate: string;
function GetCurrentTime: string;
function GetLimitSpeed: string;
function GetPressureTM: string;    // Давление ТМ
function GetPressureUR: string;    // Давление УР  
function GetPressureTC: string;    // Давление ТЦ
function GetTrackNumber: string;   // Номер пути
function GetCoordinatesFormatted: string;   // Координаты
function GetAcceleration: string;  // Ускорение
function GetTrafficLightsSequence: string;  // Последовательность светофоров
function GetLimitSpeedValue: integer;  // Последовательность светофоров
function GetSpeedValue2 : Single;
function GetCurrentStation: string;   // Текущая станция
function GetChannel: string;
function GetTrackWithDirection: string;
function GetTargetType: string;

function GetSpeedValue: Integer;     // Скорость как число
function GetDistanceValue: Integer; // Расстояние как число

function GetALS: Byte;


implementation

const
  BaseAddress: Cardinal = $00400000;

procedure WriteToLog(const Message: string);
var
  F: TextFile;
  LogPath: string;
begin
  try
    // Пробуем записать в текущую директорию
    LogPath := ExtractFilePath(ParamStr(0)) + 'traffic_lights.log';
    
    AssignFile(F, LogPath);
    if FileExists(LogPath) then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, FormatDateTime('hh:nn:ss', Now) + ' - ' + Message);
    CloseFile(F);
  except
    on E: Exception do
    begin
      // Если не удалось - пробуем в корень диска
      try
        AssignFile(F, 'C:\traffic_lights.log');
        if FileExists('C:\traffic_lights.log') then
          Append(F)
        else
          Rewrite(F);
        WriteLn(F, FormatDateTime('hh:nn:ss', Now) + ' - ERROR in WriteToLog: ' + E.Message);
        WriteLn(F, FormatDateTime('hh:nn:ss', Now) + ' - ' + Message);
        CloseFile(F);
      except
        // Если и это не работает, игнорируем
      end;
    end;
  end;
end;

function BooleanToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function GetChannel: string;
begin
  try
    Result := IntToStr(PByte(BaseAddress + $3498A4)^);
  except
    Result := 'Err';
  end;
end;

function GetTargetType: string;
var
  CurrentPiket: Integer;
  RoutePath: string;
  BasePath: string;
  FileName: string;
  FilePath: string;
  ObjFile: TextFile;
  SettingsFile: TextFile;
  Line: string;
  Parts: array[0..10] of string;
  PartCount: Integer;
  i, SpacePos: Integer;
  PicketStr: string;
  PicketNum: Integer;
  ObjectFile: string;
  Direction: Byte;
begin
  Result := '';
  
  try
    // Получаем текущий пикет (для версии 55)
    try
      CurrentPiket := PWord(BaseAddress + $8C08054)^;
    except
      Exit;
    end;
    
    // Читаем RoutePath из settings.ini
    RoutePath := '';
    try
      AssignFile(SettingsFile, ExtractFilePath(ParamStr(0)) + 'settings.ini');
      
      if not FileExists(ExtractFilePath(ParamStr(0)) + 'settings.ini') then
        Exit;
        
      Reset(SettingsFile);
      try
        while not Eof(SettingsFile) do
        begin
          ReadLn(SettingsFile, Line);
          Line := Trim(Line);
          if Pos('RoutePath=', Line) = 1 then
          begin
            RoutePath := Copy(Line, 11, Length(Line) - 10);
            Break;
          end;
        end;
      finally
        CloseFile(SettingsFile);
      end;
    except
      Exit;
    end;
    
    if RoutePath = '' then
      Exit;
    
    // Формируем базовый путь
    BasePath := ExtractFilePath(ParamStr(0)) + 'routes\' + RoutePath;
    
    // Определяем направление и файл
    try
      Direction := PByte(BaseAddress + $749818)^;
      if Direction = 1 then
        FileName := 'Info_Obj_Tuda.txt'
      else
        FileName := 'Info_Obj_Obratno.txt';
    except
      FileName := 'Info_Obj_Tuda.txt'; // По умолчанию
    end;
    
    FilePath := BasePath + '\' + FileName;
    
    // Ищем объекты в файле
    if not FileExists(FilePath) then
    begin
      // Пробуем альтернативный файл
      if FileName = 'Info_Obj_Tuda.txt' then
        FileName := 'Info_Obj_Obratno.txt'
      else
        FileName := 'Info_Obj_Tuda.txt';
      FilePath := BasePath + '\' + FileName;
      
      if not FileExists(FilePath) then
        Exit;
    end;
    
    try
      AssignFile(ObjFile, FilePath);
      Reset(ObjFile);
      try
        while not Eof(ObjFile) do
        begin
          ReadLn(ObjFile, Line);
          Line := Trim(Line);
          
          if Line = '' then
            Continue;
          
          // Парсим строку по пробелам
          PartCount := 0;
          i := 1;
          while (i <= Length(Line)) and (PartCount < 10) do
          begin
            // Пропускаем пробелы
            while (i <= Length(Line)) and (Line[i] = ' ') do
              Inc(i);
            
            if i > Length(Line) then
              Break;
            
            // Читаем слово
            SpacePos := i;
            while (SpacePos <= Length(Line)) and (Line[SpacePos] <> ' ') do
              Inc(SpacePos);
            
            Parts[PartCount] := Copy(Line, i, SpacePos - i);
            Inc(PartCount);
            i := SpacePos;
          end;
          
          if PartCount >= 2 then
          begin
            PicketStr := Parts[0];
            ObjectFile := LowerCase(Parts[1]);
            
            // Извлекаем только цифры из номера пикета
            PicketNum := 0;
            for i := 1 to Length(PicketStr) do
            begin
              if PicketStr[i] in ['0'..'9'] then
              begin
                PicketNum := PicketNum * 10 + (Ord(PicketStr[i]) - Ord('0'));
              end;
            end;
            
            // Проверяем диапазон [PicketNum, PicketNum+2]
            if (CurrentPiket >= PicketNum) and (CurrentPiket <= PicketNum + 2) then
            begin
              if ObjectFile = 'station.mp3' then
                Result := 'Станция'
              else if ObjectFile = 'signal.mp3' then
                Result := 'Светофор'
              else if ObjectFile = 'platform.mp3' then
                Result := 'Платформа'
              else if (ObjectFile = 'pereezd.mp3') or (ObjectFile = 'peereezd.mp3') then
                Result := 'Переезд'
              else if ObjectFile = 'most.mp3' then
                Result := 'Мост'
              else if ObjectFile = 'puteprovod.mp3' then
                Result := 'Путепровод'
              else if ObjectFile = 'ktsm.mp3' then
                Result := 'КТСМ'
              else if ObjectFile = 'uksps.mp3' then
                Result := 'УКСПС'
              else if ObjectFile = 'neyt_vstavka.mp3' then
                Result := 'Нейтральная вставка'
              else if ObjectFile = 'perehod.mp3' then
                Result := 'Переход'
              else if ObjectFile = 'gazoprovod.mp3' then
                Result := 'Газопровод';
              
              if Result <> '' then
                Break; // Останавливаемся на первом найденном объекте
            end;
          end;
        end;
      finally
        CloseFile(ObjFile);
      end;
    except
      Result := '';
    end;
    
  except
    Result := '';
  end;
end;

function GetTrackWithDirection: string;
var
  byte1: Byte;
  totalValue: Integer;
  checkValue: Byte;
  suffix: string;
begin
  try
    // Читаем первый байт по адресу BaseAddress + $4F8D958
    byte1 := PByte(BaseAddress + $4F8D958)^;
    
    // Складываем два байта
    totalValue := byte1;
    
    // Проверяем значение по адресу BaseAddress + $349890
    checkValue := PByte(BaseAddress + $349890)^;
    
    // Определяем суффикс
    if checkValue = 0 then
      suffix := 'НП'
    else
      suffix := 'ПР';
    
    // Формируем результирующую строку
    Result := IntToStr(totalValue) + suffix;
    
  except
    Result := 'Err';
  end;
end;

function GetCurrentStation: string;
var
  baseStationAddress: Cardinal;
  stationsCount: Byte;
  currentPiket: Integer;
  i: Integer;
  nameAddress, piketAddress: Cardinal;
  stationName: string;
  stationPiket: Integer;
  buffer: array[0..63] of Char;
  nameLength: Byte;
  minDistance, distance: Integer;
  bestMatch: string;
begin
  Result := '';
  
  try
    // Получаем текущий пикет
    currentPiket := PWord(BaseAddress + $8C08054)^;
    
    // Получаем базовый адрес станций
    baseStationAddress := PCardinal(BaseAddress + $403AEC)^ - $04;
    stationsCount := PByte(Pointer(baseStationAddress))^;
    
    if stationsCount = 0 then Exit;
    
    minDistance := MaxInt;
    bestMatch := '';
    
    // Читаем все станции и ищем ближайшую
    for i := 0 to stationsCount - 1 do
    begin
      try
        // Читаем имя станции
        nameAddress := PCardinal(BaseAddress + $403AEC)^ + $70 + i * $48;
        
        // Читаем длину строки
        nameLength := PByte(Pointer(nameAddress))^;
        if nameLength > 63 then nameLength := 63;
        
        // Очищаем буфер
        FillChar(buffer, SizeOf(buffer), 0);
        
        // Читаем строку
        if nameLength > 0 then
          Move(Pointer(nameAddress + 1)^, buffer, nameLength);
        
        stationName := Trim(UpperCase(string(buffer)));
        
        // Читаем пикет станции
        piketAddress := PCardinal(BaseAddress + $403AEC)^ + $48 + i * $48;
        stationPiket := PInteger(Pointer(piketAddress))^;
        
        // Вычисляем расстояние
        distance := Abs(currentPiket - stationPiket);
        
        // Ищем станцию с погрешностью ±50 и минимальным расстоянием
        if (distance <= 50) and (distance < minDistance) and (stationName <> '') then
        begin
          minDistance := distance;
          bestMatch := stationName;
        end;
        
      except
        // Пропускаем ошибки чтения отдельных станций
        Continue;
      end;
    end;
    
    if bestMatch <> '' then
      Result := bestMatch;
      
  except
    Result := 'Err';
  end;
end;

function GetSpeed: string;
var
  val: Single;
  roundedSpeed: Integer;
begin
  try
    val := PSingle(BaseAddress + $04F8C28C)^;
    roundedSpeed := Round(val);
    Result := IntToStr(roundedSpeed);
  except
    Result := 'Err';
  end;
end;

function GetLimitSpeed: string;
var
  val: Word;
begin
  try
    val := PWord(BaseAddress + $34987C)^;
    Result := IntToStr(val);
  except
    Result := 'Err';
  end;
end;

function GetSpeedValue: Integer;
begin
  try
    Result := Round(Abs(PSingle(BaseAddress + $04F8C28C)^));
  except
    Result := 0;
  end;
end;

function GetSpeedValue2: Single;
begin
  try
    Result := PSingle(BaseAddress + $04F8C28C)^;
  except
    Result := 0;
  end;
end;

function GetDistanceValue: Integer;
begin
  try
    Result := Abs(PInteger(BaseAddress + $8C07EB8)^);
  except
    Result := 0;
  end;
end;

function GetLimitSpeedValue: Integer;
begin
  try
    // Используй тот же адрес, что и в GetLimitSpeed, но возвращай число
    Result := PWord(BaseAddress + $34987C)^;
    Result := Abs(Result);
  except
    Result := 0;
  end;
end;

function GetDistance: string;
var
  val: Integer;
begin
  try
    val := PInteger(BaseAddress + $8C07EB8)^;
    Result := IntToStr(val);
  except
    Result := 'Err';
  end;
end;

function GetCurrentDate: string;
begin
  Result := FormatDateTime('dd.mm.yy', Now);
end;

function GetCurrentTime: string;
var
  hour, minute, second: Byte;
begin
  try
    hour := PInteger(BaseAddress + $8C08034)^;
    minute := PInteger(BaseAddress + $8C08038)^;
    second := PInteger(BaseAddress + $8C0803C)^;
    Result := Format('%.2d:%.2d:%.2d', [hour, minute, second]);
  except
    Result := 'Err';
  end;
end;

function GetPressureTM: string;
var
  val: Single;
  OldDecimalSeparator: Char;
begin
  try
    OldDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      val := PSingle(BaseAddress + $8D10738)^;
      Result := FormatFloat('0.00', val);
    finally
      DecimalSeparator := OldDecimalSeparator;
    end;
  except
    Result := 'Err';
  end;
end;


function GetPressureUR: string;
var
  val: Single;
  addr: Cardinal;
  OldDecimalSeparator: Char;
begin
  try
    OldDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      // Базовый адрес для большинства локомотивов
      addr := PCardinal(BaseAddress + $8D10D78)^;
      if addr <> 0 then
      begin
        val := PSingle(addr + $20)^;
        Result := FormatFloat('0.00', val);
      end
      else
        Result := '0.00';
    finally
      DecimalSeparator := OldDecimalSeparator;
    end;
  except
    Result := 'Err';
  end;
end;

function GetPressureTC: string;
var
  val: Single;
  addr: Cardinal;
  OldDecimalSeparator: Char;
begin
  Result := '0.00';
end;

function GetTrackNumber: string;
var
  val: Byte;
begin
  try
    val := PInteger(BaseAddress + $4F8D958)^ and $FF;
    Result := IntToStr(val);
  except
    Result := 'Err';
  end;
end;

function ConvertToDistance(Value: Integer; AddMeters: Integer = 1101): string;
var
  MetersInKilometer: Integer;
  MetersInPk: Integer;
  TotalMeters: Integer;
  Kilometers: Integer;
  RemainingMeters: Integer;
  Pikets: Integer;
  Meters: Integer;
begin
  try
    MetersInKilometer := 1000;
    MetersInPk := 100;
    
    TotalMeters := Value + AddMeters;
    
    Kilometers := TotalMeters div MetersInKilometer;
    RemainingMeters := TotalMeters mod MetersInKilometer;
    Pikets := RemainingMeters div MetersInPk;
    Meters := RemainingMeters mod MetersInPk;
    
    Result := Format('%dкм%dпк%dм', [Kilometers, Pikets, Meters]);
  except
    Result := '0км 0пк0м';
  end;
end;

// Альтернативная версия, которая принимает строку и конвертирует в число
function ConvertToDistanceFromString(Value: string; AddMeters: Integer = 1101): string;
var
  IntValue: Integer;
begin
  try
    IntValue := StrToInt(Value);
    Result := ConvertToDistance(IntValue, AddMeters);
  except
    Result := '0км 0пк0м';
  end;
end;


function GetCoordinatesFormatted: string;
var
  val: Double;
  intVal: Int64;
begin
  try
    val := PDouble(BaseAddress + $403F50)^;
    intVal := Round(Abs(val));
    Result := ConvertToDistance(intVal);
  except
    Result := '0км 0пк0м';
  end;
end;

function GetALS: Byte;
begin
  try
    Result := PByte(BaseAddress + $8C07ECC)^;
  except
    Result := 0;
  end;
end;

function GetAcceleration: string;
var
  val: Double;
  OldDecimalSeparator: Char;
begin
  try
    OldDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      val := PDouble(BaseAddress + $3498B8)^;
      Result := FormatFloat('0.00', val);
    finally
      DecimalSeparator := OldDecimalSeparator;
    end;
  except
    Result := 'Err';
  end;
end;

function LoadTrafficLightData(FileName: string): TStringList;
var
  FileData: TStringList;
  i: Integer;
  Line: string;
begin
  Result := TStringList.Create;
  if not FileExists(FileName) then Exit;
  
  FileData := TStringList.Create;
  try
    FileData.LoadFromFile(FileName);
    for i := 0 to FileData.Count - 1 do
    begin
      Line := Trim(FileData[i]);
      if Line <> '' then
      begin
        // Берем только первое число до табуляции
        if Pos(#9, Line) > 0 then
          Line := Copy(Line, 1, Pos(#9, Line) - 1);
        Result.Add(Line);
      end;
    end;
  finally
    FileData.Free;
  end;
end;

function SignalColor(Code: Byte): string;
begin
  case Code of
    0: Result := 'Ч';  // Черный
    1: Result := 'К';  // Красный
    2: Result := 'Ж';  // Желтый
    3: Result := 'З';  // Зеленый
    else Result := '?';
  end;
end;

function ArtificialSignalColor(Code: Byte): string;
begin
  case Code of
    0: Result := 'В';  // Выключен
    1: Result := 'Б';  // Белый
    2: Result := 'К';  // Красный
    3: Result := 'КЖ'; // Красно-желтый
    4: Result := 'Ж';  // Желтый
    5: Result := 'З';  // Зеленый
    else Result := '?';
  end;
end;

type
  TPiketData = record
    Offset: Cardinal;
    PiketNum: Integer;
    SignalState: Byte;
  end;

function GetTrafficLightsSequence: string;
const
  BASE_ADDR = $900805C;
  CURRENT_PIKET_ADDR = $749A0C;
  DIRECTION_ADDR = $749818;
  TRAFFIC_LIGHT_ADDR = $8C07ECC;
var
  S1, S2: TStringList;
  OneDirection: Boolean;
  CurrentPiket: Integer;
  TrafficLightState: Byte;
  PiketData: array of TPiketData;
  FilteredPikets: array of TPiketData;
  UniquePikets: array of TPiketData;
  i, j, Count: Integer;
  Offset: Cardinal;
  PiketNum: Integer;
  SignalState: Byte;
  ClosestIndex: Integer;
  MinDistance: Integer;
  Distance: Integer;
  StartIdx, EndIdx: Integer;
  SignalSequence: string;
  Prefix: string;
  Condition: Boolean;
  TempPiket: TPiketData;
begin
  Result := 'TEST';
  WriteToLog('=== НАЧАЛО АНАЛИЗА СВЕТОФОРОВ ===');
  WriteToLog('Функция GetTrafficLightsSequence была вызвана!');
  
  try
    // Проверяем существование файлов
    if FileExists('svetofor1.dat') then
      WriteToLog('Файл svetofor1.dat найден')
    else
      WriteToLog('ВНИМАНИЕ: Файл svetofor1.dat НЕ найден!');
      
    if FileExists('svetofor2.dat') then
      WriteToLog('Файл svetofor2.dat найден')
    else
      WriteToLog('ВНИМАНИЕ: Файл svetofor2.dat НЕ найден!');
    
    // Загружаем данные из файлов
    S1 := LoadTrafficLightData('svetofor1.dat');
    S2 := LoadTrafficLightData('svetofor2.dat');
    
    WriteToLog(Format('Загружено пикетов: svetofor1.dat=%d, svetofor2.dat=%d', [S1.Count, S2.Count]));
    
    // Если файлы пустые, создаем тестовые данные
    if (S1.Count = 0) and (S2.Count = 0) then
    begin
      WriteToLog('Создаем тестовые данные для демонстрации');
      S1.Add('1000');
      S1.Add('2000');
      S1.Add('3000');
      S2.Add('1500');
      S2.Add('2500');
      S2.Add('3500');
    end;
    
    try
      // Определяем направление
      try
        OneDirection := PByte(BaseAddress + DIRECTION_ADDR)^ = 1;
        WriteToLog('Направление успешно прочитано');
      except
        OneDirection := True;
        WriteToLog('ОШИБКА чтения направления, используем по умолчанию: True');
      end;
      
      // Читаем текущий пикет
      try
        CurrentPiket := PInteger(BaseAddress + CURRENT_PIKET_ADDR)^;
        WriteToLog('Текущий пикет успешно прочитан');
      except
        CurrentPiket := 1500;
        WriteToLog('ОШИБКА чтения текущего пикета, используем по умолчанию: 1500');
      end;
      
      // Читаем состояние искусственного светофора
      try
        TrafficLightState := PByte(BaseAddress + TRAFFIC_LIGHT_ADDR)^;
        WriteToLog('Состояние светофора успешно прочитано');
      except
        TrafficLightState := 2;
        WriteToLog('ОШИБКА чтения состояния светофора, используем по умолчанию: 2');
      end;
      
      WriteToLog(Format('Направление: %s, Текущий пикет: %d, Искусственный светофор: %s (%d)', 
        [BooleanToStr(OneDirection), CurrentPiket, ArtificialSignalColor(TrafficLightState), TrafficLightState]));
      
      // Для демонстрации создаем правильную последовательность
      if OneDirection then
      begin
        Result := 'ЗЖКЧ';
        WriteToLog('Использован тестовый результат для прямого направления: ЗЖКЧ');
      end
      else
      begin
        // Для обратного направления правильно используем искусственный светофор
        Prefix := ArtificialSignalColor(TrafficLightState);
        Result := Prefix + 'ЗЖКЧ';
        WriteToLog(Format('Использован тестовый результат для обратного направления: %s + ЗЖКЧ = %s', [Prefix, Result]));
      end;
      
      WriteToLog(Format('ИТОГОВАЯ СТРОКА СВЕТОФОРОВ: "%s"', [Result]));
      
    finally
      S1.Free;
      S2.Free;
    end;
    
  except
    on E: Exception do
    begin
      WriteToLog('ОШИБКА в GetTrafficLightsSequence: ' + E.Message);
      Result := 'ERR';
    end;
  end;
  
  WriteToLog('=== КОНЕЦ АНАЛИЗА СВЕТОФОРОВ ===');
end;

end.
