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

// Новые функции интегрированные из Python кода
function GetSvetoforValue: string;  // Получение значения светофора
function GetDirection: Boolean;     // Определение направления (аналог tuda)
function GetSvetoforFileName: string; // Получение имени файла светофора
function GetRoutePathFromMemory: string; // Получение пути маршрута из памяти

function GetSpeedValue: Integer;     // Скорость как число
function GetDistanceValue: Integer; // Расстояние как число

function GetALS: Byte;
function GetPressureTMf: Single;    // Давление ТМ
function GetPressureURf: Single;    // Давление УР
function GetPressureTCf: Single;    // Давление ТЦ  

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

// Новая функция: получение пути маршрута из памяти (аналог Python self.map_)
function GetRoutePathFromMemory: string;
var
  MapAddr: Cardinal;
  PathStr: string;
  Buffer: array[0..255] of AnsiChar;
  i: Integer;
begin
  Result := '';
  try
    // Адрес карты для версии 55
    MapAddr := BaseAddress + $8DD46D7;
    
    // Читаем строку из памяти
    FillChar(Buffer, SizeOf(Buffer), 0);
    for i := 0 to 254 do
    begin
      try
        Buffer[i] := PAnsiChar(MapAddr + i)^;
        if Buffer[i] = #0 then Break;
      except
        Break;
      end;
    end;
    
    PathStr := string(Buffer);
    if Pos('\', PathStr) > 0 then
    begin
      // Извлекаем первую часть пути до первого обратного слэша
      Result := Copy(PathStr, 1, Pos('\', PathStr) - 1);
    end;
  except
    Result := '';
  end;
end;

// Новая функция: определение направления (аналог Python tuda)
function GetDirection: Boolean;
var
  MapAddr: Cardinal;
  PathStr: string;
  Buffer: array[0..255] of AnsiChar;
  i: Integer;
begin
  Result := True; // По умолчанию
  try
    // Адрес карты для версии 55
    MapAddr := BaseAddress + $8DD46D7;
    
    // Читаем строку из памяти
    FillChar(Buffer, SizeOf(Buffer), 0);
    for i := 0 to 254 do
    begin
      try
        Buffer[i] := PAnsiChar(MapAddr + i)^;
        if Buffer[i] = #0 then Break;
      except
        Break;
      end;
    end;
    
    PathStr := string(Buffer);
    // Ищем 'speeds1' во второй части пути
    if Pos('\speeds1', PathStr) > 0 then
      Result := True
    else
      Result := False;
  except
    Result := True;
  end;
end;

// Новая функция: получение имени файла светофора
function GetSvetoforFileName: string;
begin
  if GetDirection then
    Result := 'svetofor1.dat'
  else
    Result := 'svetofor2.dat';
end;

// Новая функция: поиск значения светофора по пикету (аналог find_value_by_picket)
function FindValueByPicket(const FilePath: string; Picket: Integer): string;
var
  F: TextFile;
  Line: string;
  Parts: TStringList;
  Intervals: array of record
    StartValue: Integer;
    Value: string;
  end;
  i, j: Integer;
  StartValue: Integer;
  Value: string;
  Direction: Boolean;
begin
  Result := '';
  
  if not FileExists(FilePath) then
    Exit;
    
  SetLength(Intervals, 0);
  Parts := TStringList.Create;
  try
    AssignFile(F, FilePath);
    Reset(F);
    try
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        Line := Trim(Line);
        if Line = '' then Continue;
        
        Parts.Clear;
        Parts.Delimiter := ' ';
        Parts.DelimitedText := Line;
        
        if Parts.Count >= 3 then
        begin
          try
            StartValue := StrToInt(Parts[0]);
            Value := Parts[2];
            
            SetLength(Intervals, Length(Intervals) + 1);
            Intervals[High(Intervals)].StartValue := StartValue;
            Intervals[High(Intervals)].Value := Value;
          except
            // Пропускаем строки с ошибками
          end;
        end;
      end;
    finally
      CloseFile(F);
    end;
    
    // Сортировка интервалов по возрастанию значений пикетов
    for i := 0 to High(Intervals) - 1 do
      for j := i + 1 to High(Intervals) do
        if Intervals[i].StartValue > Intervals[j].StartValue then
        begin
          StartValue := Intervals[i].StartValue;
          Value := Intervals[i].Value;
          Intervals[i].StartValue := Intervals[j].StartValue;
          Intervals[i].Value := Intervals[j].Value;
          Intervals[j].StartValue := StartValue;
          Intervals[j].Value := Value;
        end;
    
    // Ищем значение для данного пикета
    Direction := GetDirection;
    for i := 0 to High(Intervals) do
    begin
      if Picket <= Intervals[i].StartValue then
      begin
        if i > 0 then
        begin
          if Direction then
            Result := Intervals[i].Value
          else
            Result := Intervals[i-1].Value;
        end
        else
          Result := Intervals[0].Value;
        Break;
      end;
    end;
    
    // Если пикет больше всех имеющихся, берем последний интервал
    if (Result = '') and (Length(Intervals) > 0) then
      Result := Intervals[High(Intervals)].Value;
      
  finally
    Parts.Free;
  end;
end;

// Новая функция: получение значения светофора
function GetSvetoforValue: string;
var
  CurrentPiket: Integer;
  RoutePath: string;
  SvetoforFile: string;
  FilePath: string;
begin
  Result := '';
  try
    // Получаем текущий пикет
    CurrentPiket := PWord(BaseAddress + $8C08054)^;
    
    // Получаем путь маршрута из памяти
    RoutePath := GetRoutePathFromMemory;
    if RoutePath = '' then Exit;
    
    // Формируем путь к файлу светофора
    SvetoforFile := GetSvetoforFileName;
    FilePath := ExtractFilePath(ParamStr(0)) + 'routes\' + RoutePath + '\' + SvetoforFile;
    
    // Корректировка пикета в зависимости от направления
    if not GetDirection then
      CurrentPiket := CurrentPiket + 4;
    
    // Ищем значение по пикету
    Result := FindValueByPicket(FilePath, CurrentPiket);
    
    // Форматируем результат (выравнивание по правому краю на 8 символов)
    if Result <> '' then
    begin
      while Length(Result) < 8 do
        Result := ' ' + Result;
    end
    else
      Result := '        '; // 8 пробелов если не найдено
      
  except
    Result := '        ';
  end;
end;

function GetChannel: string;
begin
  try
    Result := IntToStr(PByte(BaseAddress + $3498A4)^);
  except
    Result := 'Err';
  end;
end;

// Обновленная функция GetTargetType с интегрированной логикой из Python
function GetTargetType: string;
var
  CurrentPiket: Integer;
  RoutePath: string;
  BasePath: string;
  FileName: string;
  FilePath: string;
  ObjFile: TextFile;
  Line: string;
  Parts: TStringList;
  PicketStr: string;
  PicketNum: Integer;
  ObjectFile: string;
  Direction: Boolean;
  i: Integer;
begin
  Result := '';
  
  try
    // Получаем текущий пикет
    CurrentPiket := PWord(BaseAddress + $8C08054)^;
    
    // Получаем путь маршрута из памяти
    RoutePath := GetRoutePathFromMemory;
    if RoutePath = '' then Exit;
    
    // Формируем базовый путь
    BasePath := ExtractFilePath(ParamStr(0)) + 'routes\' + RoutePath;
    
    // Определяем направление и файл
    Direction := GetDirection;
    if Direction then
      FileName := 'Info_Obj_Tuda.txt'
    else
      FileName := 'Info_Obj_Obratno.txt';
    
    FilePath := BasePath + '\' + FileName;
    
    // Ищем объекты в файле
    if not FileExists(FilePath) then
    begin
      // Пробуем альтернативный файл
      if Direction then
        FileName := 'Info_Obj_Obratno.txt'
      else
        FileName := 'Info_Obj_Tuda.txt';
      FilePath := BasePath + '\' + FileName;
      
      if not FileExists(FilePath) then
        Exit;
    end;
    
    Parts := TStringList.Create;
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
          Parts.Clear;
          Parts.Delimiter := ' ';
          Parts.DelimitedText := Line;
          
          if Parts.Count >= 2 then
          begin
            PicketStr := Parts[0];
            ObjectFile := LowerCase(Parts[1]);
            
            // Извлекаем только цифры из номера пикета
            PicketNum := 0;
            for i := 1 to Length(PicketStr) do
            begin
              if (PicketStr[i] >= '0') and (PicketStr[i] <= '9') then
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
    finally
      Parts.Free;
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

function GetPressureTMf: Single;
begin
  try
    Result := PSingle(BaseAddress + $8D10738)^;
  except
    Result := 0.0;
  end;
end;

function GetPressureURf: Single;
var
  addr: Cardinal;
begin
  try
    // Базовый адрес для большинства локомотивов
    addr := PCardinal(BaseAddress + $8D10D78)^;
    if addr <> 0 then
      Result := PSingle(addr + $20)^
    else
      Result := 0.0;
  except
    Result := 0.0;
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

function GetPressureTCf: Single;
var
  locType: Integer;
  baseAddr: Cardinal;
  addr, tempAddr: Cardinal;

  // Вспомогательная функция для получения адреса через указатели (аналог GetPtrAddr из Python)
function GetPtrAddr(baseOffset: Cardinal; offsets: array of Cardinal): Cardinal;
var
  i: Integer;
  currentAddr: Cardinal;
begin
  Result := 0;
  try
    currentAddr := PCardinal(baseAddr + baseOffset)^;
    if currentAddr = 0 then Exit;
    
    // Просто прибавляем смещения, НЕ читаем по ним указатели
    for i := 0 to High(offsets) do
    begin
      currentAddr := currentAddr + offsets[i];  // УБРАТЬ PCardinal()^
    end;
    
    Result := currentAddr;
  except
    Result := 0;
  end;
end;

begin
  Result := 0.0;
  baseAddr := $00400000;
  
  try
    // Определяем тип локомотива
    locType := PInteger(Pointer(baseAddr + $4F8D93C))^;
    
    case locType of
      // Группа 1: ED9M, ED4M, 2M62 - используем float
      3159, // ЭД9М (ED9M)
      3154, // ЭД4М (ED4M)  
      1462: // М62 (2M62)
      begin
        try
          Result := PSingle(baseAddr + $8D107B0)^;
        except
          Result := 0.0;
        end;
      end;
      
      // Группа 2: 2TE10U, TEP70BS, TEP70, TEM18DM, VL85, VL80T, VL82M - используем double
      21014, // 2ТЭ10У (2TE10U)
      2071,  // ТЭП70БС (TEP70BS)
      2070,  // ТЭП70 (TEP70)
      201318,// ТЭМ18ДМ (TEM18DM)
      885,   // ВЛ85 (VL85)
      880,   // ВЛ80Т (VL80T)
      882:   // ВЛ82М (VL82M)
      begin
        try
          addr := GetPtrAddr($4F8D8D4, [$A8]);
          if addr <> 0 then
            Result := PDouble(addr)^
          else
            Result := 0.0;
        except
          Result := 0.0;
        end;
      end;
      
      // Группа 3: VL11M, CHS2K, EP1M, 2ES5K - используем float
      811,   // ВЛ11М (VL11M)
      343,   // ЧС2К (CHS2K)  
      31714, // ЭП1М (EP1M)
      23152: // 2ЭС5К (2ES5K)
      begin
        try
          addr := GetPtrAddr($8D10D78, [$68]);
          if addr <> 0 then
            Result := PSingle(addr)^
          else
            Result := 0.0;
        except
          Result := 0.0;
        end;
      end;
      
      // Группа 4: CHS7, CHS4T, CHS4 с правильными адресами памяти
      822: // ЧС7 (CHS7)
begin
  try
    tempAddr := PCardinal(baseAddr + $4F8D8D4)^;
    if tempAddr <> 0 then
    begin
      addr := tempAddr + $80;  // Просто прибавляем смещение
      Result := PDouble(addr)^;
    end;
  except
    Result := 0.0;
  end;
end;
      
      621: // ЧС4Т (CHS4T)
      begin
        try
          addr := GetPtrAddr($4F8D8D4, [$58]);
          if addr <> 0 then
            Result := PDouble(addr)^
          else
            Result := 0.0;
        except
          Result := 0.0;
        end;
      end;
      
      523: // ЧС4 (CHS4)
      begin
        // Для версии 55, CHS4 имеет значение 0.0
        Result := 0.0;
      end;
      
      // Группа 5: CHS4KVR - используем double
      524: // ЧС4КВР (CHS4KVR)
      begin
        try
          addr := GetPtrAddr($34846C, [$1C, $80]);
          if addr <> 0 then
            Result := PDouble(addr)^
          else
            Result := 0.0;
        except
          Result := 0.0;
        end;
      end;
      
      // Остальные случаи (ЧС8, 2ЭС4К и др.)
      812,   // ЧС8 (CHS8)
      23142: // 2ЭС4К (2ES4K)
      begin
        try
          addr := GetPtrAddr($04F8D8D4, [$28, $48]);
          if addr <> 0 then
            Result := PSingle(addr)^
          else
            Result := 0.0;
        except
          Result := 0.0;
        end;
      end;
      
      else
      begin
        // Случай по умолчанию - используем подход из default case
        try
          addr := GetPtrAddr($04F8D8D4, [$28, $48]);
          if addr <> 0 then
            Result := PSingle(addr)^
          else
            Result := 0.0;
        except
          Result := 0.0;
        end;
      end;
    end;
    
    // Проверяем разумность значения (давление ТЦ обычно 0-10 кгс/см²)
    if (Result < 0) or (Result > 15) then
      Result := 0.0;
      
  except
    Result := 0.0;
  end;
end;

// Строковая версия для совместимости
function GetPressureTC: string;
var
  val: Single;
  OldDecimalSeparator: Char;
begin
  try
    OldDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      val := GetPressureTCf;
      Result := FormatFloat('0.00', val);
    finally
      DecimalSeparator := OldDecimalSeparator;
    end;
  except
    Result := 'Err';
  end;
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
