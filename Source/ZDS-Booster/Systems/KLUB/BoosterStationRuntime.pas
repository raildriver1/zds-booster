unit BoosterStationRuntime;

interface

function FindStationByPiketRuntime(CurrentPiket: Integer): string;
procedure FindCurrentAndNextStationRuntime;
procedure FreeStationRuntime;
function GetCurrentStationRuntime: string;
function GetNextStationRuntime: string;

implementation

uses
  Classes, SysUtils, Windows, MMSystem, Math;

var
  StationsList: TStringList;
  StationsLoaded: Boolean = False;
  CurrentStationName: string = '';
  NextStationName: string = '';
  LastStationUpdate: Cardinal = 0;
  StationUpdateInterval: Cardinal = 1000;

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

function FindStationByPiketRuntime(CurrentPiket: Integer): string;
begin
  Result := FindStationByPiket(CurrentPiket);
end;

procedure FindCurrentAndNextStationRuntime;
begin
  FindCurrentAndNextStation;
end;

procedure FreeStationRuntime;
begin
  FreeAndNil(StationsList);
  StationsLoaded := False;
  CurrentStationName := '';
  NextStationName := '';
end;

function GetCurrentStationRuntime: string;
begin
  Result := CurrentStationName;
end;

function GetNextStationRuntime: string;
begin
  Result := NextStationName;
end;

end.
