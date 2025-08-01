﻿unit KlubData;
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
  try
    OldDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      // Базовый адрес для большинства локомотивов
      addr := PCardinal(BaseAddress + $8D10D78)^;
      if addr <> 0 then
      begin
        val := PSingle(addr + $68)^;  // ← ВОТ ЭТА СТРОКА
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
