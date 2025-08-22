// DiscordRPC.pas - Discord Rich Presence БЕЗ иконок (полная версия)
unit DiscordRPC;

interface

uses
  Windows, SysUtils, Messages;

procedure InitDiscordRPC;
procedure UpdateDiscordRPC;
procedure ShutdownDiscordRPC;

implementation

var
  PipeHandle: THandle = INVALID_HANDLE_VALUE;
  Connected: Boolean = False;
  LastUpdateTime: DWORD = 0;
  StartTimestamp: Int64 = 0;
  ApplicationID: string = '1408466421846905004'; // Ваш Discord Application ID
  DebugMode: Boolean = True; // Включаем отладку

type
  TDiscordRPCMessage = packed record
    OpCode: Cardinal;
    Length: Cardinal;
  end;

procedure DebugLog(const Msg: string);
var
  LogFile: TextFile;
  LogPath: string;
begin
  if not DebugMode then Exit;
  
  try
    LogPath := ExtractFilePath(ParamStr(0)) + 'discord_debug.log';
    AssignFile(LogFile, LogPath);
    
    if FileExists(LogPath) then
      Append(LogFile)
    else
      Rewrite(LogFile);
      
    WriteLn(LogFile, FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
    CloseFile(LogFile);
  except
    // Игнорируем ошибки логирования
  end;
end;

function ConnectToDiscord: Boolean;
var
  PipeName: string;
  i: Integer;
  LastError: DWORD;
begin
  Result := False;
  DebugLog('Attempting to connect to Discord...');
  
  // Пробуем подключиться к Discord через Named Pipes
  for i := 0 to 9 do
  begin
    PipeName := '\\.\pipe\discord-ipc-' + IntToStr(i);
    DebugLog('Trying pipe: ' + PipeName);
    
    PipeHandle := CreateFile(
      PChar(PipeName),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      0,
      0
    );
    
    if PipeHandle <> INVALID_HANDLE_VALUE then
    begin
      DebugLog('Successfully connected to: ' + PipeName);
      Result := True;
      Connected := True;
      Exit;
    end
    else
    begin
      LastError := GetLastError;
      DebugLog('Failed to connect to ' + PipeName + ', error: ' + IntToStr(LastError));
    end;
  end;
  
  DebugLog('Failed to connect to any Discord pipes');
end;

function SendDiscordMessage(OpCode: Cardinal; const Data: AnsiString): Boolean;
var
  Message: TDiscordRPCMessage;
  BytesWritten: DWORD;
  ResponseBuffer: array[0..1023] of Byte;
  BytesRead: DWORD;
  ResponseData: string;
begin
  Result := False;
  if not Connected then
  begin
    DebugLog('SendDiscordMessage: Not connected');
    Exit;
  end;
  
  DebugLog('Sending message - OpCode: ' + IntToStr(OpCode) + ', Length: ' + IntToStr(Length(Data)));
  DebugLog('Data: ' + string(Data));
  
  Message.OpCode := OpCode;
  Message.Length := Length(Data);
  
  // Отправляем заголовок
  if not WriteFile(PipeHandle, Message, SizeOf(Message), BytesWritten, nil) then
  begin
    DebugLog('Failed to write header, error: ' + IntToStr(GetLastError));
    Exit;
  end;
  
  // Отправляем данные
  if WriteFile(PipeHandle, Data[1], Length(Data), BytesWritten, nil) then
  begin
    DebugLog('Message sent successfully');
    
    // Попытаемся прочитать ответ
    Sleep(100);
    if ReadFile(PipeHandle, ResponseBuffer, SizeOf(ResponseBuffer), BytesRead, nil) then
    begin
      if BytesRead > 8 then
      begin
        SetString(ResponseData, PAnsiChar(@ResponseBuffer[8]), BytesRead - 8);
        DebugLog('Discord response: ' + ResponseData);
      end;
    end
    else
    begin
      DebugLog('No response from Discord (this is normal)');
    end;
    
    Result := True;
  end
  else
  begin
    DebugLog('Failed to write data, error: ' + IntToStr(GetLastError));
  end;
end;

function GetCurrentLocomotiveType: Integer;
begin
  try
    // Проверяем доступность адреса перед чтением
    if IsBadReadPtr(Pointer($00400000 + $4F8D93C), 4) then
    begin
      DebugLog('Cannot read locomotive type - bad memory address');
      Result := 0;
    end
    else
    begin
      Result := PInteger(Pointer($00400000 + $4F8D93C))^;
      DebugLog('Read locomotive type: ' + IntToStr(Result));
    end;
  except
    on E: Exception do
    begin
      DebugLog('Exception reading locomotive type: ' + E.Message);
      Result := 0;
    end;
  end;
end;

function GetLocomotiveName(LocType: Integer): string;
begin
  case LocType of
    // Электровозы переменного тока (только ASCII)
    812: Result := 'ChS8';
    822: Result := 'ChS7';
    523: Result := 'ChS4';
    524: Result := 'ChS4KVR';  
    621: Result := 'ChS4T';
    343: Result := 'ChS2K';
    
    // Электровозы постоянного тока
    882: Result := 'VL82';
    880: Result := 'VL80T';
    811: Result := 'VL11M';
    885: Result := 'VL85';
    
    // Тепловозы
    2070: Result := 'TEP70';
    2071: Result := 'TEP70BS';
    1462: Result := 'M62';
    21014: Result := '2TE10U';
    201318: Result := 'TEM18DM';
    
    // Электропоезда
    3154: Result := 'ED4M';
    3159: Result := 'ED9M';
    23152: Result := '2ES5K';
    23142: Result := '2ES4K';
    31714: Result := 'EP1M';
    
    // Дополнительные локомотивы
    813: Result := 'ChS8';
    823: Result := 'ChS7';
    871: Result := 'VL11';
    872: Result := 'VL11M';
    883: Result := 'VL82M';
    884: Result := 'VL85';
    2072: Result := 'TEP70U';
    2073: Result := 'TEP70BS';
    1463: Result := 'M62K';
    21015: Result := '2TE10UT';
    
    else Result := 'locomotive';
  end;
  DebugLog('Locomotive type ' + IntToStr(LocType) + ' = ' + Result);
end;

function GetUnixTimestamp: Int64;
var
  SystemTime: TSystemTime;
  FileTime: TFileTime;
  LargeInt: Int64;
begin
  // Получаем текущее время
  GetSystemTime(SystemTime);
  SystemTimeToFileTime(SystemTime, FileTime);
  
  // Конвертируем в Int64
  LargeInt := (Int64(FileTime.dwHighDateTime) shl 32) or FileTime.dwLowDateTime;
  
  // Конвертируем из 100-наносекундных интервалов с 1601 года в секунды с 1970 года
  Result := (LargeInt - 116444736000000000) div 10000000;
end;

function EscapeJSON(const S: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    case S[i] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #13: Result := Result + '\n';
      #10: Result := Result + '\r';
      else Result := Result + S[i];
    end;
  end;
end;

procedure SendHandshake;
var
  HandshakeData: AnsiString;
begin
  DebugLog('Sending handshake...');
  HandshakeData := '{"v":1,"client_id":"' + ApplicationID + '"}';
  
  if SendDiscordMessage(0, HandshakeData) then
    DebugLog('Handshake sent successfully')
  else
    DebugLog('Handshake failed');
end;

procedure SetActivity(const State, Details: string);
var
  JsonData: AnsiString;
begin
  if not Connected then
  begin
    DebugLog('SetActivity: Not connected');
    Exit;
  end;
  
  DebugLog('Setting activity - State: "' + State + '", Details: "' + Details + '"');
  
  // ВАЖНО: Discord требует pid!
  JsonData := Format(
    '{"cmd":"SET_ACTIVITY","nonce":"test-%d","args":{"pid":%d,"activity":{"details":"%s","state":"%s"}}}',
    [GetTickCount, GetCurrentProcessId, EscapeJSON(Details), EscapeJSON(State)]
  );
  
  if SendDiscordMessage(1, JsonData) then
    DebugLog('Activity set successfully')
  else
    DebugLog('Failed to set activity');
end;

procedure InitDiscordRPC;
begin
  DebugLog('=== Discord RPC Init Started ===');
  DebugLog('Application ID: ' + ApplicationID);
  
  try
    // Устанавливаем время старта (в миллисекундах)
    StartTimestamp := GetUnixTimestamp * 1000;
    DebugLog('Start timestamp: ' + IntToStr(StartTimestamp));
    
    if ConnectToDiscord then
    begin
      DebugLog('Connected to Discord, sending handshake...');
      SendHandshake;
      Sleep(200); // Увеличиваем задержку после handshake
      
      // Устанавливаем начальный статус
      DebugLog('Setting initial activity...');
      SetActivity('Starting up', 'ZDSimulator');
      LastUpdateTime := GetTickCount;
      
      DebugLog('Discord RPC initialized successfully');
    end
    else
    begin
      DebugLog('Failed to connect to Discord');
      Connected := False;
    end;
  except
    on E: Exception do
    begin
      DebugLog('Exception in InitDiscordRPC: ' + E.Message);
      Connected := False;
    end;
  end;
  
  DebugLog('=== Discord RPC Init Finished ===');
end;

procedure UpdateDiscordRPC;
var
  CurrentTime: DWORD;
  LocType: Integer;
  LocName: string;
  State, Details: string;
begin
  CurrentTime := GetTickCount;
  
  // Обновляем статус только раз в 15 секунд
  if (CurrentTime - LastUpdateTime) < 15000 then
    Exit;
    
  LastUpdateTime := CurrentTime;
  DebugLog('=== Discord RPC Update ===');
  
  // Если не подключены, пытаемся переподключиться
  if not Connected then
  begin
    DebugLog('Not connected, attempting to reconnect...');
    InitDiscordRPC;
    Exit;
  end;
  
  try
    LocType := GetCurrentLocomotiveType;
    
    if LocType > 0 then
    begin
      LocName := GetLocomotiveName(LocType);
      State := 'Driving ' + LocName;  // Используем английский
      Details := 'ZDSimulator';
    end
    else
    begin
      State := 'Main menu';           // Тоже английский
      Details := 'ZDSimulator';
    end;
    
    DebugLog('Updating activity...');
    SetActivity(State, Details);
    
  except
    on E: Exception do
    begin
      DebugLog('Exception in UpdateDiscordRPC: ' + E.Message);
      // При ошибке сбрасываем соединение для переподключения
      Connected := False;
      if PipeHandle <> INVALID_HANDLE_VALUE then
      begin
        CloseHandle(PipeHandle);
        PipeHandle := INVALID_HANDLE_VALUE;
      end;
    end;
  end;
end;

procedure ShutdownDiscordRPC;
begin
  DebugLog('=== Discord RPC Shutdown ===');
  
  try
    if Connected then
    begin
      DebugLog('Clearing activity...');
      // Очищаем активность
      SetActivity('', '');
      Connected := False;
    end;
    
    if PipeHandle <> INVALID_HANDLE_VALUE then
    begin
      DebugLog('Closing pipe handle...');
      CloseHandle(PipeHandle);
      PipeHandle := INVALID_HANDLE_VALUE;
    end;
    
    DebugLog('Discord RPC shutdown completed');
  except
    on E: Exception do
      DebugLog('Exception in ShutdownDiscordRPC: ' + E.Message);
  end;
end;

end.
