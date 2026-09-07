unit BilServerPro;
//----------------------------------------------------------------------------//
// BilServerPro                                                              //
//                                                                           //
// Обёртка над BilVServerPro.dll. Загружает её динамически (если найдена     //
// рядом с booster), вызывает Initialize/StartServer и отдаёт URL.           //
//                                                                           //
// Если DLL не лежит рядом — функция PRO просто недоступна, ZDS-Booster      //
// продолжает работать как обычно.                                           //
//----------------------------------------------------------------------------//
interface

uses Windows, SysUtils, BilVServerProApi;

procedure BilServerPro_Init(const BoosterDir: AnsiString);
procedure BilServerPro_Shutdown;

function BilServerPro_IsLoaded: Boolean;
function BilServerPro_IsAuthorized: Boolean;
function BilServerPro_IsRunning: Boolean;

function BilServerPro_Start: Boolean;
function BilServerPro_Stop: Boolean;

function BilServerPro_GetUrl: AnsiString;
function BilServerPro_GetLastError: AnsiString;

implementation

uses EngineUtils;

var
  Api: TBilVServerPro;
  Loaded: Boolean = False;

const
  PRO_LOG = 'DGLEngine_Log.txt';
  DLL_NAME = 'BilVServerPro.dll';

function GetModuleDir: AnsiString;
var
  Buffer: array[0..MAX_PATH] of AnsiChar;
  Slash: Integer;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  GetModuleFileNameA(HInstance, Buffer, MAX_PATH);
  Result := AnsiString(Buffer);
  Slash := LastDelimiter('\/', String(Result));
  if Slash > 0 then
    Result := Copy(Result, 1, Slash - 1);
end;

function CandidatePath(const BoosterDir: AnsiString; const Sub: AnsiString): AnsiString;
begin
  if BoosterDir = '' then
    Result := Sub
  else
    Result := BoosterDir + '\' + Sub;
end;

procedure BilServerPro_Init(const BoosterDir: AnsiString);
var
  Candidates: array[0..3] of AnsiString;
  i, Code: Integer;
  Path: AnsiString;
begin
  if Loaded then Exit;

  Candidates[0] := CandidatePath(BoosterDir, DLL_NAME);
  Candidates[1] := CandidatePath(GetModuleDir, DLL_NAME);
  Candidates[2] := CandidatePath(GetModuleDir, 'BilVServerPro\' + DLL_NAME);
  Candidates[3] := CandidatePath(GetModuleDir, '..\BilVServerPro\build\' + DLL_NAME);

  Path := '';
  for i := 0 to High(Candidates) do
  begin
    if FileExists(String(Candidates[i])) then
    begin
      Path := Candidates[i];
      Break;
    end;
  end;

  if Path = '' then
  begin
    AddToLogFile(PRO_LOG, 'BilServerPro: DLL not found near booster, PRO unavailable');
    Exit;
  end;

  if not BilVLoad(Api, Path) then
  begin
    AddToLogFile(PRO_LOG, AnsiString('BilServerPro: failed to load DLL ') + Path);
    Exit;
  end;

  Loaded := True;
  Code := Api.Initialize(PAnsiChar(BoosterDir));
  AddToLogFile(PRO_LOG, AnsiString('BilServerPro: DLL loaded from ') + Path +
    ', Initialize=' + AnsiString(IntToStr(Code)) +
    ', Authorized=' + AnsiString(IntToStr(Api.IsAuthorized)));
end;

procedure BilServerPro_Shutdown;
begin
  if not Loaded then Exit;
  BilVUnload(Api);
  Loaded := False;
  AddToLogFile(PRO_LOG, 'BilServerPro: DLL unloaded');
end;

function BilServerPro_IsLoaded: Boolean;
begin
  Result := Loaded;
end;

function BilServerPro_IsAuthorized: Boolean;
begin
  Result := Loaded and (Api.IsAuthorized = 1);
end;

function BilServerPro_IsRunning: Boolean;
begin
  Result := Loaded and (Api.IsServerRunning = 1);
end;

function BilServerPro_Start: Boolean;
var
  Code: Integer;
begin
  Result := False;
  if not Loaded then Exit;
  if not BilServerPro_IsAuthorized then
  begin
    AddToLogFile(PRO_LOG, 'BilServerPro: start refused, license not authorized');
    Exit;
  end;
  Code := Api.StartServer;
  Result := (Code = 0) or (Code = 1);
  AddToLogFile(PRO_LOG, AnsiString('BilServerPro: StartServer returned ') +
    AnsiString(IntToStr(Code)) +
    ', URL=' + BilServerPro_GetUrl);
end;

function BilServerPro_Stop: Boolean;
var
  Code: Integer;
begin
  Result := False;
  if not Loaded then Exit;
  Code := Api.StopServer;
  Result := (Code = 0) or (Code = 2);
  AddToLogFile(PRO_LOG, AnsiString('BilServerPro: StopServer returned ') +
    AnsiString(IntToStr(Code)));
end;

function BilServerPro_GetUrl: AnsiString;
var
  Buffer: array[0..511] of AnsiChar;
begin
  Result := '';
  if not Loaded then Exit;
  FillChar(Buffer, SizeOf(Buffer), 0);
  Api.GetServerUrl(@Buffer[0], SizeOf(Buffer));
  Result := AnsiString(Buffer);
end;

function BilServerPro_GetLastError: AnsiString;
var
  Buffer: array[0..511] of AnsiChar;
begin
  Result := '';
  if not Loaded then Exit;
  FillChar(Buffer, SizeOf(Buffer), 0);
  Api.GetLastError(@Buffer[0], SizeOf(Buffer));
  Result := AnsiString(Buffer);
end;

function AutoStartThreadProc(Param: Pointer): DWORD; stdcall;
begin
  Result := 0;
  // Стартуем после небольшой паузы, чтобы не работать в loader lock'е DllMain.
  Sleep(2000);
  try
    BilServerPro_Init('');
    if not BilServerPro_IsLoaded then
      Exit;
    if BilServerPro_IsAuthorized then
      BilServerPro_Start
    else
      AddToLogFile(PRO_LOG, 'BilServerPro: license not authorized, server not started. ' +
        BilServerPro_GetLastError);
  except
    on E: Exception do
      AddToLogFile(PRO_LOG, AnsiString('BilServerPro: auto-start exception: ') +
        AnsiString(E.Message));
  end;
end;

var
  AutoStartThread: THandle = 0;
  AutoStartThreadId: DWORD = 0;

initialization
  AutoStartThread := CreateThread(nil, 0, @AutoStartThreadProc, nil, 0, AutoStartThreadId);

finalization
  if AutoStartThread <> 0 then
    CloseHandle(AutoStartThread);
  if Loaded then BilVUnload(Api);

end.
