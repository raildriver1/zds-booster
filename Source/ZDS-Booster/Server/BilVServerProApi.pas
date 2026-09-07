unit BilVServerProApi;
//----------------------------------------------------------------------------//
// Delphi 2007 import unit для BilVServerPro.dll                             //
// API чисто C-стиль (extern "C", __stdcall), грузим через LoadLibrary.      //
//----------------------------------------------------------------------------//
interface

uses Windows, SysUtils;

const
  BILV_OK              = 0;
  BILV_ERROR           = -1;
  BILV_NOT_INITIALIZED = -2;
  BILV_NOT_AUTHORIZED  = -3;
  BILV_BUFFER_TOO_SMALL= -4;
  BILV_ALREADY_RUNNING = 1;
  BILV_NOT_RUNNING     = 2;

type
  TBilVNoArg          = function: Integer; stdcall;
  TBilVInitFn         = function(BoosterDir: PAnsiChar): Integer; stdcall;
  TBilVStringFn       = function(Buffer: PAnsiChar; BufferSize: Integer): Integer; stdcall;

  TBilVServerPro = record
    Handle:          HMODULE;
    GetApiVersion:   TBilVNoArg;
    Initialize:      TBilVInitFn;
    Shutdown:        TBilVNoArg;
    IsAvailable:     TBilVNoArg;
    IsAuthorized:    TBilVNoArg;
    StartServer:     TBilVNoArg;
    StopServer:      TBilVNoArg;
    IsServerRunning: TBilVNoArg;
    GetServerUrl:    TBilVStringFn;
    GetLastError:    TBilVStringFn;
  end;

function BilVLoad(var Api: TBilVServerPro; const DllPath: AnsiString): Boolean;
procedure BilVUnload(var Api: TBilVServerPro);

implementation

function BilVLoad(var Api: TBilVServerPro; const DllPath: AnsiString): Boolean;
var
  P: Pointer;
begin
  FillChar(Api, SizeOf(Api), 0);
  Api.Handle := LoadLibraryA(PAnsiChar(DllPath));
  Result := Api.Handle <> 0;
  if not Result then Exit;

  P := GetProcAddress(Api.Handle, 'BilV_GetApiVersion');   Api.GetApiVersion   := TBilVNoArg(P);
  P := GetProcAddress(Api.Handle, 'BilV_Initialize');      Api.Initialize      := TBilVInitFn(P);
  P := GetProcAddress(Api.Handle, 'BilV_Shutdown');        Api.Shutdown        := TBilVNoArg(P);
  P := GetProcAddress(Api.Handle, 'BilV_IsAvailable');     Api.IsAvailable     := TBilVNoArg(P);
  P := GetProcAddress(Api.Handle, 'BilV_IsAuthorized');    Api.IsAuthorized    := TBilVNoArg(P);
  P := GetProcAddress(Api.Handle, 'BilV_StartServer');     Api.StartServer     := TBilVNoArg(P);
  P := GetProcAddress(Api.Handle, 'BilV_StopServer');      Api.StopServer      := TBilVNoArg(P);
  P := GetProcAddress(Api.Handle, 'BilV_IsServerRunning'); Api.IsServerRunning := TBilVNoArg(P);
  P := GetProcAddress(Api.Handle, 'BilV_GetServerUrl');    Api.GetServerUrl    := TBilVStringFn(P);
  P := GetProcAddress(Api.Handle, 'BilV_GetLastError');    Api.GetLastError    := TBilVStringFn(P);

  Result :=
    Assigned(Api.GetApiVersion)   and
    Assigned(Api.Initialize)      and
    Assigned(Api.Shutdown)        and
    Assigned(Api.IsAvailable)     and
    Assigned(Api.IsAuthorized)    and
    Assigned(Api.StartServer)     and
    Assigned(Api.StopServer)      and
    Assigned(Api.IsServerRunning) and
    Assigned(Api.GetServerUrl)    and
    Assigned(Api.GetLastError);

  if not Result then BilVUnload(Api);
end;

procedure BilVUnload(var Api: TBilVServerPro);
begin
  if Api.Handle <> 0 then
  begin
    if Assigned(Api.Shutdown) then Api.Shutdown;
    FreeLibrary(Api.Handle);
  end;
  FillChar(Api, SizeOf(Api), 0);
end;

end.
