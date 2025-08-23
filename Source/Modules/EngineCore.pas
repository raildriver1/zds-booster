//----------------------------------------------------------------------------//
//DRON's OpenGl Engine V 1.0 http://dronprogs.mirgames.ru                     //
//                                                                            //
// EngineCore.pas V 1.1, 02.04.2008                                           //
//                                                                            //
// Simply main Engine routines.                                               //
//                                                                            //
// Copyright (C) 2005-2006 Korotkov Andrew aka DRON                           //
//                                                                            //
//This program is free software; you can redistribute it and/or               //
//modify it under the terms of the GNU General Public License                 //
//as published by the Free Software Foundation; either version 2              //
//of the License, or any later version.                                       //
//                                                                            //
//This program is distributed in the hope that it will be useful,             //
//but WITHOUT ANY WARRANTY; without even the implied warranty of              //
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               //
//GNU General Public License (http://www.gnu.org/copyleft/gpl.html)           //
//for more details.                                                           //
//----------------------------------------------------------------------------//
unit EngineCore;

interface
uses SysUtils, Windows, Messages, Variables, OpenGl, DrawFunc2D, Textures,
EngineUtils, DrawFunc3D, Console, Sound, DPC_packages, Advanced3D, Net,
IniFile, CheatMenu, DiscordRPC;

{$R DGLEngine.res}
{$R Logo.res}

function  AddTimer(Interval : Cardinal; OnTimerProcedure : pointer) : Cardinal; stdcall;
function  MaxMultiTexturingLayers : byte; stdcall;
function  IsShadowMapsSupported : boolean; stdcall;
function  IsDOT3Supported : boolean; stdcall;
function  EngineProcessInterval : byte; stdcall;
procedure DisableTimer(Ident : Cardinal); stdcall;
procedure EnableTimer(Ident : Cardinal); stdcall;
procedure SetTimerInterval(Ident, Interval : Cardinal); stdcall;
procedure StartKeyboardTextInput; stdcall;
procedure TextureCompression(Enable : boolean); stdcall;
procedure TextureMipMapping(Enable : boolean); stdcall;
procedure TextureFiltering(Enable : boolean); stdcall;
function  IsLeftMouseButtonPressed : boolean; stdcall;
function  IsRightMouseButtonPressed : boolean; stdcall;
procedure SetCutingPlanes(ZNear, ZFar : single); stdcall;
procedure SetViewAngle(Angle : single); stdcall;
procedure SetZBufferDepth(DepthBits : byte); stdcall;
function  GetScreenResX : integer; stdcall;
function  GetScreenResY : integer; stdcall;
procedure EndKeyboardTextInput; stdcall;
procedure EnableStencilBuffer; stdcall;
procedure AutoPause(Enable : boolean); stdcall;
function  GetKeyboardText : string; stdcall;
function  GetFPS : integer; stdcall;
procedure ApplicationName(Name : PAnsiChar); stdcall;
procedure SetEngineInifileName(Name : string); stdcall;
procedure SetEngineInitParametrs(ResX,ResY,ColorDepth, DisplayFrequency : integer; Fullscreen, VSync, UseEngineSettingsIni : boolean; WriteLog : boolean = true); stdcall;
procedure SetGameProcessInterval(Interval : byte); stdcall;
function  GetMousePos : Tpoint; stdcall;
function  GetMouseButtonPressed : byte; stdcall;
procedure SetWindowPosition(Left, Top : integer); stdcall;
function  IsMouseMoveing : boolean; stdcall;
function  MouseWheelDelta : integer; stdcall;
procedure PleaseNoLogo; stdcall;
procedure RegProcedure(ID: WORD; ProcAdress: pointer); stdcall;
procedure UpdateRenderRect(NewWidth,NewHeight : integer); stdcall;
procedure EngineMainDraw; stdcall;
procedure FreePlugin(Name : string); stdcall;
procedure EngineProcessMessages(var Msg: tagMSG); stdcall;
procedure QuitEngine; stdcall;
function  GetPluginHandle(Name : string) : THandle;stdcall;
function  IsPluginLoaded(Name : string) : boolean; stdcall;
function  LoadPlugin(Filename : string; DGLE_DLL_Handle : THandle) : string; stdcall;
procedure StartEngine; stdcall;
procedure StartEngine_DrawToPanel(PanelHandle : HWND); stdcall;
function  IsKeyPressed(Key : integer) : boolean; stdcall;
function  EngineVersion:shortstring; stdcall;
function  IsVShadersSupported : boolean; stdcall;
function  IsPShadersSupported : boolean; stdcall;
function  IsTexCompressionSupported : boolean; stdcall;
function  IsVBOSupported : boolean; stdcall;
procedure UseVBO(Enable : boolean); stdcall;
function  GetWindowHandle : HWND; stdcall;
procedure SceneUseMaterial(Enable : boolean); stdcall;
function  IsFBOSupported : boolean; stdcall;
procedure UseFBO(Enable : boolean); stdcall;

procedure SetTextureLODBias(x : integer); stdcall;
procedure SetTextureDefaultFilteringParameters(x: integer; y: integer); stdcall;


implementation

type
  TVSyncMode = (vsmSync, vsmNoSync);
  TSub_4877F4 = procedure(A: Double; B, C, D, E: DWORD); stdcall;

var
MBRight, MBleft : boolean;
LogoA : cardinal = 360;
Time, Time_Delta, Time_Old : DWORD;
paused : boolean = false;
FPSCount : Integer = 0;
FpsSumm : LongInt = 0;
CiclesCount : integer = 0;

TextInput : boolean = false;
InputText : string;

success: boolean = false;

keys : Array[0..255] of Boolean;
WheelDelta : integer = 0;

MouseMove : boolean = false;

MouseButton : Integer = -1;
Xcoord, Ycoord : Integer;

h_Instance : HINST;

LastConfigReadTime: DWORD = 0;

ZDSimPatched: Boolean = False;


procedure ApplyZDSimPatches;
var
  NOPs: array[0..4] of Byte;
  j: Integer;
  OldProtect: DWORD;
  
  // Переменные для CALL-инструкций
  HookSkorostemerAddr: Cardinal;
  CallAddress: Cardinal;
  NewOffset: Integer;
  CallBytes: array[0..4] of Byte;
  
  // Переменные для проверки файла
  LocType: Integer;
  LocNum: Integer;
  LocFolder: string;
  FilePath: string;
  IniPath: string;
  F: TextFile;
  Line, Key, Value: string;
  EqPos: Integer;
  
  // Функция получения папки локомотива
  function GetLocomotiveFolder(locType: Integer): string;
  begin
    case locType of
      812: Result := 'chs8';
      822: Result := 'chs7';
      882: Result := 'vl82';
      880: Result := 'vl80t';
      523: Result := 'chs4';
      524: Result := 'chs4kvr';
      621: Result := 'chs4t';
      2070: Result := 'tep70';
      2071: Result := 'tep70bs';
      1462: Result := 'm62';
      21014: Result := '2te10u';
      3154: Result := 'ed4m';
      3159: Result := 'ed9m';
      23152: Result := '2es5k';
      23142: Result := '2es4k';
      343: Result := 'chs2k';
      31714: Result := 'ep1m';
      811: Result := 'vl11m';
      885: Result := 'vl85';
      201318: Result := 'tem18dm';
      else
        Result := 'chs7'; // по умолчанию
    end;
  end;
  
begin
  if ZDSimPatched then Exit;
  
  try
    AddToLogFile(EngineLog, 'Starting ZDSim patches - checking settings and file...');
    
    // Читаем настройки из settings.ini напрямую
    IniPath := ExtractFilePath(ParamStr(0)) + 'settings.ini';
    AddToLogFile(EngineLog, 'Reading settings from: ' + IniPath);
    
    // Устанавливаем значения по умолчанию
    LocType := 885;
    LocNum := 167;
    
    if FileExists(IniPath) then
    begin
      AssignFile(F, IniPath);
      Reset(F);
      try
        while not Eof(F) do
        begin
          ReadLn(F, Line);
          
          // Убираем пробелы в начале и конце
          while (Length(Line) > 0) and (Line[1] = ' ') do
            Delete(Line, 1, 1);
          while (Length(Line) > 0) and (Line[Length(Line)] = ' ') do
            Delete(Line, Length(Line), 1);
            
          if (Length(Line) > 0) and (Line[1] <> ';') and (Line[1] <> '[') then
          begin
            EqPos := Pos('=', Line);
            if EqPos > 0 then
            begin
              Key := Copy(Line, 1, EqPos - 1);
              Value := Copy(Line, EqPos + 1, Length(Line) - EqPos);
              
              // Убираем пробелы из ключа и значения
              while (Length(Key) > 0) and (Key[Length(Key)] = ' ') do
                Delete(Key, Length(Key), 1);
              while (Length(Value) > 0) and (Value[1] = ' ') do
                Delete(Value, 1, 1);
              
              if Key = 'LocomotiveType' then
                LocType := StrToIntDef(Value, 885)
              else if Key = 'LocNum' then
                LocNum := StrToIntDef(Value, 167);
            end;
          end;
        end;
      finally
        CloseFile(F);
      end;
    end
    else
      AddToLogFile(EngineLog, 'Settings file not found, using defaults');
    
    AddToLogFile(EngineLog, 'Read from settings.ini: LocomotiveType=' + IntToStr(LocType) + ', LocNum=' + IntToStr(LocNum));
    
    // Получаем папку локомотива
    LocFolder := GetLocomotiveFolder(LocType);
    AddToLogFile(EngineLog, 'Locomotive folder: ' + LocFolder);
    
    // Формируем путь к файлу _skor.dmd
    FilePath := ExtractFilePath(ParamStr(0)) + 'data\' + LocFolder + '\' + IntToStr(LocNum) + '\_skor.dmd';
    AddToLogFile(EngineLog, 'Checking file: ' + FilePath);
    
    // Проверяем существование файла
    if not FileExists(FilePath) then
    begin
      AddToLogFile(EngineLog, 'File _skor.dmd not found, skipping ZDSim patches');
      Exit;
    end;
    
    AddToLogFile(EngineLog, 'File _skor.dmd found, applying patches...');
    
    // Подготавливаем данные для патчинга
    for j := 0 to 4 do
      NOPs[j] := $90;
    
    // Патчим главную инструкцию: mov byte ptr [eax], 1 -> mov byte ptr [eax], 0
    // По адресу .text:0072ABA2 меняем байт 01 на 00
    AddToLogFile(EngineLog, 'Patching main instruction at $0072ABA2...');
    
    if VirtualProtect(Pointer($0072ABA2), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      // Ищем байт 01 в инструкции mov byte ptr [eax], 1 (обычно C6 00 01)
      // и заменяем на 00 для mov byte ptr [eax], 0 (станет C6 00 00)
      if PByte($0072ABA2 + 2)^ = $01 then // Проверяем что это действительно mov [eax], 1
      begin
        PByte($0072ABA2 + 2)^ := $00; // Меняем 1 на 0
        AddToLogFile(EngineLog, 'Successfully patched main instruction: mov byte ptr [eax], 1 -> mov byte ptr [eax], 0');
      end
      else
      begin
        AddToLogFile(EngineLog, 'Warning: instruction pattern not found at expected offset');
        // Попробуем найти байт 01 в окрестности
        for j := 0 to 3 do
        begin
          if PByte($0072ABA2 + j)^ = $01 then
          begin
            PByte($0072ABA2 + j)^ := $00;
            AddToLogFile(EngineLog, 'Found and patched byte 01->00 at offset +' + IntToStr(j));
            Break;
          end;
        end;
      end;
      
      VirtualProtect(Pointer($0072ABA2), 4, OldProtect, OldProtect);
    end
    else
      AddToLogFile(EngineLog, 'Failed to patch main instruction at $0072ABA2, error: ' + IntToStr(GetLastError()));
    
    // Патчим call'ы
    AddToLogFile(EngineLog, 'Patching CALL addresses...');
    
    // ПЕРВЫЙ CALL - заменяем на CALL к sub_488374
    AddToLogFile(EngineLog, 'Creating CALL to sub_488374 at $006C4151...');
    
    try
      CallAddress := $006C4151;
      HookSkorostemerAddr := $00488374; // Адрес sub_488374
      
      // Вычисляем относительное смещение для CALL
      NewOffset := Integer(HookSkorostemerAddr) - Integer(CallAddress + 5);
      
      AddToLogFile(EngineLog, 'sub_488374 address: $' + IntToHex(HookSkorostemerAddr, 8));
      AddToLogFile(EngineLog, 'Call address: $' + IntToHex(CallAddress, 8));
      AddToLogFile(EngineLog, 'Calculated offset: $' + IntToHex(Cardinal(NewOffset), 8));
      
      // Формируем CALL-инструкцию
      CallBytes[0] := $E8; // CALL near
      PInteger(@CallBytes[1])^ := NewOffset; // 4 байта смещения
      
      if VirtualProtect(Pointer($006C4151), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        Move(CallBytes, Pointer($006C4151)^, 5);
        VirtualProtect(Pointer($006C4151), 5, OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'Successfully created CALL to sub_488374 at $006C4151');
      end
      else
        AddToLogFile(EngineLog, 'Failed to patch call at $006C4151, error: ' + IntToStr(GetLastError()));
        
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'Exception while creating CALL to sub_488374: ' + E.Message);
        // В случае ошибки - ставим NOPs как fallback
        if VirtualProtect(Pointer($006C4151), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          Move(NOPs, Pointer($006C4151)^, 5);
          VirtualProtect(Pointer($006C4151), 5, OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Fallback: patched $006C4151 with NOPs due to CALL creation error');
        end;
      end;
    end;
      
    // Второй CALL - оставляем как NOP
    if VirtualProtect(Pointer($006C41FE), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      Move(NOPs, Pointer($006C41FE)^, 5);
      VirtualProtect(Pointer($006C41FE), 5, OldProtect, OldProtect);
      AddToLogFile(EngineLog, 'Successfully patched call at $006C41FE');
    end
    else
      AddToLogFile(EngineLog, 'Failed to patch call at $006C41FE, error: ' + IntToStr(GetLastError()));
      
    // ТРЕТИЙ CALL - заменяем на CALL к HookSkorostemerViaKLUB
    AddToLogFile(EngineLog, 'Creating CALL to HookSkorostemerViaKLUB at $006C2FBB...');
    
    try
      // Получаем адрес функции HookSkorostemerViaKLUB
      HookSkorostemerAddr := Cardinal(@HookSkorostemerViaKLUB);
      CallAddress := $006C2FBB;
      
      // Вычисляем относительное смещение для CALL
      NewOffset := Integer(HookSkorostemerAddr) - Integer(CallAddress + 5);
      
      AddToLogFile(EngineLog, 'HookSkorostemerViaKLUB address: $' + IntToHex(HookSkorostemerAddr, 8));
      AddToLogFile(EngineLog, 'Call address: $' + IntToHex(CallAddress, 8));
      AddToLogFile(EngineLog, 'Calculated offset: $' + IntToHex(Cardinal(NewOffset), 8));
      
      // Формируем CALL-инструкцию
      CallBytes[0] := $E8; // CALL near
      PInteger(@CallBytes[1])^ := NewOffset; // 4 байта смещения
      
      if VirtualProtect(Pointer($006C2FBB), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        Move(CallBytes, Pointer($006C2FBB)^, 5);
        VirtualProtect(Pointer($006C2FBB), 5, OldProtect, OldProtect);
        AddToLogFile(EngineLog, 'Successfully created CALL to HookSkorostemerViaKLUB at $006C2FBB');
      end
      else
        AddToLogFile(EngineLog, 'Failed to patch call at $006C2FBB, error: ' + IntToStr(GetLastError()));
        
    except
      on E: Exception do
      begin
        AddToLogFile(EngineLog, 'Exception while creating CALL to HookSkorostemerViaKLUB: ' + E.Message);
        // В случае ошибки - ставим NOPs как fallback
        if VirtualProtect(Pointer($006C2FBB), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          Move(NOPs, Pointer($006C2FBB)^, 5);
          VirtualProtect(Pointer($006C2FBB), 5, OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Fallback: patched $006C2FBB with NOPs due to CALL creation error');
        end;
      end;
    end;

// Дополнительный патч для push инструкции
AddToLogFile(EngineLog, 'Applying Launcher push patch...');

// Вычисляем абсолютный адрес
CallAddress := Cardinal(GetModuleHandle(nil)) + $2C4136;
AddToLogFile(EngineLog, 'Launcher push patch address: $' + IntToHex(CallAddress, 8));

if VirtualProtect(Pointer(CallAddress), 5, PAGE_EXECUTE_READWRITE, OldProtect) then
begin
  if (PByte(CallAddress)^ = $68) and (PCardinal(CallAddress + 1)^ = $4129EB85) then
  begin
    PCardinal(CallAddress + 1)^ := $412b851f;
    AddToLogFile(EngineLog, 'Successfully patched push: $4129EB85 -> $412b851f');
  end
  else
    AddToLogFile(EngineLog, 'Push patch: pattern not found');
    
  VirtualProtect(Pointer(CallAddress), 5, OldProtect, OldProtect);
end
else
  AddToLogFile(EngineLog, 'Failed to patch push instruction, error: ' + IntToStr(GetLastError()));

    ZDSimPatched := True;
    AddToLogFile(EngineLog, 'All ZDSim patches applied successfully!');
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Critical error in ApplyZDSimPatches: ' + E.Message);
    end;
  end;
end;

procedure SetTextureLODBias(x : integer); stdcall;
begin
   x := 0;
end;

procedure SetTextureDefaultFilteringParameters(x: integer; y: integer); stdcall;
begin
   x := x + y;
end;

{Timer-------------------------------------------------------------------------}
function AddTimer(Interval : Cardinal; OnTimerProcedure : pointer) : Cardinal; stdcall;
begin
SetLength(EngTimers,Length(EngTimers)+1);
EngTimers[Length(EngTimers)-1].Active:=true;
EngTimers[Length(EngTimers)-1].Interval:=Interval;
EngTimers[Length(EngTimers)-1].Tick:=0;
@EngTimers[Length(EngTimers)-1].OnTimer:=OnTimerProcedure;
result:=Length(EngTimers)-1;
end;
{------------------------------------------------------------------}
procedure DisableTimer(Ident : Cardinal); stdcall;
begin
EngTimers[Ident].Active:=false;
end;
{------------------------------------------------------------------}
procedure EnableTimer(Ident : Cardinal); stdcall;
begin
EngTimers[Ident].Active:=true;
end;
{------------------------------------------------------------------}
procedure SetTimerInterval(Ident, Interval : Cardinal); stdcall;
begin
EngTimers[Ident].Interval:=Interval;
end;
{System Parametrs--------------------------------------------------------------}
function EngineVersion:shortstring; stdcall;
begin
result:=VERSION;
end;
{------------------------------------------------------------------}
function GetWindowHandle : HWND; stdcall;
begin
  result:=h_Wnd;
end;
function MaxMultiTexturingLayers : byte; stdcall;
var i : integer;
begin
 if not GL_ARB_multitexture then result:=1 else
 begin
  glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB,@i);
  result:=i;
 end;
end;
{------------------------------------------------------------------}
function IsShadowMapsSupported : boolean; stdcall;
begin
if not GL_ARB_shadow or not GL_ARB_depth_texture then result:=false else result:=true;
end;
{------------------------------------------------------------------}
function IsVShadersSupported : boolean; stdcall;
begin
result:=GL_ARB_vertex_program;
end;
{------------------------------------------------------------------}
function IsPShadersSupported : boolean; stdcall;
begin
result:=GL_ARB_fragment_program;
end;
{------------------------------------------------------------------}
function IsFBOSupported : boolean; stdcall;
begin
result:=GL_EXT_framebuffer_object;
end;
{------------------------------------------------------------------}
function IsVBOSupported : boolean; stdcall;
begin
result:=GL_ARB_vertex_buffer_object;
end;
{------------------------------------------------------------------}
function IsTexCompressionSupported : boolean; stdcall;
begin
result:=GL_ARB_texture_compression;
end;
{------------------------------------------------------------------}
function IsDOT3Supported : boolean; stdcall;
begin
result:=GL_ARB_texture_env_dot3;
end;
{------------------------------------------------------------------}
function EngineProcessInterval : byte; stdcall;
begin
result:=PROCESS_INTERVAL;
end;
{------------------------------------------------------------------}
procedure UseFBO(Enable : boolean); stdcall;
begin
_UseFBO:=Enable;
end;
{------------------------------------------------------------------}
procedure UseVBO(Enable : boolean); stdcall;
begin
_UseVBO:=Enable;
end;
{------------------------------------------------------------------}
procedure TextureCompression(Enable : boolean); stdcall;
begin
_TextureCompression:=Enable;
end;
{------------------------------------------------------------------}
procedure TextureMipMapping(Enable : boolean); stdcall;
begin
MipMapping:=Enable;
end;
{------------------------------------------------------------------}
procedure SceneUseMaterial(Enable : boolean); stdcall;
begin
_SceneDontUseMat:=not Enable;
end;
{------------------------------------------------------------------}
procedure TextureFiltering(Enable : boolean); stdcall;
begin
_TextureFiltering:=Enable;
end;
{------------------------------------------------------------------}
procedure SetCutingPlanes(ZNear, ZFar : single); stdcall;
begin
initZNear:=ZNear;
initZFar:=ZFar;
end;
{------------------------------------------------------------------}
procedure SetViewAngle(Angle : single); stdcall;
begin
initAngle:=Angle;
end;
{------------------------------------------------------------------}
procedure SetZBufferDepth(DepthBits : byte); stdcall;
begin
InitZBuffer:=DepthBits;
end;
{------------------------------------------------------------------}
function GetScreenResX : integer; stdcall;
begin
result:=InitResX;
end;
{------------------------------------------------------------------}
function GetScreenResY : integer; stdcall;
begin
result:=InitResY;
end;
{------------------------------------------------------------------}
procedure EnableStencilBuffer; stdcall;
begin
INITStencil:=true;
end;
{------------------------------------------------------------------}
procedure AutoPause(Enable : boolean); stdcall;
begin
_AllowAutoPause:=Enable;
end;
{------------------------------------------------------------------}
function GetFPS : integer; stdcall;
begin
result:=ExtFPS;
end;
{------------------------------------------------------------------}
procedure ApplicationName(Name : PAnsiChar); stdcall;
begin
WND_TITLE:=Name;
SetWindowText(h_Wnd, PChar(WND_TITLE));
end;
{------------------------------------------------------------------}
procedure SetEngineInifileName(Name : string); stdcall;
begin
IniFileName:=Name;
end;
{------------------------------------------------------------------}
procedure SetEngineInitParametrs(ResX,ResY,ColorDepth, DisplayFrequency : integer; Fullscreen, VSync, UseEngineSettingsIni : boolean; WriteLog : boolean = true); stdcall;
begin
  UseSettingsIni:=UseEngineSettingsIni;
  InitFullscreen:=Fullscreen;
  InitVsync:=VSync;
  InitResX:=ResX;
  InitResY:=ResY;
  InitPDepth:=ColorDepth;
  ISWriteLog:=WriteLog;
  InitFREQUENCY:=DisplayFrequency;
end;
{------------------------------------------------------------------}
procedure SetGameProcessInterval(Interval : byte); stdcall;
begin
PROCESS_INTERVAL:=Interval;
end;
{------------------------------------------------------------------}
procedure SetWindowPosition(Left, Top : integer); stdcall;
begin
 WinX:=Left;
 WinY:=Top;
end;
{------------------------------------------------------------------}
procedure PleaseNoLogo; stdcall;
begin
 ShowLogo:=false;
end;
{------------------------------------------------------------------}
procedure RegProcedure(ID: WORD; ProcAdress: pointer); stdcall;
begin

 case ID of
 0: @glDraw := ProcAdress;
 1: @ProcessGame := ProcAdress;
 2: @LoadTextures:= ProcAdress;
 3: @DestroyAll:= ProcAdress;
 end;//Case

end;
{Input-------------------------------------------------------------------------}
procedure StartKeyboardTextInput; stdcall;
begin
 TextInput:=true;
 InputText:='';
end;
{------------------------------------------------------------------}
function IsLeftMouseButtonPressed : boolean; stdcall;
begin
result:=Mbleft;
end;
{------------------------------------------------------------------}
function IsRightMouseButtonPressed : boolean; stdcall;
begin
result:=MbRight;
end;
{------------------------------------------------------------------}
procedure EndKeyboardTextInput; stdcall;
begin
 TextInput:=false;
end;
{------------------------------------------------------------------}
function GetKeyboardText : string; stdcall;
begin
result:=InputText;
end;
{------------------------------------------------------------------}
function GetMousePos : Tpoint; stdcall;
begin
 result.X:=round(MoveXcoord);
 result.Y:=round(MoveYcoord);
end;
{------------------------------------------------------------------}
function GetMouseButtonPressed : byte; stdcall;
begin
//1-left 2-right 3-middle
result:=MouseButton;
end;
{------------------------------------------------------------------}
function IsKeyPressed(Key : integer) : boolean; stdcall;
begin
result:=Keys[Key];
end;
{------------------------------------------------------------------}
function IsMouseMoveing : boolean; stdcall;
begin
result:=MouseMove;
MouseMove := False;
end;
{------------------------------------------------------------------}
function MouseWheelDelta : integer; stdcall;
begin
 result:=0;
 if WheelDelta<0 then result:=-1;
 if WheelDelta>0 then result:=1;
WheelDelta:=0;
end;

{------------------------------------------------------------------------------}
//////////////////////////////ENGINE CORE///////////////////////////////////////
{------------------------------------------------------------------------------}
procedure EngineMainProcess;
var i : integer;
CurrentTime: DWORD;
begin

CurrentTime := GetTimer;
  
  // Чтение конфига раз в 2 секунды (2000 мс)
  if (CurrentTime - LastConfigReadTime) >= 1000 then
  begin
    ProcessAllModules;
    LastConfigReadTime := CurrentTime;
  end;

  UpdateDiscordRPC; // Обновляем Discord Rich Presence

     if ShowLogo and (LogoA<>0) then LogoA:=LogoA-1;
     if @ProcessGame<>nil then ProcessGame;

     if length(Plugins)<>0 then
       for i:=0 to length(Plugins)-1 do
        if (Plugins[i].Loaded) and (@Plugins[i].Process<>nil) then Plugins[i].Process;

     if length(EngTimers)<>0 then
      for i:=0 to length(EngTimers)-1 do
       if EngTimers[i].Active then
       begin
       EngTimers[i].Tick:=EngTimers[i].Tick+PROCESS_INTERVAL;
        if EngTimers[i].Tick>=EngTimers[i].Interval then
        begin
        EngTimers[i].Tick:=0;
        EngTimers[i].OnTimer;
        end;
       end;
end;
{------------------------------------------------------------------}
procedure glResizeWnd(Width, Height : Integer);
begin
  if (Height = 0) then
    Height := 1;
  CurW := width;
  CurH := height;

  glViewport(0, 0, Width, Height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(initAngle, Width/Height, InitZNear, InitZFar);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;
{------------------------------------------------------------------}
procedure UpdateRenderRect(NewWidth,NewHeight : integer); stdcall;
begin
 glResizeWnd(NewWidth,NewHeight);
end;
{------------------------------------------------------------------}
procedure EngineMainDraw; stdcall;
var i : cardinal;
begin

      //PSingle($8CC9A4)^ := -2.0;
        ProcessFreecam;

//        if not success then
//          begin
//            ApplyMaxVisibleDistance;
//            ProcessStepForwardConfig;
//            PatchDrawSkyCall;
//
//            success := True;
//          end;
        
        LoadSettingsAndCustomModels;

        //ProcessDayNightSystem;

        FPSCount:=FPSCount+1;

       glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
       glLoadIdentity();

       _frustumcalculated:=false;

       SetLength(Obj3DInfo,0);
       MultyTexActive:=false;

       if length(Plugins)<>0 then
         for i:=0 to length(Plugins)-1 do
          if(Plugins[i].Loaded) and (@Plugins[i].DrawPre<>nil) then Plugins[i].DrawPre;


       if @glDraw<>nil then glDraw;



       if length(Plugins)<>0 then
         for i:=0 to length(Plugins)-1 do
          if(Plugins[i].Loaded) and (@Plugins[i].DrawPost<>nil) then Plugins[i].DrawPost;

       DrawCheatMenu;

       if ShowLogo and (LogoA<>0) then
       begin
        Begin2D;
        if LogoA>200 then i:=200 else i:=LogoA;
         DrawTexture2D(Logo,InitResX-LOGO_SIZE,InitResY-LOGO_SIZE,LOGO_SIZE,LOGO_SIZE,0,i,$FFFFFF,false);
        End2D;
       end;

       glFlush();
       glFinish();

      if paused then Sleep(1);

      SwapBuffers(h_DC);
end;



{------------------------------------------------------------------}
procedure FreePlugin(Name : string); stdcall;
var i : integer;
begin
 for i:=0 to length(Plugins)-1 do
  if (Plugins[i].Name=Name) and Plugins[i].Loaded then
  begin
  Plugins[i].Loaded:=false;
  if @Plugins[i].Free<>nil then Plugins[i].Free;
  if not FreeLibrary(Plugins[i].Handle) then
  begin
  AddToLogFile(EngineLog,'Error unloading plugin "'+ Name +'"!');
  Exit;
  end;
  AddToLogFile(EngineLog,'Plugin "'+ Name +'" has been freed properly.');
  Exit;
  end;
AddToLogFile(EngineLog,'Plugin "'+ Name +'" is not loaded!');
end;
{------------------------------------------------------------------}
procedure glKillWnd(Fullscreen : Boolean);
var 
  i : integer;
  CleanupTimeout: Cardinal;
  ForcedShutdown: Boolean;
begin
  CleanupTimeout := GetTickCount;
  ForcedShutdown := False;
  
  try
    // Устанавливаем флаг завершения как можно раньше
    StartQuitingEngine := True;
    
    // Быстрое отключение таймеров
    try
      KillTimer(h_Wnd, FPS_TIMER);
      KillTimer(h_Wnd, PROCESS_TIMER);
    except
      // Игнорируем ошибки с таймерами
    end;

    // Вызываем пользовательскую процедуру очистки с тайм-аутом
    try
      if @DestroyAll <> nil then
      begin
        DestroyAll;
        // Проверяем, не зависла ли очистка
        if (GetTickCount - CleanupTimeout) > 2000 then
        begin
          try
            AddToLogFile(EngineLog, 'DestroyAll timeout exceeded, forcing shutdown');
          except
            // Игнорируем ошибки записи в лог
          end;
          ForcedShutdown := True;
        end;
      end;
    except
      on E: Exception do
      begin
        try
          AddToLogFile(EngineLog, 'DestroyAll error: ' + E.Message);
        except
          // Игнорируем ошибки записи в лог
        end;
        ForcedShutdown := True;
      end;
    end;

    if ForcedShutdown then Exit;

    // Освобождаем плагины с тайм-аутом
    try
      if length(Plugins) <> 0 then
      begin
        for i := 0 to length(Plugins) - 1 do
        begin
          if Plugins[i].Loaded then
          begin
            try
              FreePlugin(Plugins[i].Name);
            except
              // Игнорируем ошибки освобождения плагинов
            end;
          end;
          
          // Проверяем тайм-аут
          if (GetTickCount - CleanupTimeout) > 3000 then
          begin
            try
              AddToLogFile(EngineLog, 'Plugin cleanup timeout, forcing shutdown');
            except
              // Игнорируем ошибки записи в лог
            end;
            ForcedShutdown := True;
            Break;
          end;
        end;
      end;
      Plugins := nil;
    except
      on E: Exception do
      begin
        try
          AddToLogFile(EngineLog, 'Plugin cleanup error: ' + E.Message);
        except
          // Игнорируем ошибки записи в лог
        end;
        ForcedShutdown := True;
      end;
    end;

    if ForcedShutdown then Exit;

    // Освобождаем движок с тайм-аутом
    try
      FreeEng;
      if (GetTickCount - CleanupTimeout) > 5000 then
      begin
        try
          AddToLogFile(EngineLog, 'FreeEng timeout exceeded');
        except
          // Игнорируем ошибки записи в лог
        end;
        ForcedShutdown := True;
      end;
    except
      on E: Exception do
      begin
        try
          AddToLogFile(EngineLog, 'FreeEng error: ' + E.Message);
        except
          // Игнорируем ошибки записи в лог
        end;
        ForcedShutdown := True;
      end;
    end;

    if ForcedShutdown then Exit;

    // Быстрая очистка массивов
    try
      ConsoleList := nil;
      Commands := nil;
      EngTimers := nil;
    except
      // Игнорируем ошибки очистки массивов
    end;

    // Освобождаем звук
    try
      FreeSound;
    except
      // Игнорируем ошибки звука
    end;

    // Восстанавливаем дисплей
    if Fullscreen and not DrawToPanel then
    begin
      try
        ChangeDisplaySettings(devmode(nil^), 0);
      except
        // Игнорируем ошибки изменения дисплея
      end;
    end;

    if not DrawToPanel then
    begin
      try
        ShowCursor(True);
      except
        // Игнорируем ошибки курсора
      end;
    end;

    // OpenGL очистка с защитой от зависания
    try
      if h_RC <> 0 then
      begin
        if not wglMakeCurrent(h_DC, 0) then
        begin
          try
            AddToLogFile(EngineLog, 'Release of DC and RC failed!');
          except
            // Игнорируем ошибки записи в лог
          end;
        end;

        if not wglDeleteContext(h_RC) then
        begin
          try
            AddToLogFile(EngineLog, 'Release of rendering context failed!');
          except
            // Игнорируем ошибки записи в лог
          end;
        end;
        h_RC := 0;
      end;
    except
      on E: Exception do
      begin
        try
          AddToLogFile(EngineLog, 'OpenGL cleanup error: ' + E.Message);
        except
          // Игнорируем ошибки записи в лог
        end;
      end;
    end;

    try
      if (h_DC > 0) and (h_Wnd <> 0) then
      begin
        if ReleaseDC(h_Wnd, h_DC) = 0 then
        begin
          try
            AddToLogFile(EngineLog, 'Release of device context failed!');
          except
            // Игнорируем ошибки записи в лог
          end;
        end;
        h_DC := 0;
      end;
    except
      on E: Exception do
      begin
        try
          AddToLogFile(EngineLog, 'DC cleanup error: ' + E.Message);
        except
          // Игнорируем ошибки записи в лог
        end;
      end;
    end;

    if not DrawToPanel then
    begin
      try
        if h_Wnd <> 0 then
        begin
          if not DestroyWindow(h_Wnd) then
          begin
            try
              AddToLogFile(EngineLog, 'Unable to destroy window!');
            except
              // Игнорируем ошибки записи в лог
            end;
          end;
          h_Wnd := 0;
        end;
      except
        on E: Exception do
        begin
          try
            AddToLogFile(EngineLog, 'Window destruction error: ' + E.Message);
          except
            // Игнорируем ошибки записи в лог
          end;
        end;
      end;

      try
        if not UnRegisterClass('DGLEngine', h_Instance) then
        begin
          try
            AddToLogFile(EngineLog, 'Unable to unregister window class!');
          except
            // Игнорируем ошибки записи в лог
          end;
        end;
      except
        on E: Exception do
        begin
          try
            AddToLogFile(EngineLog, 'Class unregistration error: ' + E.Message);
          except
            // Игнорируем ошибки записи в лог
          end;
        end;
      end;
    end;

    // Финальная запись в лог с информацией о FPS
    try
      if ciclescount = 0 then ciclescount := 1;
      AddToLogFile(EngineLog, 'Average FPS: ' + inttostr(FpsSumm div ciclescount));
      AddToLogFile(EngineLog, 'Engine shutdown completed.', false, true);
    except
      // Если лог не работает, ничего не делаем
    end;

  except
    on E: Exception do
    begin
      // Критическая ошибка - записываем и принудительно завершаем
      try
        AddToLogFile(EngineLog, 'CRITICAL shutdown error: ' + E.Message + ' - forcing immediate exit');
      except
        // Если даже это не работает, просто выходим
      end;
    end;
  end;
end;


{------------------------------------------------------------------}
function WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var 
  i : integer; 
  s: string;
  PlMsg : tagMSG;
begin
  // В начале функции добавляем проверку флага завершения
  if StartQuitingEngine then
  begin
    Result := DefWindowProc(hWnd, Msg, wParam, lParam);
    Exit;
  end;

  // Обработка сообщений плагинов с защитой от ошибок
  try
    if length(Plugins) <> 0 then
    begin
      for i := 0 to length(Plugins) - 1 do
      begin
        if Plugins[i].Loaded then
        begin
          try
            PlMsg.hWnd := hWnd;
            PlMsg.Message := Msg;
            PlMsg.wParam := wParam;
            PlMsg.lParam := lParam;
            if @Plugins[i].OnMessage <> nil then 
              Plugins[i].OnMessage(PlMsg);
          except
            // Игнорируем ошибки в плагинах
          end;
        end;
      end;
    end;
  except
    // Игнорируем ошибки массива плагинов
  end;

  case (Msg) of
    WM_KILLFOCUS:
     begin
      try
        if _AllowAutoPause then
        begin
          paused := true;
          SetWindowText(h_Wnd, PChar(WND_TITLE + ' [Paused]'));
        end;
      except
        // Игнорируем ошибки паузы
      end;
      result := 0;
     end;

    WM_SETFOCUS:
     begin
      try
        paused := false;
        SetWindowText(h_Wnd, PChar(WND_TITLE));
      except
        // Игнорируем ошибки снятия паузы
      end;
      result := 0;
     end;
     
    WM_CLOSE:
     if not DrawToPanel then
      begin
        // Устанавливаем флаг немедленно и максимально быстро завершаем
        StartQuitingEngine := True;
        try
          PostQuitMessage(0);
        except
          // Если PostQuitMessage не работает, просто выходим
        end;
        Result := 0;
      end;
      
    WM_KEYDOWN:
      begin
        try
          keys[wParam] := True;

          if wParam = 123 then
            ToggleMenu;

        except
          // Игнорируем ошибки массива клавиш
        end;
        Result := 0;
      end;
      
    WM_KEYUP:
      begin
        try
          if wParam = 44 then PrintScreen;
          keys[wParam] := False;
        except
          // Игнорируем ошибки обработки клавиш
        end;
        Result := 0;
      end;
      
   WM_CHAR:
      begin
        try
          if textinput then
          begin
           if wParam in [32..255] then 
             InputText := InputText + chr(wParam) 
           else if wParam = 8 then
            begin
            s := InputText;
            InputText := '';
            if length(s) > 1 then
             for i := 1 to length(s) - 1 do 
               InputText := InputText + s[i];
            end;
          end;

          if (ConsoleActive) and (not ConGoUp) then
          begin
          if (wParam = ord('`')) or (wParam = ord('~')) or (wParam = ord('¨')) or (wParam = ord('¸')) then
            if (not ConsoleDraw) then 
              ConsoleDraw := true 
            else if ConY > InitResY div 3 then 
              ConGoUp := true;
          if wParam = 8 then 
            CurentString := OnBack;
          if (wParam in [32..255]) and not (wParam in [ord('`'), ord('~')]) and (wParam <> ord('¨')) and (wParam <> ord('¸'))
          and (ConsoleDraw) then 
            CurentString := CurentString + chr(wParam);
          end;
        except
          // Игнорируем ошибки обработки текста
        end;
      end;
      
    WM_MOUSEWHEEL :
      begin
        try
          WheelDelta := SmallInt(HIWORD(wParam));
        except
          WheelDelta := 0;
        end;
        Result := 0;
      end;
      
    WM_MOUSEMOVE:
      begin
        try
          MouseMove := True;
          MoveXcoord := LOWORD(lParam);
          MoveYcoord := HIWORD(lParam);

          HandleMenuHover(Round(MoveXcoord), Round(MoveYcoord));
        except
          // Игнорируем ошибки мыши
        end;
        Result := 0;
      end;
      
    WM_LBUTTONDOWN:
      begin
        try
          ReleaseCapture();
          SetCapture(h_Wnd);
          MouseButton := 1;
          MBLeft := true;
          Xcoord := LOWORD(lParam);
          Ycoord := HIWORD(lParam);

          HandleMenuClick(Xcoord, Ycoord);

        except
          // Игнорируем ошибки мыши
        end;
        Result := 0;
      end;
      
    WM_RBUTTONDOWN:
      begin
        try
          ReleaseCapture();
          SetCapture(h_Wnd);
          MouseButton := 2;
          MBRight := true;
          Xcoord := LOWORD(lParam);
          Ycoord := HIWORD(lParam);
        except
          // Игнорируем ошибки мыши
        end;
        Result := 0;
      end;
      
    WM_MBUTTONDOWN:
      begin
        try
          ReleaseCapture();
          SetCapture(h_Wnd);
          MouseButton := 3;
          Xcoord := LOWORD(lParam);
          Ycoord := HIWORD(lParam);
        except
          // Игнорируем ошибки мыши
        end;
        Result := 0;
      end;
      
    WM_RBUTTONUP:
      begin
        try
          ReleaseCapture();
          MBRight := false;
          MouseButton := 0;
          XCoord := 0;
          YCoord := 0;
        except
          // Игнорируем ошибки мыши
        end;
        Result := 0;
      end;
      
    WM_LBUTTONUP:
      begin
        try
          ReleaseCapture();
          MBLeft := false;
          MouseButton := 0;
          XCoord := 0;
          YCoord := 0;

          HandleMenuMouseUp;
        except
          // Игнорируем ошибки мыши
        end;
        Result := 0;
      end;
      
    WM_MBUTTONUP:
      begin
        try
          ReleaseCapture();
          MouseButton := 0;
          XCoord := 0;
          YCoord := 0;
        except
          // Игнорируем ошибки мыши
        end;
        Result := 0;
      end;
      
    WM_SIZE:
     if not DrawToPanel then
      begin
        try
          if OpenGLInitialized then
           glResizeWnd(LOWORD(lParam), HIWORD(lParam));
        except
          // Игнорируем ошибки изменения размера
        end;
        Result := 0;
      end;
      
    WM_TIMER :
       begin
        try
          if wParam = FPS_TIMER then
          begin
          try
            Mci_loop;
          except
            // Игнорируем ошибки звука
          end;

            FPSCount := Round(FPSCount * 1000/FPS_INTERVAL);
            ExtFPS := FPSCount;
            FpsSumm := FpsSumm + ExtFPS;
            ciclescount := ciclescount + 1;
            FPSCount := 0;
          end;
          
          if DrawToPanel and (wParam = PROCESS_TIMER) then 
          begin
            try
              EngineMainProcess;
            except
              // Игнорируем ошибки основного процесса
            end;
          end;
        except
          // Игнорируем ошибки таймера
        end;
       end;
    else
      Result := DefWindowProc(hWnd, Msg, wParam, lParam);
  end;
end;


{------------------------------------------------------------------}
procedure EngineProcessMessages(var Msg: tagMSG); stdcall;
begin
if (Msg.hwnd = h_Wnd) or (Msg.message=WM_KEYDOWN) or
(Msg.message=WM_KEYUP) or (Msg.message=WM_CHAR) then
 WndProc(h_Wnd,Msg.message,Msg.wParam,Msg.lParam);
end;
{------------------------------------------------------------------}
procedure QuitEngine; stdcall;
begin
StartQuitingEngine := True;
if DrawToPanel then glKillWnd(InitFullScreen);
end;
{------------------------------------------------------------------}
function GetPluginHandle(Name : string) : THandle;stdcall;
var i : integer;
begin
result:=0;
if length(Plugins)<>0 then
 for i:=0 to length(Plugins)-1 do
  if lowercase(Plugins[i].Name)=lowercase(Name) then
  begin
  result:=Plugins[i].Handle;
  Exit;
  end;
end;
{------------------------------------------------------------------}
function IsPluginLoaded(Name : string) : boolean; stdcall;
var i : integer;
begin
result:=false;
if length(Plugins)<>0 then
 for i:=0 to length(Plugins)-1 do
  if lowercase(Plugins[i].Name)=lowercase(Name) then
  begin
  result:=Plugins[i].Loaded;
  Exit;
  end;
end;
{------------------------------------------------------------------}
function LoadPlugin(Filename : string; DGLE_DLL_Handle : THandle) : string; stdcall;
var current : integer;
 function ExtractName(s : string):string;
 var i : integer;
 slash : boolean;
 begin
  result:='';
  slash:=false;
   for i:=1 to length(s)-4 do
   begin
    if s[i]='\' then slash:=true;
   result:=result+s[i];
   end;

 if not slash then exit;

 s:=result;
 result:='';

  for i:=1 to length(s) do
    if s[i]='\' then result:='' else result:=result+s[i];
end;

begin
result:='';
if fileexists(filename) then
begin
setlength(Plugins,length(Plugins)+1);
current:=length(Plugins)-1;

Plugins[current].Name:= ExtractName(Filename);
Plugins[current].Handle := LoadLibrary(PChar(Filename));

if Plugins[current].Handle=0 then
begin
setlength(Plugins,length(Plugins)-1);
MessageBox(0, PChar('Error loading plugin "'+ ExtractName(Filename) +'"!'), PChar('Plugin Unit'), MB_OK);
AddToLogFile(EngineLog,'Error loading plugin "'+ ExtractName(Filename) +'"!');
end;

@Plugins[current].Init:=GetProcAddress(Plugins[current].Handle,'Init');
@Plugins[current].DrawPre:=GetProcAddress(Plugins[current].Handle,'DrawPre');
@Plugins[current].DrawPost:=GetProcAddress(Plugins[current].Handle,'DrawPost');
@Plugins[current].Process:=GetProcAddress(Plugins[current].Handle,'Process');
@Plugins[current].Free:=GetProcAddress(Plugins[current].Handle,'Free');
@Plugins[current].OnMessage:=GetProcAddress(Plugins[current].Handle,'OnMessage');

result:=Plugins[current].Name;
Plugins[current].Loaded:=true;

if not StartQuitingEngine then
AddToLogFile(EngineLog,'Plugin "'+ ExtractName(Filename) +'" loaded successfully.');

if @Plugins[current].Init<>nil then Plugins[current].Init(DGLE_DLL_Handle,h_Wnd,PROCESS_INTERVAL);

end else
begin
MessageBox(0, PChar('File "'+ Filename +'" not found!'), PChar('Plugin Unit'), MB_OK);
AddToLogFile(EngineLog,'File "'+ Filename +'" not found!');
end;
end;
{------------------------------------------------------------------}
procedure VBL2(vsync : TVSyncMode);
var
   i : Integer;
begin
   if WGL_EXT_swap_control then
   begin
      i := wglGetSwapIntervalEXT;
      case VSync of
         vsmSync    : if i<>1 then wglSwapIntervalEXT(1);
         vsmNoSync  : if i<>0 then wglSwapIntervalEXT(0);
      else
         Assert(False);
      end;
   end;

end;

function IsTargetProcess: Boolean;
var
  ModuleName: array[0..MAX_PATH] of Char;
  ProcessName: string;
begin
  Result := False;
  
  // Получаем полный путь к исполняемому файлу текущего процесса
  if GetModuleFileName(0, ModuleName, MAX_PATH) > 0 then
  begin
    ProcessName := ExtractFileName(string(ModuleName));
    
    // Проверяем, является ли это процессом Launcher.exe (без учета регистра)
    Result := LowerCase(ProcessName) = 'launcher.exe';
  end;
end;
{--------------------------------------------------------------------}
function glCreateWnd(Width, Height : Integer; Fullscreen : Boolean; PixelDepth, Freq : Integer; Vsync : boolean) : Boolean;
var
  wndClass : TWndClass;
  LocType: Integer; 
  dwStyle : DWORD;
  dwExStyle : DWORD;
  DTDC :  HDC;
  DT_W,DT_H : integer;
  dmScreenSettings : DEVMODE;
  PixelFormat : GLuint;
  pfd : TPIXELFORMATDESCRIPTOR;
  EngineIni : TIniFile;
  real_win_h,real_win_w, i, Major, Minor : integer;
  SysInfo : _SYSTEM_INFO;



 function BoolToInt(Bool : boolean) : byte;
 begin
 if bool then result:=1 else result:=0;
 end;

begin

  LastConfigReadTime := GetTimer;

  AddToLogFile(EngineLog,ENGINE_LABEL+' Started...',true,true,true);
  AddToLogFile(EngineLog,'Build: '+VERSION);
  AddToLogFile(EngineLog,'http://dronprogs.mirgames.ru');

  GetSystemInfo(SysInfo);
  SetProcessAffinityMask(GetCurrentProcess, SysInfo.dwActiveProcessorMask);

if (UseSettingsIni) and (not DrawToPanel) then
begin
  if fileexists(IniFileName) then
  begin

    EngineIni:=TIniFile.Create(IniFileName);

    AddToLogFile(EngineLog,'Read Ini File ("'+IniFileName+'").');

    Width:=strtoint(EngineIni.GetIniSectionKeyValue('Screen Mode','ResX'));
    Height:=strtoint(EngineIni.GetIniSectionKeyValue('Screen Mode','ResY'));
    PixelDepth:=strtoint(EngineIni.GetIniSectionKeyValue('Screen Mode','PixelDepth'));
    Freq:= strtoint(EngineIni.GetIniSectionKeyValue('Screen Mode','DisplayFrequency'));

    InitResX:=Width;
    InitResY:=Height;
    InitPDepth:=PixelDepth;
    InitFREQUENCY:=Freq;

    if strtoint(EngineIni.GetIniSectionKeyValue('Screen Mode','Fullscreen')) = 1 then
    Fullscreen:=true else Fullscreen:=false;

    if strtoint(EngineIni.GetIniSectionKeyValue('Screen Mode','VSync')) = 1 then
    Vsync:=true else Vsync:=false;

    InitVSync:=Vsync;
    InitFullscreen:=Fullscreen;

 end else
 begin
 EngineIni:=TIniFile.Create(IniFileName);
 AddToLogFile(EngineLog,'Write Ini File ("'+IniFileName+'").');
 EngineIni.CreateIniSection('Screen Mode');
 EngineIni.SetIniSectionKeyValue('Screen Mode','ResX',inttostr(Width));
 EngineIni.SetIniSectionKeyValue('Screen Mode','ResY',inttostr(Height));
 EngineIni.SetIniSectionKeyValue('Screen Mode','PixelDepth',inttostr(PixelDepth));
 EngineIni.SetIniSectionKeyValue('Screen Mode','DisplayFrequency',inttostr(Freq));
 if Fullscreen then EngineIni.SetIniSectionKeyValue('Screen Mode','Fullscreen','1')
 else EngineIni.SetIniSectionKeyValue('Screen Mode','Fullscreen','0');
 if Vsync then EngineIni.SetIniSectionKeyValue('Screen Mode','VSync','1')
 else EngineIni.SetIniSectionKeyValue('Screen Mode','VSync','0');
 EngineIni.SaveToFile;
 end;

EngineIni.Destroy;
end;

InitOpenGL;

if not DrawToPanel then
begin
  h_Instance := GetModuleHandle(nil);
  ZeroMemory(@wndClass, SizeOf(wndClass));

  with wndClass do
  begin
    style         := CS_HREDRAW or
                     CS_VREDRAW or
                     CS_OWNDC;
    lpfnWndProc   := @WndProc;
    hInstance     := h_Instance;
    hCursor       := LoadCursor(0, IDC_ARROW);
    lpszClassName := 'DGLEngine';
  end;

  if (RegisterClass(wndClass) = 0) then
  begin
    MessageBox(0, 'Failed to register the window class! May be one DGLEngine instance is already opened.', 'Error', MB_OK or MB_ICONERROR);
    AddToLogFile(EngineLog,'Failed to register the window class! May be one DGLEngine instance is already opened.');
    Result := False;
    Exit
  end;

  if Fullscreen then
  begin
    ZeroMemory(@dmScreenSettings, SizeOf(dmScreenSettings));
    with dmScreenSettings do begin
      dmSize       := SizeOf(dmScreenSettings);
      dmPelsWidth  := Width;
      dmPelsHeight := Height;
      dmBitsPerPel := PixelDepth;
      dmDisplayFrequency:=Freq;
      dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
    end;

    if (ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) = DISP_CHANGE_FAILED) then
    begin
      MessageBox(0, 'Unable to switch to fullscreen!', 'Error', MB_OK or MB_ICONERROR);
      AddToLogFile(EngineLog,'Unable to switch to fullscreen!');
      Fullscreen := False;
    end;
  end;


  if (Fullscreen) then
  begin
    dwStyle := WS_POPUP or
               WS_CLIPCHILDREN
               or WS_CLIPSIBLINGS;
    dwExStyle := WS_EX_APPWINDOW;
  real_win_w := Width;
  real_win_h := Height;
  WinX:=0;
  WinY:=0;
  end
  else
  begin
    dwStyle := WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX;
    dwExStyle := WS_EX_APPWINDOW or
                 WS_EX_WINDOWEDGE;
  real_win_w := Width + GetSystemMetrics(SM_CXDLGFRAME) * 2 + 2;
  real_win_h := Height + GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYDLGFRAME) * 2 + 2;
  DTDC := GetDC( GetDesktopWindow() );
    DT_W := GetDeviceCaps( DTDC, HORZRES );
    DT_H := GetDeviceCaps( DTDC, VERTRES );
    ReleaseDC( GetDesktopWindow(), DTDC );
   if WinX+WinY=0 then
   begin
   WinX:=(DT_W-real_win_w) div 2;
   WinY:=(DT_H-real_win_h) div 2;
   end;
  end;

  ShowCursor(False);

  if Fullscreen then
  begin
  WinX:=0;
  WinY:=0;
  end;

  h_Wnd := CreateWindowEx(dwExStyle,'DGLEngine',WND_TITLE,dwStyle,WinX, WinY,real_win_w, real_win_h,0,0,h_Instance,nil);

  if h_Wnd = 0 then
  begin
    AddToLogFile(EngineLog,'Unable to create window!');
    glKillWnd(Fullscreen);
    MessageBox(0, 'Unable to create window!', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
    Exit;
  end;
end;

  h_DC := GetDC(h_Wnd);
  if (h_DC = 0) then
  begin
    AddToLogFile(EngineLog,'Unable to get a device context!');
    glKillWnd(Fullscreen);
    MessageBox(0, 'Unable to get a device context!', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
    Exit;
  end;


  with pfd do
  begin
    nSize           := SizeOf(TPIXELFORMATDESCRIPTOR);
    nVersion        := 1;
    dwFlags         := PFD_DRAW_TO_WINDOW
                       or PFD_SUPPORT_OPENGL
                       or PFD_DOUBLEBUFFER;
    iPixelType      := PFD_TYPE_RGBA;
    cColorBits      := PixelDepth;
    cRedBits        := 0;
    cRedShift       := 0;
    cGreenBits      := 0;
    cGreenShift     := 0;
    cBlueBits       := 0;
    cBlueShift      := 0;
    cAlphaBits      := 0;
    cAlphaShift     := 0;
    cAccumBits      := BoolToInt(INITStencil);
    cAccumRedBits   := 0;
    cAccumGreenBits := 0;
    cAccumBlueBits  := 0;
    cAccumAlphaBits := 0;
    cDepthBits      := InitZBuffer;
    cStencilBits    := BoolToInt(INITStencil);
    cAuxBuffers     := 0;
    iLayerType      := PFD_MAIN_PLANE;
    bReserved       := 0;
    dwLayerMask     := 0;
    dwVisibleMask   := 0;
    dwDamageMask    := 0;
  end;

  PixelFormat := ChoosePixelFormat(h_DC, @pfd);
  if (PixelFormat = 0) then
  begin
    AddToLogFile(EngineLog,'Unable to find a suitable pixel format!');
    glKillWnd(Fullscreen);
    MessageBox(0, 'Unable to find a suitable pixel format!', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
    Exit;
  end;

  if (not SetPixelFormat(h_DC, PixelFormat, @pfd)) then
  begin
    AddToLogFile(EngineLog,'Unable to set the pixel format!');
    glKillWnd(Fullscreen);
    MessageBox(0, 'Unable to set the pixel format!', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
    Exit;
  end;

  h_RC := wglCreateContext(h_DC);
  if (h_RC = 0) then
  begin
    AddToLogFile(EngineLog,'Unable to create an OpenGL rendering context!');
    glKillWnd(Fullscreen);
    MessageBox(0, 'Unable to create an OpenGL rendering context!', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
    Exit;
  end;

  if (not wglMakeCurrent(h_DC, h_RC)) then
  begin
    AddToLogFile(EngineLog,'Unable to activate OpenGL rendering context!');
    glKillWnd(Fullscreen);
    MessageBox(0, 'Unable to activate OpenGL rendering context!', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
    Exit;
  end;
  ReadExtensions;
  ReadImplementationProperties;
  OpenGLInitialized := True;

  if not DrawToPanel then
   if Vsync then VBL2(vsmSync) else VBL2(vsmNoSync);

  SetTimer(h_Wnd, FPS_TIMER, FPS_INTERVAL, nil);

  if DrawToPanel then
  SetTimer(h_Wnd, PROCESS_TIMER, PROCESS_INTERVAL, nil);

  if not DrawToPanel then
  begin
  ShowWindow(h_Wnd, SW_SHOW);
  SetForegroundWindow(h_Wnd);
  SetFocus(h_Wnd);
  end;

  glResizeWnd(Width, Height);

      AddToLogFile(EngineLog,'--System Information--');
      GetWindowsVersion(Major,Minor);
      AddToLogFile(EngineLog,'Windows version: '+IntToStr(Major)+'.'+IntToStr(Minor));
      AddToLogFile(EngineLog,'CPU            : '+GetCPU);
      AddToLogFile(EngineLog,'RAM Available  : '+inttostr(round(GetMemoryFree/1048576+0.40))+'Mb');
      AddToLogFile(EngineLog,'RAM Total      : '+inttostr(round(GetMemory/1048576+0.40))+'Mb');
      AddToLogFile(EngineLog,'Video device   : '+glGetString(GL_RENDERER));
      AddToLogFile(EngineLog,'OpenGL         : '+glGetString(GL_VERSION)+' ('+glGetString(GL_VENDOR)+')');
      //AddToLogFile(EngineLog,'OpenGL Extensions:'+glGetString(GL_EXTENSIONS));

      glGetIntegerv(GL_MAX_TEXTURE_SIZE,@i);
      AddToLogFile(EngineLog,'Maximum texture resolution:'+inttostr(i)+'x'+inttostr(i));
      if not GL_ARB_multitexture then
      AddToLogFile(EngineLog,'WARNING: Your card does not support multitexturing!') else
      begin
      glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB,@i);
      AddToLogFile(EngineLog,'Maximum multytexturing layers (in pipeline):'+inttostr(i));
      glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS_ARB,@i);
      AddToLogFile(EngineLog,'Maximum multytexturing layers (in shader):'+inttostr(i));
      end;

      if not WGL_EXT_swap_control then
      AddToLogFile(EngineLog,'WARNING: Your card does not support VSync!');

      if not GL_ARB_texture_compression then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Texture compression!');

      if not GL_ARB_texture_non_power_of_two then
      AddToLogFile(EngineLog,'WARNING: Your card doesnot support non power of two textures!');

      if not GL_ARB_vertex_program then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Vertex shaders!');

      if not GL_ARB_fragment_program then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Pixel shaders!');

      if not GL_ARB_texture_env_combine then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Texture enviroment combine extention!');

      if not GL_ARB_texture_compression then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Texture compression!');

      if not GL_ARB_shadow then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Shadow maps!');

      if not GL_ARB_depth_texture then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Depth textures!');

      if not GL_ARB_texture_env_dot3 then
      AddToLogFile(EngineLog,'WARNING: Your card does not support DOT3 Bump Mapping!');

      if not GL_ARB_texture_cube_map then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Cube Maps!');

      if not GL_ARB_vertex_buffer_object then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Vertex Buffer Objects!');

      if not GL_EXT_framebuffer_object then
      AddToLogFile(EngineLog,'WARNING: Your card does not support Frame Buffer Objects!');

      AddToLogFile(EngineLog,'----------------------');

  AddToLogFile(EngineLog,'Engine created.');
  if DrawToPanel then AddToLogFile(EngineLog,'Draw_To_Panel Mode.') else
  if FullScreen then if Freq=0 then
  AddToLogFile(EngineLog,'Fullscreen Mode:'+inttostr(Width)+'X'+inttostr(Height)+'X'+inttostr(PixelDepth)+' Windows define monitor friquency.'+#13+'VSync is '+booltostr(Vsync)+'.')
  else AddToLogFile(EngineLog,'Fullscreen Mode:'+inttostr(Width)+'X'+inttostr(Height)+'X'+inttostr(PixelDepth)+' '+inttostr(Freq)+'Hz.'+#13+'VSync is '+booltostr(Vsync)+'.')
  else AddToLogFile(EngineLog,'Windowed Mode.');

  glClearColor(0.0, 0.0, 0.0, 0.0);
  glShadeModel(GL_SMOOTH);
  glHint(GL_SHADE_MODEL,GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glDisable(GL_ALPHA_TEST);
  glClearDepth(1.0);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_NORMALIZE);
  glEnable(GL_COLOR_MATERIAL);

  InitEng;

if IsTargetProcess then
begin
  // БЕЗОПАСНАЯ ПРОВЕРКА ТИПА ЛОКОМОТИВА
  try
    // Сначала проверяем, что адрес читается
    if IsBadReadPtr(Pointer($00400000 + $4F8D93C), 4) then
    begin
      AddToLogFile(EngineLog, 'Locomotive type address not readable yet, hook skipped');
      Exit;
    end;
    
    // Читаем тип локомотива
    LocType := PInteger(Pointer($00400000 + $4F8D93C))^;
    
    // Проверяем валидность (типы локомотивов обычно в диапазоне 100-50000)
    if (LocType > 50000) or (LocType < 100) then
    begin
      AddToLogFile(EngineLog, 'Invalid locomotive type: ' + IntToStr(LocType) + ', hook skipped');
    end;
    
        case LocType of
          524, 880, 2070, 21014, 1462, 811, 882: begin
            ApplyKPD3Patch();
            AddToLogFile(EngineLog, 'KPD-3 patch applied for locomotive type: ' + IntToStr(LocType));
          end;
          822: begin
            WriteHookAddress;
            ApplyKPD3Patch();
            AddToLogFile(EngineLog, 'Hook activated for locomotive type 822 (CS7)');
          end;
          812: begin
            WriteHookAddressCHS8;
            ApplyKPD3Patch();
            AddToLogFile(EngineLog, 'Hook activated for locomotive type 812 (CS8)');
          end;
          3154: begin
            WriteHookAddressED4M;
            AddToLogFile(EngineLog, 'Hook activated for locomotive type 3154 (ED4M)');
          end;
          885: begin
            AddToLogFile(EngineLog, 'VL85 detected, applying ZDSim patches immediately...');
            ApplyZDSimPatches;  // Вызываем сразу при обнаружении
            ApplyKPD3Patch();
            AddToLogFile(EngineLog, 'ZDSim patches activation completed for locomotive type 885 (VL85)');
          end;

          else
            AddToLogFile(EngineLog, 'Hook not supported for locomotive type: ' + IntToStr(LocType));
        end;

  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Failed to check locomotive type: ' + E.Message + ', hook skipped');
    end;
  end;
end;

  //InitializeTrafficLightSystem;

  LoadLogo;

//  if not ConfigLoaded then
//  begin
//    LoadConfigFile;  // Эта функция определена в DrawFunc3D
//  end;

  if @LoadTextures<>nil then LoadTextures;

  InitCheatMenu;

 try
    InitDiscordRPC;
    AddToLogFile(EngineLog, 'Discord RPC initialized successfully');
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'Discord RPC init failed: ' + E.Message);
  end;

  Result := True;
end;
{------------------------------------------------------------------}
function WinMain(hInstance : HINST; hPrevInstance : HINST;
                 lpCmdLine : PChar; nCmdShow : Integer) : Integer;
var
  msg : TMsg;
  i    : integer;
  flag : boolean;
begin
  StartQuitingEngine := False;

  ShutdownDiscordRPC; // Завершаем Discord RPC

  if not glCreateWnd(InitResX, InitResY, InitFullscreen, InitPDepth, InitFrequency ,InitVsync) then
  begin
    Result := 0;
    Exit;
  end;

  Time_Old := GetTimer - PROCESS_INTERVAL;

  while not StartQuitingEngine do
  begin

    if (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then
    begin
      if (msg.message = WM_QUIT) then
        StartQuitingEngine := True
      else
      begin
          TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end
    else
    begin

    Time       := GetTimer;
    Time_Delta := Time - Time_Old;

    flag := false;

   for i := 1 to Time_Delta div PROCESS_INTERVAL do
   begin
   if not paused then
     EngineMainProcess;
     flag := true;
   end;

   if flag = true then
    Time_Old := Time - Time_Delta mod PROCESS_INTERVAL;

    EngineMainDraw;

  end;
 end;

  glKillWnd(InitFullScreen);
  Result := msg.wParam;

end;
{------------------------------------------------------------------}
procedure StartEngine; stdcall;
begin
WinMain( hInstance, hPrevInst, CmdLine, CmdShow );
end;
{------------------------------------------------------------------}
procedure StartEngine_DrawToPanel(PanelHandle : HWND); stdcall;
begin
DrawToPanel:=TRUE;
StartQuitingEngine:=false;
h_Wnd:=PanelHandle;
glCreateWnd(InitResX, InitResY, FALSE, InitPDepth, 0 ,FALSE);
end;

end.
