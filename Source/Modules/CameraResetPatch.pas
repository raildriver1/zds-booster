//----------------------------------------------------------------------------//
// CameraResetPatch.pas — toggle 6 байт по адресу Launcher.exe+$324CEC.       //
//                                                                            //
// При hover на контроллер/кран/РБ/РБС/гизмо ИЛИ зажатой ЛКМ — пишем туда     //
// безусловный JMP в конец блока сброса:                                      //
//   E9 3C 01 00 00 90    ; jmp Launcher.exe+$324E2D + nop                    //
// Когда условия сняты — восстанавливаем оригинал:                            //
//   0F 84 3B 01 00 00    ; je  Launcher.exe+$324E2D                          //
//                                                                            //
// Edge-trigger: VirtualProtect+Move+FlushInstructionCache зовутся ТОЛЬКО     //
// на смене состояния, а не каждый кадр. Так почти ноль оверхеда.             //
//                                                                            //
// Безопасно: запись 6 байт под VirtualProtect(PAGE_EXECUTE_READWRITE),      //
// потом возврат старого Protect. FlushInstructionCache гарантирует, что      //
// CPU увидит новый код даже при разогретом instruction cache.                //
//----------------------------------------------------------------------------//
unit CameraResetPatch;

interface

uses Windows, SysUtils, EngineUtils;

procedure InstallCameraResetPatch;
procedure UpdateCameraResetSuppress;

implementation

uses RA3, CheatMenu;

const
  CRP_LOG = 'DGLEngine_Log.txt';

  LAUNCHER_BASE = $00400000;
  PATCH_ADDR    = LAUNCHER_BASE + $324CEC;   // $00724CEC
  PATCH_SIZE    = 6;

  // 0F 84 3B 01 00 00 = je Launcher.exe+$324E2D (оригинал)
  ORIGINAL_BYTES: array[0..PATCH_SIZE-1] of Byte =
    ($0F, $84, $3B, $01, $00, $00);

  // E9 3C 01 00 00 90 = jmp Launcher.exe+$324E2D + nop (сброс отключён)
  // rel32 = $324E2D - ($324CEC+5) = $13C, нативно little-endian → 3C 01 00 00.
  SKIP_BYTES: array[0..PATCH_SIZE-1] of Byte =
    ($E9, $3C, $01, $00, $00, $90);

var
  CurrentlySkipping:   Boolean = False;
  SignatureValid:      Boolean = False;
  InstallLogged:       Boolean = False;
  FailLogged:          Boolean = False;

procedure WriteSitebytes(const NewBytes: array of Byte);
var
  OldProtect: DWORD;
begin
  if IsBadReadPtr(Pointer(PATCH_ADDR), PATCH_SIZE) then Exit;
  if not VirtualProtect(Pointer(PATCH_ADDR), PATCH_SIZE,
                        PAGE_EXECUTE_READWRITE, OldProtect) then Exit;
  try
    Move(NewBytes[0], Pointer(PATCH_ADDR)^, PATCH_SIZE);
    FlushInstructionCache(GetCurrentProcess, Pointer(PATCH_ADDR), PATCH_SIZE);
  finally
    VirtualProtect(Pointer(PATCH_ADDR), PATCH_SIZE, OldProtect, OldProtect);
  end;
end;

// Один раз проверяем, что в патч-сайте лежит ожидаемый JE. Если нет —
// версия Launcher.exe другая, патч игнорируем и пишем варнинг в лог.
procedure ValidateSignatureOnce;
var
  CurrentBytes: array[0..PATCH_SIZE-1] of Byte;
begin
  if SignatureValid then Exit;
  if IsBadReadPtr(Pointer(PATCH_ADDR), PATCH_SIZE) then Exit;
  Move(Pointer(PATCH_ADDR)^, CurrentBytes[0], PATCH_SIZE);
  if CompareMem(@CurrentBytes[0], @ORIGINAL_BYTES[0], PATCH_SIZE) then
  begin
    SignatureValid := True;
  end
  else if CompareMem(@CurrentBytes[0], @SKIP_BYTES[0], PATCH_SIZE) then
  begin
    // Уже стоит наш SKIP с прошлого запуска / прошлой версии патча
    // (DLL перезагружена). Считаем валидным, состояние = skipping.
    SignatureValid     := True;
    CurrentlySkipping  := True;
  end
  else
  begin
    if not FailLogged then
    begin
      AddToLogFile(CRP_LOG,
        'CameraResetPatch: неожиданная сигнатура по $' + IntToHex(PATCH_ADDR, 8) +
        ' (' + IntToHex(CurrentBytes[0], 2) + ' ' +
        IntToHex(CurrentBytes[1], 2) + ' ' +
        IntToHex(CurrentBytes[2], 2) + ' ' +
        IntToHex(CurrentBytes[3], 2) + ' ' +
        IntToHex(CurrentBytes[4], 2) + ' ' +
        IntToHex(CurrentBytes[5], 2) + ') — патч не активен.');
      FailLogged := True;
    end;
  end;
end;

procedure InstallCameraResetPatch;
begin
  ValidateSignatureOnce;
  if SignatureValid and (not InstallLogged) then
  begin
    AddToLogFile(CRP_LOG,
      'CameraResetPatch: сигнатура валидна по $' + IntToHex(PATCH_ADDR, 8) +
      '. Edge-trigger toggle активен (hover/LMB → SKIP, иначе → JE).');
    InstallLogged := True;
  end;
end;

// Per-frame: edge-trigger переключение байт между JE-оригиналом и JMP-skip.
// Условие активации: любой hover (controller/brake/RB/RBs/gizmo) ИЛИ зажата
// ЛКМ (на случай если hit-test потерял мышь во время быстрого drag'а).
procedure UpdateCameraResetSuppress;
var
  Suppress, LMB: Boolean;
begin
  if not SignatureValid then
  begin
    InstallCameraResetPatch;
    if not SignatureValid then Exit;
  end;

  LMB := (GetAsyncKeyState(VK_LBUTTON) and $8000) <> 0;

  Suppress :=
    HoveredController or
    HoveredBrake      or
    HoveredRB         or
    HoveredRBS        or
    IsGizmoActive     or
    LMB;

  if Suppress and (not CurrentlySkipping) then
  begin
    WriteSitebytes(SKIP_BYTES);
    CurrentlySkipping := True;
  end
  else if (not Suppress) and CurrentlySkipping then
  begin
    WriteSitebytes(ORIGINAL_BYTES);
    CurrentlySkipping := False;
  end;
end;

end.
