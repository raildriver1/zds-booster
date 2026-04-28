//----------------------------------------------------------------------------//
// FreecamWorldAnchor.pas — фрикам, привязанный к мировой системе координат.  //
//                                                                            //
// Базовый freecam (DrawFunc3D.ProcessFreecam) пишет в $9008028..$9008030 —   //
// это локомотив-локальные координаты камеры в кабине. Когда поезд едет,      //
// кабина (и камера) едут с ним → freecam «летит за поездом».                 //
//                                                                            //
// Идея: каждый кадр читаем мировую позицию локомотива из LOCSECTIONS         //
// (статический pointer slot в Launcher.exe+$34AEA0, разыменованный →         //
// доли по offset 24/32/40 в byte). Считаем дельту относительно прошлого     //
// кадра и вычитаем её из freecam X/Y/Z, чтобы камера осталась в той же      //
// мировой точке.                                                             //
//                                                                            //
// Ограничения counter-translation:                                           //
//   * При повороте локомотива камера будет вращаться вокруг него (нельзя    //
//     полностью «отвязать» без знания yaw'а локо и инверсной матрицы —      //
//     сейчас делаем только translation).                                     //
//   * Если есть лишние трансформации в render (например масштаб) — потеряем //
//     точность.                                                              //
//   * Точность — single (4 байта) против double (8 байт) в LOCSECTIONS,     //
//     поэтому при больших координатах будут дрожания.                        //
//                                                                            //
// Тоггл: Alt+Shift+X (Alt+X — обычный фрикам). Состояние хранится в         //
// WorldAnchorEnabled. Включается только когда FreecamEnabled = True.        //
//----------------------------------------------------------------------------//
unit FreecamWorldAnchor;

interface

uses Windows, SysUtils, EngineUtils;

var
  WorldAnchorEnabled: Boolean = False;

procedure ApplyFreecamWorldAnchor;

implementation

uses Variables, DrawFunc3D;

const
  FWA_LOG = 'DGLEngine_Log.txt';

  // Тот же базовый pointer, что и в RA3Physics (ADDR_LOCS_BASE_PTR).
  // По адресу лежит DWORD = указатель на heap-блок LOCSECTIONS.
  LOCS_PTR_SLOT = $004AAEA0;

  // Offsets в LOCSECTIONS структуре (в байтах).
  // Из IDA: `*((double *)&LOCSECTIONS + 3..5)` = смещения 24/32/40 в byte.
  OFF_LOCO_WORLD_X = 24;
  OFF_LOCO_WORLD_Y = 32;
  OFF_LOCO_WORLD_Z = 40;

  // Адреса freecam (из DrawFunc3D.ADDR_X/Y/Z).
  FREECAM_X = $9008028;
  FREECAM_Y = $900802C;
  FREECAM_Z = $9008030;

  // Hotkey toggle delay (мс).
  TOGGLE_KEY_DELAY = 250;

var
  WorldAnchorInitialized: Boolean = False;
  PrevLocoX, PrevLocoY, PrevLocoZ: Double;
  LastToggleTime: Cardinal = 0;
  LogOnceEnabled: Boolean = False;
  LogOnceDisabled: Boolean = False;
  // Диагностика: логируем первый успешный/неуспешный read loco-pos после
  // включения тоггла, и каждые 60 кадров — текущие значения чтобы видеть
  // движется ли локомотив в наших данных.
  DiagFrameCount:  Cardinal = 0;
  DiagFirstLog:    Boolean = False;
  DiagPtrFailLog:  Boolean = False;
  ApplyEverCalled: Boolean = False;

// Прочитать текущую мировую позицию локомотива. False если адрес
// не валиден (трасса ещё не загружена / LOCSECTIONS не инициализирован).
function ReadLocoWorldPos(out X, Y, Z: Double): Boolean;
var
  PtrSlot: PCardinal;
  Base: Cardinal;
begin
  Result := False;
  PtrSlot := PCardinal(LOCS_PTR_SLOT);
  if IsBadReadPtr(Pointer(PtrSlot), 4) then Exit;

  Base := PtrSlot^;
  if Base = 0 then Exit;
  if (Base and $FFFF0000) = 0 then Exit;  // мусорный указатель

  if IsBadReadPtr(Pointer(Base + OFF_LOCO_WORLD_X), 8) then Exit;
  if IsBadReadPtr(Pointer(Base + OFF_LOCO_WORLD_Y), 8) then Exit;
  if IsBadReadPtr(Pointer(Base + OFF_LOCO_WORLD_Z), 8) then Exit;

  X := PDouble(Base + OFF_LOCO_WORLD_X)^;
  Y := PDouble(Base + OFF_LOCO_WORLD_Y)^;
  Z := PDouble(Base + OFF_LOCO_WORLD_Z)^;
  Result := True;
end;

procedure ProcessToggleHotkey;
var
  Now: Cardinal;
begin
  Now := GetTickCount;
  if Now - LastToggleTime < TOGGLE_KEY_DELAY then Exit;

  // Alt+Shift+X (X = $58). Foreground-check убран — иногда фокус теряется
  // (например при alt-tab'е) и хоткей не срабатывает.
  if ((GetAsyncKeyState($58) and $8000) <> 0) and
     ((GetAsyncKeyState(VK_MENU) and $8000) <> 0) and
     ((GetAsyncKeyState(VK_SHIFT) and $8000) <> 0) then
  begin
    WorldAnchorEnabled := not WorldAnchorEnabled;
    WorldAnchorInitialized := False;
    LastToggleTime := Now;

    AddToLogFile(FWA_LOG, Format(
      'FreecamWorldAnchor: TOGGLE → %d (FreecamEnabled=%d).',
      [Ord(WorldAnchorEnabled), Ord(FreecamEnabled)]));
    DiagFirstLog   := False;
    DiagPtrFailLog := False;
  end;
end;

procedure ApplyFreecamWorldAnchor;
var
  CurLocoX, CurLocoY, CurLocoZ: Double;
  DX, DY, DZ: Double;
  FX, FY, FZ: Single;
begin
  if not ApplyEverCalled then
  begin
    AddToLogFile(FWA_LOG,
      'FreecamWorldAnchor: модуль активен (ApplyFreecamWorldAnchor вызван). ' +
      'Тоггл — Alt+Shift+X.');
    ApplyEverCalled := True;
  end;

  ProcessToggleHotkey;

  if not WorldAnchorEnabled then
  begin
    WorldAnchorInitialized := False;
    Exit;
  end;

  // World-anchor работает только когда фрикам включён — иначе игра сама
  // двигает камеру по кабине, и наша запись будет драться с её записью.
  if not FreecamEnabled then
  begin
    WorldAnchorInitialized := False;
    Exit;
  end;

  if not ReadLocoWorldPos(CurLocoX, CurLocoY, CurLocoZ) then
  begin
    WorldAnchorInitialized := False;
    if not DiagPtrFailLog then
    begin
      AddToLogFile(FWA_LOG,
        'FreecamWorldAnchor: LOCSECTIONS pointer ($' +
        IntToHex(LOCS_PTR_SLOT, 8) + ') невалиден или 0 — нет данных позиции.');
      DiagPtrFailLog := True;
    end;
    Exit;
  end;

  // Каждые 60 кадров логируем что прочиталось — чтобы можно было
  // увидеть в логе, движется ли локомотив в наших данных.
  Inc(DiagFrameCount);
  if (not DiagFirstLog) or ((DiagFrameCount mod 60) = 0) then
  begin
    AddToLogFile(FWA_LOG, Format(
      'FreecamWorldAnchor: loco_world = (%.3f, %.3f, %.3f), prev = (%.3f, %.3f, %.3f)',
      [CurLocoX, CurLocoY, CurLocoZ, PrevLocoX, PrevLocoY, PrevLocoZ]));
    DiagFirstLog := True;
  end;

  if not WorldAnchorInitialized then
  begin
    PrevLocoX := CurLocoX;
    PrevLocoY := CurLocoY;
    PrevLocoZ := CurLocoZ;
    WorldAnchorInitialized := True;
    Exit;
  end;

  // Дельта мирового перемещения локомотива за этот кадр.
  DX := CurLocoX - PrevLocoX;
  DY := CurLocoY - PrevLocoY;
  DZ := CurLocoZ - PrevLocoZ;

  // Обновляем "прошлую" позицию для следующего кадра.
  PrevLocoX := CurLocoX;
  PrevLocoY := CurLocoY;
  PrevLocoZ := CurLocoZ;

  // Если дельта аномально большая (телепорт / перезагрузка трассы),
  // не трогаем камеру — иначе уедет к чёрту на кулички.
  if (Abs(DX) > 1000.0) or (Abs(DY) > 1000.0) or (Abs(DZ) > 1000.0) then Exit;

  // Вычитаем дельту из freecam coords. Если локо проехал +1 по X в мире,
  // freecam должен -1 в loco-локальных, чтобы остаться в той же мировой
  // точке. Использует Single arithmetic (точность игры).
  if IsBadWritePtr(Pointer(FREECAM_X), 4) then Exit;
  if IsBadWritePtr(Pointer(FREECAM_Y), 4) then Exit;
  if IsBadWritePtr(Pointer(FREECAM_Z), 4) then Exit;

  FX := PSingle(FREECAM_X)^;
  FY := PSingle(FREECAM_Y)^;
  FZ := PSingle(FREECAM_Z)^;

  PSingle(FREECAM_X)^ := FX - DX;
  PSingle(FREECAM_Y)^ := FY - DY;
  PSingle(FREECAM_Z)^ := FZ - DZ;
end;

end.
