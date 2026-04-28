//----------------------------------------------------------------------------//
// RA3Patches.pas — одно- и покадровые патчи памяти, специфичные для RA-3.    //
//                                                                            //
// 1. ZeroBlock @ [$9110D70]                                                  //
//    Адрес $9110D70 — статический pointer slot (4 байта). По его значению    //
//    лежит heap-блок длиной 120 байт, который игра постоянно перезаписывает  //
//    (по факту это похоже на массив биндингов клавиш — после нашего обнуле- //
//    ния игра тут же кладёт обратно scan-codes O/P/Q/R/S/T/V/U/N на свои    //
//    позиции). Поэтому one-shot патч не работает: чтобы блок реально оста- //
//    вался нулевым, мы обнуляем его *каждый кадр* в EngineMainDraw.         //
//    IsBadReadPtr/IsBadWritePtr защищают от ошибок до загрузки трассы.      //
//                                                                            //
// 2. Camera anchor (привязка камеры в кабине RA-3)                          //
//    Адреса $9008028 / $900802C / $9008030 — это XYZ позиции игровой         //
//    камеры в кабине RA-3 (32-bit float, лежат смежно). Freecam (DrawFunc3D)//
//    использует РОВНО ЭТИ ЖЕ адреса для своих перемещений по миру, поэтому //
//    жёсткий per-frame lock вырубал freecam подчистую.                       //
//                                                                            //
//    Поведение сейчас:                                                       //
//      * Freecam OFF → каждый кадр пишем "якорные" значения, чтобы любые    //
//        side-эффекты (физика / шейдеры / модели приборов) видели стабильную//
//        позицию X=0.55 / Y=9.8 / Z=3.2.                                    //
//      * Freecam ON  → НЕ трогаем — freecam свободно двигает камеру.        //
//      * Edge OFF→ON → один раз ставим якорь, чтобы freecam стартовал       //
//        ровно из кабины, а не там где случайно осталась камера.            //
//      * Edge ON→OFF → один раз ставим якорь, чтобы выход из freecam       //
//        возвращал камеру в кабину.                                          //
//                                                                            //
//    Состояние freecam читается из глобала FreecamEnabled (DrawFunc3D.pas). //
//                                                                            //
// Никаких NOP-патчей по машинному коду — только запись в data, через         //
// IsBadReadPtr/IsBadWritePtr защиту: пока симулятор не загрузил трассу,     //
// ничего не делаем (no-op), как только адреса валидны — патч применяется.   //
//----------------------------------------------------------------------------//
unit RA3Patches;

interface

uses Windows, SysUtils, EngineUtils, DrawFunc3D;

procedure ApplyRA3PerFrame;

implementation

const
  RA3_LOG = 'DGLEngine_Log.txt';

  // Static pointer slot — 4 byte указатель на heap-блок в 120 байт.
  PTR_ZEROBLOCK    = $9110D70;
  ZEROBLOCK_SIZE   = 120;

  // Heap-адреса камеры RA-3 (3 × 32-bit float, последовательно).
  ADDR_CAM_X       = $9008028;
  ADDR_CAM_Y       = $900802C;
  ADDR_CAM_Z       = $9008030;

  CAM_INIT_X: Single = 0.5500000119;
  CAM_INIT_Y: Single = 9.800003052;
  CAM_INIT_Z: Single = 3.200000048;

var
  // True хотя бы один раз обнулилось — нужно чтобы лог писался один раз
  // (а не каждый кадр), но сам zero-fill идёт всё равно каждый кадр.
  ZeroBlockEverLogged: Boolean = False;

procedure TryZeroBlock;
var
  PtrSlot: PCardinal;
  TargetPtr: Pointer;
begin
  PtrSlot := PCardinal(PTR_ZEROBLOCK);
  if IsBadReadPtr(Pointer(PtrSlot), 4) then Exit;

  TargetPtr := Pointer(PtrSlot^);
  if TargetPtr = nil then Exit;
  if IsBadWritePtr(TargetPtr, ZEROBLOCK_SIZE) then Exit;

  // Игра постоянно перезаписывает блок обратно — поэтому без условий
  // зануляем его на каждом кадре. FillChar 120 байт (~10-30 ns на CPU
  // 2025 года), overhead невидим даже на 1000 fps.
  FillChar(TargetPtr^, ZEROBLOCK_SIZE, 0);

  if not ZeroBlockEverLogged then
  begin
    AddToLogFile(RA3_LOG,
      'RA3Patches: zero-block активен (' + IntToStr(ZEROBLOCK_SIZE) +
      ' байт по адресу $' + IntToHex(Cardinal(TargetPtr), 8) +
      ', обнуляется каждый кадр)');
    ZeroBlockEverLogged := True;
  end;
end;

var
  PrevFreecamState: Boolean = False;

procedure WriteCameraAnchor;
begin
  PSingle(ADDR_CAM_X)^ := CAM_INIT_X;
  PSingle(ADDR_CAM_Y)^ := CAM_INIT_Y;
  PSingle(ADDR_CAM_Z)^ := CAM_INIT_Z;
end;

procedure LockCameraPosition;
var
  EdgeChange: Boolean;
begin
  if IsBadWritePtr(Pointer(ADDR_CAM_X), 4) then Exit;
  if IsBadWritePtr(Pointer(ADDR_CAM_Y), 4) then Exit;
  if IsBadWritePtr(Pointer(ADDR_CAM_Z), 4) then Exit;

  // Edge detection: однократная запись якоря на любой смене состояния
  // freecam (OFF→ON или ON→OFF).
  EdgeChange := FreecamEnabled <> PrevFreecamState;
  PrevFreecamState := FreecamEnabled;

  if FreecamEnabled then
  begin
    // Freecam активен — он сам пишет в эти адреса каждый кадр, не мешаем.
    // Только на сам момент перехода OFF→ON прокинем якорь, чтобы freecam
    // стартовал из кабины.
    if EdgeChange then WriteCameraAnchor;
  end
  else
  begin
    // Freecam выключен — держим якорь каждый кадр. Это перебивает любые
    // случайные записи от игры/freecam-патчей, которые могли остаться.
    WriteCameraAnchor;
  end;
end;

procedure ApplyRA3PerFrame;
begin
  TryZeroBlock;        // every frame — игра постоянно пишет туда биндинги
  LockCameraPosition;  // anchor когда freecam OFF, edge-trigger при on/off
end;

end.
