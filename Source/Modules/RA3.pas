unit RA3;

interface

function IsRA3Active: Boolean;
procedure InitRA3;
procedure DrawRA3;
procedure ApplyRA3BlockTransform(x, y, z, AngZ: Single);

implementation

uses
  OpenGL, Windows, SysUtils, Variables, EngineUtils, DrawFunc3D, Advanced3D, Textures, KlubData, CheatMenu;


const
  ADDR_CAM_X: Cardinal = $9008028;
  ADDR_CAM_Y: Cardinal = $900802C;
  ADDR_CAM_Z: Cardinal = $9008030;

  CAM_INIT_X: Single = 0.5500000119;
  CAM_INIT_Y: Single = 9.800003052;
  CAM_INIT_Z: Single = 3.200000048;

  BUTTON_PRESS_DELTA: Single = 0.005;



var
  RA3_LocoInitialized: Boolean = False;
  RA3_DynamicInitialized: Boolean = False;
  RA3_CabInitialized: Boolean = False;
  RA3_CameraWritten: Boolean = False;
  RA3_WagonInitialized: Boolean = False;

  WagonModelIDs: array of Integer;
  WagonTextureIDs: array of Integer;
  WagonWheelsetID: Integer = 0;
  WagonWheelsetTextureID: Integer = 0;
  WagonWheelRotation: Single = 0;

  LocoModelIDs: array of Integer;
  LocoTextureIDs: array of Integer;

  ControllerModelID: Integer = 0;
  ControllerTextureID: Integer = 0;

  ControllerBrakingModelID: Integer = 0;

  ButtonRBModelID: Integer = 0;
  ButtonRBPModelID: Integer = 0;
  ButtonRBSModelID: Integer = 0;

  // ALS-EN (АЛС-ЕН) green signal count blocks
  IndicatorG1ModelID: Integer = 0;
  IndicatorG2ModelID: Integer = 0;
  IndicatorG3ModelID: Integer = 0;
  IndicatorG4ModelID: Integer = 0;

  // Standard ALS colors
  IndicatorR_M_ModelID:  Integer = 0;  // Red (value 2)
  IndicatorY_M_ModelID:  Integer = 0;  // Yellow (value 4)
  IndicatorW_M_ModelID:  Integer = 0;  // White (value 1)
  IndicatorRY_M_ModelID: Integer = 0;  // Red-Yellow / КЖ (value 3)

  ALSTextureID: Integer = 0;

  ArrowPMModelID: Integer = 0;
  ArrowTC1ModelID: Integer = 0;
  ArrowTC2ModelID: Integer = 0;
  ArrowTMModelID: Integer = 0;

  TelegaMotorID: Integer = 0;
  TelegaNemotorID: Integer = 0;

  CabModelIDs: array[0..7] of Integer;
  CabTextureIDs: array[0..7] of Integer;

  ControllerMemValue: Integer = 0;
  ControllerAngle: Single = 0;
  TargetAngle: Single = 0;
  InputTimer: Cardinal = 0;
  MemTimer: Cardinal = 0;
  LastWriteValue: Integer = -1;

  WheelRotation: Single = 0;
  WheelModelIDs: array[0..3] of Integer;
  TelegaTextureID: Integer = 0;

  HoveredController: Boolean = False;
  HoveredBrake: Boolean = False;

  HoverMode: Boolean = False;
  PatchActive: Boolean = False;

  DraggingController: Boolean = False;
  LastLButtonState: Boolean = False;
  DragStartX: Single = 0;
  DragStartValue: Integer = 0;
  DragStartAngle: Single = 0;

  LastTState: Boolean = False;
  LastYState: Boolean = False;
  LeftDoorsOpen: Boolean = False;
  LeftDoorOffset: Single = 0;
  LeftForwardOffset: Single = 0;
  DoorForwardOffset: Single = 0;

const
  // Нижний якорь контроллера (у основания)
  CONTROLLER_POS_X: Single = 0.199975;
  CONTROLLER_POS_Y: Single = 10.4241;
  CONTROLLER_POS_Z: Single = 2.59945;
  // Верхний якорь (верхушка рукояти) — для расширения зоны по высоте
  CONTROLLER_TOP_X: Single = 0.199975;
  CONTROLLER_TOP_Y: Single = 10.4241;
  CONTROLLER_TOP_Z: Single = 2.85;

  BRAKE_POS_X: Single = 0.981241;
  BRAKE_POS_Y: Single = 10.3818;
  BRAKE_POS_Z: Single = 2.5848;

  HOVER_RADIUS_PX: Integer = 50;
  DRAG_PX_PER_STEP: Single = 30.0;
  DRAG_ANGLE_PER_PX: Single = 1.5;

  // Смещение первого вагона относительно начала координат локомотива (по оси Y):
  WAGON_FIRST_OFFSET_Y: Single = -23.33;
  // Шаг между вагонами по Y (негатив — «назад»):
  WAGON_STEP_Y: Single = -23.0;
  // X и Z локального положения (обычно оставить 0):
  WAGON_POS_X: Single = 0.0;
  WAGON_POS_Z: Single = 0.0;
  // Колёсные пары вагона (относительно центра вагона):
  WAGON_WS_X: Single = 0.000053;
  WAGON_WS_Z: Single = 0.710448;
  WAGON_WS_Y_1: Single =  8.40357;
  WAGON_WS_Y_2: Single =  6.25357;
  WAGON_WS_Y_3: Single = -6.17666;
  WAGON_WS_Y_4: Single = -8.32665;

var
  DoorModelIDs: array of Integer;
  DoorPos: array of record
    x, y, z: Single;
  end;
  DoorTextureID: Integer = 0;
  RightDoorsOpen: Boolean = False;
  RightDoorOffset: Single = 0;
const
  DOOR_FORWARD_MAX: Single = 0.12; // выдвиг по X наружу перед разъездом
  DOOR_SIDE_MAX: Single = 0.55;    // разъезд по Y
  DOOR_STEP: Single = 0.04;
  // направление каждой створки по Y при открытии (пары: 0-1, 2-3, 4-5, 6-7)
  DOOR_Y_DIR: array[0..7] of Integer = (-1, 1, 1, -1, -1, 1, -1, 1);

function IsKeyDownEx(Key: Integer): Boolean;
begin
  Result := (GetAsyncKeyState(Key) and $8000) <> 0;
end;

procedure SyncHoverModeWithSettings;
var
  want: Boolean;
begin
  want := IsRA3HoverEnabled;
  if want = HoverMode then Exit;
  HoverMode := want;
  if HoverMode then
    ShowCursor(True)
  else
  begin
    ShowCursor(False);
    if PatchActive then
    begin
      RemoveMenuPatch;
      PatchActive := False;
    end;
    DraggingController := False;
  end;
end;

// Проверка попадания курсора в «капсулу» вдоль отрезка A→B в экранных координатах
function HitCapsule2D(const A, B: TPoint; px, py, radius: Integer): Boolean;
var
  abx, aby, apx, apy, t: Single;
  len2: Single;
  cx, cy, dx, dy: Single;
begin
  abx := B.X - A.X; aby := B.Y - A.Y;
  apx := px - A.X;  apy := py - A.Y;
  len2 := abx*abx + aby*aby;
  if len2 < 0.0001 then t := 0
  else t := (apx*abx + apy*aby) / len2;
  if t < 0 then t := 0 else if t > 1 then t := 1;
  cx := A.X + t * abx;
  cy := A.Y + t * aby;
  dx := px - cx; dy := py - cy;
  Result := (dx*dx + dy*dy) < (radius * radius);
end;

procedure UpdateHover;
var
  v: TVertex;
  pTop, pBot, pCenter: TPoint;
  px, py, dx, dy, rr: Integer;
  wantPatch: Boolean;
begin
  SyncHoverModeWithSettings;

  HoveredController := False;
  HoveredBrake := False;

  if HoverMode then
  begin
    px := Round(MoveXcoord);
    py := Round(MoveYcoord);
    rr := HOVER_RADIUS_PX * HOVER_RADIUS_PX;

    // Капсула между основанием и верхушкой контроллера
    v.X := CONTROLLER_POS_X; v.Y := CONTROLLER_POS_Y; v.Z := CONTROLLER_POS_Z;
    pBot := Get2DPos(v);
    v.X := CONTROLLER_TOP_X; v.Y := CONTROLLER_TOP_Y; v.Z := CONTROLLER_TOP_Z;
    pTop := Get2DPos(v);
    if HitCapsule2D(pBot, pTop, px, py, HOVER_RADIUS_PX) then
      HoveredController := True;

    // Тормозной — круговая зона
    v.X := BRAKE_POS_X; v.Y := BRAKE_POS_Y; v.Z := BRAKE_POS_Z;
    pCenter := Get2DPos(v);
    dx := pCenter.X - px; dy := pCenter.Y - py;
    if (dx*dx + dy*dy) < rr then
      HoveredBrake := True;
  end;

  // Патч держим пока: 1) наведён, ИЛИ 2) идёт драг контроллера.
  // Снимаем когда оба условия сняты (курсор ушёл И ЛКМ отпущена).
  wantPatch := HoverMode and (HoveredController or DraggingController);
  if wantPatch and not PatchActive then
  begin
    ApplyMenuPatch;
    PatchActive := True;
  end
  else if (not wantPatch) and PatchActive then
  begin
    RemoveMenuPatch;
    PatchActive := False;
  end;
end;

procedure DrawSimpleModel(ModelID: Integer; x, y, z: Single; TextureID: Integer = 0);
begin
  if ModelID = 0 then Exit;
  BeginObj3D;
  Position3D(x, y, z);
  if TextureID <> 0 then
    SetTexture(TextureID);
  DrawModel(ModelID, 0, False);
  EndObj3D;
end;

function IsRA3Active: Boolean;
begin
  Result := Pos('RA3', UpperCase(LocNum)) > 0;
end;

procedure WriteRA3CameraInit;
begin
  if RA3_CameraWritten then Exit;
  try
    PSingle(ADDR_CAM_X)^ := CAM_INIT_X;
    PSingle(ADDR_CAM_Y)^ := CAM_INIT_Y;
    PSingle(ADDR_CAM_Z)^ := CAM_INIT_Z;
    RA3_CameraWritten := True;
  except
    // writes may fail if address not yet mapped; retry next frame
  end;
end;

procedure InitDynamicRA3;
var
  basePath, texPath: string;

  function Load(const FileName: string): Integer;
  begin
    if FileExists(basePath + FileName) then
      Result := LoadModel(basePath + FileName, 0, False)
    else
      Result := 0;
  end;

begin
  if RA3_DynamicInitialized then Exit;
  RA3_DynamicInitialized := True;

  basePath := 'C:\ZDSimulator55.008new\ra3\dynamic\';

  // Controllers
  ControllerModelID         := Load('kontroller.dmd');
  ControllerBrakingModelID  := Load('controller_braking.dmd');

  // Buttons
  ButtonRBModelID   := Load('button_RB.dmd');
  ButtonRBPModelID  := Load('button_RBP.dmd');
  ButtonRBSModelID  := Load('button_RBS.dmd');

  // ALS-EN (АЛС-ЕН) green-signal-count blocks
  IndicatorG1ModelID := Load('indicator_LS_G1_M.dmd');
  IndicatorG2ModelID := Load('indicator_LS_G2_M.dmd');
  IndicatorG3ModelID := Load('indicator_LS_G3_M.dmd');
  IndicatorG4ModelID := Load('indicator_LS_G4_M.dmd');

  // Standard ALS indicators
  IndicatorR_M_ModelID  := Load('indicator_LS_R_M.dmd');
  IndicatorY_M_ModelID  := Load('indicator_LS_Y_M.dmd');
  IndicatorW_M_ModelID  := Load('indicator_LS_W_M.dmd');
  IndicatorRY_M_ModelID := Load('indicator_LS_RY_M.dmd');

  // Gauge arrows
  ArrowPMModelID  := Load('arrow_PM.dmd');
  ArrowTC1ModelID := Load('arrow_TC1.dmd');
  ArrowTC2ModelID := Load('arrow_TC2.dmd');
  ArrowTMModelID  := Load('arrow_TM.dmd');

  // Textures
  texPath := basePath + 'ra3_22_RGBA.bmp';
  if FileExists(texPath) then
    ControllerTextureID := LoadTextureFromFile(texPath, 0, -1);

  texPath := basePath + 'ra3_21_RGBA.bmp';
  if FileExists(texPath) then
    ALSTextureID := LoadTextureFromFile(texPath, 0, -1);
end;

procedure InitLocoRA3;
var
  sr, sr2: TSearchRec;
  i: Integer;
  basePath, matPath, texPath: string;
begin
  if RA3_LocoInitialized then Exit;
  RA3_LocoInitialized := True;

  basePath := 'C:\ZDSimulator55.008new\ra3\loco\';

WheelModelIDs[0] := LoadModel('C:\ZDSimulator55.008new\ra3\loco\Material10.001\wheelset_1.dmd', 0, False);
WheelModelIDs[1] := LoadModel('C:\ZDSimulator55.008new\ra3\loco\Material10.001\wheelset_2.dmd', 0, False);
WheelModelIDs[2] := LoadModel('C:\ZDSimulator55.008new\ra3\loco\Material10.001\wheelset_3.dmd', 0, False);
WheelModelIDs[3] := LoadModel('C:\ZDSimulator55.008new\ra3\loco\Material10.001\wheelset_4.dmd', 0, False);

TelegaMotorID :=
  LoadModel('C:\ZDSimulator55.008new\ra3\loco\Material10.001\telega_motor.dmd', 0, False);

TelegaNemotorID :=
  LoadModel('C:\ZDSimulator55.008new\ra3\loco\Material10.001\telega_nemotor.dmd', 0, False);

texPath := 'C:\ZDSimulator55.008new\ra3\loco\Material10.001\ra3_10_RGBA.bmp';
if FileExists(texPath) then
  TelegaTextureID := LoadTextureFromFile(texPath, 0, -1);

  SetLength(LocoModelIDs, 0);
  SetLength(LocoTextureIDs, 0);

  i := 0;

  if FindFirst(basePath + '*', faDirectory, sr) = 0 then
  begin
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        matPath := basePath + sr.Name + '\';

        if not DirectoryExists(matPath) then
          Continue;

        // --- текстура ---
        texPath := '';
        if FileExists(matPath + 'ra3_1_RGBA.bmp') then texPath := matPath + 'ra3_1_RGBA.bmp'
        else if FileExists(matPath + 'ra3_2_RGBA.bmp') then texPath := matPath + 'ra3_2_RGBA.bmp'
        else if FileExists(matPath + 'ra3_3_RGBA.bmp') then texPath := matPath + 'ra3_3_RGBA.bmp'
        else if FileExists(matPath + 'ra3_4_RGBA.bmp') then texPath := matPath + 'ra3_4_RGBA.bmp'
        else if FileExists(matPath + 'ra3_5_RGBA.bmp') then texPath := matPath + 'ra3_5_RGBA.bmp'
        else if FileExists(matPath + 'ra3_6_RGBA.bmp') then texPath := matPath + 'ra3_6_RGBA.bmp'
        else if FileExists(matPath + 'ra3_7_RGBA.bmp') then texPath := matPath + 'ra3_7_RGBA.bmp'
        else if FileExists(matPath + 'ra3_8_RGBA.bmp') then texPath := matPath + 'ra3_8_RGBA.bmp'
        else if FileExists(matPath + 'ra3_9_RGBA.bmp') then texPath := matPath + 'ra3_9_RGBA.bmp'
        else if FileExists(matPath + 'ra3_10_RGBA.bmp') then texPath := matPath + 'ra3_10_RGBA.bmp'
        else if FileExists(matPath + 'ra3_11_RGBA.bmp') then texPath := matPath + 'ra3_11_RGBA.bmp'
        else if FileExists(matPath + 'ra3_12_RGBA.bmp') then texPath := matPath + 'ra3_12_RGBA.bmp'
        else if FileExists(matPath + 'ra3_13_RGBA.bmp') then texPath := matPath + 'ra3_13_RGBA.bmp'
        else if FileExists(matPath + 'ra3_14_RGBA.bmp') then texPath := matPath + 'ra3_14_RGBA.bmp'
        else if FileExists(matPath + 'ra3_15_RGBA.bmp') then texPath := matPath + 'ra3_15_RGBA.bmp'
        else if FileExists(matPath + 'ra3_16_RGBA.bmp') then texPath := matPath + 'ra3_16_RGBA.bmp'
        else if FileExists(matPath + 'ra3_17_RGBA.bmp') then texPath := matPath + 'ra3_17_RGBA.bmp'
        else if FileExists(matPath + 'ra3_18_RGBA.bmp') then texPath := matPath + 'ra3_18_RGBA.bmp';

        // --- загружаем ВСЕ .dmd ---
        if FindFirst(matPath + '*.dmd', faAnyFile, sr2) = 0 then
        begin
repeat
  if Pos('wheelset', LowerCase(sr2.Name)) > 0 then
    Continue;

  SetLength(LocoModelIDs, Length(LocoModelIDs) + 1);
  SetLength(LocoTextureIDs, Length(LocoTextureIDs) + 1);

  LocoModelIDs[High(LocoModelIDs)] :=
    LoadModel(matPath + sr2.Name, 0, False);

  if texPath <> '' then
    LocoTextureIDs[High(LocoTextureIDs)] :=
      LoadTextureFromFile(texPath, 0, -1)
  else
    LocoTextureIDs[High(LocoTextureIDs)] := 0;

until FindNext(sr2) <> 0;

          FindClose(sr2);
        end;

        Inc(i);
      end;
    until FindNext(sr) <> 0;

    FindClose(sr);
  end;
end;

procedure InitWagonRA3;
var
  sr, sr2: TSearchRec;
  basePath, matPath, texPath: string;
  k: Integer;
  rgbaNames: array[1..18] of string;

function FindTexture(const path: string; const IsWheelset: Boolean): string;
var
  sr: TSearchRec;
begin
  Result := '';

  if FindFirst(path + '*.bmp', faAnyFile, sr) = 0 then
  begin
    repeat
      if IsWheelset then
      begin
        if Pos('wheelset', LowerCase(sr.Name)) > 0 then
        begin
          Result := path + sr.Name;
          Break;
        end;
      end
      else
      begin
        if Pos('rgba', LowerCase(sr.Name)) > 0 then
        begin
          Result := path + sr.Name;
          Break;
        end;
      end;

    until FindNext(sr) <> 0;

    FindClose(sr);
  end;
end;

  function LoadWagonModel(const fileName, tex: string): Integer;
  begin
    Result := LoadModel(fileName, 0, False);
  end;

begin
  if RA3_WagonInitialized then Exit;
  RA3_WagonInitialized := True;

RightDoorOffset := 0;
DoorForwardOffset := 0;
RightDoorsOpen := False;
LeftDoorOffset := 0;
LeftForwardOffset := 0;
LeftDoorsOpen := False;

  basePath := 'C:\ZDSimulator55.008new\ra3\wagon\';

  SetLength(WagonModelIDs, 0);
  SetLength(WagonTextureIDs, 0);

  WagonWheelsetID := 0;
  WagonWheelsetTextureID := 0;

  rgbaNames[1]  := 'ra3_1_RGBA.bmp';
  rgbaNames[2]  := 'ra3_2_RGBA.bmp';
  rgbaNames[3]  := 'ra3_3_RGBA.bmp';
  rgbaNames[4]  := 'ra3_4_RGBA.bmp';
  rgbaNames[5]  := 'ra3_5_RGBA.bmp';
  rgbaNames[6]  := 'ra3_6_RGBA.bmp';
  rgbaNames[7]  := 'ra3_7_RGBA.bmp';
  rgbaNames[8]  := 'ra3_8_RGBA.bmp';
  rgbaNames[9]  := 'ra3_9_RGBA.bmp';
  rgbaNames[10] := 'ra3_10_RGBA.bmp';
  rgbaNames[11] := 'ra3_11_RGBA.bmp';
  rgbaNames[12] := 'ra3_12_RGBA.bmp';
  rgbaNames[13] := 'ra3_13_RGBA.bmp';
  rgbaNames[14] := 'ra3_14_RGBA.bmp';
  rgbaNames[15] := 'ra3_15_RGBA.bmp';
  rgbaNames[16] := 'ra3_16_RGBA.bmp';
  rgbaNames[17] := 'ra3_17_RGBA.bmp';
  rgbaNames[18] := 'ra3_18_RGBA.bmp';

// === ДВЕРИ ===
SetLength(DoorModelIDs, 8);
SetLength(DoorPos, 8);

DoorModelIDs[0] := LoadModel(basePath + 'Material #4.004\doors\left1.dmd', 0, False);
DoorModelIDs[1] := LoadModel(basePath + 'Material #4.004\doors\left2.dmd', 0, False);
DoorModelIDs[2] := LoadModel(basePath + 'Material #4.004\doors\left2_1.dmd', 0, False);
DoorModelIDs[3] := LoadModel(basePath + 'Material #4.004\doors\left2_002.dmd', 0, False);

DoorModelIDs[4] := LoadModel(basePath + 'Material #4.004\doors\right1.dmd', 0, False);
DoorModelIDs[5] := LoadModel(basePath + 'Material #4.004\doors\right1_1.dmd', 0, False);
DoorModelIDs[6] := LoadModel(basePath + 'Material #4.004\doors\right2.dmd', 0, False);
DoorModelIDs[7] := LoadModel(basePath + 'Material #4.004\doors\right2_1.dmd', 0, False);

// координаты
DoorPos[0].x :=  1.54027; DoorPos[0].y := -10.364;  DoorPos[0].z := 2.72325;
DoorPos[1].x :=  1.54086; DoorPos[1].y := -9.72216; DoorPos[1].z := 2.73019;
DoorPos[2].x :=  1.53711; DoorPos[2].y := 10.3636;  DoorPos[2].z := 2.72394;
DoorPos[3].x :=  1.53756; DoorPos[3].y := 9.72142;  DoorPos[3].z := 2.73681;

DoorPos[4].x := -1.53711; DoorPos[4].y := -10.3636; DoorPos[4].z := 2.72394;
DoorPos[5].x := -1.53756; DoorPos[5].y := -9.72142; DoorPos[5].z := 2.73681;
DoorPos[6].x := -1.54086; DoorPos[6].y := 9.72214;  DoorPos[6].z := 2.73019;
DoorPos[7].x := -1.54027; DoorPos[7].y := 10.364;   DoorPos[7].z := 2.72325;

DoorTextureID := LoadTextureFromFile(
  'C:\ZDSimulator55.008new\ra3\wagon\Material #4.004\ra3_27_RGBA.bmp',
  0, -1
);

  if FindFirst(basePath + '*', faDirectory, sr) = 0 then
  begin
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then Continue;

      matPath := basePath + sr.Name + '\';
      if not DirectoryExists(matPath) then Continue;

      texPath := FindTexture(matPath, False);

      // --- wheelset отдельно ---
      if (WagonWheelsetID = 0) and FileExists(matPath + 'wheelset.dmd') then
      begin
        WagonWheelsetID := LoadModel(matPath + 'wheelset.dmd', 0, False);

        if texPath <> '' then
          WagonWheelsetTextureID := LoadTextureFromFile(texPath, 0, -1);
      end;

      // --- корпус вагона ---
      if FindFirst(matPath + '*.dmd', faAnyFile, sr2) = 0 then
      begin
        repeat
          if Pos('wheelset', LowerCase(sr2.Name)) > 0 then
            Continue;

          SetLength(WagonModelIDs, Length(WagonModelIDs) + 1);
          SetLength(WagonTextureIDs, Length(WagonTextureIDs) + 1);

          WagonModelIDs[High(WagonModelIDs)] :=
            LoadModel(matPath + sr2.Name, 0, False);

          if texPath <> '' then
            WagonTextureIDs[High(WagonTextureIDs)] :=
              LoadTextureFromFile(texPath, 0, -1)
          else
            WagonTextureIDs[High(WagonTextureIDs)] := 0;

        until FindNext(sr2) <> 0;

        FindClose(sr2);
      end;

    until FindNext(sr) <> 0;

    FindClose(sr);
  end;
end;

const
  NSVETOFORS_ADDR: Cardinal = $091D4EA8; // из ассемблера
  POEZD_PTR_ADDR = $0074B52C;
  POEZD_FIRST_WAGON_OFFSET = 108; // начальное смещение ebx от базы
  WAGON_STRIDE = 144;             // 0x90 байт на вагон

  // Смещения относительно ebx (указателя на текущий вагон)
  // ebx указывает на поле [+108] первого вагона
  // Позиции читаются как [ebx - 0x6C], [ebx - 0x64], [ebx - 0x2C]
  REL_OFF_X    = -$6C;  // -108 -> позиция X (double)
  REL_OFF_Y    = -$64;  // -100 -> позиция Y (double)
  REL_OFF_Z    = -$2C;  // -44  -> позиция Z (double)
  REL_OFF_ANGZ = -$14;  // -20  -> RotateZ (double)
  REL_OFF_PITCH= -$24;  // -36  -> RotateX/pitch (double)

function GetLocoAngleZ: Single;
begin
  // NSVETOFORS[584] для локомотива — или из LOCSECTIONS
  // *((double *)&U__VARIABLES____LOCSECTIONS + 61) — это RotateZ локомотива
  // Но проще взять U__VARIABLES____PREVLOKANGLEZDEG
  Result := PSingle($09110D3C)^;  // off_74B4F4 = PREVLOKANGLEZDEG (нужно уточнить)
end;

function GetWagonAngleZ(wagonIdx: Integer): Single;
var
  i: Integer;
begin
  i := wagonIdx + 1;
  // 584 = offset AngleZ (градусы) внутри NSVETOFORS
  Result := PDouble(NSVETOFORS_ADDR + Cardinal(144 * i + 584))^;
end;



var targetDoor: Single;
const
  WAGON_CENTER_OFFSET_X: Single = 0.0;
  WAGON_CENTER_OFFSET_Y: Single = 0.0;
  WAGON_CENTER_OFFSET_Z: Single = 0.0;
var
  targetSide, targetForward: Single;
var
  baseX, baseY, baseZ: Single;


procedure DrawWagonRA3;
var
  w, i: Integer;
  wagonAngZ: Single;
  locoAngZ, wagAng, diff: Double;
  locoX, locoY, locoZ: Double;
  wagX, wagY, wagZ, wagPitch: Double;
  dx, dy, cosA, sinA: Double;
  localX, localY, relZ: Single;
  poezdBase, ebxAddr, locsecPtr: Cardinal;
  speed: Single;
  shakeX, shakeY, shakeZ, shakeAng: Single;
  shakeTime: Double;
  shakeIntensity: Single;

  baseX, baseY, baseZ: Single;

  procedure DrawModelSafe(ModelID, TexID: Integer);
  begin
    if ModelID = 0 then Exit;

    BeginObj3D;
      Position3D(localX + shakeX, localY + shakeY, relZ + shakeZ);
      RotateZ(wagonAngZ + shakeAng);
      Position3D(WAGON_CENTER_OFFSET_X, WAGON_CENTER_OFFSET_Y, WAGON_CENTER_OFFSET_Z);

      if TexID <> 0 then
        SetTexture(TexID);

      DrawModel(ModelID, 0, False);
    EndObj3D;
  end;

  procedure DrawWheelset(offsetY: Single);
  begin
    if WagonWheelsetID = 0 then Exit;

    BeginObj3D;
      Position3D(localX, localY, relZ);
      RotateZ(wagonAngZ);

      Position3D(WAGON_WS_X, offsetY, WAGON_WS_Z);
      RotateX(WagonWheelRotation);

      if WagonWheelsetTextureID <> 0 then
        SetTexture(WagonWheelsetTextureID);

      DrawModel(WagonWheelsetID, 0, False);
    EndObj3D;
  end;

begin
  WagonWheelRotation := WagonWheelRotation + StrToFloatDef(GetSpeed, 0) * 0.05;

  poezdBase := PCardinal(POEZD_PTR_ADDR)^;
  if poezdBase = 0 then Exit;

  locsecPtr := PCardinal($0074AEA0)^;
  if locsecPtr = 0 then Exit;

  locoAngZ := PDouble($09007C5C)^;
  locoX := PDouble(locsecPtr + 24)^;
  locoY := PDouble(locsecPtr + 32)^;
  locoZ := PDouble(locsecPtr + 40)^;

  cosA := Cos(locoAngZ * Pi / 180.0);
  sinA := Sin(locoAngZ * Pi / 180.0);

  speed := Abs(StrToFloatDef(GetSpeed, 0));
  shakeTime := GetTickCount / 1000.0;

// левые двери
if LeftDoorsOpen then
begin
  if LeftForwardOffset < DOOR_FORWARD_MAX then
    LeftForwardOffset := LeftForwardOffset + DOOR_STEP
  else if LeftDoorOffset < DOOR_SIDE_MAX then
    LeftDoorOffset := LeftDoorOffset + DOOR_STEP;
end
else
begin
  if LeftDoorOffset > 0 then
    LeftDoorOffset := LeftDoorOffset - DOOR_STEP
  else if LeftForwardOffset > 0 then
    LeftForwardOffset := LeftForwardOffset - DOOR_STEP;
end;

// правые двери
if RightDoorsOpen then
begin
  if DoorForwardOffset < DOOR_FORWARD_MAX then
    DoorForwardOffset := DoorForwardOffset + DOOR_STEP
  else if RightDoorOffset < DOOR_SIDE_MAX then
    RightDoorOffset := RightDoorOffset + DOOR_STEP;
end
else
begin
  if RightDoorOffset > 0 then
    RightDoorOffset := RightDoorOffset - DOOR_STEP
  else if DoorForwardOffset > 0 then
    DoorForwardOffset := DoorForwardOffset - DOOR_STEP;
end;

  // =========================
  // ВАГОНЫ
  // =========================
  for w := 0 to GetWagonCount - 1 do
  begin
    ebxAddr := poezdBase + 108 + Cardinal(w) * 144;

    wagPitch := PDouble(ebxAddr - $24)^;
    if Abs(wagPitch) > 0.1 then
      Continue;

    wagX := PDouble(ebxAddr - $6C)^;
    wagY := PDouble(ebxAddr - $64)^;
    wagZ := PDouble(ebxAddr - $2C)^;

    wagAng := PDouble(ebxAddr - $14)^;

    diff := wagAng - locoAngZ;
    while diff > 180 do diff := diff - 360;
    while diff < -180 do diff := diff + 360;
    wagonAngZ := diff;

    dx := wagX - locoX;
    dy := wagY - locoY;

    localX := dx * cosA - dy * sinA;
    localY := (dx * sinA + dy * cosA) * 1.05;
    relZ := 0;

    shakeIntensity := speed / 400.0;
    if shakeIntensity > 0.03 then shakeIntensity := 0.03;

    shakeX := Sin(shakeTime * (7.3 + w * 2.1) + w * 17.0) * shakeIntensity;
    shakeY := 0;
    shakeZ := 0;
    shakeAng := Sin(shakeTime * (6.4 + w * 1.5) + w * 13.0) * shakeIntensity * 15.0;

    // =========================
    // ДВЕРИ
    // =========================
    for i := 0 to High(DoorModelIDs) do
    begin
      if DoorModelIDs[i] = 0 then Continue;

      BeginObj3D;
      Position3D(localX, localY, relZ);
      RotateZ(wagonAngZ);

      baseX := DoorPos[i].x;
      baseY := DoorPos[i].y;
      baseZ := DoorPos[i].z;

      if i < 4 then
      begin
        baseX := baseX + LeftForwardOffset;
        baseY := baseY + DOOR_Y_DIR[i] * LeftDoorOffset;
      end
      else
      begin
        baseX := baseX - DoorForwardOffset;
        baseY := baseY + DOOR_Y_DIR[i] * RightDoorOffset;
      end;

      Position3D(baseX, baseY, baseZ);

      if DoorTextureID <> 0 then
        SetTexture(DoorTextureID);

      DrawModel(DoorModelIDs[i], 0, False);
      EndObj3D;
    end;

    // модели вагона
    for i := 0 to High(WagonModelIDs) do
      DrawModelSafe(WagonModelIDs[i], WagonTextureIDs[i]);

    // колесные пары
    DrawWheelset(WAGON_WS_Y_1);
    DrawWheelset(WAGON_WS_Y_2);
    DrawWheelset(WAGON_WS_Y_3);
    DrawWheelset(WAGON_WS_Y_4);

  end;
end;

procedure InitCabRA3;
var
  i: Integer;
  basePath, matPath, modelPath, texPath: string;
begin
  if RA3_CabInitialized then Exit;
  RA3_CabInitialized := True;

  basePath := 'C:\ZDSimulator55.008new\ra3\cab\material';

  for i := 0 to 7 do
  begin
    CabModelIDs[i] := 0;
    CabTextureIDs[i] := 0;

    matPath := basePath + IntToStr(i) + '\';

    if not DirectoryExists(matPath) then
      Continue;

    modelPath := matPath + 'main.dmd';
    if FileExists(modelPath) then
      CabModelIDs[i] := LoadModel(modelPath, 0, False);

    texPath := '';
    if FileExists(matPath + 'ra3_19_RGBA.bmp') then texPath := matPath + 'ra3_19_RGBA.bmp'
    else if FileExists(matPath + 'ra3_20_RGBA.bmp') then texPath := matPath + 'ra3_20_RGBA.bmp'
    else if FileExists(matPath + 'ra3_21_RGBA.bmp') then texPath := matPath + 'ra3_21_RGBA.bmp'
    else if FileExists(matPath + 'ra3_22_RGBA.bmp') then texPath := matPath + 'ra3_22_RGBA.bmp'
    else if FileExists(matPath + 'ra3_23_RGBA.bmp') then texPath := matPath + 'ra3_23_RGBA.bmp'
    else if FileExists(matPath + 'ra3_20_RGBA.png') then texPath := matPath + 'ra3_20_RGBA.png'
    else if FileExists(matPath + 'ra3_20_RGBA.jpg') then texPath := matPath + 'ra3_20_RGBA.jpg';

    if texPath <> '' then
      CabTextureIDs[i] := LoadTextureFromFile(texPath, 0, -1);
  end;
end;

procedure DrawTelega;
begin
  DrawSimpleModel(TelegaMotorID, 0, 0, 0, TelegaTextureID);
end;

procedure DrawWheels;
const
  X = 0.000053;
  Z = 0.710448;
var
  Y: array[0..3] of Single;
  i: Integer;
begin
  Y[0] := 8.36029;
  Y[1] := 6.21029;
  Y[2] := -6.64924;
  Y[3] := -8.79924;

  WheelRotation := WheelRotation + StrToFloatDef(GetSpeed, 0) * 0.05;

  for i := 0 to 3 do
  begin
    if WheelModelIDs[i] = 0 then Continue;

    BeginObj3D;

    Position3D(X, Y[i], Z);
    RotateX(WheelRotation);

    if TelegaTextureID <> 0 then
      SetTexture(TelegaTextureID);

    DrawModel(WheelModelIDs[i], 0, False);

    EndObj3D;
  end;
end;

procedure DrawLocoRA3;
var
  i: Integer;
begin
  // вращение от скорости
  WheelRotation := WheelRotation + StrToFloat(GetSpeed) * 0.05;

  for i := 0 to High(LocoModelIDs) do
  begin
    if LocoModelIDs[i] = 0 then
      Continue;

    BeginObj3D;

    Position3D(0, 0, 0);


    if LocoTextureIDs[i] <> 0 then
      SetTexture(LocoTextureIDs[i]);

    DrawModel(LocoModelIDs[i], 0, False);

    EndObj3D;
  end;
end;

procedure DrawCabRA3;
var
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    if CabModelIDs[i] = 0 then
      Continue;

    BeginObj3D;

    Position3D(0, 0, 0);

    if CabTextureIDs[i] <> 0 then
      SetTexture(CabTextureIDs[i]);

    DrawModel(CabModelIDs[i], 0, False);

    EndObj3D;
  end;
end;

function IsKeyDown(Key: Integer): Boolean;
begin
  Result := (GetAsyncKeyState(Key) and $8000) <> 0;
end;

procedure UpdateControllerAndDraw;
var
  NowTick: Cardinal;
  Addr: Cardinal;
  lbNow: Boolean;
  deltaPx: Single;
  steps, newValue: Integer;
  mouseHandled: Boolean;
begin
  if ControllerModelID = 0 then Exit;

  NowTick := GetTickCount;
  lbNow := IsKeyDownEx(VK_LBUTTON);
  mouseHandled := False;

  // Старт драга: курсор на контроллере и только что нажали ЛКМ
  if HoveredController and lbNow and not LastLButtonState then
  begin
    DraggingController := True;
    DragStartX := MoveXcoord;
    DragStartValue := ControllerMemValue;
    DragStartAngle := ControllerAngle;
  end;

  // Отпустили ЛКМ — конец драга
  if DraggingController and not lbNow then
    DraggingController := False;

  LastLButtonState := lbNow;

  if DraggingController then
  begin
    deltaPx := MoveXcoord - DragStartX;

    TargetAngle := DragStartAngle + deltaPx * DRAG_ANGLE_PER_PX;
    if TargetAngle > 45 then TargetAngle := 45
    else if TargetAngle < -45 then TargetAngle := -45;

    steps := Trunc(deltaPx / DRAG_PX_PER_STEP);
    newValue := DragStartValue + steps;
    if newValue < 0 then newValue := 0
    else if newValue > 5 then newValue := 5;
    ControllerMemValue := newValue;

    mouseHandled := True;
  end;

  if (not mouseHandled) and ((NowTick - InputTimer) > 500) then
  begin
    InputTimer := NowTick;

    if IsKeyDown(Ord('A')) then
    begin
      TargetAngle := 45;
      if ControllerMemValue < 5 then
        Inc(ControllerMemValue);
    end
    else if IsKeyDown(Ord('D')) then
    begin
      TargetAngle := -45;
      if ControllerMemValue > 0 then
        Dec(ControllerMemValue)
      else
        ControllerMemValue := 255;
    end
    else
    begin
      TargetAngle := 0;
    end;
  end;

  ControllerAngle := ControllerAngle + (TargetAngle - ControllerAngle) * 0.15;

  if (NowTick - MemTimer) > 150 then
  begin
    MemTimer := NowTick;

    if ControllerMemValue <> LastWriteValue then
    begin
      Addr := $400000 + $08DD5B05;
      PByte(Addr)^ := Byte(ControllerMemValue);
      LastWriteValue := ControllerMemValue;
    end;
  end;

  BeginObj3D;

  Position3D(CONTROLLER_POS_X, CONTROLLER_POS_Y, CONTROLLER_POS_Z);
  RotateX(ControllerAngle);

  if ControllerTextureID <> 0 then
    SetTexture(ControllerTextureID);

  if HoveredController then
    glColor4f(1.0, 1.0, 0.0, 1.0)
  else
    glColor4f(1.0, 1.0, 1.0, 1.0);

  DrawModel(ControllerModelID, 0, False);

  EndObj3D;
end;



procedure DrawControllerBraking;
begin
  if ControllerBrakingModelID = 0 then Exit;
  BeginObj3D;
  Position3D(BRAKE_POS_X, BRAKE_POS_Y, BRAKE_POS_Z);
  if ControllerTextureID <> 0 then
    SetTexture(ControllerTextureID);
  if HoveredBrake then
    glColor4f(1.0, 1.0, 0.0, 1.0)
  else
    glColor4f(1.0, 1.0, 1.0, 1.0);
  DrawModel(ControllerBrakingModelID, 0, False);
  EndObj3D;
end;

procedure DrawButtons;
var
  zRB, zRBS: Single;
begin
  zRB := 2.62716;
  if IsKeyDown(Ord('Z')) then
    zRB := zRB - 0.005;

  zRBS := 3.65138;
  if IsKeyDown(Ord('M')) then
    zRBS := zRBS - 0.008;

  DrawSimpleModel(ButtonRBModelID,   1.17063, 10.4761, zRB,     ControllerTextureID);
  DrawSimpleModel(ButtonRBPModelID, -1.18603, 10.4965, 2.62716, ControllerTextureID);
  DrawSimpleModel(ButtonRBSModelID,  1.36343, 9.82186, zRBS,    ControllerTextureID);
end;

procedure DrawALSIndicator;
const
  IND_X: Single = 0.353528;
  IND_Y: Single = 10.7587;
  IND_Z: Single = 2.73252;
var
  alsValue: Byte;
  modelID: Integer;
  visibleCount: Integer;
begin
  alsValue := GetALS;
  modelID := 0;

  if alsValue = 5 then
  begin
    // Green: show ALS-EN block count (G1..G4) if ALS-EN active
    if not GetAlsEnState then Exit;
    visibleCount := GetAlsEnVisibleSignalCount;
    if visibleCount <= 0 then Exit;
    if visibleCount >= 4 then
      modelID := IndicatorG4ModelID
    else
      case visibleCount of
        1: modelID := IndicatorG1ModelID;
        2: modelID := IndicatorG2ModelID;
        3: modelID := IndicatorG3ModelID;
      end;
  end
  else
  begin
    case alsValue of
      1: modelID := IndicatorW_M_ModelID;   // White
      2: modelID := IndicatorR_M_ModelID;   // Red
      3: modelID := IndicatorRY_M_ModelID;  // КЖ
      4: modelID := IndicatorY_M_ModelID;   // Yellow
    else
      Exit;
    end;
  end;

  DrawSimpleModel(modelID, IND_X, IND_Y, IND_Z, ALSTextureID);
end;

procedure DrawArrow(ModelID: Integer; x, y, z, rx, ry, rz: Single);
begin
  if ModelID = 0 then Exit;
  BeginObj3D;
  Position3D(x, y, z);
  RotateX(rx);
  RotateY(ry);
  RotateZ(rz);
  if ControllerTextureID <> 0 then
    SetTexture(ControllerTextureID);
  DrawModel(ModelID, 0, False);
  EndObj3D;
end;

procedure DrawArrows;
begin
  DrawArrow(ArrowPMModelID,  1.16464, 10.6383, 2.75221,  0.481073, -0.141711,  0);
  DrawArrow(ArrowTC1ModelID, 1.06527, 10.669,  2.75172,  0, 0, 0);
  DrawArrow(ArrowTC2ModelID, 1.06527, 10.679,  2.75172,  0.492883, -0.092628, -0.170907);
  DrawArrow(ArrowTMModelID,  1.16464, 10.6383, 2.75221,  0.481069, -0.141727, -0.084749);
end;

procedure InitRA3;
begin
  if not IsRA3Active then Exit;
  InitCabRA3;
  InitLocoRA3;
  InitWagonRA3;
  InitDynamicRA3;
  WriteRA3CameraInit;
end;

procedure DrawRA3;
var
  tNow, yNow: Boolean;
begin
  if not IsRA3Active then Exit;

  InitRA3;
  UpdateHover;

  // T = открыть левые, Shift+T = закрыть левые
  tNow := IsKeyDown(Ord('T'));
  if tNow and not LastTState then
    LeftDoorsOpen := not IsKeyDown(VK_SHIFT);
  LastTState := tNow;

  // Y = открыть правые, Shift+Y = закрыть правые
  yNow := IsKeyDown(Ord('Y'));
  if yNow and not LastYState then
    RightDoorsOpen := not IsKeyDown(VK_SHIFT);
  LastYState := yNow;

  // --- отрисовка ---
  DrawCabRA3;
  DrawLocoRA3;
  DrawTelega;
  DrawWheels;
  DrawWagonRA3;
  UpdateControllerAndDraw;
  DrawControllerBraking;
  DrawButtons;
  DrawALSIndicator;
  DrawArrows;
end;

procedure ApplyRA3BlockTransform(x, y, z, AngZ: Single);
begin
  Position3D(AngZ - 0.93, z + 0.65, y - 0.32);
  RotateX(35);
end;

end.
