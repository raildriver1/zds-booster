unit RA3;

interface

function IsRA3Active: Boolean;
procedure InitRA3;
procedure DrawRA3;
procedure ApplyRA3BlockTransform(x, y, z, AngZ: Single);

// RRS-state контроллера машиниста (mode_pos + trac_level/brake_level).
// Возвращает handle_pos = (mode_pos*10 + trac_level - brake_level) / 100,
// диапазон [-1.1 .. +1.0]. Тождественно RRS getHandlePosition().
// Используется физикой РА-3 чтобы получить точный float-уровень тяги/тормоза
// без потерь точности через byte (0..5/251..255).
function GetRRSHandlePos: Single;

implementation

uses
  OpenGL, Windows, SysUtils, Variables, EngineUtils, DrawFunc3D, Advanced3D, Textures, KlubData, CheatMenu, RA3Physics;


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

  DragStartValue: Integer = 0;

  LastTState: Boolean = False;
  LastYState: Boolean = False;
  LeftDoorsOpen: Boolean = False;
  LeftDoorOffset: Single = 0;
  LeftForwardOffset: Single = 0;
  DoorForwardOffset: Single = 0;

  BrakeControllerPos: Integer = 0;    // 0=отпуск, 1=перекрыша, 2=торможение
  BrakeControllerAngle: Single = 0.0;
  BrakeTargetAngle: Single = 0.0;
  LastBrakeIncKey: Boolean = False;
  LastBrakeDecKey: Boolean = False;
  DraggingBrake: Boolean = False;
  DragBrakeStartX: Single = 0;
  DragBrakeStartPos: Integer = 0;
  LastBrakeLButtonState: Boolean = False;

  LastBoosterToggle: Boolean = False;

  ControllerAngle: Single = 0.0;

  LastNState: Boolean = False;
  LastLButtonState: Boolean;

  DraggingController: Boolean;
  DragStartX: Single;
  DragStartAngle: Single;

  InputCooldown: Cardinal = 0;

RB_Offset: Single = 0;
RBS_Offset: Single = 0;

RB_Target: Single = 0;
RBS_Target: Single = 0;

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

  RB_X = 1.17063;
  RB_Y = 10.4761;
  RB_Z = 2.62716;

  RBS_X = 1.36343;
  RBS_Y = 9.82186;
  RBS_Z = 2.61900;

var
  DoorModelIDs: array of Integer;
  DoorPos: array of record
    x, y, z: Single;
  end;
  DoorTextureID: Integer = 0;
  RightDoorsOpen: Boolean = False;
  RightDoorOffset: Single = 0;

HoveredRB: Boolean = False;
HoveredRBS: Boolean = False;

RBPressed: Boolean = False;
RBSPressed: Boolean = False;

LastButtonsLMB: Boolean = False;

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

procedure UpdateButtonsHoverAndClick;
var
  v: TVertex;
  p: TPoint;
  dx, dy: Integer;
  lb: Boolean;
  r2: Integer;
begin
  lb := IsKeyDownEx(VK_LBUTTON);

  HoveredRB := False;
  HoveredRBS := False;

  r2 := 40 * 40;

  // =========================
  // RB
  // =========================
  v.X := RB_X;
  v.Y := RB_Y;
  v.Z := RB_Z;
  p := Get2DPos(v);

  dx := p.X - Round(MoveXcoord);
  dy := p.Y - Round(MoveYcoord);

  if (dx*dx + dy*dy) < r2 then
  begin
    HoveredRB := True;

    if lb then
      RB_Target := -0.004
    else
      RB_Target := 0;
  end
  else
    RB_Target := 0;

  // =========================
  // RBS
  // =========================
  v.X := RBS_X;
  v.Y := RBS_Y;
  v.Z := RBS_Z;
  p := Get2DPos(v);

  dx := p.X - Round(MoveXcoord);
  dy := p.Y - Round(MoveYcoord);

  if (dx*dx + dy*dy) < r2 then
  begin
    HoveredRBS := True;

    if lb then
      RBS_Target := -0.006
    else
      RBS_Target := 0;
  end
  else
    RBS_Target := 0;
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
  // Кроме того, если активен гизмо кастомных текстов — он сам уже зовёт
  // ApplyMenuPatch (UpdateGizmoFrameRA3 живёт по тому же паттерну), так что
  // не мешаем ему: HoverMode-ветка просто не должна снимать патч в этом случае.
  wantPatch := (HoverMode and (HoveredController or DraggingController or HoveredBrake or DraggingBrake)) or IsGizmoActive;
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


var
  lastX: Single;
  MouseDeltaAccum: Single;
  MouseAccum: Single;
  // Запоминаем последний записанный нами в память байт контроллера —
  // если в памяти он стал другим, значит игра (native input) изменила его,
  // и мы должны синхронизировать наш визуальный угол с ним.
  LastWrittenCtrlByte: Byte = 0;
  LastWrittenKranByte: Byte = 0;

const
  // Адреса памяти ZDsim 55.008 (синхронизация визуальных контроллеров кабины
  // РА-3 со штатными переменными игры).
  ADDR_KONTROLLER_BYTE: Cardinal = $091D5B05;  // byte 0..5 / 251..255 (тяга/тормоз)
  ADDR_KRAN395_BYTE   : Cardinal = $090043A0;  // byte позиции крана 395

// Преобразование угла визуального тягового контроллера (-59°..+59°) в байт
// штатного контроллера ZDsim (0..5 для тяги, 251..255 для тормоза).
//
// Полный диапазон ±59° теперь равномерно покрывает 5 позиций ZDsim:
//   мёртвая зона ±4°       = byte 0 (нейтраль)
//   ±4..16°                = byte 1 / 255  (центр 10°)
//   ±16..28°               = byte 2 / 254  (центр 22°)
//   ±28..40°               = byte 3 / 253  (центр 34°)
//   ±40..52°               = byte 4 / 252  (центр 46°)
//   ±52..59°               = byte 5 / 251  (центр 58°)
//
// Шаги в UpdateControllerAndDraw уменьшены в 3 раза (5° / 1°) — но мёртвая зона
// тоже уменьшена с 7.5° до 4°, поэтому первый прыжок 5° гарантированно даёт
// byte=1 (выход из нейтрали в первую позицию).
function ControllerAngleToByte(angle: Single): Byte;
var
  pos: Integer;
begin
  // Мёртвая зона ±4° (нейтраль).
  if Abs(angle) < 4.0 then
  begin
    Result := 0;
    Exit;
  end;
  // |angle|=10° → pos=1, |angle|=58° → pos=5
  pos := 1 + Round((Abs(angle) - 4.0) / 12.0);
  if pos < 1 then pos := 1;
  if pos > 5 then pos := 5;
  if angle > 0 then Result := pos
  else Result := 256 - pos;  // -1=>255, -5=>251
end;

// Обратное преобразование: byte → угол. Используется при синхронизации
// нашего visual angle с тем что игра сама поставила в память.
// Возвращает центр соответствующего диапазона.
function ControllerByteToAngle(b: Byte): Single;
begin
  case b of
    0:        Result := 0.0;
    1..5:     Result := 10.0 + (b - 1) * 12.0;        // 1→10, 5→58
    251..255: Result := -(10.0 + (255 - b) * 12.0);   // 255→-10, 251→-58
  else
    Result := 0.0;
  end;
end;

// Безопасное чтение байта по абсолютному адресу (try/except).
function ReadByteSafe(addr: Cardinal; def: Byte): Byte;
begin
  try
    Result := PByte(addr)^;
  except
    Result := def;
  end;
end;

// Безопасная запись байта.
procedure WriteByteSafe(addr: Cardinal; value: Byte);
begin
  try
    PByte(addr)^ := value;
  except
  end;
end;

// ============================================================================
// КОНТРОЛЛЕР РА-3 — портирован 1:1 из RRS v4.0.5 trac-controller.{cpp,h}
// (автор Дмитрий Притыкин, https://gitlab.com/maisvendoo/ra3).
//
// Это state-машина, не "угол прыжками". Состояния mode_pos:
//   0  — выбег (нейтраль)
//   1  — тяга,  trac_level  растёт от 0 до RRS_TRAC_MAX
//  -1  — тормоз, brake_level растёт от 0 до RRS_BRAKE_MAX
//  -2  — экстренное торможение
//
// Алгоритм клавиш A/D ровно как в trac-controller.cpp::stepKeysControl:
//   • Ctrl+D = моментальный сброс в выбег (приоритетная проверка, не
//     конфликтует с обычным D потому что идёт ПЕРВОЙ).
//   • mode_pos=0:    rising-edge A/D переводят в режим, ставят флаг traction
//                    /brake = false (блокировка непрерывного управления до
//                    отпускания клавиши и повторного нажатия).
//   • mode_pos=1:    A с traction=true → +1 шаг trac_level каждые
//                    HANDLE_MOTION_TIME сек (с Shift × HANDLE_HIGH_SPEED_COEFF).
//                    D → -1 шаг trac_level (или возврат в выбег при 0).
//   • mode_pos=-1:   D с brake=true → +1 brake_level. A → -1 brake_level
//                    (или возврат в выбег при 0). brake_level=90 + новое
//                    нажатие D → переход в экстренное (-2).
//   • mode_pos=-2:   A → возврат в -1 с brake_level=90.
//
// Параметры из cfg/vehicles/ra3-head/trac-controller.xml: "0.1 8 10 10 8.0e-2"
//   handle_motion_time      = 0.1 сек (1 шаг level)
//   handle_high_speed_coeff = 8       (множитель при Shift)
//   trac_min                = 10      (% — минимум при включении тяги)
//   brake_min               = 10
//
// Визуальный угол ControllerAngle вычисляется как
//   handle_pos = (mode_pos*10 + trac_level - brake_level) / 100  ∈ [-1.1, +1]
//   ControllerAngle = handle_pos * 59°                            ∈ [-65, +59]°
// и передаётся в физику через SetVirtualController(handle_pos * 5).
// ============================================================================
const
  HANDLE_MOTION_TIME      : Single = 0.1;     // сек на 1 шаг level
  HANDLE_HIGH_SPEED_COEFF : Integer = 8;      // Shift × 8
  RRS_TRAC_MIN            : Integer = 10;     // %
  RRS_BRAKE_MIN           : Integer = 10;
  RRS_TRAC_MAX            : Integer = 90;     // 100 - trac_min
  RRS_BRAKE_MAX           : Integer = 90;     // 100 - brake_min

var
  // RRS state-машина (соответствует trac-controller.h:60-77)
  RRS_mode_pos          : Integer = 0;       // 0 / 1 / -1 / -2
  RRS_mode_pos_old      : Integer = 0;
  RRS_trac_level        : Integer = 0;       // 0..90
  RRS_brake_level       : Integer = 0;       // 0..90
  RRS_traction          : Boolean = False;   // разрешение непрерывного управления в тяге
  RRS_brake             : Boolean = False;   // разрешение непрерывного управления в тормозе
  RRS_old_a_key         : Boolean = False;
  RRS_old_d_key         : Boolean = False;
  RRS_handle_motion_speed: Integer = 0;      // ±1, ±8
  RRS_motion_timer_acc  : Single  = 0.0;     // аккумулятор для шага через HANDLE_MOTION_TIME

  CtrlLastUpdateTick    : Cardinal = 0;

// Публичный геттер handle_pos из RRS state-машины (см. interface).
// Это то самое, что RRS возвращает из TracController::getHandlePosition().
function GetRRSHandlePos: Single;
begin
  Result := (RRS_mode_pos * 10 + RRS_trac_level - RRS_brake_level) / 100.0;
end;

// Шаг уровня — вызывается каждые HANDLE_MOTION_TIME сек.
// Соответствует RRS slotTracLevelProcess + slotBrakeLevelProcess:
//   trac_level  += handle_motion_speed * mode_pos
//   brake_level += handle_motion_speed * mode_pos
procedure StepRRSLevels;
begin
  if RRS_mode_pos = 1 then
  begin
    RRS_trac_level := RRS_trac_level + RRS_handle_motion_speed * RRS_mode_pos;
    if RRS_trac_level < 0 then RRS_trac_level := 0;
    if RRS_trac_level > RRS_TRAC_MAX then RRS_trac_level := RRS_TRAC_MAX;
  end
  else if RRS_mode_pos = -1 then
  begin
    RRS_brake_level := RRS_brake_level + RRS_handle_motion_speed * RRS_mode_pos;
    if RRS_brake_level < 0 then RRS_brake_level := 0;
    if RRS_brake_level > RRS_BRAKE_MAX then RRS_brake_level := RRS_BRAKE_MAX;
  end;
end;

// Когда native input ZDsim изменил byte (через штатные клавиши игры) —
// синхронизируем нашу RRS-машину с этим byte.
procedure SyncFromZDsimByte(b: Byte);
begin
  case b of
    0:
      begin
        RRS_mode_pos := 0;
        RRS_trac_level := 0;
        RRS_brake_level := 0;
        RRS_traction := False;
        RRS_brake := False;
      end;
    1..5:
      begin
        RRS_mode_pos := 1;
        RRS_brake_level := 0;
        // byte 1=начало, byte 5=макс → trac_level от 0 до RRS_TRAC_MAX
        RRS_trac_level := Round((b - 1) * RRS_TRAC_MAX / 4.0);
        RRS_traction := True;
      end;
    251..255:
      begin
        RRS_mode_pos := -1;
        RRS_trac_level := 0;
        RRS_brake_level := Round((255 - b) * RRS_BRAKE_MAX / 4.0);
        RRS_brake := True;
      end;
  end;
end;

procedure UpdateControllerAndDraw;
var
  lbNow: Boolean;
  aKey, dKey, shiftKey, ctrlKey: Boolean;
  deltaPx: Single;
  curMemByte, newByte: Byte;
  now: Cardinal;
  dtSec: Single;
  handle_pos_norm: Single;
begin
  if ControllerModelID = 0 then Exit;

  now := GetTickCount;
  if CtrlLastUpdateTick = 0 then
    dtSec := 0.016
  else
    dtSec := (now - CtrlLastUpdateTick) / 1000.0;
  if dtSec > 0.1 then dtSec := 0.1;
  CtrlLastUpdateTick := now;

  // ── НАШ КОНТРОЛЛЕР — ЕДИНОЛИЧНЫЙ ХОЗЯИН BYTE ─────────────────────────
  // Мы НЕ синхронизируем RRS-машину из byte. ZDsim native input при не-
  // нейтральном реверсе пишет в этот byte ПОЛНЫМИ позициями (1,2,3...) ―
  // если мы будем подтягивать наше mode_pos+trac_level из его байта, то
  // RRS-плавный набор по 0.1°/100мс превратится в скачкú по 22% уровня.
  // Поэтому: читаем byte, ничего не делаем, далее наш state перезаписывает
  // byte в конце функции (это и есть единственный путь обновления byte).
  curMemByte := ReadByteSafe(ADDR_KONTROLLER_BYTE, 0);
  // (resync отключён — оставляем переменную для совместимости диагностики)
  if curMemByte = $FF then ;  // подавление warning об unused

  lbNow := IsKeyDownEx(VK_LBUTTON);

  // =========================
  // DRAG START/STOP
  // =========================
  if HoveredController and lbNow and not LastLButtonState then
  begin
    DraggingController := True;
    DragStartX := MoveXcoord;
  end;

  if DraggingController and not lbNow then
    DraggingController := False;

  LastLButtonState := lbNow;

  // =========================
  // KEYBOARD — RRS trac-controller.cpp::stepKeysControl 1:1
  // =========================
  aKey     := IsKeyDown(Ord('A'));
  dKey     := IsKeyDown(Ord('D'));
  shiftKey := IsKeyDown(VK_SHIFT);
  ctrlKey  := IsKeyDown(VK_CONTROL);

  // По умолчанию мотор не двигается (если ничего не нажато)
  RRS_handle_motion_speed := 0;

  // ── Ctrl+D = моментальный сброс в выбег (RRS:71) ─────────────────────
  // Эта проверка ИДЁТ ПЕРВОЙ — и потому Ctrl+D не конфликтует с обычным
  // D. Когда зажат Ctrl, обычная ветка обработки D не выполняется.
  if ctrlKey and dKey then
  begin
    RRS_mode_pos := 0;
    RRS_handle_motion_speed := 0;
    RRS_brake := False; RRS_brake_level := 0;
    RRS_traction := False; RRS_trac_level := 0;
    RRS_old_a_key := True;  // блокируем непрерывное управление до отпускания
    RRS_old_d_key := True;
  end
  else if RRS_mode_pos = -2 then
  begin
    // ── Экстренное торможение (RRS:81) ─────────────────────────────────
    // Возврат в максимальный уровень торможения по нажатию A
    if aKey then
    begin
      RRS_mode_pos := -1;
      RRS_brake_level := 90;
    end;
  end
  else if RRS_mode_pos = 0 then
  begin
    // ── Контроллер в выбеге (RRS:90) ───────────────────────────────────
    RRS_trac_level := 0;
    RRS_brake_level := 0;
    if dKey and not RRS_old_d_key then
    begin
      RRS_mode_pos := -1;
      RRS_brake := False;  // блокируем непрерывное до 2-го нажатия
    end;
    if aKey and not RRS_old_a_key then
    begin
      RRS_mode_pos := 1;
      RRS_traction := False;
    end;
  end
  else if RRS_mode_pos = -1 then
  begin
    // ── Контроллер в торможении (RRS:104) ──────────────────────────────
    RRS_traction := False;
    if aKey then
    begin
      // Возврат в выбег при brake_level=0
      if RRS_brake_level = 0 then
      begin
        RRS_mode_pos := 0;
        RRS_brake := False;
        RRS_handle_motion_speed := 0;
      end
      else
      begin
        if shiftKey then RRS_handle_motion_speed := HANDLE_HIGH_SPEED_COEFF
        else RRS_handle_motion_speed := 1;
      end;
    end;
    if dKey then
    begin
      if RRS_brake then
      begin
        if shiftKey then RRS_handle_motion_speed := -HANDLE_HIGH_SPEED_COEFF
        else RRS_handle_motion_speed := -1;
      end
      else
        RRS_handle_motion_speed := 0;
      // После максимального уровня торможения переход в экстренное
      // (только новым нажатием клавиши)
      if (RRS_brake_level = 90) and not RRS_old_d_key then
        RRS_mode_pos := -2;
    end
    else
      RRS_brake := True;  // разрешаем после отпускания клавиши
  end
  else if RRS_mode_pos = 1 then
  begin
    // ── Контроллер в тяге (RRS:139) ────────────────────────────────────
    RRS_brake := False;
    if dKey then
    begin
      // Возврат в выбег при trac_level=0
      if RRS_trac_level = 0 then
      begin
        RRS_mode_pos := 0;
        RRS_traction := False;
        RRS_handle_motion_speed := 0;
      end
      else
      begin
        if shiftKey then RRS_handle_motion_speed := -HANDLE_HIGH_SPEED_COEFF
        else RRS_handle_motion_speed := -1;
      end;
    end;
    if aKey then
    begin
      if RRS_traction then
      begin
        if shiftKey then RRS_handle_motion_speed := HANDLE_HIGH_SPEED_COEFF
        else RRS_handle_motion_speed := 1;
      end
      else
        RRS_handle_motion_speed := 0;
    end
    else
      RRS_traction := True;  // разрешаем после отпускания клавиши
  end;

  RRS_old_a_key := aKey;
  RRS_old_d_key := dKey;

  // ── N = аварийный сброс в выбег (наша добавка для удобства) ──────────
  if IsKeyDown(Ord('N')) and not LastNState then
  begin
    RRS_mode_pos := 0;
    RRS_handle_motion_speed := 0;
    RRS_brake := False; RRS_brake_level := 0;
    RRS_traction := False; RRS_trac_level := 0;
  end;
  LastNState := IsKeyDown(Ord('N'));

  // ── ТАЙМЕР: каждые HANDLE_MOTION_TIME сек → 1 шаг level ──────────────
  // Эквивалент tracTimer/brakeTimer в RRS (timeout=0.1).
  RRS_motion_timer_acc := RRS_motion_timer_acc + dtSec;
  while RRS_motion_timer_acc >= HANDLE_MOTION_TIME do
  begin
    RRS_motion_timer_acc := RRS_motion_timer_acc - HANDLE_MOTION_TIME;
    StepRRSLevels;
  end;

  RRS_mode_pos_old := RRS_mode_pos;

  // =========================
  // MOUSE DRAG — переведено на RRS-state
  // 1 шаг drag (DRAG_PX_PER_STEP пикселей) = 1 единица level (или
  // переход режима 0→1/-1).
  // =========================
  if DraggingController then
  begin
    deltaPx := MoveXcoord - DragStartX;
    DragStartX := MoveXcoord;
    MouseAccum := MouseAccum + deltaPx;

    // вправо (тяга)
    while MouseAccum >= DRAG_PX_PER_STEP do
    begin
      if RRS_mode_pos = 0 then
      begin
        RRS_mode_pos := 1;
        RRS_traction := True;
      end
      else if RRS_mode_pos = 1 then
      begin
        if RRS_trac_level < RRS_TRAC_MAX then
          RRS_trac_level := RRS_trac_level + 1;
      end
      else if RRS_mode_pos = -1 then
      begin
        if RRS_brake_level > 0 then
          RRS_brake_level := RRS_brake_level - 1
        else
          RRS_mode_pos := 0;
      end
      else if RRS_mode_pos = -2 then
      begin
        RRS_mode_pos := -1;
        RRS_brake_level := 90;
      end;
      MouseAccum := MouseAccum - DRAG_PX_PER_STEP;
    end;

    // влево (тормоз)
    while MouseAccum <= -DRAG_PX_PER_STEP do
    begin
      if RRS_mode_pos = 0 then
      begin
        RRS_mode_pos := -1;
        RRS_brake := True;
      end
      else if RRS_mode_pos = -1 then
      begin
        if RRS_brake_level < RRS_BRAKE_MAX then
          RRS_brake_level := RRS_brake_level + 1;
      end
      else if RRS_mode_pos = 1 then
      begin
        if RRS_trac_level > 0 then
          RRS_trac_level := RRS_trac_level - 1
        else
          RRS_mode_pos := 0;
      end;
      MouseAccum := MouseAccum + DRAG_PX_PER_STEP;
    end;
  end;

  // ── ВЫЧИСЛЕНИЕ УГЛА ИЗ handle_pos ────────────────────────────────────
  // RRS handle_pos = (mode_pos*10 + trac_level - brake_level)/100  ∈ [-1.1, +1]
  // Маппим в визуальные ±59° (для экстренного допускаем -65° зрительно).
  handle_pos_norm := (RRS_mode_pos * 10 + RRS_trac_level - RRS_brake_level) / 100.0;
  ControllerAngle := handle_pos_norm * 59.0;
  if ControllerAngle > 59  then ControllerAngle := 59;
  if ControllerAngle < -65 then ControllerAngle := -65;

  // ── ЗАПИСЬ В ZDsim BYTE-КОНТРОЛЛЕР ───────────────────────────────────
  newByte := ControllerAngleToByte(ControllerAngle);
  WriteByteSafe(ADDR_KONTROLLER_BYTE, newByte);
  LastWrittenCtrlByte := newByte;

  // =========================
  // DRAW
  // =========================
  BeginObj3D;

    Position3D(CONTROLLER_POS_X, CONTROLLER_POS_Y, CONTROLLER_POS_Z);
    RotateX(ControllerAngle);

    if ControllerTextureID <> 0 then
      SetTexture(ControllerTextureID);

    if HoveredController then
      glColor4f(1,1,0,1)
    else
      glColor4f(1,1,1,1);

    DrawModel(ControllerModelID, 0, False);

  EndObj3D;
end;




// Маппинг 3-позиционного визуального тормозного крана → byte KRAN395.
// Реальный кран машиниста 395 имеет 7 позиций (I..VI + V_A), но наша
// упрощённая модель даёт только 3:
//   Pos=0 (отпуск)     → byte 2 (II — поездное, тормоз отпущен)
//   Pos=1 (перекрыша)  → byte 4 (IV — перекрыша с питанием)
//   Pos=2 (торможение) → byte 5 (V — служебное торможение)
function BrakePosToKran395Byte(pos: Integer): Byte;
begin
  case pos of
    0: Result := 2;  // II — поездное
    1: Result := 4;  // IV — перекрыша с питанием
    2: Result := 5;  // V — служебное торможение
  else
    Result := 2;
  end;
end;

// Обратное: byte KRAN395 → наша 3-позиционная модель.
// Покрываем все возможные значения от ZDsim.
function Kran395ByteToBrakePos(b: Byte): Integer;
begin
  case b of
    1, 2:        Result := 0;  // I (зарядка), II (поездное) → отпуск
    3, 4:        Result := 1;  // III, IV (перекрыша) → перекрыша
    5, 6, 50:    Result := 2;  // V (служебное), VI (экстр.) → торможение
  else
    Result := 0;
  end;
end;

procedure DrawControllerBraking;
const
  VK_OEM_SEMICOLON = $BA;
  VK_OEM_QUOTE     = $DE;
var
  incKey, decKey, lbNow: Boolean;
  deltaPx: Single;
  newPos: Integer;
  v1, v2: TVertex;
  p1, p2: TPoint;
  curKranByte, newKranByte: Byte;
begin
  if ControllerBrakingModelID = 0 then Exit;

  // ── СИНХРОНИЗАЦИЯ С KRAN395 BYTE ─────────────────────────────────────
  // Если игра/native input изменили крановый байт — обновляем визуал.
  curKranByte := ReadByteSafe(ADDR_KRAN395_BYTE, 2);
  if curKranByte <> LastWrittenKranByte then
    BrakeControllerPos := Kran395ByteToBrakePos(curKranByte);

  lbNow := IsKeyDownEx(VK_LBUTTON);

  // начало drag
  if HoveredBrake and lbNow and not LastBrakeLButtonState then
  begin
    DraggingBrake := True;
    DragBrakeStartX := MoveXcoord;
    DragBrakeStartPos := BrakeControllerPos;
  end;

  // конец drag
  if DraggingBrake and not lbNow then
    DraggingBrake := False;

  LastBrakeLButtonState := lbNow;

  if DraggingBrake then
  begin
    deltaPx := DragBrakeStartX - MoveXcoord;

    // Для тормозного "увеличение" направлено в +Y (т.к. используется RotateX(-angle))
    v1.X := BRAKE_POS_X;
    v1.Y := BRAKE_POS_Y;
    v1.Z := BRAKE_POS_Z + 0.2;
    v2.X := BRAKE_POS_X;
    v2.Y := BRAKE_POS_Y + 0.1;
    v2.Z := BRAKE_POS_Z + 0.2;
    p1 := Get2DPos(v1);
    p2 := Get2DPos(v2);
    if p2.X < p1.X then
      deltaPx := -deltaPx;

    newPos := DragBrakeStartPos + Trunc(deltaPx / DRAG_PX_PER_STEP);
    if newPos < 0 then newPos := 0
    else if newPos > 2 then newPos := 2;
    BrakeControllerPos := newPos;
  end
  else
  begin
    // клавиши: ; = отпуск, ' = торможение
    incKey := IsKeyDown(VK_OEM_QUOTE);
    decKey := IsKeyDown(VK_OEM_SEMICOLON);
    if incKey and not LastBrakeIncKey then
      if BrakeControllerPos < 2 then Inc(BrakeControllerPos);
    if decKey and not LastBrakeDecKey then
      if BrakeControllerPos > 0 then Dec(BrakeControllerPos);
    LastBrakeIncKey := incKey;
    LastBrakeDecKey := decKey;
  end;

  // ── ЗАПИСЬ В KRAN395 BYTE ─────────────────────────────────────────────
  // Преобразуем 3-позиционный визуал → byte (2/4/5) и пишем в память игры.
  // Запоминаем что записали для последующей детекции native изменений.
  newKranByte := BrakePosToKran395Byte(BrakeControllerPos);
  WriteByteSafe(ADDR_KRAN395_BYTE, newKranByte);
  LastWrittenKranByte := newKranByte;

  BrakeTargetAngle := BrakeControllerPos * 30.0;
  BrakeControllerAngle := BrakeControllerAngle + (BrakeTargetAngle - BrakeControllerAngle) * 0.15;

  BeginObj3D;
  Position3D(BRAKE_POS_X, BRAKE_POS_Y, BRAKE_POS_Z);
  RotateX(-BrakeControllerAngle);
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
begin
  // =========================
  // ПРУЖИНА (важно!)
  // =========================
  RB_Offset := RB_Offset + (RB_Target - RB_Offset) * 0.25;
  RBS_Offset := RBS_Offset + (RBS_Target - RBS_Offset) * 0.25;

  // =========================
  // RB
  // =========================
  if HoveredRB then
    glColor4f(1, 1, 0, 1)
  else
    glColor4f(1, 1, 1, 1);

  DrawSimpleModel(
    ButtonRBModelID,
    RB_X, RB_Y,
    RB_Z + RB_Offset,
    ControllerTextureID
  );

  // =========================
  // RBP (без логики — как у тебя было)
  // =========================
  DrawSimpleModel(
    ButtonRBPModelID,
    -1.18603, 10.4965, 2.62716,
    ControllerTextureID
  );

  // =========================
  // RBS
  // =========================
  if HoveredRBS then
    glColor4f(1, 1, 0, 1)
  else
    glColor4f(1, 1, 1, 1);

  DrawSimpleModel(
    ButtonRBSModelID,
    RBS_X, RBS_Y,
    RBS_Z + RBS_Offset,
    ControllerTextureID
  );
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
    if visibleCount >= 5 then
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
      5: modelID := IndicatorG4ModelID;
    else
      Exit;
    end;
  end;

  DrawSimpleModel(modelID, IND_X, IND_Y, IND_Z, ALSTextureID);
end;

procedure DrawArrow(ModelID: Integer; x, y, z, angle: Single);
begin
  if ModelID = 0 then Exit;

  BeginObj3D;

    // 1. ставим модель
    Position3D(x, y, z);
    SetTexture(ControllerTextureID);
    DrawModel(ArrowPMModelID, 0, False);

  EndObj3D;
end;

procedure DrawArrows;
var
  angle: Single;
begin
  angle := 0; // временно

  DrawArrow(ArrowPMModelID, 1.16464, 10.6383, 2.75221, angle);
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

// Флаг "предыдущее состояние IsRA3Active" — чтобы детектировать момент
// ВХОДА в РА-3 (False → True). На этом моменте автоматически:
//   1) Перезапускаем камеру (CAM_INIT_X/Y/Z) если опция MainCamera в кфг
//   2) Включаем РА-3 booster (если ещё не активен)
// Раньше это делалось вручную через Alt+Z — теперь автоматически.
var
  RA3_PrevActive: Boolean = False;

procedure DrawRA3;
var
  tNow, yNow: Boolean;
  isActive, justEntered: Boolean;
begin
  isActive := IsRA3Active;
  justEntered := isActive and not RA3_PrevActive;
  RA3_PrevActive := isActive;

  if not isActive then Exit;

  // ── АВТОИНИЦИАЛИЗАЦИЯ ПРИ ВХОДЕ В РА-3 ───────────────────────────────
  if justEntered then
  begin
    // 1) Перезапуск основной камеры — только если опция MainCamera в кфг
    //    включена. Сбрасываем флаг RA3_CameraWritten, чтобы WriteRA3CameraInit
    //    в InitRA3 (вызывается ниже) применила CAM_INIT_X/Y/Z заново.
    if Config_MainCamera then
      RA3_CameraWritten := False;

    // 2) Автоматический запуск РА-3 бустера. ToggleRA3Booster включает физику,
    //    запись в SilaTyagi и дефолтные настройки (TractionScale=0.008,
    //    SpeedLimit=120 км/ч). Если уже был активен — не трогаем.
    if not IsRA3BoosterActive then
      ToggleRA3Booster;
  end;

  InitRA3;
  UpdateHover;

  // ── ПЕРЕДАЧА RRS-STATE В ФИЗИКУ ─────────────────────────────────────
  // Физика РА-3 получает значение НАПРЯМУЮ из RRS state-машины
  // (mode_pos + trac_level/brake_level), минуя byte-контроллер ZDsim.
  // GetRRSHandlePos = handle_pos ∈ [-1.1, +1.0] — тождество RRS
  // TracController::getHandlePosition(). Умножаем на 5 чтобы попасть в
  // [-5.5, +5.0] — формат VirtualCtrlValue в RA3Physics (там MapController
  // делит обратно на 5 → trac_level/brake_level в [0..1]).
  //
  // Условие "только если ControllerAngle > 0.5" больше не нужно: при
  // mode_pos=0 GetRRSHandlePos=0, что эквивалентно нейтрали в физике.
  if IsRA3BoosterActive then
    SetVirtualController(GetRRSHandlePos * 5.0)
  else
    ClearVirtualController;

  StepRA3Physics;

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
  UpdateButtonsHoverAndClick;
  DrawButtons;
  DrawALSIndicator;
  DrawArrows;
  // Пользовательские 3D-тексты + гизмо. Используем общую процедуру с
  // per-frame дедуплексом — её же зовут хуки для других локомотивов
  // (HookKLUB / DrawKPD3 / DrawBLOCK), и за кадр случится только один рендер.
  RenderCustomTextsAndGizmoForFrame;
end;

procedure ApplyRA3BlockTransform(x, y, z, AngZ: Single);
begin
  Position3D(AngZ - 0.93, z + 0.65, y - 0.32);
  RotateX(35);
end;

end.
