unit RA3;

interface

function IsRA3Active: Boolean;
procedure InitRA3;
procedure DrawRA3;
procedure ApplyRA3BlockTransform(x, y, z, AngZ: Single);

implementation

uses
  OpenGL, Windows, SysUtils, Variables, EngineUtils, DrawFunc3D, Textures, KlubData;

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

procedure DrawSimpleModel(ModelID: Integer; x, y, z: Single; TextureID: Integer = 0);
begin
  if ModelID = 0 then Exit;
  BeginObj3D;
  glDisable(GL_LIGHTING);
  Position3D(x, y, z);
  if TextureID <> 0 then
    SetTexture(TextureID);
  DrawModel(ModelID, 0, False);
  glEnable(GL_LIGHTING);
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
    glDisable(GL_LIGHTING);

    Position3D(X, Y[i], Z);
    RotateX(WheelRotation);

    if TelegaTextureID <> 0 then
      SetTexture(TelegaTextureID);

    DrawModel(WheelModelIDs[i], 0, False);

    glEnable(GL_LIGHTING);
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
    glDisable(GL_LIGHTING);

    Position3D(0, 0, 0);
    

    if LocoTextureIDs[i] <> 0 then
      SetTexture(LocoTextureIDs[i]);

    DrawModel(LocoModelIDs[i], 0, False);

    glEnable(GL_LIGHTING);
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
    glDisable(GL_LIGHTING);

    Position3D(0, 0, 0);

    if CabTextureIDs[i] <> 0 then
      SetTexture(CabTextureIDs[i]);

    DrawModel(CabModelIDs[i], 0, False);

    glEnable(GL_LIGHTING);
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
begin
  if ControllerModelID = 0 then Exit;

  NowTick := GetTickCount;

  if (NowTick - InputTimer) > 500 then
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
  glDisable(GL_LIGHTING);

  Position3D(0.199975, 10.4241, 2.59945);
  RotateX(ControllerAngle);

  if ControllerTextureID <> 0 then
    SetTexture(ControllerTextureID);

  DrawModel(ControllerModelID, 0, False);

  glEnable(GL_LIGHTING);
  EndObj3D;
end;



procedure DrawControllerBraking;
begin
  DrawSimpleModel(ControllerBrakingModelID, 0.981241, 10.3818, 2.5848, ControllerTextureID);
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
  glDisable(GL_LIGHTING);
  Position3D(x, y, z);
  RotateX(rx);
  RotateY(ry);
  RotateZ(rz);
  if ControllerTextureID <> 0 then
    SetTexture(ControllerTextureID);
  DrawModel(ModelID, 0, False);
  glEnable(GL_LIGHTING);
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
  InitDynamicRA3;
  WriteRA3CameraInit;
end;

procedure DrawRA3;
begin
  if not IsRA3Active then Exit;
  InitRA3;
  DrawCabRA3;
  DrawLocoRA3;
  DrawTelega;
  DrawWheels;
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
