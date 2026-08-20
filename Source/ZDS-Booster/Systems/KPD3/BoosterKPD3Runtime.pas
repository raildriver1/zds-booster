unit BoosterKPD3Runtime;

interface

var
  KPD3ModelID, ArrowModelID: Integer;
  KPD3TextureID: Integer;
  KPD3Initialized: Boolean = False;

function ApplyKPD3PatchRuntime: Boolean;
function CheckKPD3FilesExistRuntime(LocType: Integer; const LocNumber: string): Boolean;
procedure InitKPD3ModelsRuntime;
procedure InitializeKPD3SystemRuntime;
procedure DrawKPD3Runtime(X, Y, Z, AngZ, AngPrivod: Single);

implementation

uses DrawFunc3D, DrawFunc2D, BoosterKPD3Display, Advanced3D, KlubData, CheatMenu, OpenGL,
  Variables, Textures, EngineUtils, LocomotiveHookRegistry, Windows, SysUtils;

function ApplyKPD3PatchRuntime: Boolean;
var
  CurrentLocType: Integer;
  PatchOffset, PatchAddress, DrawKPD3Address: Cardinal;
  NewOffset: Integer;
  OldProtect: DWORD;
begin
  Result := False;
  try
    CurrentLocType := GetLocomotiveTypeFromMemory;
    PatchOffset := KPD3PatchOffset(CurrentLocType);
    if (PatchOffset = 0) and (CurrentLocType <> 885) then
    begin
      AddToLogFile(EngineLog, 'KPD-3 патч не поддерживается для типа локомотива: ' + IntToStr(CurrentLocType));
      Exit;
    end;
    if not CheckKPD3FilesExist(CurrentLocType, LocNum) then
    begin
      AddToLogFile(EngineLog, 'KPD-3 файлы не найдены, патч не применяется');
      Exit;
    end;
    InitKPD3Models;

    if CurrentLocType = 885 then
    begin
      PatchAddress := $6C2FBB;
      DrawKPD3Address := Cardinal(@DrawKPD3VL85);
      NewOffset := Integer(DrawKPD3Address) - Integer(PatchAddress + 5);
    end
    else
    begin
      if not KPD3Initialized then
      begin
        AddToLogFile(EngineLog, 'Не удалось инициализировать KPD-3, патч не применяется');
        Exit;
      end;
      PatchAddress := $00400000 + PatchOffset;
      DrawKPD3Address := Cardinal(@DrawKPD3);
      NewOffset := Integer(DrawKPD3Address) - Integer(PatchAddress + 5);
    end;

    if VirtualProtect(Pointer(PatchAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      PInteger(PatchAddress + 1)^ := NewOffset;
      VirtualProtect(Pointer(PatchAddress + 1), 4, OldProtect, OldProtect);
      Result := True;
      AddToLogFile(EngineLog, 'KPD-3 патч применен успешно');
    end
    else
      AddToLogFile(EngineLog, 'ОШИБКА: не удалось изменить защиту памяти для KPD-3 патча');
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при применении KPD-3 патча: ' + E.Message);
  end;
end;

function CheckKPD3FilesExistRuntime(LocType: Integer; const LocNumber: string): Boolean;
var
  LocFolder, KPDPath: string;
begin
  Result := False;
  try
    LocFolder := GetLocomotiveFolder(LocType);
    KPDPath := 'data\' + LocFolder + '\' + LocNumber + '\kpd-3\';
    Result := FileExists(KPDPath + 'kpd3.dmd') and
      FileExists(KPDPath + 'kpd3.bmp') and
      FileExists(KPDPath + 'strelka.dmd');
  except
    Result := False;
  end;
end;

procedure InitKPD3ModelsRuntime;
var
  CurrentLocType: Integer;
  KPDPath: string;
begin
  if KPD3Initialized then Exit;
  try
    CurrentLocType := GetLocomotiveTypeFromMemory;
    KPDPath := 'data\' + GetLocomotiveFolder(CurrentLocType) + '\' +
      LocNum + '\kpd-3\';
    KPD3ModelID := LoadModel(KPDPath + 'kpd3.dmd', 0, False);
    KPD3TextureID := LoadTextureFromFile(KPDPath + 'kpd3.bmp', 0, -1);
    ArrowModelID := LoadModel(KPDPath + 'strelka.dmd', 0, False);
    KPD3Initialized := (KPD3ModelID > 0) and (KPD3TextureID > 0) and
      (ArrowModelID > 0);
  except
    KPD3Initialized := False;
  end;
end;

procedure InitializeKPD3SystemRuntime;
begin
  if ApplyKPD3PatchRuntime then
    AddToLogFile(EngineLog, 'KPD-3 система инициализирована успешно')
  else
    AddToLogFile(EngineLog, 'KPD-3 система не была инициализирована');
end;

procedure DrawKPD3Runtime(X, Y, Z, AngZ, AngPrivod: Single);
const
  ARROW_BASE_ANGLE = 119.0;
  SPEED_MULTIPLIER = 1.5;
var
  ArrowTexID: GLuint;
  CurrentTime: string;
  Hours, Minutes: Integer;
  HourDigit1, HourDigit2, MinuteDigit1, MinuteDigit2: string;
begin
  InitKPD3ModelsRuntime;
  ArrowTexID := 0;
  CurrentTime := KlubData.GetCurrentTime;
  Hours := StrToInt(Copy(CurrentTime, 1, 2));
  Minutes := StrToInt(Copy(CurrentTime, 4, 2));
  HourDigit1 := IntToStr(Hours div 10);
  HourDigit2 := IntToStr(Hours mod 10);
  MinuteDigit1 := IntToStr(Minutes div 10);
  MinuteDigit2 := IntToStr(Minutes mod 10);

  BeginObj3D;
  try
    Position3D(AngPrivod, AngZ, Z);
    RotateZ(Y);
    DrawKPD3DigitalDisplay(Round(GetSpeedValue2));
    SetTexture(KPD3TextureID);
    DrawModel(KPD3ModelID, 0, True);

    BeginObj3D; glDisable(GL_LIGHTING);
    Position3D(-0.022, -0.033, -0.063); RotateX(-90); Scale3D(0.017);
    Color3D(3407667, 255, False, 0.0); SetTexture(0);
    DrawText3D(SevenSegmentFont, HourDigit1); glEnable(GL_LIGHTING); EndObj3D;

    BeginObj3D; glDisable(GL_LIGHTING);
    Position3D(-0.011, -0.033, -0.063); RotateX(-90); Scale3D(0.017);
    Color3D(3407667, 255, False, 0.0); SetTexture(0);
    DrawText3D(SevenSegmentFont, HourDigit2 + '.'); glEnable(GL_LIGHTING); EndObj3D;

    BeginObj3D; glDisable(GL_LIGHTING);
    Position3D(0, -0.033, -0.063); RotateX(-90); Scale3D(0.017);
    Color3D(3407667, 255, False, 0.0); SetTexture(0);
    DrawText3D(SevenSegmentFont, MinuteDigit1); glEnable(GL_LIGHTING); EndObj3D;

    BeginObj3D; glDisable(GL_LIGHTING);
    Position3D(0.011, -0.033, -0.063); RotateX(-90); Scale3D(0.017);
    Color3D(3407667, 255, False, 0.0); SetTexture(0);
    DrawText3D(SevenSegmentFont, MinuteDigit2); glEnable(GL_LIGHTING); EndObj3D;

    BeginObj3D;
    try
      RotateY(ARROW_BASE_ANGLE - GetSpeedValue2 * SPEED_MULTIPLIER);
      SetTexture(ArrowTexID);
      DrawModel(ArrowModelID, 0, True);
    finally
      EndObj3D;
    end;
  finally
    EndObj3D;
  end;

  if GetLocomotiveTypeFromMemory = 822 then
  begin
    if SevenSegmentFont = 0 then SevenSegmentFont := CreateFont3D('7-Segment');
    if PSingle(Pointer(FloatValueAddr))^ > 9 then
    begin
      BeginObj3D; glDisable(GL_LIGHTING);
      Position3D(0.142, 7.48, 3.162); RotateX(-57.3); RotateY(0); RotateZ(0);
      Scale3D(0.018); SetTexture(0); Color3D($0000FF, 255, False, 0);
      DrawText3D(SevenSegmentFont, GetFloatDigit(1)); glEnable(GL_LIGHTING); EndObj3D;
    end;
    BeginObj3D; glDisable(GL_LIGHTING);
    Position3D(0.1533, 7.48, 3.162); RotateX(-57.3); RotateY(0); RotateZ(0);
    Scale3D(0.018); SetTexture(0); Color3D($0000FF, 255, False, 0);
    DrawText3D(SevenSegmentFont, GetFloatDigit(2)); glEnable(GL_LIGHTING); EndObj3D;
  end;
  RenderCustomTextsAndGizmoForFrame;
end;

end.
