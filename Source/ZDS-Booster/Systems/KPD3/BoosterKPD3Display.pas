unit BoosterKPD3Display;

interface

procedure DrawKPD3Digit(X, Y, Z: Single; const Digit: string);
procedure DrawKPD3DigitalDisplay(Speed: Integer);

implementation

uses DrawFunc3D, DrawFunc2D, Advanced3D, OpenGL, SysUtils;

procedure DrawKPD3Digit(X, Y, Z: Single; const Digit: string);
begin
  if SevenSegmentFont = 0 then
    SevenSegmentFont := CreateFont3D('7-Segment');
  BeginObj3D;
  Position3D(X, Y, Z);
  RotateX(-90);
  Scale3D(0.017);
  Color3D(3407667, 255, False, 0.0);
  SetTexture(0);
  DrawText3D(SevenSegmentFont, Digit);
  EndObj3D;
end;

procedure DrawKPD3DigitalDisplay(Speed: Integer);
const
  DISPLAY_Y = -0.03;
  DISPLAY_Z = -0.03;
  DIGIT_POSITIONS: array[0..2] of Single = (-0.016, -0.004, 0.008);
var
  Hundreds, Tens, Units: Integer;
begin
  glDisable(GL_LIGHTING);
  try
    if Speed > 999 then Speed := 999;
    if Speed < 0 then Speed := 0;
    Hundreds := Speed div 100;
    Tens := (Speed mod 100) div 10;
    Units := Speed mod 10;
    if Speed >= 100 then
      DrawKPD3Digit(DIGIT_POSITIONS[0], DISPLAY_Y, DISPLAY_Z, IntToStr(Hundreds));
    if Speed >= 10 then
      DrawKPD3Digit(DIGIT_POSITIONS[1], DISPLAY_Y, DISPLAY_Z, IntToStr(Tens));
    DrawKPD3Digit(DIGIT_POSITIONS[2], DISPLAY_Y, DISPLAY_Z, IntToStr(Units));
  finally
    glEnable(GL_LIGHTING);
  end;
end;

end.
