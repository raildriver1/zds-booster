unit BoosterPrimitives;

interface

procedure DrawBooster3DDisk(Radius: Single);
procedure DrawBooster3DSemiCircle(Radius, StartAngle, EndAngle: Single);
procedure DrawBooster3DArcProgress(Radius, StartAngleDeg, EndAngleDeg,
  CurrentValue, MaxValue: Single; Segments: Integer);

implementation

uses OpenGL, Math;

procedure DrawBooster3DDisk(Radius: Single);
var Quad: PGLUquadric;
begin
  Quad := gluNewQuadric;
  gluQuadricDrawStyle(Quad, GLU_FILL);
  gluDisk(Quad, 0.0, Radius, 64, 1);
  gluDeleteQuadric(Quad);
end;

procedure DrawBooster3DSemiCircle(Radius, StartAngle, EndAngle: Single);
var I, Segments: Integer; Angle, X, Y: Single;
begin
  Segments := 32;
  glBegin(GL_TRIANGLE_FAN);
  glNormal3f(0, 0, 1); glVertex3f(0, 0, 0);
  for I := 0 to Segments do
  begin
    Angle := (StartAngle + (EndAngle - StartAngle) * I / Segments) * (Pi / 180.0);
    X := Radius * Cos(Angle); Y := Radius * Sin(Angle);
    glNormal3f(0, 0, 1); glVertex3f(X, Y, 0);
  end;
  glEnd;
end;

procedure DrawBooster3DArcProgress(Radius, StartAngleDeg, EndAngleDeg,
  CurrentValue, MaxValue: Single; Segments: Integer);
var I: Integer; Angle, AngleStep, EndAngleCurrent, X, Y: Single;
begin
  if (Segments <= 0) or (MaxValue <= 0) then Exit;
  if CurrentValue > MaxValue then CurrentValue := MaxValue;
  if CurrentValue < 0 then CurrentValue := 0;
  AngleStep := (EndAngleDeg - StartAngleDeg) / Segments;
  EndAngleCurrent := StartAngleDeg + (CurrentValue / MaxValue) * (EndAngleDeg - StartAngleDeg);
  glBegin(GL_TRIANGLE_FAN); glVertex3f(0, 0, 0);
  for I := 0 to Segments do
  begin
    Angle := StartAngleDeg + I * AngleStep;
    if Angle > EndAngleCurrent then Break;
    Angle := Angle * (Pi / 180.0);
    X := Radius * Cos(Angle); Y := Radius * Sin(Angle);
    glVertex3f(X, Y, 0);
  end;
  glEnd;
end;

end.
