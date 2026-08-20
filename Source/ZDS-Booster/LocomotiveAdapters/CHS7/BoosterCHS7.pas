unit BoosterCHS7;

interface

procedure HookSkorostemerCHS7Runtime(X, Y, Z, AngZ: Single);

implementation

uses DrawFunc3D, Advanced3D, OpenGL;

procedure HookSkorostemerCHS7Runtime(
  x: Single;
  y: Single;
  z: Single;
  AngZ: Single
);
begin
  // Сначала всегда вызываем оригинальную функцию
  asm
    push $3F8F9DB2    // 1.26
    push $40E5EB85    // 4.928 (было 6.928)
    push $40623D71    // 3.497
    push $42140000    // 25.0
    push $0C0A00000  // 0
    xor eax, eax
    mov eax, $4877F4  // Адрес оригинальной функции
    call eax
  end;
  
  if SevenSegmentFont = 0 then
  begin
    SevenSegmentFont := CreateFont3D('7-Segment');
  end;

  if PSingle(Pointer(FloatValueAddr))^ > 9 then
  begin
    // Отрисовываем цифру на позиции 34
    BeginObj3D;
    glDisable(GL_LIGHTING);
    Position3D(0.142, 7.48, 3.162);
    RotateX(-57.3);
    RotateY(0.0);
    RotateZ(0.0);
    Scale3D(0.018);
    SetTexture(0);
    Color3D($0000FF, 255, False, 0);
    DrawText3D(SevenSegmentFont, GetFloatDigit(1));
    glEnable(GL_LIGHTING);
    EndObj3D;
  end;

  
  // Отрисовываем цифру на позиции 35
  BeginObj3D;
  glDisable(GL_LIGHTING);
  Position3D(0.1533, 7.48, 3.162);
  RotateX(-57.3);
  RotateY(0.0);
  RotateZ(0.0);
  Scale3D(0.018);
  SetTexture(0);
  Color3D($0000FF, 255, False, 0);
  DrawText3D(SevenSegmentFont, GetFloatDigit(2));
  glEnable(GL_LIGHTING);
  EndObj3D;
end;

end.