unit BoosterKLUB;

interface

procedure DrawKLUBDisplay(X, Y, Z, AngleZ: Single);
procedure DrawKLUBSpeedometer(X, Y, Z, AngleZ, AngleDrive: Single);
procedure DrawSpeedometerNeedleRuntime(FontID: Integer; Speed: Single);
procedure DrawKlubBilVDataRuntime(ModelParam2, ModelParam1: Cardinal;
  AngZ, Z, Y, X: Single); stdcall;
procedure DrawSpeedometerRuntime;
procedure HookSkorostemerViaKLUBRuntime(X, Y, Z, AngZ: Single);
procedure DrawSkorostemerRuntime(X, Y, Z, AngZ, AngPrivod: Single); stdcall;
procedure DrawKLUBRuntime(X, Y, Z, AngZ: Single); stdcall;

implementation

uses DrawFunc3D, KlubData, BilServer, EngineUtils, Variables, Advanced3D, OpenGL, Math, Windows, SysUtils;

type
  TKlubVertexArray = array of array[0..2] of GLfloat;

var
  KlubYellowZoneVerts: TKlubVertexArray;
  KlubLastTarget, KlubLastLimit: Single;

procedure DrawKLUBDisplay(X, Y, Z, AngleZ: Single);
begin
  DrawKLUB(X, Y, Z, AngleZ);
end;

procedure DrawKLUBSpeedometer(X, Y, Z, AngleZ, AngleDrive: Single);
begin
  DrawSkorostemer(X, Y, Z, AngleZ, AngleDrive);
end;

procedure DrawSpeedometerNeedleRuntime(FontID: Integer; Speed: Single);
const
  START_ANGLE = 225.0;   // начальный угол шкалы (градусы)
  SPEED_RANGE = 270.0;   // диапазон шкалы (градусы)
  MAX_SPEED   = 250.0;   // максимальная скорость на шкале
  DEG2RAD     = Pi / 180.0;
  RADIUS      = 35.0;    // радиус шкалы в единицах (масштабируется Scale3D)
  TICK_STEP   = 5;       // шаг делений (км/ч)
var
  i, tickCount: Integer;
  angle, ca, sa: Single;
  needleAngle: Single;
  segments: Integer;
begin
  tickCount := Round(MAX_SPEED) div TICK_STEP;

  // --- Деления шкалы ---
  BeginObj3D;
  Position3D(0.10, 0.001, 0.06);
  RotateZ(-90.0);
  Scale3D(0.0009);
  Color3D($FFFFFF, 255, False, 0.0);
  SetTexture(0);

  glLineWidth(2);
  glBegin(GL_LINES);
  for i := 0 to tickCount do
  begin
    angle := (START_ANGLE - (i * TICK_STEP / MAX_SPEED) * SPEED_RANGE) * DEG2RAD;
    ca := Cos(angle);
    sa := Sin(angle);
    glVertex3f(RADIUS * ca, RADIUS * sa, 0);
    glVertex3f((RADIUS + 5) * ca, (RADIUS + 5) * sa, 0);
  end;
  glEnd;
  glLineWidth(1);
  EndObj3D;

  // --- Цифры делений (каждые 10 км/ч) ---
  for i := 0 to (Round(MAX_SPEED) div 10) do
  begin
    angle := (START_ANGLE - (i * 10 / MAX_SPEED) * SPEED_RANGE) * DEG2RAD;
    ca := Cos(angle);
    sa := Sin(angle);

    BeginObj3D;
    Position3D(
      0.10 + (RADIUS - 8) * ca * 0.0009,
      0.001,
      0.06 + (RADIUS - 8) * sa * 0.0009
    );
    RotateZ(-90.0);
    Scale3D(0.006);
    Color3D($FFFFFF, 255, False, 0.0);
    SetTexture(0);
    DrawText3D(FontID, IntToStr(i * 10));
    EndObj3D;
  end;

  // --- Стрелка ---
  needleAngle := (START_ANGLE - (Speed / MAX_SPEED) * SPEED_RANGE) * DEG2RAD;

  BeginObj3D;
  Position3D(0.10, 0.001, 0.06);
  RotateZ(-90.0);
  Scale3D(0.0009);
  Color3D($FF6600, 255, False, 0.0);
  SetTexture(0);

  ca := Cos(needleAngle);
  sa := Sin(needleAngle);

  // Треугольная стрелка
  glBegin(GL_TRIANGLES);
    glVertex3f(3 * Cos(needleAngle + Pi/2), 3 * Sin(needleAngle + Pi/2), 0.5);
    glVertex3f(3 * Cos(needleAngle - Pi/2), 3 * Sin(needleAngle - Pi/2), 0.5);
    glVertex3f((RADIUS - 2) * ca, (RADIUS - 2) * sa, 0.5);
  glEnd;

  // Линия стрелки
  glLineWidth(2);
  glBegin(GL_LINES);
    glVertex3f(0, 0, 0.5);
    glVertex3f((RADIUS - 2) * ca, (RADIUS - 2) * sa, 0.5);
  glEnd;
  glLineWidth(1);

  // Центральный круг
  segments := 24;
  glBegin(GL_TRIANGLE_FAN);
    glVertex3f(0, 0, 0.6);
    for i := 0 to segments do
    begin
      angle := (i * 2 * Pi / segments);
      glVertex3f(8 * Cos(angle), 8 * Sin(angle), 0.6);
    end;
  glEnd;

  EndObj3D;
end;

procedure DrawKlubBilVDataRuntime(
  ModelParam2: Cardinal;
  ModelParam1: Cardinal;
  AngZ: Single;
  Z: Single;
  Y: Single;
  X: Single
); stdcall;
const
  BASE: Cardinal = $00400000;
  // Цвета из ассемблера
  CLR_GREEN      = $0033FF33;   // зелёный (скорость, время, давления)
  CLR_LIMIT      = $003333FF;   // ограничение скорости
  CLR_CYAN       = $0033FFFF;   // целевая скорость
  CLR_ORANGE     = $0000BBFF;   // оранжевый (станция)
  // Цвета АЛСН из ассемблера (circle indicators)
  CLR_ALSN_WHITE  = $00FFFFFF;  // Б — белый
  CLR_ALSN_RED    = $003333FF;  // К — красный
  CLR_ALSN_REDYEL = $0033AAFF;  // КЖ — красно-жёлтый
  CLR_ALSN_YELLOW = $0033DDFF;  // Ж — жёлтый
  CLR_ALSN_GREEN  = $0055FF33;  // З — зелёный
  // Константы спидометра из ассемблера (80-bit float → decoded)
  MAX_SPEED_SCALE = 252.0;      // [0x4873C4] максимум шкалы (градусы)
  HALF_CIRCLE     = 180.0;      // [0x4873D4]
  DIAL_RADIUS_Z   = 0.0450;     // [0x4873D8] радиус шкалы по Z (sin)
  DIAL_OFFSET_Z   = 0.2300;     // [0x4873E4] смещение центра Z
  DIAL_RADIUS_X   = 0.0549;     // [0x4873F0] радиус шкалы по X (cos)
  DIAL_OFFSET_X   = -0.0050;    // [0x4873FC] смещение центра X
  TICK_RADIUS_Z   = 0.0370;     // [0x487484] радиус меток Z (sin)
  TICK_RADIUS_X   = 0.0486;     // [0x487490] радиус меток X (cos)
  TICK_STEP       = 5;          // [0x48749C] шаг делений (5 км/ч)
  ACCEL_DIVISOR   = 1000.0;     // [0x487480] делитель ускорения
var
  speedVal: Single;
  speedInt, limitInt, targetInt: Integer;
  alsMode: Byte;
  revByte: Byte;
  hourVal, minVal, secVal: Integer;
  tmPressure, urPressure, tcPressure: Single;
  accelVal: Double;
  urAddr: Cardinal;
  speedStr, limitStr: string;
  timeStr, pressStr, accelStr: string;
  revStr, stationStr: string;
  alsColor: Integer;
  alsZ: Single;
  OldDecSep: Char;
  BilFont: Integer;
  angleRad: Single;
  i, maxTick: Integer;
begin
  // Кешированная загрузка шрифта
  if KLUBUFont = 0 then
    KLUBUFont := CreateFont3D('KLUBU');
  BilFont := KLUBUFont;

  try
    // ===== ЧТЕНИЕ ДАННЫХ ИЗ ПАМЯТИ =====
    speedVal := Abs(PSingle(BASE + $4F8C28C)^);
    speedInt := Round(speedVal);
    limitInt := PWord(BASE + $34987C)^;
    targetInt := PWord(BASE + $349880)^;
    alsMode := PByte(BASE + $8C07ECC)^;
    revByte := PByte(BASE + $3498A0)^;
    hourVal := PInteger(BASE + $8C08034)^;
    minVal  := PInteger(BASE + $8C08038)^;
    secVal  := PInteger(BASE + $8C0803C)^;
    tmPressure := PSingle(BASE + $8D10738)^;
    urPressure := 0.0;
    try
      urAddr := PCardinal(BASE + $8D10D78)^;
      if urAddr <> 0 then
        urPressure := PSingle(urAddr + $20)^;
    except
      urPressure := 0.0;
    end;
    tcPressure := GetPressureTCf;
    accelVal := PDouble(BASE + $3498B8)^;

    // ===== ФОРМАТИРОВАНИЕ СТРОК =====
    OldDecSep := DecimalSeparator;
    DecimalSeparator := '.';
    try
      speedStr := Format('%.3d', [speedInt]);
      limitStr := Format('%.3d', [limitInt]);
      timeStr := Format('%.2d:%.2d:%.2d', [hourVal, minVal, secVal]);
      pressStr := FormatFloat('0.0', tmPressure);
      accelStr := FormatFloat('0.00', accelVal / ACCEL_DIVISOR);
      if revByte = 0 then
        revStr := #$CF  // "П" — вперёд (CP1251)
      else
        revStr := #$CD; // "Н" — назад (CP1251)
      stationStr := KlubData.GetCurrentStation;
      if Length(stationStr) > 8 then
        stationStr := Copy(stationStr, 1, 8);
    finally
      DecimalSeparator := OldDecSep;
    end;

    // ===== ОТРИСОВКА =====
    // Ассемблер: glPushMatrix → Position3D(X,Y,Z) → RotateZ(AngZ) → элементы → glPopMatrix
    BeginObj3D;
    Position3D(X, Y, Z);
    RotateZ(AngZ);

    glDisable(GL_LIGHTING);

    // --- 1. ТЕКУЩАЯ СКОРОСТЬ ---
    // Asm: Position3D(-0.011, 0.001, 0.237), RotateZ(-90), Scale(0.02), Color(0x33FF33)
    BeginObj3D;
    Position3D(-0.011, 0.001, 0.237);
    RotateZ(-90.0);
    Scale3D(0.02);
    SetTexture(0);
    Color3D(CLR_GREEN, 255, False, 0.0);
    DrawText3D(BilFont, speedStr);
    EndObj3D;

    // --- 2. МАРКЕР РЕВЕРСА (стрелка-точка) ---
    // Asm: Position3D(-0.005, 0.001, brightness*0.003+0.1885), RotateZ(-90), Scale(0.035), text="."
    BeginObj3D;
    Position3D(-0.005, 0.001, 0.1915);
    RotateZ(-90.0);
    Scale3D(0.035);
    SetTexture(0);
    Color3D(CLR_GREEN, 255, False, 0.0);
    DrawText3D(BilFont, '.');
    EndObj3D;

    // --- 3. ОГРАНИЧЕНИЕ СКОРОСТИ ---
    // Asm: Position3D(-0.011, 0.001, 0.213), RotateZ(-90), Scale(0.02), Color(0x3333FF)
    BeginObj3D;
    Position3D(-0.011, 0.001, 0.213);
    RotateZ(-90.0);
    Scale3D(0.02);
    SetTexture(0);
    Color3D(CLR_LIMIT, 255, False, 0.0);
    DrawText3D(BilFont, limitStr);
    EndObj3D;

    // --- 4. АЛСН ИНДИКАТОР (цветной круг) ---
    // Asm: Position3D(-0.0865, 0.002, Z), Scale(0.89), цвет по режиму
    // Режимы: 1=Б(белый), 2=К(красный), 3=КЖ(оранжевый), 4=Ж(жёлтый), 5=З(зелёный)
    if (alsMode >= 1) and (alsMode <= 5) then
    begin
      case alsMode of
        1: begin alsColor := CLR_ALSN_WHITE;  alsZ := 0.194; end;
        2: begin alsColor := CLR_ALSN_RED;    alsZ := 0.209; end;
        3: begin alsColor := CLR_ALSN_REDYEL; alsZ := 0.223; end;
        4: begin alsColor := CLR_ALSN_YELLOW; alsZ := 0.236; end;
        5: begin alsColor := CLR_ALSN_GREEN;  alsZ := 0.236; end;
      else
        begin alsColor := CLR_ALSN_WHITE; alsZ := 0.194; end;
      end;

      BeginObj3D;
      Position3D(-0.0865, 0.002, alsZ);
      Scale3D(0.89);
      SetTexture(0);
      Color3D(alsColor, 254, False, 0.0);
      // Рисуем заполненный круг как замену 3D-модели сигнала
      glBegin(GL_TRIANGLE_FAN);
        glVertex3f(0, 0, 0);
        for i := 0 to 24 do
          glVertex3f(
            0.008 * Cos(i * 2 * Pi / 24),
            0.008 * Sin(i * 2 * Pi / 24),
            0
          );
      glEnd;
      EndObj3D;
    end;

    // --- 5. МАРКЕР ОГРАНИЧЕНИЯ НА ШКАЛЕ ---
    // Asm: Position на круговой шкале, RotateZ(-90), Scale(0.035), Color(0x3333FF), text="."
    if limitInt > 0 then
    begin
      angleRad := (MAX_SPEED_SCALE - limitInt) * Pi / HALF_CIRCLE;
      BeginObj3D;
      Position3D(
        Cos(angleRad) * DIAL_RADIUS_X + DIAL_OFFSET_X,
        0.001,
        Sin(angleRad) * DIAL_RADIUS_Z + DIAL_OFFSET_Z
      );
      RotateZ(-90.0);
      Scale3D(0.035);
      SetTexture(0);
      Color3D(CLR_LIMIT, 255, False, 0.0);
      DrawText3D(BilFont, '.');
      EndObj3D;
    end;

    // --- 6. МАРКЕР ЦЕЛЕВОЙ СКОРОСТИ НА ШКАЛЕ ---
    // Asm: аналогично, Color(0x33FFFF), условие: target > limit и target > 5
    if (targetInt > limitInt) and (targetInt > 5) then
    begin
      angleRad := (MAX_SPEED_SCALE - targetInt) * Pi / HALF_CIRCLE;
      BeginObj3D;
      Position3D(
        Cos(angleRad) * DIAL_RADIUS_X + DIAL_OFFSET_X,
        0.001,
        Sin(angleRad) * DIAL_RADIUS_Z + DIAL_OFFSET_Z
      );
      RotateZ(-90.0);
      Scale3D(0.035);
      SetTexture(0);
      Color3D(CLR_CYAN, 255, False, 0.0);
      DrawText3D(BilFont, '.');
      EndObj3D;
    end;

    // --- 7. НАЗВАНИЕ СТАНЦИИ ---
    // Asm: Position3D(-0.026, 0.001, 0.301), RotateZ(-90), Scale(0.011), Color(0x00BBFF)
    if stationStr <> '' then
    begin
      BeginObj3D;
      Position3D(-0.026, 0.001, 0.301);
      RotateZ(-90.0);
      Scale3D(0.011);
      SetTexture(0);
      Color3D(CLR_ORANGE, 255, False, 0.0);
      DrawText3D(BilFont, stationStr);
      EndObj3D;
    end;

    // --- 8. ИНДИКАТОР НАПРАВЛЕНИЯ (П/Н) ---
    // Asm: Position3D(0.068, -0.001, 0.311), RotateZ(-90), Scale(0.012), Color(0x33FF33)
    BeginObj3D;
    Position3D(0.068, -0.001, 0.311);
    RotateZ(-90.0);
    Scale3D(0.012);
    SetTexture(0);
    Color3D(CLR_GREEN, 255, False, 0.0);
    DrawText3D(BilFont, revStr);
    EndObj3D;

    // --- 9. ВРЕМЯ HH:MM:SS ---
    // Asm: Position3D(0.021, 0.001, 0.301), inner RotateZ(-90), Scale(0.011)
    BeginObj3D;
    Position3D(0.021, 0.001, 0.301);
    RotateZ(-90.0);
    Scale3D(0.011);
    SetTexture(0);
    Color3D(CLR_GREEN, 255, False, 0.0);
    DrawText3D(BilFont, timeStr);
    EndObj3D;

    // --- 10. ДАВЛЕНИЕ ТМ ---
    // Asm: Position3D(-0.092, 0.001, 0.1215), inner RotateZ(-90), Scale(0.011)
    BeginObj3D;
    Position3D(-0.092, 0.001, 0.1215);
    RotateZ(-90.0);
    Scale3D(0.011);
    SetTexture(0);
    Color3D(CLR_GREEN, 255, False, 0.0);
    DrawText3D(BilFont, pressStr);
    EndObj3D;

    // --- 11. УСКОРЕНИЕ ---
    // Asm: Position3D(-0.072, 0.001, 0.301), RotateZ(-90), Scale(0.011)
    BeginObj3D;
    Position3D(-0.072, 0.001, 0.301);
    RotateZ(-90.0);
    Scale3D(0.011);
    SetTexture(0);
    Color3D(CLR_GREEN, 255, False, 0.0);
    DrawText3D(BilFont, accelStr);
    EndObj3D;

    // --- 12. СПИДОМЕТР — ДУГА СКОРОСТИ (метки от 0 до текущей скорости) ---
    // Asm: цикл i=0..round(speed/5)*5, шаг 5
    //   angle = (252 - i) * Pi / 180
    //   Z = sin(angle) * 0.037 + 0.230
    //   X = cos(angle) * 0.0486 + (-0.005)
    //   RotateZ(-90), Scale(0.035), text="."
    maxTick := (Round(speedVal / 5.0)) * TICK_STEP;
    i := 0;
    while i <= maxTick do
    begin
      angleRad := (MAX_SPEED_SCALE - i) * Pi / HALF_CIRCLE;
      BeginObj3D;
      Position3D(
        Cos(angleRad) * TICK_RADIUS_X + DIAL_OFFSET_X,
        0.001,
        Sin(angleRad) * TICK_RADIUS_Z + DIAL_OFFSET_Z
      );
      RotateZ(-90.0);
      Scale3D(0.035);
      SetTexture(0);
      Color3D(CLR_GREEN, 255, False, 0.0);
      DrawText3D(BilFont, '.');
      EndObj3D;
      Inc(i, TICK_STEP);
    end;

    glEnable(GL_LIGHTING);

    // Внешний EndObj3D — закрываем позиционирование панели
    EndObj3D;

  except
    on E: Exception do
    begin
      glEnable(GL_LIGHTING);
    end;
  end;
end;

// Вспомогательная процедура: стрелка + деления скоростемера

procedure DrawSpeedometerRuntime;
var
  i: Integer;
  angle, needleAngle: Single;
  speed, speedLimit, maxSpeed, speedTarget: Single;
  tickStep, tickCount: Integer;
  tc, tm, ur: Single;
  speedText: string;
  segments: Integer;
  x, y: Single;
  blinkState: Boolean;
  innerRadius, outerRadius: Single;
  alsValue: Integer;
  needleBlink: Boolean;
  ca, sa: Single; // cached cos/sin

const
  MAX_SPEED = 300;
  START_ANGLE = 225;
  SPEED_RANGE = 270;
  BASE_RADIUS = 60;
  DEG2RAD = Pi / 180.0;

procedure UpdateYellowZone(speedTarget, speedLimit, maxSpeed: Single);
var
  i, segments: Integer;
  angle, ca, sa: Single;
begin
  if (Abs(KlubLastTarget - speedTarget) < 0.1) and
     (Abs(KlubLastLimit - speedLimit) < 0.1) then Exit;

  KlubLastTarget := speedTarget;
  KlubLastLimit := speedLimit;

  segments := Round(((speedLimit - speedTarget) / maxSpeed) * SPEED_RANGE * 0.5);
  if segments > 20 then segments := 20;
  if segments < 3 then segments := 3;

  SetLength(KlubYellowZoneVerts, (segments+1) * 2);

  for i := 0 to segments do
  begin
    angle := (START_ANGLE - (speedTarget / maxSpeed) * SPEED_RANGE -
              (i * (speedLimit - speedTarget) / maxSpeed * SPEED_RANGE / segments)) * DEG2RAD;
    ca := cos(angle);
    sa := sin(angle);

    KlubYellowZoneVerts[i*2][0] := outerRadius * ca;
    KlubYellowZoneVerts[i*2][1] := outerRadius * sa;
    KlubYellowZoneVerts[i*2][2] := 0.2;

    KlubYellowZoneVerts[i*2+1][0] := innerRadius * ca;
    KlubYellowZoneVerts[i*2+1][1] := innerRadius * sa;
    KlubYellowZoneVerts[i*2+1][2] := 0.2;
  end;
end;

begin
  try
    speed := GetSpeedValue2;
    speedLimit := GetLimitSpeedValue;
    speedTarget := GetTargetSpeedValue;
    if BilBlock160 then
    begin
      maxSpeed := 160;
      tickStep := 10;
      tickCount := 16;
    end
    else
    begin
      maxSpeed := MAX_SPEED;
      tickStep := 20;
      tickCount := 15;
    end;
    tc := StrToFloatDef(GetPressureTC, 0);
    tm := StrToFloatDef(GetPressureTM, 0);
    ur := StrToFloatDef(GetPressureUR, 0);

    if speed > maxSpeed then speed := maxSpeed;
    if speedLimit > maxSpeed then speedLimit := maxSpeed;

    alsValue := GetALS;
    blinkState := (GetTickCount and 512) = 0; // fast blink ~500ms via bit test
    needleBlink := (speed > speedLimit - 3) and (alsValue > 0) and (speed > 0);

    glDisable(GL_LIGHTING);

    innerRadius := BASE_RADIUS - 1;
    outerRadius := BASE_RADIUS + 1;

    // === БЕЛАЯ ДУГА ===
    BeginObj3D;
    Position3D(-0.01, 0, 0.18);
    RotateX(-90);
    Scale3D(0.0009);
    Color3D($FFFFFF, 255, False, 0.0);
    SetTexture(0);

    if alsValue > 0 then
      segments := Round((speedLimit / maxSpeed) * SPEED_RANGE)
    else
      segments := SPEED_RANGE;
    if segments < 1 then segments := 1;

    glBegin(GL_TRIANGLE_STRIP);
    for i := 0 to segments do
    begin
      if alsValue > 0 then
        angle := (START_ANGLE - (i * (speedLimit / maxSpeed) * SPEED_RANGE / segments)) * DEG2RAD
      else
        angle := (START_ANGLE - (i * SPEED_RANGE / segments)) * DEG2RAD;
      ca := cos(angle);
      sa := sin(angle);

      glVertex3f(outerRadius * ca, outerRadius * sa, 0);
      glVertex3f(innerRadius * ca, innerRadius * sa, 0);
    end;
    glEnd;
    EndObj3D;

    // === КРАСНАЯ ЗОНА ===
    if alsValue > 0 then
    begin
      segments := Round(((maxSpeed - speedLimit) / maxSpeed) * SPEED_RANGE);

      if segments > 0 then
      begin
        BeginObj3D;
        Position3D(-0.01, 0, 0.18);
        RotateX(-90);
        Scale3D(0.0009);
        Color3D($0000FF, 255, False, 0.0);
        SetTexture(0);

        glBegin(GL_TRIANGLE_STRIP);
        for i := 0 to segments do
        begin
          angle := (START_ANGLE - (speedLimit / maxSpeed) * SPEED_RANGE - (i * ((maxSpeed - speedLimit) / maxSpeed) * SPEED_RANGE / segments)) * DEG2RAD;
          ca := cos(angle);
          sa := sin(angle);

          glVertex3f(outerRadius * ca, outerRadius * sa, 0.1);
          glVertex3f(innerRadius * ca, innerRadius * sa, 0.1);
        end;
        glEnd;
        EndObj3D;
      end;
    end;

    // === ЖЕЛТАЯ ЗОНА ===
    if (alsValue > 0) and (speedTarget > 0) and (speedTarget < speedLimit) and
       (speedLimit - speedTarget > 3) then
    begin
      UpdateYellowZone(speedTarget, speedLimit, maxSpeed);

      BeginObj3D;
      Position3D(-0.01, 0, 0.18);
      RotateX(-90);
      Scale3D(0.0009);
      Color3D($00FFFF, 255, False, 0.0);
      SetTexture(0);

      glBegin(GL_TRIANGLE_STRIP);
        for i := 0 to High(KlubYellowZoneVerts) do
          glVertex3fv(@KlubYellowZoneVerts[i]);
      glEnd;

      EndObj3D;
    end;

    // === ДЕЛЕНИЯ (все в одном batch) ===
    BeginObj3D;
    Position3D(-0.01, 0, 0.18);
    RotateX(-90);
    Scale3D(0.0009);
    Color3D($FFFFFF, 255, False, 0.0);
    SetTexture(0);

    glLineWidth(2);
    glBegin(GL_LINES);
    for i := 0 to tickCount do
    begin
      angle := (START_ANGLE - (i * tickStep / maxSpeed) * SPEED_RANGE) * DEG2RAD;
      ca := cos(angle);
      sa := sin(angle);
      glVertex3f(BASE_RADIUS * ca, BASE_RADIUS * sa, 0);
      glVertex3f((BASE_RADIUS + 5) * ca, (BASE_RADIUS + 5) * sa, 0);
    end;
    glEnd;
    glLineWidth(1);
    EndObj3D;

    // === ЦИФРЫ делений ===
    for i := 0 to tickCount do
    begin
      angle := (START_ANGLE - (i * tickStep / maxSpeed) * SPEED_RANGE) * DEG2RAD;
      ca := cos(angle);
      sa := sin(angle);

      BeginObj3D;
        Position3D(
          -0.017 + (BASE_RADIUS - 6) * ca * 0.0008,
          0,
          0.18 + (BASE_RADIUS - 6) * sa * 0.0008
        );
      RotateX(-90);
      Scale3D(0.008);
      Color3D($FFFFFF, 255, False, 0.0);
      SetTexture(0);
      DrawText3D(0, IntToStr(i * tickStep));
      EndObj3D;
    end;

    // === СТРЕЛКА + центральный круг (один batch) ===
    needleAngle := (START_ANGLE - (speed / maxSpeed) * SPEED_RANGE) * DEG2RAD;

    BeginObj3D;
    Position3D(-0.01, 0, 0.18);
    RotateX(-90);
    Scale3D(0.0009);

    if needleBlink and blinkState then
      Color3D($FFFFFF, 255, False, 0.0)
    else
      Color3D($FF6600, 255, False, 0.0);

    SetTexture(0);

    ca := cos(needleAngle);
    sa := sin(needleAngle);

    glBegin(GL_TRIANGLES);
      glVertex3f(4 * cos(needleAngle + Pi/2), 4 * sin(needleAngle + Pi/2), 0.5);
      glVertex3f(4 * cos(needleAngle - Pi/2), 4 * sin(needleAngle - Pi/2), 0.5);
      glVertex3f((BASE_RADIUS - 1) * ca, (BASE_RADIUS - 1) * sa, 0.5);
    glEnd;

    glLineWidth(2);
    glBegin(GL_LINES);
      glVertex3f(0, 0, 0.5);
      glVertex3f((BASE_RADIUS - 1) * ca, (BASE_RADIUS - 1) * sa, 0.5);
    glEnd;
    glLineWidth(1);

    // Центральный круг (заливка)
    segments := 30;
    glBegin(GL_TRIANGLE_FAN);
      glVertex3f(0, 0, 0.6);
      for i := 0 to segments do
      begin
        angle := (i * 2 * Pi / segments);
        glVertex3f(12 * cos(angle), 12 * sin(angle), 0.6);
      end;
    glEnd;

    EndObj3D;

    // Обводка центрального круга
    BeginObj3D;
    Position3D(-0.01, 0, 0.18);
    RotateX(-90);
    Scale3D(0.0009);
    Color3D($FFFFFF, 255, False, 0.0);
    SetTexture(0);

    glLineWidth(2);
    glBegin(GL_LINE_LOOP);
      for i := 0 to segments do
      begin
        angle := (i * 2 * Pi / segments);
        glVertex3f(12 * cos(angle), 12 * sin(angle), 0.6);
      end;
    glEnd;
    glLineWidth(1);
    EndObj3D;

    // === ТЕКСТ СКОРОСТИ ===
    speedText := FormatFloat('000', Trunc(speed));

    BeginObj3D;
    Position3D(-0.019, -0.001, 0.177);
    RotateX(-90);
    Scale3D(0.012);

    if needleBlink and blinkState then
      Color3D($FF6600, 255, False, 0.0)
    else
      Color3D($FFFFFF, 255, False, 0.0);

    SetTexture(0);
    DrawText3D(0, speedText);
    EndObj3D;

    // === ТЕКСТ ОГРАНИЧЕНИЯ ===
    if alsValue > 0 then
    begin
      BeginObj3D;
      Position3D(-0.019, 0, 0.157);
      RotateX(-90);
      Scale3D(0.012);
      Color3D($0000FF, 255, False, 0.0);
      SetTexture(0);
      DrawText3D(0, FormatFloat('000', Trunc(speedLimit)));
      EndObj3D;
    end;

    // === ИНДИКАТОРЫ ДАВЛЕНИЯ (один batch для всех) ===
    if (tc > 0) or (tm > 0) or (ur > 0) then
    begin
      BeginObj3D;
      Position3D(-0.01, 0, 0.18);
      RotateX(-90);
      Scale3D(0.0009);
      Color3D($0101F8, 200, False, 0.0);
      SetTexture(0);

      glBegin(GL_QUADS);
      if tc > 0 then
      begin
        glVertex3f(-3, -tc * 12, 0);
        glVertex3f(3, -tc * 12, 0);
        glVertex3f(3, 0, 0);
        glVertex3f(-3, 0, 0);
      end;
      if tm > 0 then
      begin
        glVertex3f(-3, -5 * 12, 0);
        glVertex3f(3, -5 * 12, 0);
        glVertex3f(3, 0, 0);
        glVertex3f(-3, 0, 0);
      end;
      if ur > 0 then
      begin
        glVertex3f(-3, -ur * 12, 0);
        glVertex3f(3, -ur * 12, 0);
        glVertex3f(3, 0, 0);
        glVertex3f(-3, 0, 0);
      end;
      glEnd;

      EndObj3D;
    end;

    glEnable(GL_LIGHTING);

  except
    on E: Exception do
    begin
      glEnable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      AddToLogFile(EngineLog, 'Ошибка отрисовки 3D спидометра: ' + E.Message);
    end;
  end;
end;




// Функция записи байта в память

// Функция проверки и обновления таймера К123

// Обработка кнопки "СТР" - очистка буфера

// Обработка ввода числа для кнопки "П"

// Обработка кнопки "ВВОД" для кнопки "П"


// Инициализация позиций кнопок

// Основная функция отрисовки клавиатуры БЛОК

var
  RMPState: Byte = 0; // 0, 1, 2 - циклические значения (теперь читается из памяти)
  BlinkTimer: Cardinal = 0; // Таймер для мигания
  BlinkVisible: Boolean = True; // Флаг видимости мигающего текста

// Обновление мигания для РМП

procedure HookSkorostemerViaKLUBRuntime(  x: Single;
  y: Single;
  z: Single;
  AngZ: Single);
begin
  DrawSkorostemerRuntime(1,1,1,1,1);
end;

procedure DrawSkorostemerRuntime(x, y, z, AngZ, AngPrivod: Single); stdcall;
begin
  asm
    push $3fab851f    // 1.26
    push $41258937    // 4.928 (было 6.928)
    push $40601387    // 3.497
    push $41C80000    // 25.0
    push $00          // 0

    xor eax, eax
    mov eax, $4877F4  // Адрес оригинальной функции
    call eax
  end;
end;

procedure DrawKLUBRuntime(x, y, z, AngZ: Single); stdcall;
begin
  asm
    // Добавить проверку условия, если нужно
    mov eax, $007498A8  // адрес переменной для проверки
    cmp byte ptr [eax], 0
    je @skip
    
    // Использовать исходные значения
    push $3F95C28F    // 1.17
    push $40E5C28F    // 7.18  
    push $405BA5E3    // 3.43
    push $42340000    // 45.00
    
    // Правильный адрес функции
    mov eax, $00483804  // базовый адрес + смещение
    call eax
    
@skip:
  end;
end;

end.
