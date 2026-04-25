unit RA3Physics;

// Физическая модель РА-3: дизель + гидропередача (ГДТ/ГДМ/ГТ) + реверсор.
// Замещает штатную физику ЭД4М в ZDSimulator путём записи в LOCSECTIONS[36]
// каждые 100мс (10Гц тик).
//
// Основано на исходниках РА-3 (ra3-equipment/src/disel.cpp,
// hydro-transmission.cpp) с полиномиальной аппроксимацией кривых ГДТ/ГДМ.

interface

procedure InitRA3Physics;
procedure StepRA3Physics;        // вызывать каждый кадр из DrawRA3
procedure ShutdownRA3Physics;

procedure SetPhysicsEnabled(Enable: Boolean);
function  IsPhysicsEnabled: Boolean;

procedure SetWriteEnabled(Enable: Boolean);
function  IsWriteEnabled: Boolean;

procedure AddToLog(const msg: string);
procedure SetLoggingEnabled(Enable: Boolean);
function  IsLoggingEnabled: Boolean;

// Диагностика (для вывода в консоль/меню)
function  GetDieselRPM: Double;
function  GetM_out: Double;
function  GetRailForce: Double;
function  GetFillGT: Double;
function  GetFillGM: Double;
function  GetFillGB: Double;
function  GetLocSection36: Single;

// Главный тумблер: одной кнопкой (Alt+Z) включает физику + запись в loc36
// + дефолтные настройки (TractionScale=1.0, SpeedLimit=100). Повторное
// нажатие выключает всё.
procedure ToggleRA3Booster;
function  IsRA3BoosterActive: Boolean;

// Внешний (из UI) float-контроллер. Диапазон [-5..+5]:
//   0   — нейтраль
//   +1..+5 — позиции тяги (плавно интерполируется n_ref)
//   -1..-5 — тормоз
// Если задан (через SetVirtualController) — используется вместо чтения
// байтового контроллера из памяти игры.
procedure SetVirtualController(v: Single);
procedure ClearVirtualController;

// Хук физического интегратора скорости в Launcher.exe+345F00.
// Перехватывает строки v += a/dt и домножает ускорение на SpeedAccelScale.
// SpeedAccelScale=1.0 → штатное поведение, 2.0 → удвоенный разгон.
procedure InstallSpeedHook;
procedure RemoveSpeedHook;
function  IsSpeedHookActive: Boolean;

implementation

uses
  Windows, SysUtils, Math;

// ============================================================================
// Адреса (относительно Launcher.exe base = $400000)
// ============================================================================
const
  ADDR_KONTROLLER      : Cardinal = $400000 + $08DD5B05;  // byte
  ADDR_LOCS_BASE_PTR   : Cardinal = $400000 + $0034AEA0;  // ptr → struct; LOCSECTIONS[36] at +$B4
  ADDR_REVERS_BASE_PTR : Cardinal = $400000 + $0034B28C;  // ptr → struct; Revers at +$1C
  ADDR_SPEED_BASE_PTR  : Cardinal = $400000 + $0034B41C;  // ptr → struct; SpeedKPH at +$3C

  OFF_LOCS_36  = $B4;
  OFF_REVERS   = $1C;
  OFF_SPEEDKPH = $3C;

// ============================================================================
// Константы физики (из ra3-equipment/src/)
// ============================================================================
const
  TICK_DT          : Double = 0.1;     // 10Гц

  // Дизель (disel.cpp)
  N_IDLE           : Double = 800.0;
  Q_MAX            : Double = 0.166;
  K0_DRAIN         : Double = 15.0;
  K1_PUMP          : Double = 0.0774;
  K3_TORQUE        : Double = 1.7e5;
  K4_P_GOVERNOR    : Double = 0.01;
  K5_I_GOVERNOR    : Double = 0.15;
  J_SHAFT          : Double = 1.0;
  MC_LOAD          : Double = 50.0;
  IP_REDUCTOR      : Double = 3.3;
  OMEGA_MIN        : Double = 19.9;
  OMEGA_MAX_CLAMP  : Double = 200.0;

  // Гидропередача (hydro-transmission.cpp)
  K_HT             : Double = 0.062;
  T_GT             : Double = 0.5;
  T_GM             : Double = 0.5;
  T_GB             : Double = 0.5;
  T_REVERS         : Double = 1.0;
  I_MIN_SWITCH     : Double = 0.8;
  I_MAX_SWITCH     : Double = 0.9;
  P_GB_KW          : Double = 300.0;
  OMEGA_GB_MAX     : Double = 162.0;

  // Механика тележки
  I_GEAR           : Double = 4.5;
  WHEEL_RADIUS     : Double = 0.475;

  // Нормализация для LOCSECTIONS[36].
  // loc36 — накопитель тяги, который игра использует в диапазоне 0..~10
  // (штатно при k=5 растёт до 7.8 при F~37 кН — масштаб игры ≈ F/4700).
  // Если F_MAX_RAIL слишком высокий, наш loc36 на крейсере становится
  // меньше чем нужно для удержания скорости — состав плавно тормозится.
  // Поэтому подобрали F_MAX_RAIL так, чтобы при F=45 кН (типовой пик
  // нашей физики) loc36 достигал ~10.
  // ВАЖНО: игра штатно использует loc36 в диапазоне 0..~8. Значения выше 10
  // приводят к АНОМАЛЬНОМУ замедлению (наблюдалось: v падала с 2.28 до 1.70
  // при loc36=25). Поэтому жёстко лимитируем 10.
  F_MAX_RAIL       : Double = 25000.0;
  LOC36_FULL_SCALE : Double = 10.0;
  LOC36_LIMIT      : Double = 10.0;     // НЕ повышать — игра ломается выше

// ============================================================================
// Состояние
// ============================================================================
var
  PhysicsEnabled: Boolean = False;
  LastTickMs: Cardinal = 0;

  // Дизель
  omega_eng: Double = 0;          // рад/с
  oil_pressure: Double = 0;
  integ_governor: Double = 0;
  is_fuel_ignition: Boolean = True;  // считаем что дизель уже заведён
  n_ref: Double = 800.0;
  M_d: Double = 0;

  // Гидропередача
  y_gt: Double = 0;
  y_gm: Double = 0;
  y_gb: Double = 0;
  i_gp: Double = 0;
  gt_mode: Boolean = False;        // false = ГДТ активен, true = ГДМ активен
  revers_soft: Double = 1.0;       // апериодическое сглаживание реверса
  revers_pos_ref: Integer = 1;

  M_in_cur: Double = 0;
  M_out_cur: Double = 0;
  M_gb_cur: Double = 0;

  // Управление (обновляется из kontroller_0)
  u_traction: Double = 0;
  u_brake: Double = 0;

  // Выход
  F_rail_cur: Double = 0;
  LocSection36_cur: Single = 0;

  // Главный рубильник: писать ли в LOCSECTIONS[36] вообще.
  // Выключен по умолчанию чтобы не ломать штатную физику.
  // Включается через SetWriteEnabled(True) (хоткей).
  WriteEnabled: Boolean = False;

  // Калибровочные параметры тяги
  TractionScale: Single = 1.0;     // общий множитель тяги
  SpeedLimitKPH: Single = 100.0;   // отсечка тяги при превышении (0 = выкл)

  // Внешний float-контроллер из UI (-5..+5)
  VirtualCtrlActive: Boolean = False;
  VirtualCtrlValue:  Single  = 0.0;

  // Хук интегратора скорости. ВРЕМЕННО 5.0 для проверки что хук вообще
  // воздействует на скорость (если разгон не изменится — это не основной
  // интегратор и нужно искать другой).
  SpeedAccelScale:   Double  = 5.0;
  SpeedHookInstalled: Boolean = False;
  SpeedHook_Saved:   array[0..27] of Byte;

const
  ADDR_SPEED_INTEG     : Cardinal = $400000 + $00345F00;  // начало секции v+=a/dt
  ADDR_SPEED_INTEG_END : Cardinal = $400000 + $00345F1B;  // следующая инструкция (после fstp + wait)
  SPEED_INTEG_LEN      = 28;                              // байт от F00 до F1B

var
  // Логирование
  LoggingEnabled: Boolean = True;  // по умолчанию включено
  LogPath: string = 'C:\Users\8888\Desktop\zds-booster 2026\zds-booster\physics.log';
  LogStartTick: Cardinal = 0;
  LogCleared: Boolean = False;
  LastLogTickMs: Cardinal = 0;
  LogIntervalMs: Cardinal = 100;   // писать в лог не чаще раза в 100мс

// ============================================================================
// Логирование
// ============================================================================
function B2S(b: Boolean): string;
begin
  if b then Result := 'T' else Result := 'F';
end;

procedure AddToLog(const msg: string);
var
  f: TextFile;
  t_s: Double;
begin
  if not LoggingEnabled then Exit;
  try
    AssignFile(f, LogPath);
    if LogCleared then
      Append(f)
    else
    begin
      Rewrite(f);
      LogCleared := True;
      LogStartTick := GetTickCount;
      WriteLn(f, '# RA-3 physics log');
    end;
    t_s := (GetTickCount - LogStartTick) / 1000.0;
    WriteLn(f, Format('%7.2f  %s', [t_s, msg]));
    CloseFile(f);
  except
  end;
end;

procedure SetLoggingEnabled(Enable: Boolean); begin LoggingEnabled := Enable; end;
function  IsLoggingEnabled: Boolean;          begin Result := LoggingEnabled;  end;

// ============================================================================
// Утилиты
// ============================================================================
function Clamp(v, lo, hi: Double): Double;
begin
  if v < lo then Result := lo
  else if v > hi then Result := hi
  else Result := v;
end;

// Проверка что Single — обычное конечное число (не NaN, не Inf, не безумно большое).
// Используем диапазонный тест: сравнение с NaN всегда false, так что NaN
// отсеивается автоматически.
function SafeFloat(v: Single): Single;
begin
  if (v > -1.0e10) and (v < 1.0e10) then
    Result := v
  else
    Result := 0;
end;

function SafePtr(PtrAddr: Cardinal; Offset: Cardinal): Pointer;
var
  base: Cardinal;
begin
  Result := nil;
  try
    base := PCardinal(PtrAddr)^;
    if base = 0 then Exit;
    if (base and $FFFF0000) = 0 then Exit;  // явный мусор
    Result := Pointer(base + Offset);
  except
    Result := nil;
  end;
end;

function ReadKontroller: Byte;
begin
  try
    Result := PByte(ADDR_KONTROLLER)^;
  except
    Result := 0;
  end;
end;

// Возвращает «плавную» позицию контроллера в float-форме.
// Если активен виртуальный контроллер из UI — используем его (-5..+5).
// Иначе читаем байт из памяти игры:
//   0..5      → 0..5 (тяга по позициям)
//   251..255  → -5..-1 (тормозные позиции в byte-кодировке)
function ReadKontrollerFloat: Single;
var
  b: Byte;
begin
  if VirtualCtrlActive then
  begin
    Result := VirtualCtrlValue;
    if Result < -5.0 then Result := -5.0;
    if Result >  5.0 then Result :=  5.0;
    Exit;
  end;
  b := ReadKontroller;
  case b of
    0..5:        Result := b;
    251..255:    Result := Integer(b) - 256;  // -5..-1
  else
    Result := 0;
  end;
end;

function ReadSpeedKPH: Single;
var
  p: PSingle;
  v: Single;
begin
  Result := 0;
  p := SafePtr(ADDR_SPEED_BASE_PTR, OFF_SPEEDKPH);
  if p <> nil then
  try
    v := p^;
    // Клампим невменяемые значения — если адрес неверный
    if (v >= -500.0) and (v <= 500.0) then
      Result := v
    else
      Result := 0;
  except
  end;
end;

// Диагностика: прочитать 4 байта как float на заданном смещении от base_ptr
function ReadProbeFloat(ptr_addr, offset: Cardinal): Single;
var
  p: PSingle;
begin
  Result := 0;
  p := SafePtr(ptr_addr, offset);
  if p <> nil then
  try
    Result := p^;
  except
  end;
end;

function ReadReverser: Integer;
var
  p: PShortInt;
  b: PByte;
begin
  // Ревесор может быть byte/shortint или integer — читаем байт-знак
  Result := 1;
  b := SafePtr(ADDR_REVERS_BASE_PTR, OFF_REVERS);
  if b = nil then Exit;
  try
    p := PShortInt(b);
    if p^ < 0 then Result := -1
    else if p^ > 0 then Result := 1
    else Result := 0;
  except
    Result := 1;
  end;
end;

procedure WriteLocSection36(value: Single);
var
  p: PSingle;
begin
  p := SafePtr(ADDR_LOCS_BASE_PTR, OFF_LOCS_36);
  if p = nil then Exit;
  try
    p^ := value;
  except
  end;
end;

// ============================================================================
// Аппроксимация гидротрансформатора и гидромуфты
// i = omega_out / omega_in (передаточное отношение)
// ============================================================================
function GTCoeff(i: Double): Double;
begin
  // Трансформация момента: на i=0 коэф ~2.5, при i→1 падает к 0.
  // Аппроксимация: K_gt(i) ≈ 2.5 * (1-i)^1.5, с плавным сходом
  i := Clamp(i, 0.0, 1.2);
  if i >= 1.0 then
    Result := 0.0
  else
    Result := 2.5 * Power(1.0 - i, 1.5);
end;

function GMCoeff(i: Double): Double;
begin
  // Гидромуфта: близка к единице при i→1, падает при i<0.85.
  i := Clamp(i, 0.0, 1.2);
  if i < 0.5 then
    Result := 0.0
  else if i >= 1.0 then
    Result := 0.95
  else
    Result := 0.95 * (i - 0.5) / 0.5;
end;

function BrakeTorqueLimit(omega_out: Double): Double;
begin
  omega_out := Abs(omega_out);
  if omega_out >= OMEGA_GB_MAX then
    Result := P_GB_KW * 1000.0 / omega_out
  else
    Result := P_GB_KW * 1000.0 / OMEGA_GB_MAX;
end;

// ============================================================================
// Шаг интегрирования дизеля
// ============================================================================
procedure StepDiesel(dt: Double);
var
  kontr_f: Single;
  delta_omega, u, Q, Mr, dOmega: Double;
begin
  // Float-контроллер от 0 до 5 (отрицательные = тормоз → дизель на хх)
  kontr_f := ReadKontrollerFloat;
  if kontr_f < 0 then kontr_f := 0;
  if kontr_f > 5 then kontr_f := 5;

  // Линейная интерполяция оборотов: 800 (k=0) → 1800 (k=5)
  n_ref := 800.0 + 200.0 * kontr_f;

  delta_omega := n_ref * Pi / 30.0 - omega_eng;

  // PI-регулятор
  integ_governor := integ_governor + delta_omega * dt;
  integ_governor := Clamp(integ_governor, 0.0, 1.0);

  u := K4_P_GOVERNOR * delta_omega + K5_I_GOVERNOR * integ_governor;
  u := Clamp(u, 0.0, 1.0);

  Q := u * Q_MAX;
  M_d := K3_TORQUE * Q;
  if not is_fuel_ignition then M_d := 0;

  // Сопротивление (из disel.cpp)
  Mr := Clamp(MC_LOAD * omega_eng / 20.0, -MC_LOAD, MC_LOAD);

  // dOmega/dt = (M_d - M_in_hydro - Mr) / J_shaft
  dOmega := (M_d - IP_REDUCTOR * M_in_cur - Mr) / J_SHAFT;
  omega_eng := omega_eng + dOmega * dt;
  omega_eng := Clamp(omega_eng, 0.0, OMEGA_MAX_CLAMP);

  // Давление масла (формально, пока не используется)
  dOmega := (K1_PUMP * omega_eng - K0_DRAIN * oil_pressure) / 1.5;
  oil_pressure := oil_pressure + dOmega * dt;
  oil_pressure := Clamp(oil_pressure, 0.0, 10.0);
end;

// ============================================================================
// Шаг интегрирования гидропередачи
// ============================================================================
procedure StepHydroTrans(dt: Double);
var
  kontr_f: Single;
  speed_kph: Single;
  omega_out, omega_in: Double;
  M_gt_tmp, M_gm_tmp: Double;
  u_gt_ref, u_gm_ref, u_gb_ref: Double;
  brake_lim: Double;
  rev_hw: Integer;
begin
  kontr_f := ReadKontrollerFloat;
  speed_kph := ReadSpeedKPH;
  rev_hw := ReadReverser;
  if rev_hw <> 0 then revers_pos_ref := rev_hw;

  // Сглаживание реверсирования
  revers_soft := revers_soft + (1.05 * revers_pos_ref - revers_soft) / T_REVERS * dt;

  // Float-управление: тяга = доля положительного контроллера, тормоз = модуль
  // отрицательного. Контроллер на 1.0 уже даёт полную тягу (плавное включение
  // через апериодическое звено y_gt/y_gm).
  if kontr_f > 0.05 then
    u_traction := Clamp(kontr_f / 1.0, 0.0, 1.0)
  else
    u_traction := 0;
  if kontr_f < -0.05 then
    u_brake := Clamp(-kontr_f / 5.0, 0.0, 1.0)
  else
    u_brake := 0;

  omega_in := omega_eng;
  omega_out := Abs(speed_kph) / 3.6 / WHEEL_RADIUS * I_GEAR;

  if Abs(omega_in) >= 0.1 then
    i_gp := omega_out / omega_in
  else
    i_gp := 0;

  // Гистерезис ГДТ ↔ ГДМ
  if (not gt_mode) and (i_gp >= I_MAX_SWITCH) then
    gt_mode := True
  else if gt_mode and (i_gp <= I_MIN_SWITCH) then
    gt_mode := False;

  // Задание уровней заполнения
  if gt_mode then
  begin
    u_gt_ref := 0;
    u_gm_ref := u_traction;
  end
  else
  begin
    u_gt_ref := u_traction;
    u_gm_ref := 0;
  end;
  u_gb_ref := u_brake;

  // Апериодические звенья
  y_gt := y_gt + (u_gt_ref - y_gt) / T_GT * dt;
  y_gm := y_gm + (u_gm_ref - y_gm) / T_GM * dt;
  y_gb := y_gb + (u_gb_ref - y_gb) / T_GB * dt;

  y_gt := Clamp(y_gt, 0.0, 1.0);
  y_gm := Clamp(y_gm, 0.0, 1.0);
  y_gb := Clamp(y_gb, 0.0, 1.0);

  // Момент на входе (отбирается от дизеля)
  M_in_cur := Clamp(y_gt + y_gm, 0.0, 1.0) * K_HT * Sqr(omega_in);

  // Момент на выходе
  M_gt_tmp := y_gt * GTCoeff(i_gp) * M_in_cur;
  M_gm_tmp := y_gm * GMCoeff(i_gp) * M_in_cur;

  // Гидротормоз
  M_gb_cur := y_gb * 0.05 * Sqr(omega_out);
  brake_lim := BrakeTorqueLimit(omega_out);
  M_gb_cur := Clamp(M_gb_cur, 0.0, brake_lim);

  M_out_cur := M_gt_tmp + M_gm_tmp - M_gb_cur;
end;

// ============================================================================
// Главный шаг
// ============================================================================
procedure DoPhysicsTick(dt: Double);
const
  SUB_STEPS = 20;   // 20 субшагов × 0.005с = стабильный Эйлер
var
  sign_f, sub_dt: Double;
  kontr_raw: Byte;
  speed_raw: Single;
  revers_raw, i: Integer;
  loc36_before, loc36_after: Single;
  loc_ptr_val, rev_ptr_val, spd_ptr_val: Cardinal;
  p: PSingle;
begin
  // Читаем сырые значения ДО физики (чтобы видеть что видит модуль)
  kontr_raw := ReadKontroller;
  speed_raw := ReadSpeedKPH;
  revers_raw := ReadReverser;

  // Читаем что сейчас лежит в LOCSECTIONS[36] (до нашей записи)
  loc36_before := 0;
  p := SafePtr(ADDR_LOCS_BASE_PTR, OFF_LOCS_36);
  if p <> nil then loc36_before := p^;

  // Сырые значения указателей для диагностики
  try loc_ptr_val := PCardinal(ADDR_LOCS_BASE_PTR)^   except loc_ptr_val := 0; end;
  try rev_ptr_val := PCardinal(ADDR_REVERS_BASE_PTR)^ except rev_ptr_val := 0; end;
  try spd_ptr_val := PCardinal(ADDR_SPEED_BASE_PTR)^  except spd_ptr_val := 0; end;

  sub_dt := dt / SUB_STEPS;
  for i := 1 to SUB_STEPS do
  begin
    StepDiesel(sub_dt);
    StepHydroTrans(sub_dt);
  end;

  F_rail_cur := M_out_cur * I_GEAR / WHEEL_RADIUS;

  if revers_pos_ref < 0 then
    sign_f := -1.0
  else
    sign_f := 1.0;
  F_rail_cur := F_rail_cur * sign_f;

  // Прямая нормализация без внутреннего clamp [-1..1] — позволяет высокой F
  // пробивать выше LOC36_FULL_SCALE (до LOC36_LIMIT). Без этого мы упирались
  // в 10·scale при F >= F_MAX, и состав вставал на 40 км/ч.
  if u_brake > 0.5 then
    LocSection36_cur := -Abs(F_rail_cur) / F_MAX_RAIL * LOC36_FULL_SCALE
  else
    LocSection36_cur := F_rail_cur / F_MAX_RAIL * LOC36_FULL_SCALE;

  // Калибровка: общий множитель тяги
  LocSection36_cur := LocSection36_cur * TractionScale;

  // Отсечка по скорости — если выше лимита и тяга положительная, режем в 0.
  // (тормоз — отрицательное loc36 — не трогаем).
  if (SpeedLimitKPH > 0.1) and (Abs(speed_raw) >= SpeedLimitKPH) and
     (LocSection36_cur > 0) then
    LocSection36_cur := 0;

  LocSection36_cur := Clamp(LocSection36_cur, -LOC36_LIMIT, LOC36_LIMIT);

  // Пишем в LOCSECTIONS[36] ТОЛЬКО если явно включено.
  // Иначе — только считаем физику, но в игру не вмешиваемся.
  if WriteEnabled then
    WriteLocSection36(LocSection36_cur);

  // Читаем сразу после записи — чтобы увидеть, удержалось ли наше значение
  loc36_after := 0;
  p := SafePtr(ADDR_LOCS_BASE_PTR, OFF_LOCS_36);
  if p <> nil then loc36_after := p^;

  // Лог — не каждый кадр, а раз в LogIntervalMs (иначе файл огромный).
  if (GetTickCount - LastLogTickMs) >= LogIntervalMs then
  begin
    LastLogTickMs := GetTickCount;
    try
      AddToLog(Format(
        'k=%3d v=%7.2f rev=%3d | rpm=%6.1f Mo=%9.1f F=%9.1f | ' +
        'loc36 before=%7.4f want=%7.4f after=%7.4f | ' +
        'y_gt=%.3f y_gm=%.3f y_gb=%.3f gt_mode=%s write=%s | ' +
        'ptrs: loc=%x rev=%x spd=%x',
        [kontr_raw, SafeFloat(speed_raw), revers_raw,
         SafeFloat(GetDieselRPM), SafeFloat(M_out_cur), SafeFloat(F_rail_cur),
         SafeFloat(loc36_before), SafeFloat(LocSection36_cur), SafeFloat(loc36_after),
         SafeFloat(y_gt), SafeFloat(y_gm), SafeFloat(y_gb),
         B2S(gt_mode), B2S(WriteEnabled),
         loc_ptr_val, rev_ptr_val, spd_ptr_val]));
    except
      AddToLog('<<Format failed in main log line>>');
    end;
  end;
end;

// ============================================================================
// Public API
// ============================================================================
procedure InitRA3Physics;
begin
  omega_eng := N_IDLE * Pi / 30.0;
  oil_pressure := 0;
  integ_governor := 0;
  y_gt := 0;
  y_gm := 0;
  y_gb := 0;
  revers_soft := 1.0;
  revers_pos_ref := 1;
  gt_mode := False;
  M_in_cur := 0;
  M_out_cur := 0;
  M_gb_cur := 0;
  F_rail_cur := 0;
  LocSection36_cur := 0;
  LastTickMs := GetTickCount;
  AddToLog(Format(
    '=== INIT === addrs: kontr=%x loc_ptr=%x rev_ptr=%x spd_ptr=%x',
    [ADDR_KONTROLLER, ADDR_LOCS_BASE_PTR, ADDR_REVERS_BASE_PTR, ADDR_SPEED_BASE_PTR]));
end;

procedure StepRA3Physics;
var
  now: Cardinal;
  elapsed: Cardinal;
  dt_actual: Double;
begin
  if not PhysicsEnabled then Exit;

  now := GetTickCount;
  elapsed := now - LastTickMs;
  if elapsed < 1 then Exit;  // каждый кадр (игра перезаписывает, так что пишем часто)
  LastTickMs := now;

  // dt = реальное время между вызовами, ограничим сверху чтобы не было взрывов
  dt_actual := elapsed / 1000.0;
  if dt_actual > 0.1 then dt_actual := 0.1;

  DoPhysicsTick(dt_actual);
end;

procedure ShutdownRA3Physics;
begin
  PhysicsEnabled := False;
end;

procedure SetPhysicsEnabled(Enable: Boolean);
begin
  if Enable and not PhysicsEnabled then
    InitRA3Physics;
  PhysicsEnabled := Enable;
  if Enable then
    AddToLog('=== PHYSICS ON ===')
  else
    AddToLog('=== PHYSICS OFF ===');
end;

function IsPhysicsEnabled: Boolean;
begin
  Result := PhysicsEnabled;
end;

procedure SetWriteEnabled(Enable: Boolean);
begin
  WriteEnabled := Enable;
  if Enable then
    AddToLog('=== WRITE TO LOC36 ENABLED ===')
  else
    AddToLog('=== WRITE TO LOC36 DISABLED ===');
end;

function IsWriteEnabled: Boolean;
begin
  Result := WriteEnabled;
end;

function GetDieselRPM: Double;
begin
  Result := omega_eng * 30.0 / Pi;
end;

function GetM_out: Double;     begin Result := M_out_cur;       end;
function GetRailForce: Double; begin Result := F_rail_cur;      end;
function GetFillGT: Double;    begin Result := y_gt;            end;
function GetFillGM: Double;    begin Result := y_gm;            end;
function GetFillGB: Double;    begin Result := y_gb;            end;
function GetLocSection36: Single; begin Result := LocSection36_cur; end;

procedure ToggleRA3Booster;
begin
  if IsPhysicsEnabled and IsWriteEnabled then
  begin
    // Сейчас всё включено — выключаем
    if IsSpeedHookActive then RemoveSpeedHook;
    SetWriteEnabled(False);
    SetPhysicsEnabled(False);
    AddToLog('=== RA3 BOOSTER OFF ===');
  end
  else
  begin
    // Выключено — включаем с дефолтами.
    TractionScale   := 1.0;
    SpeedLimitKPH   := 100.0;
    SpeedAccelScale := 5.0;   // ВРЕМЕННО — для проверки эффективности хука
    SetPhysicsEnabled(True);
    SetWriteEnabled(True);
    InstallSpeedHook;
    AddToLog(Format('=== RA3 BOOSTER ON === scale=%.2f limit=%.0f accel_scale=%.2f',
      [TractionScale, SpeedLimitKPH, SpeedAccelScale]));
  end;
end;

function IsRA3BoosterActive: Boolean;
begin
  Result := IsPhysicsEnabled and IsWriteEnabled;
end;

procedure SetVirtualController(v: Single);
begin
  VirtualCtrlActive := True;
  VirtualCtrlValue  := v;
end;

procedure ClearVirtualController;
begin
  VirtualCtrlActive := False;
  VirtualCtrlValue  := 0;
end;

// ============================================================================
// Хук физического интегратора скорости.
// Оригинал в Launcher.exe+345F00 (28 байт):
//   mov eax,[Launcher.exe+34B418]   ; ptr → acceleration qword
//   fld qword[eax]                   ; ST(0) = a
//   mov eax,[Launcher.exe+34AD98]   ; ptr → dt qword
//   fdiv qword[eax]                  ; ST(0) = a/dt
//   mov eax,[Launcher.exe+34B340]   ; ptr → speed qword (00749940)
//   fadd qword[eax]                  ; ST(0) = speed + a/dt
//   mov eax,[Launcher.exe+34B340]
//   fstp qword[eax]                  ; speed = new
//
// Заменяем на JMP к нашему обработчику, который делает то же самое, но
// домножает результат a/dt на SpeedAccelScale.
// ============================================================================

procedure SpeedIntegHandler; assembler;
asm
  // ST(0) = a
  mov   eax, dword ptr [$0074B418]
  fld   qword ptr [eax]

  // ST(0) = a / dt
  mov   eax, dword ptr [$0074AD98]
  fdiv  qword ptr [eax]

  // ST(0) = (a/dt) * SpeedAccelScale
  fmul  qword ptr [SpeedAccelScale]

  // ST(0) = speed + (a/dt)*scale
  mov   eax, dword ptr [$0074B340]
  fadd  qword ptr [eax]

  // store new speed
  fstp  qword ptr [eax]

  // Возврат на инструкцию ПОСЛЕ оригинального fstp
  push  $00745F1B
  ret
end;

procedure InstallSpeedHook;
var
  target: Pointer;
  rel32: Integer;
  i: Integer;
  oldProtect, dummy: DWORD;
begin
  if SpeedHookInstalled then Exit;
  target := Pointer(ADDR_SPEED_INTEG);
  if not VirtualProtect(target, SPEED_INTEG_LEN, PAGE_EXECUTE_READWRITE, oldProtect) then
    Exit;

  // Сохраняем оригинал
  for i := 0 to SPEED_INTEG_LEN - 1 do
    SpeedHook_Saved[i] := PByte(Cardinal(target) + Cardinal(i))^;

  // E9 xx xx xx xx = JMP rel32 к нашему хендлеру
  rel32 := Integer(@SpeedIntegHandler) - Integer(target) - 5;
  PByte(Cardinal(target))^ := $E9;
  PCardinal(Cardinal(target) + 1)^ := Cardinal(rel32);

  // Остальное забиваем NOP'ами
  for i := 5 to SPEED_INTEG_LEN - 1 do
    PByte(Cardinal(target) + Cardinal(i))^ := $90;

  VirtualProtect(target, SPEED_INTEG_LEN, oldProtect, dummy);
  SpeedHookInstalled := True;
  AddToLog(Format('=== SPEED HOOK INSTALLED at %x -> %x (scale=%.2f) ===',
    [Cardinal(target), Cardinal(@SpeedIntegHandler), SpeedAccelScale]));
end;

procedure RemoveSpeedHook;
var
  target: Pointer;
  i: Integer;
  oldProtect, dummy: DWORD;
begin
  if not SpeedHookInstalled then Exit;
  target := Pointer(ADDR_SPEED_INTEG);
  if not VirtualProtect(target, SPEED_INTEG_LEN, PAGE_EXECUTE_READWRITE, oldProtect) then
    Exit;

  for i := 0 to SPEED_INTEG_LEN - 1 do
    PByte(Cardinal(target) + Cardinal(i))^ := SpeedHook_Saved[i];

  VirtualProtect(target, SPEED_INTEG_LEN, oldProtect, dummy);
  SpeedHookInstalled := False;
  AddToLog('=== SPEED HOOK REMOVED ===');
end;

function IsSpeedHookActive: Boolean;
begin
  Result := SpeedHookInstalled;
end;

end.
