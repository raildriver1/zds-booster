unit RA3Physics;

// =============================================================================
//  Физическая модель РА-3 для ZDSimulator booster
//  Дизель + гидропередача (ГДТ/ГДМ/ГТ) + реверсор.
//  Пишет силу тяги в SilaTyagi (0x09007CD8 double, 8 байт) каждый кадр.
// =============================================================================
//
//  ИСТОЧНИК ФИЗИКИ:
//    Все формулы и константы — порт 1:1 из открытого проекта RRS RA-3 v4.0.5
//    автора Дмитрия Притыкина (maisvendoo) и команды РГУПС (Центр развития
//    инновационных компетенций).
//    Репозиторий: https://gitlab.com/maisvendoo/ra3 (GPL v2)
//
//  Авторы оригинального проекта RRS РА-3:
//    • Дмитрий Притыкин (maisvendoo)  — DLL моторного вагона, физика
//    • Ксения Шалобаева (KsyVI)       — 3D-модель кабины
//    • Азер Рустамов                  — 3D-модель моторного вагона, МФДУ
//    • Богос Даглдиян (Bogos387)      — дисплейные модули СКИФ-РА и БЛОК
//    • Денис Максименко (ITVR_CRIK)   — фото/видео материалы, текстуры
//    • Александр Мищенко (Ulovskii2017) — фото/видео, технический консультант
//
//  Калибровка: TractionScale=0.005 даёт паспортный пик acc 0.5-0.7 м/с².
// =============================================================================

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
const
  // ── ВХОДЫ (читаем напрямую) ─────────────────────────────────────────────
  ADDR_KONTROLLER      : Cardinal = $091D5B05;  // byte: позиция КМ (0..5, 251..255)
  ADDR_REVERS_DIRECT   : Cardinal = $0538BFD6;  // byte: реверсор (0/1/255 = neutral/fwd/bwd)
  ADDR_SPEED_DIRECT    : Cardinal = $0538C28C;  // float: скорость с колеса (км/ч)
  ADDR_BV              : Cardinal = $091D5B06;  // byte: ВУ
  ADDR_PANTOGRAF       : Cardinal = $091D5B07;  // byte: пантограф

  // ── ВЫХОД (пишем как DOUBLE = 8 байт!) ──────────────────────────────────
  // Это ОСНОВНАЯ переменная силы тяги, которой игра считает ускорение.
  ADDR_SILATYAGI       : Cardinal = $09007CD8;  // double: сила тяги в Ньютонах (8 байт)

  // ── СТАРЫЕ ПОЙНТЕРНЫЕ АДРЕСА (оставлены как fallback) ───────────────────
  // У меня раньше revers через $0034B28C → +$1C попадал в 0x9110D98 — это
  // НЕ та переменная, что использует физика. Реальный revers по абсолютному
  ADDR_LOCS_BASE_PTR   : Cardinal = $400000 + $0034AEA0;  // legacy: LOCSECTIONS[36] (НЕ физическая F)
  ADDR_REVERS_BASE_PTR : Cardinal = $400000 + $0034B28C;
  ADDR_SPEED_BASE_PTR  : Cardinal = $400000 + $0034B41C;

  OFF_LOCS_36  = $B4;
  OFF_REVERS   = $1C;
  OFF_SPEEDKPH = $3C;

// ============================================================================
// Константы физики — ОБНОВЛЕНО под RRS v4.0.5 (https://gitlab.com/maisvendoo/ra3):
//   cfg/vehicles/ra3-head/disel.xml              (Q_max, K3, K2, etc.)
//   cfg/vehicles/ra3-head/hydro-transmission.xml (k, i_min, i_max, T_*)
//   cfg/vehicles/ra3-head/mpsu.xml               (n_min, n_max, n_min_gb)
//   cfg/vehicles/ra3-head/ra3-NNNNN.xml          (WheelDiameter)
//   ra3-head/include/ra3-head.h                  (ip1=1.2, ip2=2.78)
//   ra3-equipment/src/disel.cpp                  (формулы)
//   ra3-equipment/src/hydro-transmission.cpp     (формулы)
//   ra3-equipment/src/mpsu.cpp                   (getTracRefDiselFreq)
//
// Изменения от v4.0.0 → v4.0.5:
//   • Q_max: 0.166 → 0.125 (новый cfg)
//   • K3 (torque): 5.25e4 → 7.0e4 (новый cfg)
//   • K2 (I-gov): новое имя/значение 0.3 (раньше использовалось K[5]=0.15)
//   • n_ref упрощён: убрана зависимость от trac_min/brake_min в формуле
// ============================================================================
const
  TICK_DT          : Double = 0.1;     // 10Гц

  // ---- Дизель v4.0.5 (disel.xml + disel.cpp) ----
  // Регулятор: s1 = K[4]·δω + K[2]·Y[2]; u = clamp(s1, 0, 1)
  //            Q_fuel = u · Q_max; M_d = is_fuel_ignition · Q_fuel · K[3]
  // Mc = 300 — момент сопротивления холостого хода. В формуле cut(Mc·ω/20, -Mc, Mc)
  // J_shaft = 10.0 — момент инерции коленвала + редуктора
  // ip = 3.3 — передаточное число привода стартер-генератора
  // Q_max = 0.125 кг/с (v4.0.5: было 0.166 в v4.0.0)
  // K3 = 7.0e4 коэф момента M_d = K3·Q_fuel (v4.0.5: было 5.25e4 в v4.0.0)
  //   ⇒ M_d_max = K3·Q_max = 7.0e4·0.125 = 8750 Н·м (≈8715 в v4.0.0)
  // K4 = 0.01 P-составляющая регулятора
  // K2 = 0.3  I-составляющая регулятора (v4.0.5: было K[5]=0.15 в v4.0.0)
  // omega_min = 19.9 рад/с (~190 RPM) — минимум прокрутки до воспламенения
  N_IDLE           : Double = 800.0;
  Q_MAX            : Double = 0.125;   // v4.0.5: было 0.166
  K0_DRAIN         : Double = 0.125;
  K1_PUMP          : Double = 3.0e-4;
  K3_TORQUE        : Double = 7.0e4;   // v4.0.5: было 5.25e4
  K4_P_GOVERNOR    : Double = 0.01;
  K2_I_GOVERNOR    : Double = 0.3;     // v4.0.5: было K[5]=0.15 в v4.0.0
  J_SHAFT          : Double = 10.0;
  MC_LOAD          : Double = 300.0;
  IP_REDUCTOR      : Double = 3.3;
  OMEGA_MIN        : Double = 19.9;
  OMEGA_MAX_CLAMP  : Double = 300.0;   // ~2864 RPM — выше потолка n_max=2500

  // ---- MPSU v4.0.5 (mpsu.xml + mpsu.cpp::getTracRefDiselFreq) ----
  // Формула в v4.0.5 УПРОЩЕНА (убраны trac_min/brake_min):
  //   if hydro_brake_ON: n_ref = n_min_gb + brake_level · (n_max - n_min_gb)
  //   else:              n_ref = n_min    + trac_level  · (n_max - n_min)
  // (в v4.0.0 было: n_min + (n_max-n_min)·(trac-trac_min)/(1-trac_min))
  N_MIN_RPM        : Double = 800.0;
  N_MAX_RPM        : Double = 2500.0;
  N_MIN_GB_RPM     : Double = 1700.0;

  // ---- Гидропередача (hydro-transmission.xml) ----
  // k = 0.033 — коэффициент M_in = (y_gt+y_gm)*k*omega_in² (RRS: было 0.062)
  // i_min = 0.41, i_max = 0.57 — гистерезис переключения ГДТ↔ГДМ (RRS: было 0.8/0.9)
  // T_gt = T_gm = T_gb = 0.5 — постоянные времени наполнения, T_revers = 1.0
  // P_gb = 300 кВт — мощность гидротормоза, omega_gb_max = 162 рад/с
  K_HT             : Double = 0.033;
  T_GT             : Double = 0.5;
  T_GM             : Double = 0.5;
  T_GB             : Double = 0.5;
  T_REVERS         : Double = 1.0;
  I_MIN_SWITCH     : Double = 0.41;
  I_MAX_SWITCH     : Double = 0.57;
  P_GB_KW          : Double = 300.0;
  OMEGA_GB_MAX     : Double = 162.0;

  // ---- Механика тележки (ra3-head.h + ra3-NNNNN.xml) ----
  // ip = ip1 * ip2 = 1.2 * 2.78 = 3.336 (RRS: было 4.5)
  // wheel_diameter = 0.86 → r = 0.43 (RRS: было 0.475)
  // 2 оси на моторной секции (Q_a[1] + Q_a[2]) — на каждую идёт M_out_hydro/2
  IP_GEAR          : Double = 3.336;   // ip1 * ip2
  WHEEL_RADIUS     : Double = 0.43;
  N_AXLES          : Double = 2.0;     // моторная секция РА3 — 2 оси с тягой

  // Нормализация для LOCSECTIONS[36].
  // loc36 — накопитель тяги, который игра использует в диапазоне 0..~10.
  // ВАЖНО: игра штатно использует loc36 в диапазоне 0..~8. Значения выше 10
  // приводят к АНОМАЛЬНОМУ замедлению — поэтому жёстко лимитируем 10.
  // С новыми константами пик F_rail на старте ~80-100 кН (GT-зона, k_gt~3.0
  // умножает M_in). На крейсере (i→1) ~30 кН. F_MAX_RAIL подобран чтобы
  // на крейсере получать ~loc36 4-6 (хватает для удержания скорости).
  F_MAX_RAIL       : Double = 50000.0;  // ~50 кН маппится на loc36=10
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
  v: Single;
  p: PSingle;
begin
  // Если не получится (например, симулятор перезагрузил карту памяти) —
  // fallback на pointer-based чтение через $0034B41C +$3C.
  Result := 0;
  try
    v := PSingle(ADDR_SPEED_DIRECT)^;
    if (v >= -500.0) and (v <= 500.0) then
    begin
      Result := v;
      Exit;
    end;
  except
  end;
  p := SafePtr(ADDR_SPEED_BASE_PTR, OFF_SPEEDKPH);
  if p <> nil then
  try
    v := p^;
    if (v >= -500.0) and (v <= 500.0) then Result := v;
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
  b: Byte;
  p: PShortInt;
begin
  Result := 1;
  try
    b := PByte(ADDR_REVERS_DIRECT)^;
    case b of
      0:        Result := 0;
      1:        Result := 1;
      251..255: Result := -1;
    else
      Result := 1;
    end;
    Exit;
  except
  end;
  // Fallback: старый pointer-based путь.
  p := SafePtr(ADDR_REVERS_BASE_PTR, OFF_REVERS);
  if p = nil then Exit;
  try
    if p^ < 0 then Result := -1
    else if p^ > 0 then Result := 1
    else Result := 0;
  except
    Result := 1;
  end;
end;

// Запись силы тяги (Ньютоны) в ОСНОВНУЮ переменную физики симулятора.
// Это переменная, по которой игра считает v += a*dt. LOCSECTIONS[36] —
// другое (легаси), его трогать не нужно.
procedure WriteSilaTyagi(value: Double);
begin
  try
    PDouble(ADDR_SILATYAGI)^ := value;
  except
  end;
end;

// Legacy: старый путь записи в LOCSECTIONS[36]. Оставлен на случай если
// кто-то захочет вернуть прежнее поведение или для отладки.
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
// Характеристики гидротрансформатора и гидромуфты — точки из RRS-проекта
// ra3-4.0.0/cfg/vehicles/ra3-head/gdt.csv и gdm.csv.
// LinearInterpolation по этим точкам — повторяет RRS::getValue().
// i = omega_out / omega_in (передаточное отношение)
// ============================================================================
const
  GDT_N = 13;
  GDM_N = 22;

  GDT_X: array[0..GDT_N-1] of Double = (
    0.0,
    0.0823164170782268,
    0.1646328341564536,
    0.24694925123468042,
    0.3292656683129072,
    0.411582085391134,
    0.49389850246936084,
    0.5762149195475876,
    0.6585313366258144,
    0.7408477537040412,
    0.823164170782268,
    0.9054805878604948,
    0.9877970049387217);
  GDT_Y: array[0..GDT_N-1] of Double = (
    3.0,
    2.567147613762486,
    1.9844617092119865,
    1.524972253052164,
    1.225305216426193,
    1.0155382907880133,
    0.8257491675915648,
    0.6925638179800222,
    0.6526082130965594,
    0.6159822419533851,
    0.5660377358490566,
    0.532741398446171,
    0.4994450610432852);

  GDM_X: array[0..GDM_N-1] of Double = (
    0.000, 0.071, 0.171, 0.254, 0.343, 0.425,
    0.49389850246936084, 0.5762149195475876, 0.6585313366258144,
    0.7408477537040412, 0.823164170782268, 0.9054805878604948,
    0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1.00);
  GDM_Y: array[0..GDM_N-1] of Double = (
    1.000, 0.983, 0.948, 0.910, 0.857, 0.82,
    0.77,  0.71,  0.6526082130965594,
    0.6159822419533851, 0.5660377358490566, 0.53,
    0.52, 0.51, 0.50, 0.49, 0.48, 0.47, 0.46, 0.45, 0.44, 0.0);

// pf(x) = max(x, 0) — повторяет Physics::pf() из RRS.
function PFloor(x: Double): Double;
begin
  if x > 0 then Result := x else Result := 0;
end;

// Линейная интерполяция по таблицам X/Y (X должен быть монотонно возрастающим).
// За пределами таблицы — возвращает крайнее значение (как RRS::LinearInterpolation).
function InterpTable(const X, Y: array of Double; N: Integer; arg: Double): Double;
var
  i: Integer;
  t: Double;
begin
  if N <= 0 then begin Result := 0; Exit; end;
  if arg <= X[0]   then begin Result := Y[0];     Exit; end;
  if arg >= X[N-1] then begin Result := Y[N-1];   Exit; end;
  for i := 0 to N - 2 do
  begin
    if (arg >= X[i]) and (arg <= X[i+1]) then
    begin
      if X[i+1] - X[i] < 1.0e-12 then
        Result := Y[i]
      else
      begin
        t := (arg - X[i]) / (X[i+1] - X[i]);
        Result := Y[i] + t * (Y[i+1] - Y[i]);
      end;
      Exit;
    end;
  end;
  Result := Y[N-1];
end;

// Коэффициент трансформации момента ГДТ. Повторяет RRS getHydroTranstCoeff:
//   pf(gt_char.getValue(|omega_out|/|omega_in|))
function GTCoeff(omega_in_v, omega_out_v: Double): Double;
var
  i_arg: Double;
begin
  if Abs(omega_in_v) < 0.1 then begin Result := 0; Exit; end;
  i_arg := Abs(omega_out_v) / Abs(omega_in_v);
  Result := PFloor(InterpTable(GDT_X, GDT_Y, GDT_N, i_arg));
end;

// Коэффициент трансформации момента ГДМ. Повторяет RRS getHydroCouplingCoeff.
function GMCoeff(omega_in_v, omega_out_v: Double): Double;
var
  i_arg: Double;
begin
  if Abs(omega_in_v) < 0.1 then begin Result := 0; Exit; end;
  i_arg := Abs(omega_out_v) / Abs(omega_in_v);
  Result := PFloor(InterpTable(GDM_X, GDM_Y, GDM_N, i_arg));
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
// Маппинг контроллера: kontr_f ∈ [-5..+5] → trac_level и brake_level [0..1].
// Повторяет TracController + MPSU::getTracRefDiselFreq из RRS.
//
// В RRS контроллер имеет режимы (-2 экстренное, -1 тормоз, 0 нейтраль, 1 тяга)
// и уровень trac_level/brake_level [0..100]. У нас же сразу float-вход — поэтому
// |kontr_f|/5 = уровень тяги (или тормоза).
// ============================================================================
procedure MapController(out trac_level, brake_level: Double);
var
  kontr_f: Single;
begin
  kontr_f := ReadKontrollerFloat;
  if kontr_f > 0.05 then
  begin
    trac_level  := Clamp(kontr_f / 5.0, 0.0, 1.0);
    brake_level := 0;
  end
  else if kontr_f < -0.05 then
  begin
    trac_level  := 0;
    brake_level := Clamp(-kontr_f / 5.0, 0.0, 1.0);
  end
  else
  begin
    trac_level  := 0;
    brake_level := 0;
  end;
end;

// RRS v4.0.5 mpsu.cpp::getTracRefDiselFreq (упрощённая формула без trac_min):
//   if hydro_brake_ON: n_ref = n_min_gb + brake_level · (n_max - n_min_gb)
//   else:              n_ref = n_min    + trac_level  · (n_max - n_min)
//   возвращает cut(n_ref, n_min, n_max).
function ComputeNRef(trac_level, brake_level: Double; hydro_brake_on: Boolean): Double;
var
  v: Double;
begin
  if hydro_brake_on then
    v := N_MIN_GB_RPM + brake_level * (N_MAX_RPM - N_MIN_GB_RPM)
  else
    v := N_MIN_RPM + trac_level * (N_MAX_RPM - N_MIN_RPM);
  Result := Clamp(v, N_MIN_RPM, N_MAX_RPM);
end;

// ============================================================================
// Шаг интегрирования дизеля (RRS Disel::preStep + ode_system)
// ============================================================================
procedure StepDiesel(dt: Double);
var
  trac_lev, brake_lev: Double;
  delta_omega, s1, u, Q, Mr, dOmega: Double;
begin
  MapController(trac_lev, brake_lev);

  // n_ref из MPSU. При тяге используем trac_level, при гидротормозе brake_level.
  // y_gb > 0.1 — считаем что гидротормоз активен → дизель удерживается на n_min_gb.
  n_ref := ComputeNRef(trac_lev, brake_lev, y_gb > 0.1);

  delta_omega := n_ref * Pi / 30.0 - omega_eng;

  // ── РЕГУЛЯТОР ─────────────────────────────────────────────────────────
  // RRS preStep: s1 = (K[4]*delta_omega + K[5]*Y[2]) * mv6_state, u = cut(s1,0,1)
  // Y[2] — интеграл delta_omega (dY[2]/dt = delta_omega), ограниченный 0..1.
  integ_governor := integ_governor + delta_omega * dt;
  integ_governor := Clamp(integ_governor, 0.0, 1.0);

  // RRS v4.0.5: s1 = K[4]·δω + K[2]·Y[2] (использует K[2], не K[5] как было в v4.0.0)
  s1 := K4_P_GOVERNOR * delta_omega + K2_I_GOVERNOR * integ_governor;
  u := Clamp(s1, 0.0, 1.0);

  Q := u * Q_MAX;
  M_d := K3_TORQUE * Q;
  if not is_fuel_ignition then M_d := 0;

  // ── ВРАЩЕНИЕ КОЛЕНВАЛА ────────────────────────────────────────────────
  // RRS: dY[1]/dt = (M_d + ip*M_sg - ip*M_fg - M_gen - Mr) / J_shaft
  //      Mr = cut(Mc * omega/20, -Mc, Mc)
  // У нас M_sg, M_fg, M_gen = 0 (нет стартера/возбудителя в данной симуляции),
  // зато ip*M_in_hydro отнимает момент гидропередачи (нагрузка от трансмиссии).
  Mr := Clamp(MC_LOAD * omega_eng / 20.0, -MC_LOAD, MC_LOAD);

  dOmega := (M_d - IP_REDUCTOR * M_in_cur - Mr) / J_SHAFT;
  omega_eng := omega_eng + dOmega * dt;
  omega_eng := Clamp(omega_eng, 0.0, OMEGA_MAX_CLAMP);

  // ── ДАВЛЕНИЕ МАСЛА ────────────────────────────────────────────────────
  // RRS: dY[0]/dt = (Q_emn + K[1]*Y[1] - K[0]*Y[0]) / V_oil
  // Q_emn=0 (нет ЭМН в нашей упрощённой модели). V_oil=1.5.
  dOmega := (K1_PUMP * omega_eng - K0_DRAIN * oil_pressure) / 1.5;
  oil_pressure := oil_pressure + dOmega * dt;
  oil_pressure := Clamp(oil_pressure, 0.0, 10.0);
end;

// Адрес позиции крана 395.
// 1,2 = поездное (отпуск), 3,4 = перекрыша, 5,6,50 = торможение.
const
  ADDR_KRAN395  : Cardinal = $090043A0;
  V_HYDRO_BRAKE_OFF_KPH : Double = 5.0;  // RRS MPSU: ниже этой v гидротормоз отключается

function ReadKran395Byte: Byte;
begin
  try
    Result := PByte(ADDR_KRAN395)^;
  except
    Result := 2;  // II — поездное (отпуск)
  end;
end;

// MPSU-логика гидротормоза: смотрит и на контроллер тяги (-1..-5), и на
// позицию крана 395, и на скорость. Возвращает уровень "запроса" на
// гидротормоз 0..1, и флаг hydro_brake_ON.
procedure ComputeBrakeRequest(speed_kph: Single; trac_brake_lev: Double;
  out u_brake_req: Double; out hydro_brake_on: Boolean);
var
  kran: Byte;
begin
  kran := ReadKran395Byte;
  // Базовый уровень — от контроллера тяги (если он в позиции тормоза).
  u_brake_req := trac_brake_lev;
  // Override от крана 395 — RRS MPSU так делает: brake_ref_level_GB > 0 при
  // переводе крана в любую тормозную позицию.
  case kran of
    1, 2:        ;  // поездное — гидротормоз не запрашиваем
    3, 4:        if u_brake_req < 0.5 then u_brake_req := 0.5;  // перекрыша = средний
    5, 6, 50:    u_brake_req := 1.0;  // V/V_A/VI — полный гидротормоз
  end;
  if u_brake_req > 1.0 then u_brake_req := 1.0;
  // RRS MPSU: гидротормоз эффективен только на v >= ~5 км/ч.
  // Ниже этого порога переключаемся на пневмо (которым ZDsim управляет сам).
  hydro_brake_on := (u_brake_req > 0.001) and (Abs(speed_kph) >= V_HYDRO_BRAKE_OFF_KPH);
  if not hydro_brake_on then u_brake_req := 0;
end;

// ============================================================================
// Шаг интегрирования гидропередачи (RRS HydroTransmission::preStep + ode_system)
// ============================================================================
procedure StepHydroTrans(dt: Double);
var
  trac_lev, brake_lev: Double;
  u_brake_req: Double;
  hydro_brake_on: Boolean;
  speed_kph: Single;
  omega_out, omega_in: Double;
  M_gt_tmp, M_gm_tmp: Double;
  u_gt_ref, u_gm_ref, u_gb_ref: Double;
  brake_lim, k_gb_local, M_gb_max_local: Double;
  rev_hw, revers_state_local: Integer;
  is_traction, is_brake: Boolean;
  motion: Boolean;
begin
  MapController(trac_lev, brake_lev);
  speed_kph := ReadSpeedKPH;
  rev_hw := ReadReverser;

  // ── БЛОКИРОВКА ПЕРЕКЛЮЧЕНИЯ РЕВЕРСА (RRS setRefReversState) ───────────
  // В RRS HydroTransmission::setRefReversState запрещает менять revers_pos_ref
  // если состав движется ИЛИ активна тяга/тормоз. Замок имитирует реальное
  // механическое блокирование реверсивного механизма.
  motion      := Abs(speed_kph) >= 0.04;  // ≈ 0.01 м/с (RRS: omega_out >= 0.01 rad/s)
  is_traction := trac_lev  > 0.001;
  is_brake    := brake_lev > 0.001;
  if (rev_hw <> 0) and (rev_hw <> revers_pos_ref) then
  begin
    if (not motion) and (not is_traction) and (not is_brake) then
      revers_pos_ref := rev_hw;
    // Иначе игнорируем команду — реверс остаётся в текущем положении.
  end;

  // ── РЕВЕРС: апериодика + gap (RRS hydro-transmission.cpp:159, 72-88) ──
  // dY[3]/dt = (1.05 * revers_pos_ref - Y[3]) / T_revers
  // revers_state = gap(Y[3]) — мёртвая зона ±1.
  revers_soft := revers_soft + (1.05 * revers_pos_ref - revers_soft) / T_REVERS * dt;
  if revers_soft >= 1.0 then revers_state_local := 1
  else if revers_soft <= -1.0 then revers_state_local := -1
  else revers_state_local := 0;

  // ── ЗАПРОС НА ГИДРОТОРМОЗ (RRS MPSU + кран 395) ──────────────────────
  // Учитываем и контроллер тяги (-1..-5), и позицию крана машиниста, и
  // скорость (на v<5 км/ч гидротормоз отключается, тормозит пневмо).
  ComputeBrakeRequest(speed_kph, brake_lev, u_brake_req, hydro_brake_on);

  u_traction := trac_lev;
  u_brake    := u_brake_req;
  // is_brake обновляем после applique MPSU-логики, чтобы наполнение y_gb
  // включалось при запросе крана даже если контроллер тяги в нуле.
  is_brake := u_brake_req > 0.001;

  // ── СКОРОСТИ ВРАЩЕНИЯ ─────────────────────────────────────────────────
  // omega_in = omega дизеля
  // omega_out = ip_total * wheel_omega (RRS: hydro_trans->setOmegaOutput(ip*wheel_omega))
  omega_in := omega_eng;
  omega_out := Abs(speed_kph) / 3.6 / WHEEL_RADIUS * IP_GEAR;

  if Abs(omega_in) >= 0.1 then
    i_gp := omega_out / omega_in
  else
    i_gp := 0;

  // ── ГИСТЕРЕЗИС ГДТ↔ГДМ (Hysteresis::setValue) ─────────────────────────
  // Поведение RRS Hysteresis: state остаётся False пока value < i_max,
  // переключается в True при value >= i_max. Возвращается в False
  // только когда value <= i_min. gt_mode = state (True = ГДМ активна).
  if (not gt_mode) and (i_gp >= I_MAX_SWITCH) then
    gt_mode := True
  else if gt_mode and (i_gp <= I_MIN_SWITCH) then
    gt_mode := False;

  // ── ЗАДАНИЕ ЗАПОЛНЕНИЯ (RRS preStep) ──────────────────────────────────
  // RRS:
  //   u_gt = !switch_state * |revers_state * revers_handle| * u_traction
  //   u_gm = (1 - u_gt) * |revers_state * revers_handle| * u_traction
  //   u_gb = u_brake
  // Т.е. при revers_state=0 (в момент перехода) u_gt=u_gm=0 — естественная
  // защита от тяги при движущемся реверсе.
  if (revers_state_local <> 0) and is_traction then
  begin
    if not gt_mode then
    begin
      // ГДТ-режим (низкий i_gp)
      u_gt_ref := u_traction;
      u_gm_ref := 0;
    end
    else
    begin
      // ГДМ-режим (высокий i_gp)
      u_gt_ref := 0;
      u_gm_ref := u_traction;
    end;
  end
  else
  begin
    u_gt_ref := 0;
    u_gm_ref := 0;
  end;
  if is_brake then u_gb_ref := u_brake else u_gb_ref := 0;

  // ── АПЕРИОДИЧЕСКИЕ ЗВЕНЬЯ НАПОЛНЕНИЯ (ode_system) ────────────────────
  y_gt := y_gt + (u_gt_ref - y_gt) / T_GT * dt;
  y_gm := y_gm + (u_gm_ref - y_gm) / T_GM * dt;
  y_gb := y_gb + (u_gb_ref - y_gb) / T_GB * dt;
  y_gt := Clamp(y_gt, 0.0, 1.0);
  y_gm := Clamp(y_gm, 0.0, 1.0);
  y_gb := Clamp(y_gb, 0.0, 1.0);

  // ── МОМЕНТ НА ВХОДЕ (RRS: M_in = cut(y_gt+y_gm,0,1) * k * omega_in²) ─
  M_in_cur := Clamp(y_gt + y_gm, 0.0, 1.0) * K_HT * Sqr(omega_in);

  // ── ГИДРОТОРМОЗ ───────────────────────────────────────────────────────
  // RRS: M_gb = k_gb * brake_level_ref * Y[2] * omega_out²
  //      k_gb = M_gb_max / omega_db_max² где M_gb_max = P_gb*1000/omega_db_max
  // brake_level_ref у нас = u_brake (управление непосредственно от ручки).
  M_gb_max_local := P_GB_KW * 1000.0 / OMEGA_GB_MAX;
  k_gb_local := M_gb_max_local / Sqr(OMEGA_GB_MAX);
  M_gb_cur := k_gb_local * u_brake * y_gb * Sqr(omega_out);
  brake_lim := BrakeTorqueLimit(omega_out);
  M_gb_cur := Clamp(M_gb_cur, 0.0, brake_lim * u_brake);

  // ── МОМЕНТ НА ВЫХОДЕ ──────────────────────────────────────────────────
  // RRS: M_out = Y[0]*GTcoef*M_in + Y[1]*GMcoef*M_in - M_gb
  M_gt_tmp := y_gt * GTCoeff(omega_in, omega_out) * M_in_cur;
  M_gm_tmp := y_gm * GMCoeff(omega_in, omega_out) * M_in_cur;
  M_out_cur := M_gt_tmp + M_gm_tmp - M_gb_cur;

  // RRS getOutputTorque() умножает M_out на revers_state — направление силы.
  // Применим здесь (в стартовой позиции с revers_state=0 момент = 0).
  M_out_cur := M_out_cur * revers_state_local;
end;

// ============================================================================
// Главный шаг
// ============================================================================
procedure DoPhysicsTick(dt: Double);
const
  SUB_STEPS = 20;   // 20 субшагов × dt/20 — стабильный Эйлер
var
  sub_dt, F_to_write: Double;
  kontr_raw: Byte;
  speed_raw: Single;
  revers_raw, i: Integer;
  silat_before, silat_after: Double;
begin
  // Читаем сырые значения ДО физики
  kontr_raw := ReadKontroller;
  speed_raw := ReadSpeedKPH;
  revers_raw := ReadReverser;

  // До нашей записи — что лежит в SilaTyagi (для диагностики).
  silat_before := 0;
  try silat_before := PDouble(ADDR_SILATYAGI)^ except end;

  sub_dt := dt / SUB_STEPS;
  for i := 1 to SUB_STEPS do
  begin
    StepDiesel(sub_dt);
    StepHydroTrans(sub_dt);
  end;

  // ── СИЛА НА РЕЛЬСЕ ────────────────────────────────────────────────────
  // RRS: tractionForce = ip * M_out / r_wheel (см. ra3-head step-traction.cpp).
  // M_out_cur уже содержит знак реверса (revers_state local в StepHydroTrans).
  F_rail_cur := M_out_cur * IP_GEAR / WHEEL_RADIUS;

  // Если M_out почти ноль (например на старте, когда revers_state=0),
  // но ручка реверса в "назад" — даём отрицательный знак для консистентности.
  if (Abs(M_out_cur) < 0.1) and (revers_pos_ref < 0) then
    F_rail_cur := -Abs(F_rail_cur);

  // ── ВЫХОДНОЕ ЗНАЧЕНИЕ (КАЛИБРОВКА ПОД ZDsim) ─────────────────────────
  // Пишем в SilaTyagi (8-byte double) по адресу 0x09007CD8.
  //
  // Эмпирическое соотношение из тестов:
  //   silat 300 → ~1.5 м/с² ускорения в игре (для РА-3 массы)
  //   ⇒ "коэффициент игры" ≈ 0.005 м/с² на 1 unit silat
  //
  // Реалистичный паспорт РА-3:
  //   - пик acc на старте  ~0.7-0.9 м/с² (паспорт)
  //   - acc на крейсере    ~0.0 м/с² (равновесие с drag)
  //
  // Наша физика выдаёт F_rail в Ньютонах:
  //   - пик F_rail (~v=22 км/ч)  = 18000 Н
  //   - F_rail на крейсере 80    = 4000 Н
  //   - F_rail на крейсере 100   = 1500 Н
  //
  // Масштаб 1/100 (TractionScale=0.01) без клампа сохраняет ЕСТЕСТВЕННУЮ
  // форму кривой тяги F(v) и при этом даёт реалистичные ускорения:
  //   v=22:  silat=180 → acc 0.9 м/с² ← паспорт
  //   v=80:  silat=40  → acc 0.2 м/с² ← плавное снижение
  //   v=100: silat=15  → ~0 acc       ← равновесие с drag игры
  //
  // Клампа больше нет — он сглаживал кривую и убивал реалистичность.
  F_to_write := F_rail_cur * TractionScale;

  // ── ПЛАВНОЕ ОГРАНИЧЕНИЕ ПО СКОРОСТИ ───────────────────────────────────
  // Жёсткая отсечка `if v >= limit: F=0` приводила к тому что поезд резко
  // переставал тянуть и начинал катиться. У РА-3 паспортная скорость 120
  // км/ч; делаем линейное гашение в последних 10 км/ч до лимита.
  // (тормоз — отрицательное F — не трогаем)
  if (SpeedLimitKPH > 0.1) and (F_to_write > 0) then
  begin
    if Abs(speed_raw) >= SpeedLimitKPH then
      F_to_write := 0
    else if Abs(speed_raw) >= SpeedLimitKPH - 10.0 then
      F_to_write := F_to_write * (SpeedLimitKPH - Abs(speed_raw)) / 10.0;
  end;

  LocSection36_cur := F_to_write;  // оставляем для GetLocSection36() API

  // Пишем в реальный адрес физики (8 байт double).
  if WriteEnabled then
    WriteSilaTyagi(F_to_write);

  // Читаем после записи — проверка что значение удержалось.
  silat_after := 0;
  try silat_after := PDouble(ADDR_SILATYAGI)^ except end;

  // Лог — не каждый кадр, а раз в LogIntervalMs.
  if (GetTickCount - LastLogTickMs) >= LogIntervalMs then
  begin
    LastLogTickMs := GetTickCount;
    try
      AddToLog(Format(
        'k=%3d v=%7.2f rev=%3d | rpm=%6.1f Mo=%9.1f F=%10.1f | ' +
        'silat before=%10.1f want=%10.1f after=%10.1f | ' +
        'y_gt=%.3f y_gm=%.3f y_gb=%.3f gt_mode=%s write=%s',
        [kontr_raw, SafeFloat(speed_raw), revers_raw,
         SafeFloat(GetDieselRPM), SafeFloat(M_out_cur), SafeFloat(F_rail_cur),
         silat_before, F_to_write, silat_after,
         SafeFloat(y_gt), SafeFloat(y_gm), SafeFloat(y_gb),
         B2S(gt_mode), B2S(WriteEnabled)]));
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
    // Выключено — включаем с дефолтами для ПАСПОРТНОЙ физики РА-3.
    //
    // Калибровка вычислена из лога:
    //   silat=300 в игре → acc=1.5 м/с² (измерено)
    //   ⇒ 1 game-unit ≈ 0.005 м/с²
    //   паспорт acc на пике = 0.7-0.9 м/с² (для одной мотор-секции 100т)
    //   ⇒ нужен silat пик ~ 150-180 unit
    //
    // Наша физика дает F_rail пик 18000 Н. С TractionScale=0.01 силы становятся
    // в реалистичном диапазоне ZDsim:
    //   v=22 км/ч (пик ГДТ): silat=180 → acc 0.9 м/с² (паспорт)
    //   v=80 (на ГДМ):       silat=40  → acc 0.2 м/с² (постепенно)
    //   v=100 (крейсер):     silat=15  → равновесие с drag игры
    // Клампа нет — сохраняем естественную форму кривой тяги F(v).
    // Калибровка под паспорт РА-3 (0→100 км/ч за 50-60 сек):
    //   0.005 → пик acc 0.7 м/с² (паспорт), но КРЕЙСЕР 0.16 м/с² → 1:48 до 100 (медленно)
    //   0.008 → пик acc 1.1 м/с² (чуть выше нормы), КРЕЙСЕР 0.26 м/с² → ~1:05 до 100 ✓
    //   0.010 → пик acc 1.4 м/с² (резкий старт), КРЕЙСЕР 0.32 → ~50 сек до 100
    // Выбор 0.008 — лучший компромисс пик/крейсер (паспортные 50-60 сек до 100 км/ч).
    TractionScale   := 0.008;
    SpeedLimitKPH   := 120.0;
    SpeedAccelScale := 1.0;
    SetPhysicsEnabled(True);
    SetWriteEnabled(True);
    AddToLog(Format('=== RA3 BOOSTER ON === scale=%.4f limit=%.0f',
      [TractionScale, SpeedLimitKPH]));
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
