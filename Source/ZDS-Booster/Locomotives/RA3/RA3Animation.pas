unit RA3Animation;

// =============================================================================
//  Система анимации РА-3 — порт из RRS v4.0.5 (AnalogSignal + KeyPoints)
//  Позволяет определить каналы анимации (вращение/перемещение/материал)
//  с ключевыми точками (Param → Value) как в XML-конфигах RRS.
// =============================================================================

interface

const
  MAX_ANALOG_SIGNALS = 256;

  // Индексы сигналов (публичные — используются из RA3.pas)
  SIGNAL_WHEELSET_1     = 1;
  SIGNAL_WHEELSET_2     = 2;
  SIGNAL_WHEELSET_3     = 3;
  SIGNAL_WHEELSET_4     = 4;
  SIGNAL_DRIVESHAFT     = 7;
  SIGNAL_INTERAXLE      = 8;

  SIGNAL_DOOR_L_WARN    = 9;
  SIGNAL_DOOR_L_STEP    = 10;
  SIGNAL_DOOR_L_SKID    = 11;
  SIGNAL_DOOR_L_LEFT    = 12;
  SIGNAL_DOOR_L_RIGHT   = 13;
  SIGNAL_DOOR_R_WARN    = 14;
  SIGNAL_DOOR_R_STEP    = 15;
  SIGNAL_DOOR_R_SKID    = 16;
  SIGNAL_DOOR_R_LEFT    = 17;
  SIGNAL_DOOR_R_RIGHT   = 18;

  SIGNAL_SPOTLIGHT      = 38;
  SIGNAL_CAB_LIGHT      = 43;
  SIGNAL_PULT_LIGHT     = 44;
  SIGNAL_SALON_LIGHT    = 45;

  SIGNAL_KM_HANDLE      = 101;
  SIGNAL_BRAKE_HANDLE   = 102;
  SIGNAL_BUTTON_SAND    = 128;
  SIGNAL_SPEED_HOLD     = 137;

  SIGNAL_ARROW_PM       = 173;
  SIGNAL_ARROW_BP       = 174;
  SIGNAL_ARROW_BC_FWD   = 175;
  SIGNAL_ARROW_BC_BWD   = 176;
  SIGNAL_ARROW_VOLT_24V = 59;
  SIGNAL_ARROW_VOLT_110V = 60;

type
  TAnimKeyPoint = record
    Param: Single;   // входное значение сигнала
    Value: Single;   // выход: угол (°) / смещение (м) / цвет (0..1)
  end;

  TAnimChannelType = (
    actRotation,      // вращение по оси X/Y/Z
    actTranslation,   // перемещение по X/Y/Z
    actMaterial       // цвет эмиссии/прозрачность
  );

  TAnimChannel = record
    SignalID: Integer;           // индекс в AnalogSignal
    Duration: Single;            // скорость lerp (сек⁻¹), 0 = мгновенно
    AxisX, AxisY, AxisZ: Single; // ось вращения/перемещения
    Infinity: Boolean;           // бесконечное вращение (колёса)
    ChannelType: TAnimChannelType;
    KeyPoints: array of TAnimKeyPoint;
  end;

var
  AnalogSignal: array[0..MAX_ANALOG_SIGNALS - 1] of Single;
  AnimChannels: array[0..MAX_ANALOG_SIGNALS - 1] of TAnimChannel;
  AnimChannelsInitialized: Boolean = False;

// Вспомогательная функция создания KeyPoint
function KP(AParam, AValue: Single): TAnimKeyPoint;

// Инициализация каналов анимации (вызвать один раз)
procedure InitAnimChannels;

// Вычислить значение анимации по сигналу с интерполяцией KeyPoints
function EvalAnim(const Channel: TAnimChannel; SignalValue: Single): Single;

// Плавное движение текущего значения к цели
function LerpTo(var Current: Single; Target: Single; Speed: Single; dt: Single): Single;

// Заполнение AnalogSignal из игровой памяти
procedure FillAnalogSignals;

implementation

uses
  SysUtils, Math, KlubData;

// ============================================================================
function KP(AParam, AValue: Single): TAnimKeyPoint;
begin
  Result.Param := AParam;
  Result.Value := AValue;
end;

// ============================================================================
// KeyPoint-интерполятор
// ============================================================================
function EvalAnim(const Channel: TAnimChannel; SignalValue: Single): Single;
var
  i, n: Integer;
  t: Single;
begin
  n := Length(Channel.KeyPoints);
  if n = 0 then begin Result := 0; Exit; end;
  if n = 1 then begin Result := Channel.KeyPoints[0].Value; Exit; end;

  if SignalValue <= Channel.KeyPoints[0].Param then
  begin
    Result := Channel.KeyPoints[0].Value;
    Exit;
  end;
  if SignalValue >= Channel.KeyPoints[n - 1].Param then
  begin
    Result := Channel.KeyPoints[n - 1].Value;
    Exit;
  end;

  for i := 0 to n - 2 do
  begin
    if (SignalValue >= Channel.KeyPoints[i].Param) and
       (SignalValue <= Channel.KeyPoints[i + 1].Param) then
    begin
      t := (SignalValue - Channel.KeyPoints[i].Param) /
           (Channel.KeyPoints[i + 1].Param - Channel.KeyPoints[i].Param);
      Result := Channel.KeyPoints[i].Value +
                t * (Channel.KeyPoints[i + 1].Value - Channel.KeyPoints[i].Value);
      Exit;
    end;
  end;

  Result := Channel.KeyPoints[n - 1].Value;
end;

// ============================================================================
// Плавное движение к цели
// ============================================================================
function LerpTo(var Current: Single; Target: Single; Speed: Single; dt: Single): Single;
begin
  if Speed <= 0 then
    Current := Target
  else
    Current := Current + (Target - Current) * (1 - Exp(-Speed * dt));
  Result := Current;
end;

// ============================================================================
// Инициализация каналов
// ============================================================================
procedure AddRotationChannel(SignalID: Integer; Duration: Single;
  AX, AY, AZ: Single; Infinity: Boolean; const KPs: array of TAnimKeyPoint);
var
  i: Integer;
begin
  AnimChannels[SignalID].SignalID := SignalID;
  AnimChannels[SignalID].Duration := Duration;
  AnimChannels[SignalID].AxisX := AX;
  AnimChannels[SignalID].AxisY := AY;
  AnimChannels[SignalID].AxisZ := AZ;
  AnimChannels[SignalID].Infinity := Infinity;
  AnimChannels[SignalID].ChannelType := actRotation;
  SetLength(AnimChannels[SignalID].KeyPoints, Length(KPs));
  for i := 0 to High(KPs) do
    AnimChannels[SignalID].KeyPoints[i] := KPs[i];
end;

procedure AddTranslationChannel(SignalID: Integer; Duration: Single;
  AX, AY, AZ: Single; const KPs: array of TAnimKeyPoint);
var
  i: Integer;
begin
  AnimChannels[SignalID].SignalID := SignalID;
  AnimChannels[SignalID].Duration := Duration;
  AnimChannels[SignalID].AxisX := AX;
  AnimChannels[SignalID].AxisY := AY;
  AnimChannels[SignalID].AxisZ := AZ;
  AnimChannels[SignalID].Infinity := False;
  AnimChannels[SignalID].ChannelType := actTranslation;
  SetLength(AnimChannels[SignalID].KeyPoints, Length(KPs));
  for i := 0 to High(KPs) do
    AnimChannels[SignalID].KeyPoints[i] := KPs[i];
end;

procedure AddMaterialChannel(SignalID: Integer; Duration: Single;
  const KPs: array of TAnimKeyPoint);
var
  i: Integer;
begin
  AnimChannels[SignalID].SignalID := SignalID;
  AnimChannels[SignalID].Duration := Duration;
  AnimChannels[SignalID].AxisX := 0; AnimChannels[SignalID].AxisY := 0; AnimChannels[SignalID].AxisZ := 0;
  AnimChannels[SignalID].Infinity := False;
  AnimChannels[SignalID].ChannelType := actMaterial;
  SetLength(AnimChannels[SignalID].KeyPoints, Length(KPs));
  for i := 0 to High(KPs) do
    AnimChannels[SignalID].KeyPoints[i] := KPs[i];
end;

// ============================================================================
// Заполнение каналов анимации
// ============================================================================
procedure InitAnimChannels;
begin
  if AnimChannelsInitialized then Exit;

  // Контроллер тяги — как RRS controller_driver.xml
  AddRotationChannel(SIGNAL_KM_HANDLE, 20.0, -1, 0, 0, False,
    [KP(-1.1, -55), KP(-1.0, -47), KP(-0.1, -8),
     KP(0.0, 0), KP(0.1, 8), KP(1.0, 55)]);

  // Контроллер тормоза — как RRS controller_braking.xml
  AddRotationChannel(SIGNAL_BRAKE_HANDLE, 20.0, 1, 0, 0, False,
    [KP(0.0, 0), KP(0.5, 30), KP(1.0, 60)]);

  // Стрелка ПМ (давление) — как arrow_PM.xml
  AddRotationChannel(SIGNAL_ARROW_PM, 20.0, 0, -1, 0, False,
    [KP(0.0, 0), KP(0.2333, 50.8), KP(0.5, 124.6),
     KP(0.7, 178.96), KP(1.0, 260.9)]);

  // Стрелка ТМ
  AddRotationChannel(SIGNAL_ARROW_BP, 20.0, 0, -1, 0, False,
    [KP(0.0, 0), KP(0.2333, 50.8), KP(0.5, 124.6),
     KP(0.7, 178.96), KP(1.0, 260.9)]);

  // Стрелка ТЦ1
  AddRotationChannel(SIGNAL_ARROW_BC_FWD, 20.0, 0, -1, 0, False,
    [KP(0.0, 0), KP(0.2333, 50.8), KP(0.5, 124.6),
     KP(0.7, 178.96), KP(1.0, 260.9)]);

  // Стрелка ТЦ2
  AddRotationChannel(SIGNAL_ARROW_BC_BWD, 20.0, 0, -1, 0, False,
    [KP(0.0, 0), KP(0.2333, 50.8), KP(0.5, 124.6),
     KP(0.7, 178.96), KP(1.0, 260.9)]);

  // Вольтметр 24В
  AddRotationChannel(SIGNAL_ARROW_VOLT_24V, 20.0, 0, -1, 0, False,
    [KP(0.0, 0), KP(1.0, 260.9)]);

  // Вольтметр 110В
  AddRotationChannel(SIGNAL_ARROW_VOLT_110V, 20.0, 0, -1, 0, False,
    [KP(0.0, 0), KP(1.0, 260.9)]);

  // Прожектор (материал — заглушка для будущего использования)
  AddMaterialChannel(SIGNAL_SPOTLIGHT, 10.0,
    [KP(0.0, 0.01), KP(1.0, 1.0)]);

  // Освещение салона
  AddMaterialChannel(SIGNAL_SALON_LIGHT, 10.0,
    [KP(0.0, 0.01), KP(1.0, 1.0)]);

  AnimChannelsInitialized := True;
end;

// ============================================================================
// Заполнение аналоговых сигналов
// ============================================================================
procedure FillAnalogSignals;
var
  pm, bp, tc: Single;
begin
  if not AnimChannelsInitialized then
    InitAnimChannels;

  // Давления из KlubData
  pm := GetPressureTMf;  // ТМ
  bp := GetPressureURf;  // УР (аналог ПМ)
  tc := GetPressureTCf;  // ТЦ

  // Нормализация 0..1: ПМ/ТМ макс ~12 кгс/см², ТЦ макс ~6 кгс/см²
  AnalogSignal[SIGNAL_ARROW_PM]     := pm / 12.0;
  AnalogSignal[SIGNAL_ARROW_BP]     := bp / 12.0;
  AnalogSignal[SIGNAL_ARROW_BC_FWD] := tc / 6.0;
  AnalogSignal[SIGNAL_ARROW_BC_BWD] := tc / 6.0;

  // Вольтметры (заглушка)
  AnalogSignal[SIGNAL_ARROW_VOLT_24V]  := 0.7;
  AnalogSignal[SIGNAL_ARROW_VOLT_110V] := 0.73;
end;

end.
