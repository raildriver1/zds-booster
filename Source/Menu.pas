unit Menu;

interface
uses DrawFunc2D, Variables, Windows, SysUtils;

type
  TMenuTab = (mtRender, mtSAVPE, mtSAUT);

  TSAVPESettings = record
    Enabled: Boolean;
    PosX, PosY, PosZ: Single;
  end;
  
  TSAUTSettings = record
    Enabled: Boolean;
    PosX, PosY, PosZ: Single;
  end;
  
  TMenuSettings = record
    SAVPE: TSAVPESettings;
    SAUT: TSAUTSettings;
  end;

var
  MenuVisible: Boolean = False;
  MenuAnimation: Single = 0.0;
  CurrentTab: TMenuTab = mtRender;
  Settings: TMenuSettings;
  AnimationSpeed: Single = 8.0;

procedure InitMenu;
procedure UpdateMenu;
procedure DrawMenu;
procedure ToggleMenu;
procedure HandleMenuClick(X, Y: Integer);
function IsPointInRect(X, Y, RX, RY, RW, RH: Integer): Boolean;

implementation

const
  VK_INSERT = 45;

procedure InitMenu;
begin
  // Инициализация настроек по умолчанию
  Settings.SAVPE.Enabled := False;
  Settings.SAVPE.PosX := 0.0;
  Settings.SAVPE.PosY := 0.0;
  Settings.SAVPE.PosZ := 0.0;
  
  Settings.SAUT.Enabled := False;
  Settings.SAUT.PosX := 0.0;
  Settings.SAUT.PosY := 0.0;
  Settings.SAUT.PosZ := 0.0;
end;

procedure UpdateMenu;
begin
  if MenuVisible then
  begin
    if MenuAnimation < 1.0 then
      MenuAnimation := MenuAnimation + AnimationSpeed * (PROCESS_INTERVAL / 1000.0);
    if MenuAnimation > 1.0 then
      MenuAnimation := 1.0;
  end
  else
  begin
    if MenuAnimation > 0.0 then
      MenuAnimation := MenuAnimation - AnimationSpeed * (PROCESS_INTERVAL / 1000.0);
    if MenuAnimation < 0.0 then
      MenuAnimation := 0.0;
  end;
end;

function EaseOutCubic(t: Single): Single;
begin
  t := t - 1;
  Result := t * t * t + 1;
end;

function IsPointInRect(X, Y, RX, RY, RW, RH: Integer): Boolean;
begin
  Result := (X >= RX) and (X <= RX + RW) and (Y >= RY) and (Y <= RY + RH);
end;

procedure DrawCheckbox(X, Y: Integer; Checked: Boolean; Text: string);
const
  CheckboxSize = 18;
  CheckboxColor = $404040;
  CheckedColor = $00AA00;
  TextColor = $FFFFFF;
begin
  // Рамка чекбокса
  DrawRectangle2D(X, Y, CheckboxSize, CheckboxSize, $606060, 255, False);
  
  // Заливка чекбокса
  if Checked then
    DrawRectangle2D(X + 2, Y + 2, CheckboxSize - 4, CheckboxSize - 4, CheckedColor, 255, True)
  else
    DrawRectangle2D(X + 2, Y + 2, CheckboxSize - 4, CheckboxSize - 4, CheckboxColor, 255, True);
    
  // Галочка
  if Checked then
  begin
    DrawLine2D(X + 5, Y + 9, X + 8, Y + 12, $FFFFFF, 255, 2.0, True);
    DrawLine2D(X + 8, Y + 12, X + 13, Y + 7, $FFFFFF, 255, 2.0, True);
  end;
  
  // Текст
  DrawText2D(0, X + CheckboxSize + 8, Y + 2, Text, TextColor, 255, 0.8);
end;

procedure DrawSlider(X, Y, Width: Integer; var Value: Single; MinVal, MaxVal: Single; Text: string);
const
  SliderHeight = 20;
  HandleWidth = 12;
  TextColor = $FFFFFF;
  SliderColor = $404040;
  HandleColor = $606060;
var
  HandlePos: Integer;
  ValueText: string;
begin
  // Текст слайдера
  DrawText2D(0, X, Y - 20, Text, TextColor, 255, 0.8);
  
  // Фон слайдера
  DrawRectangle2D(X, Y, Width, SliderHeight, SliderColor, 255, True);
  
  // Ручка слайдера
  HandlePos := Round((Value - MinVal) / (MaxVal - MinVal) * (Width - HandleWidth));
  DrawRectangle2D(X + HandlePos, Y - 2, HandleWidth, SliderHeight + 4, HandleColor, 255, True);
  
  // Значение
  ValueText := Format('%.2f', [Value]);
  DrawText2D(0, X + Width + 10, Y + 2, ValueText, TextColor, 255, 0.8);
end;

procedure DrawTab(X, Y, Width, Height: Integer; Active: Boolean; Text: string);
const
  ActiveColor = $505050;
  InactiveColor = $303030;
  TextColor = $FFFFFF;
  ActiveTextColor = $00AA00;
var
  BgColor, TxtColor: Cardinal;
begin
  if Active then
  begin
    BgColor := ActiveColor;
    TxtColor := ActiveTextColor;
  end
  else
  begin
    BgColor := InactiveColor;
    TxtColor := TextColor;
  end;
  
  DrawRectangle2D(X, Y, Width, Height, BgColor, 255, True);
  DrawRectangle2D(X, Y, Width, Height, $606060, 255, False);
  
  // Центрируем текст
  DrawText2D(0, X + Width div 2 - Length(Text) * 4, Y + Height div 2 - 8, Text, TxtColor, 255, 0.9);
end;

procedure DrawMenu;
const
  MenuWidth = 420;
  MenuHeight = 350;
  TabHeight = 35;
  TabWidth = 120;
var
  MenuX, MenuY: Integer;
  AnimProgress: Single;
  ContentY: Integer;
begin
  if MenuAnimation <= 0.0 then Exit;
  
  AnimProgress := EaseOutCubic(MenuAnimation);
  
  MenuX := (InitResX - MenuWidth) div 2;
  MenuY := Round((InitResY - MenuHeight) div 2 - (1.0 - AnimProgress) * 100);
  
  Begin2D;
  try
    // Полупрозрачный фон
    DrawRectangle2D(0, 0, InitResX, InitResY, $000000, Round(120 * AnimProgress), True);
    
    // Основное окно меню
    DrawRectangle2D(MenuX, MenuY, MenuWidth, MenuHeight, $252525, Round(255 * AnimProgress), True);
    DrawRectangle2D(MenuX, MenuY, MenuWidth, MenuHeight, $505050, Round(255 * AnimProgress), False);
    
    // Заголовок
    DrawText2D(0, MenuX + 15, MenuY + 10, 'ЧС7 - Меню настроек', $FFFFFF, Round(255 * AnimProgress), 1.2);
    
    // Вкладки
    DrawTab(MenuX + 15, MenuY + 40, TabWidth, TabHeight, CurrentTab = mtRender, 'Рендер');
    DrawTab(MenuX + 15 + TabWidth, MenuY + 40, TabWidth, TabHeight, CurrentTab = mtSAVPE, 'Поз. САВПЭ');
    DrawTab(MenuX + 15 + TabWidth * 2, MenuY + 40, TabWidth, TabHeight, CurrentTab = mtSAUT, 'Поз. САУТ');
    
    // Линия под вкладками
    DrawLine2D(MenuX + 15, MenuY + 75, MenuX + MenuWidth - 15, MenuY + 75, $505050, Round(255 * AnimProgress), 1.0);
    
    ContentY := MenuY + 90;
    
    // Содержимое вкладок
    case CurrentTab of
      mtRender:
      begin
        DrawCheckbox(MenuX + 30, ContentY + 20, Settings.SAVPE.Enabled, 'Включить САВПЭ');
        DrawCheckbox(MenuX + 30, ContentY + 50, Settings.SAUT.Enabled, 'Включить САУТ');
      end;
      
      mtSAVPE:
      begin
        DrawText2D(0, MenuX + 30, ContentY, 'Настройки позиции САВПЭ:', $FFFFFF, Round(255 * AnimProgress), 1.0);
        DrawSlider(MenuX + 30, ContentY + 40, 250, Settings.SAVPE.PosX, -10.0, 10.0, 'Позиция X:');
        DrawSlider(MenuX + 30, ContentY + 90, 250, Settings.SAVPE.PosY, -10.0, 10.0, 'Позиция Y:');
        DrawSlider(MenuX + 30, ContentY + 140, 250, Settings.SAVPE.PosZ, -10.0, 10.0, 'Позиция Z:');
      end;
      
      mtSAUT:
      begin
        DrawText2D(0, MenuX + 30, ContentY, 'Настройки позиции САУТ:', $FFFFFF, Round(255 * AnimProgress), 1.0);
        DrawSlider(MenuX + 30, ContentY + 40, 250, Settings.SAUT.PosX, -10.0, 10.0, 'Позиция X:');
        DrawSlider(MenuX + 30, ContentY + 90, 250, Settings.SAUT.PosY, -10.0, 10.0, 'Позиция Y:');
        DrawSlider(MenuX + 30, ContentY + 140, 250, Settings.SAUT.PosZ, -10.0, 10.0, 'Позиция Z:');
      end;
    end;
    
    // Инструкция
    DrawText2D(0, MenuX + 30, MenuY + MenuHeight - 40, 'Insert - закрыть меню', $808080, Round(255 * AnimProgress), 0.8);
    
  finally
    End2D;
  end;
end;

procedure ToggleMenu;
begin
  MenuVisible := not MenuVisible;
end;

procedure HandleMenuClick(X, Y: Integer);
const
  MenuWidth = 420;
  MenuHeight = 350;
  TabHeight = 35;
  TabWidth = 120;
var
  MenuX, MenuY: Integer;
  ContentY: Integer;
begin
  if MenuAnimation < 1.0 then Exit;
  
  MenuX := (InitResX - MenuWidth) div 2;
  MenuY := (InitResY - MenuHeight) div 2;
  ContentY := MenuY + 90;
  
  // Проверка кликов по вкладкам
  if IsPointInRect(X, Y, MenuX + 15, MenuY + 40, TabWidth, TabHeight) then
    CurrentTab := mtRender
  else if IsPointInRect(X, Y, MenuX + 15 + TabWidth, MenuY + 40, TabWidth, TabHeight) then
    CurrentTab := mtSAVPE
  else if IsPointInRect(X, Y, MenuX + 15 + TabWidth * 2, MenuY + 40, TabWidth, TabHeight) then
    CurrentTab := mtSAUT;
  
  // Обработка кликов по содержимому
  case CurrentTab of
    mtRender:
    begin
      // Чекбокс САВПЭ
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 20, 18, 18) then
        Settings.SAVPE.Enabled := not Settings.SAVPE.Enabled;
        
      // Чекбокс САУТ
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 50, 18, 18) then
        Settings.SAUT.Enabled := not Settings.SAUT.Enabled;
    end;
    
    mtSAVPE:
    begin
      // Слайдеры для САВПЭ
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 40, 250, 20) then
        Settings.SAVPE.PosX := -10.0 + ((X - MenuX - 30) / 250.0) * 20.0;
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 90, 250, 20) then
        Settings.SAVPE.PosY := -10.0 + ((X - MenuX - 30) / 250.0) * 20.0;
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 140, 250, 20) then
        Settings.SAVPE.PosZ := -10.0 + ((X - MenuX - 30) / 250.0) * 20.0;
    end;
    
    mtSAUT:
    begin
      // Слайдеры для САУТ
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 40, 250, 20) then
        Settings.SAUT.PosX := -10.0 + ((X - MenuX - 30) / 250.0) * 20.0;
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 90, 250, 20) then
        Settings.SAUT.PosY := -10.0 + ((X - MenuX - 30) / 250.0) * 20.0;
      if IsPointInRect(X, Y, MenuX + 30, ContentY + 140, 250, 20) then
        Settings.SAUT.PosZ := -10.0 + ((X - MenuX - 30) / 250.0) * 20.0;
    end;
  end;
end;

end.
