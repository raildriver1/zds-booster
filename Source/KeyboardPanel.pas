unit KeyboardPanel;

interface

uses
  Windows, SysUtils, Classes, Math, MMSystem, OpenGL, DrawFunc2D;

// ===== КОНСТАНТЫ =====
const
  // Размеры панели
  PANEL_WIDTH = 280;
  PANEL_TARGET_HEIGHT = 200.0;
  PANEL_HIDDEN_HEIGHT = 20.0;
  PANEL_DETECTION_ZONE = 100;
  PANEL_DETECTION_MARGIN = 50;
  
  // Размеры кнопок
  BUTTON_WIDTH = 40;
  BUTTON_HEIGHT = 40;
  BUTTON_SPACING = 50;
  ENTER_BUTTON_WIDTH = 140;
  
  // Цвета и прозрачность
  PANEL_ALPHA = 200;
  BUTTON_BORDER_ALPHA = 150;
  BUTTON_HOVER_ALPHA = 100;
  BUTTON_PRESSED_ALPHA = 180;
  TEXT_ALPHA = 200;
  
  COLOR_WHITE = $FFFFFF;
  COLOR_YELLOW = $FFFF00;
  COLOR_BLUE = $0000FF;
  
  // Анимация
  ANIMATION_SPEED_MULTIPLIER = 8.0;
  UPDATE_INTERVAL = 16; // ~60 FPS

var
  InitResX: Integer = 800;
  InitResY: Integer = 600;

// ===== ТИПЫ =====
type
  TKeyButton = record
    X, Y: Integer;
    Width, Height: Integer;
    KeyChar: string;
    IsPressed: Boolean;
    LastPressTime: Cardinal;
  end;

  TKeyboardState = record
    PanelPosition: Single;
    TargetPosition: Single;
    IsVisible: Boolean;
    MouseOverButton: Integer; // -1 если нет наведения
    LastUpdateTime: Cardinal;
    LastAnimationTime: Cardinal;
  end;

  TKeyboardCallback = procedure(const KeyValue: string; IsEnter: Boolean);

// ===== ОСНОВНОЙ КЛАСС КЛАВИАТУРЫ =====
type
  TBoosterKeyboard = class
  private
    FButtons: array[0..11] of TKeyButton;
    FState: TKeyboardState;
    FTextureID: Cardinal;
    FFont: Integer;
    FInitialized: Boolean;
    FScreenWidth: Integer;
    FScreenHeight: Integer;
    FOnKeyPressed: TKeyboardCallback;
    FCommandBuffer: string;
    FEnabled: Boolean;
    
    // Внутренние методы
    procedure InitializeButtons;
    procedure LoadResources;
    function GetPanelBounds(out PanelLeft, PanelBottom: Integer): Boolean;
    function IsMouseInDetectionZone(MouseX, MouseY: Integer): Boolean;
    function IsMouseOverButton(MouseX, MouseY: Integer; ButtonIndex: Integer): Boolean;
    procedure UpdateAnimation;
    procedure ProcessInput;
    procedure RenderPanel;
    procedure RenderButtons;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Основные методы
    procedure Initialize;
    procedure Update;
    procedure Render;
    procedure Reset;
    procedure SetEnabled(Enabled: Boolean);
    
    // События
    procedure SetCallback(Callback: TKeyboardCallback);
    
    // Свойства
    property CommandBuffer: string read FCommandBuffer;
    property IsVisible: Boolean read FState.IsVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

// ===== ГЛОБАЛЬНЫЕ ФУНКЦИИ =====
procedure InitializeBoosterKeyboard; stdcall;
procedure UpdateBoosterKeyboard; stdcall;
procedure RenderBoosterKeyboard; stdcall;
procedure SetBoosterKeyboardCallback(Callback: TKeyboardCallback); stdcall;
function GetBoosterKeyboardCommand: string; stdcall;
procedure ResetBoosterKeyboard; stdcall;
procedure SetBoosterKeyboardEnabled(Enabled: Boolean); stdcall;

implementation

var
  GlobalKeyboard: TBoosterKeyboard = nil;


// ===== ВНЕШНИЕ ФУНКЦИИ ДВИЖКА =====
function LoadTextureFromFile(const Filename: string; Param1: Integer; Param2: Integer): Cardinal; external 'DGLEngine.dll';
procedure GetMousePos(var MousePos: TPoint); external 'DGLEngine.dll';
function IsLeftMouseButtonPressed: Boolean; external 'DGLEngine.dll';

// ===== РЕАЛИЗАЦИЯ КЛАССА =====

constructor TBoosterKeyboard.Create;
begin
  inherited Create;
  FillChar(FState, SizeOf(FState), 0);
  FState.PanelPosition := PANEL_HIDDEN_HEIGHT;
  FState.TargetPosition := PANEL_HIDDEN_HEIGHT;
  FState.MouseOverButton := -1;
  FState.LastUpdateTime := 0;
  FState.LastAnimationTime := 0;
  
  FTextureID := 0;
  FFont := 0;
  FInitialized := False;
  FCommandBuffer := '';
  FEnabled := True;
  FOnKeyPressed := nil;
  
  // Получаем размеры экрана
  FScreenWidth := InitResX;
  FScreenHeight := InitResY;
end;

destructor TBoosterKeyboard.Destroy;
begin
  // Освобождение ресурсов если нужно
  inherited Destroy;
end;

procedure TBoosterKeyboard.InitializeButtons;
var
  i: Integer;
begin
  // Первый ряд: 1,2,3,4,5
  FButtons[1].X := 20;   FButtons[1].Y := 20;  FButtons[1].KeyChar := '1';
  FButtons[2].X := 70;   FButtons[2].Y := 20;  FButtons[2].KeyChar := '2';
  FButtons[3].X := 120;  FButtons[3].Y := 20;  FButtons[3].KeyChar := '3';
  FButtons[4].X := 170;  FButtons[4].Y := 20;  FButtons[4].KeyChar := '4';
  FButtons[5].X := 220;  FButtons[5].Y := 20;  FButtons[5].KeyChar := '5';
  
  // Второй ряд: 6,7,8,9,0
  FButtons[6].X := 20;   FButtons[6].Y := 70;  FButtons[6].KeyChar := '6';
  FButtons[7].X := 70;   FButtons[7].Y := 70;  FButtons[7].KeyChar := '7';
  FButtons[8].X := 120;  FButtons[8].Y := 70;  FButtons[8].KeyChar := '8';
  FButtons[9].X := 170;  FButtons[9].Y := 70;  FButtons[9].KeyChar := '9';
  FButtons[0].X := 220;  FButtons[0].Y := 70;  FButtons[0].KeyChar := '0';
  
  // Служебные кнопки
  FButtons[10].X := 20;  FButtons[10].Y := 120; FButtons[10].KeyChar := 'ENTER';
  FButtons[11].X := 170; FButtons[11].Y := 120; FButtons[11].KeyChar := 'ESC';
  
  // Установка размеров
  for i := 0 to 11 do
  begin
    FButtons[i].IsPressed := False;
    FButtons[i].LastPressTime := 0;
    
    if i = 10 then // ENTER
    begin
      FButtons[i].Width := ENTER_BUTTON_WIDTH;
      FButtons[i].Height := BUTTON_HEIGHT;
    end
    else
    begin
      FButtons[i].Width := BUTTON_WIDTH;
      FButtons[i].Height := BUTTON_HEIGHT;
    end;
  end;
end;

procedure TBoosterKeyboard.LoadResources;
begin
  if FTextureID = 0 then
  begin
    try
      FTextureID := LoadTextureFromFile('booster\klava_block.bmp', 0, -1);
    except
      FTextureID := 0; // Будем рисовать без текстуры
    end;
  end;
end;

function TBoosterKeyboard.GetPanelBounds(out PanelLeft, PanelBottom: Integer): Boolean;
begin
  PanelLeft := (FScreenWidth - PANEL_WIDTH) div 2;
  PanelBottom := FScreenHeight;
  Result := True;
end;

function TBoosterKeyboard.IsMouseInDetectionZone(MouseX, MouseY: Integer): Boolean;
var
  PanelLeft, PanelBottom: Integer;
begin
  GetPanelBounds(PanelLeft, PanelBottom);
  Result := (MouseY > FScreenHeight - PANEL_DETECTION_ZONE) and
            (MouseX > PanelLeft - PANEL_DETECTION_MARGIN) and 
            (MouseX < PanelLeft + PANEL_WIDTH + PANEL_DETECTION_MARGIN);
end;

function TBoosterKeyboard.IsMouseOverButton(MouseX, MouseY: Integer; ButtonIndex: Integer): Boolean;
var
  PanelLeft, PanelBottom: Integer;
  ButtonX, ButtonY: Integer;
begin
  Result := False;
  if (ButtonIndex < 0) or (ButtonIndex > 11) then Exit;
  
  GetPanelBounds(PanelLeft, PanelBottom);
  ButtonX := PanelLeft + FButtons[ButtonIndex].X;
  ButtonY := PanelBottom - Round(FState.PanelPosition) + FButtons[ButtonIndex].Y;
  
  Result := (MouseX >= ButtonX) and 
            (MouseX <= ButtonX + FButtons[ButtonIndex].Width) and
            (MouseY >= ButtonY) and 
            (MouseY <= ButtonY + FButtons[ButtonIndex].Height);
end;

procedure TBoosterKeyboard.UpdateAnimation;
var
  CurrentTime: Cardinal;
  DeltaTime: Single;
  AnimSpeed: Single;
begin
  CurrentTime := timeGetTime;
  
  if FState.LastAnimationTime = 0 then
    FState.LastAnimationTime := CurrentTime;
    
  DeltaTime := (CurrentTime - FState.LastAnimationTime) / 1000.0; // в секундах
  FState.LastAnimationTime := CurrentTime;
  
  if Abs(FState.TargetPosition - FState.PanelPosition) > 0.1 then
  begin
    AnimSpeed := ANIMATION_SPEED_MULTIPLIER * DeltaTime * 60.0; // нормализация к 60 FPS
    
    FState.PanelPosition := FState.PanelPosition + 
      (FState.TargetPosition - FState.PanelPosition) * AnimSpeed;
  end
  else
  begin
    FState.PanelPosition := FState.TargetPosition;
  end;
  
  FState.IsVisible := FState.PanelPosition > PANEL_HIDDEN_HEIGHT + 5;
end;

procedure TBoosterKeyboard.ProcessInput;
var
  MousePos: TPoint;
  MouseX, MouseY: Integer;
  LeftButtonPressed: Boolean;
  i: Integer;
  CurrentTime: Cardinal;
  IsButtonHovered: Boolean;
const
  DEBOUNCE_TIME = 150; // 150мс защита от дребезга
begin
  if not FEnabled then Exit;
  
  CurrentTime := timeGetTime;
  
  // Получаем состояние мыши
  try
    GetMousePos(MousePos);
    MouseX := MousePos.X;
    MouseY := MousePos.Y;
    LeftButtonPressed := IsLeftMouseButtonPressed;
  except
    Exit;
  end;
  
  // Определяем целевую позицию панели
  if IsMouseInDetectionZone(MouseX, MouseY) then
    FState.TargetPosition := PANEL_TARGET_HEIGHT
  else
    FState.TargetPosition := PANEL_HIDDEN_HEIGHT;
  
  // Сброс состояния наведения
  FState.MouseOverButton := -1;
  
  // Обработка кнопок только если панель видна
  if FState.IsVisible then
  begin
    for i := 0 to 11 do
    begin
      IsButtonHovered := IsMouseOverButton(MouseX, MouseY, i);
      
      if IsButtonHovered then
      begin
        FState.MouseOverButton := i;
        
        // Обработка клика с защитой от дребезга
        if LeftButtonPressed and 
           ((CurrentTime - FButtons[i].LastPressTime) > DEBOUNCE_TIME) then
        begin
          FButtons[i].IsPressed := True;
          FButtons[i].LastPressTime := CurrentTime;
          
          // Вызываем callback
          if Assigned(FOnKeyPressed) then
          begin
            if FButtons[i].KeyChar = 'ENTER' then
            begin
              FOnKeyPressed(FCommandBuffer, True);
              FCommandBuffer := ''; // Очищаем буфер после ENTER
            end
            else if FButtons[i].KeyChar = 'ESC' then
            begin
              FCommandBuffer := ''; // Очищаем буфер при ESC
              FOnKeyPressed('', False);
            end
            else
            begin
              FCommandBuffer := FCommandBuffer + FButtons[i].KeyChar;
              FOnKeyPressed(FButtons[i].KeyChar, False);
            end;
          end;
        end;
      end;
      
      // Сброс состояния нажатия через некоторое время
      if FButtons[i].IsPressed and 
         ((CurrentTime - FButtons[i].LastPressTime) > 100) then
      begin
        FButtons[i].IsPressed := False;
      end;
    end;
  end;
end;

procedure TBoosterKeyboard.RenderPanel;
var
  PanelLeft, PanelBottom: Integer;
  PanelHeight: Integer;
begin
  if not FState.IsVisible then Exit;
  
  GetPanelBounds(PanelLeft, PanelBottom);
  PanelHeight := Round(FState.PanelPosition);
  
  Begin2D;
  try
    // Рисуем фон панели
    if FTextureID > 0 then
    begin
      // С текстурой
      DrawTexture2D(FTextureID, PanelLeft, PanelBottom - PanelHeight, 
                   PANEL_WIDTH, PanelHeight, 0, PANEL_ALPHA, COLOR_WHITE);
    end
    else
    begin
      // Без текстуры - простой прямоугольник
      DrawRectangle2D(PanelLeft, PanelBottom - PanelHeight, 
                     PANEL_WIDTH, PanelHeight, $404040, PANEL_ALPHA, True);
    end;
  finally
    End2D;
  end;
end;

procedure TBoosterKeyboard.RenderButtons;
var
  PanelLeft, PanelBottom: Integer;
  ButtonX, ButtonY: Integer;
  i: Integer;
  ButtonColor: Cardinal;
  ButtonAlpha: Integer;
begin
  if not FState.IsVisible then Exit;
  
  GetPanelBounds(PanelLeft, PanelBottom);
  
  Begin2D;
  try
    // Рисуем все кнопки
    for i := 0 to 11 do
    begin
      ButtonX := PanelLeft + FButtons[i].X;
      ButtonY := PanelBottom - Round(FState.PanelPosition) + FButtons[i].Y;
      
      // Определяем цвет и прозрачность кнопки
      if FButtons[i].IsPressed then
      begin
        ButtonColor := COLOR_YELLOW;
        ButtonAlpha := BUTTON_PRESSED_ALPHA;
      end
      else if FState.MouseOverButton = i then
      begin
        ButtonColor := COLOR_WHITE;
        ButtonAlpha := BUTTON_HOVER_ALPHA;
      end
      else
      begin
        ButtonColor := COLOR_WHITE;
        ButtonAlpha := 0; // Невидимая кнопка, только рамка
      end;
      
      // Рисуем фон кнопки если нужно
      if ButtonAlpha > 0 then
      begin
        DrawRectangle2D(ButtonX, ButtonY, FButtons[i].Width, FButtons[i].Height,
                       ButtonColor, ButtonAlpha, True);
      end;
      
      // Рисуем рамку кнопки
      DrawRectangle2D(ButtonX, ButtonY, FButtons[i].Width, FButtons[i].Height,
                     COLOR_WHITE, BUTTON_BORDER_ALPHA, False);
      
      // Рисуем текст кнопки
      if FFont > 0 then
      begin
        DrawText2D(FFont, ButtonX + 5, ButtonY + 5, FButtons[i].KeyChar,
                  COLOR_WHITE, TEXT_ALPHA, 1.0);
      end;
    end;
    
    // Показываем буфер команд если он не пустой
    if FCommandBuffer <> '' then
    begin
      DrawText2D(FFont, PanelLeft + 10, PanelBottom - Round(FState.PanelPosition) - 20,
                'Command: ' + FCommandBuffer, COLOR_YELLOW, 255, 1.2);
    end;
    
  finally
    End2D;
  end;
end;

procedure TBoosterKeyboard.Initialize;
begin
  if FInitialized then Exit;
  
  InitializeButtons;
  LoadResources;
  
  FState.LastUpdateTime := timeGetTime;
  FInitialized := True;
end;

procedure TBoosterKeyboard.Update;
var
  CurrentTime: Cardinal;
begin
  if not FInitialized then Initialize;
  if not FEnabled then Exit;
  
  CurrentTime := timeGetTime;
  
  // Ограничиваем частоту обновления
  if (CurrentTime - FState.LastUpdateTime) < UPDATE_INTERVAL then
    Exit;
    
  FState.LastUpdateTime := CurrentTime;
  
  ProcessInput;
  UpdateAnimation;
end;

procedure TBoosterKeyboard.Render;
begin
  if not FInitialized or not FEnabled then Exit;
  
  RenderPanel;
  RenderButtons;
end;

procedure TBoosterKeyboard.Reset;
begin
  FCommandBuffer := '';
  FState.PanelPosition := PANEL_HIDDEN_HEIGHT;
  FState.TargetPosition := PANEL_HIDDEN_HEIGHT;
  FState.IsVisible := False;
  FState.MouseOverButton := -1;
end;

procedure TBoosterKeyboard.SetEnabled(Enabled: Boolean);
begin
  FEnabled := Enabled;
  if not Enabled then
    Reset;
end;

procedure TBoosterKeyboard.SetCallback(Callback: TKeyboardCallback);
begin
  FOnKeyPressed := Callback;
end;

// ===== ГЛОБАЛЬНЫЕ ФУНКЦИИ =====

procedure InitializeBoosterKeyboard; stdcall;
begin
  if not Assigned(GlobalKeyboard) then
  begin
    GlobalKeyboard := TBoosterKeyboard.Create;
    GlobalKeyboard.Initialize;
  end;
end;

procedure UpdateBoosterKeyboard; stdcall;
begin
  if Assigned(GlobalKeyboard) then
    GlobalKeyboard.Update;
end;

procedure RenderBoosterKeyboard; stdcall;
begin
  if Assigned(GlobalKeyboard) then
    GlobalKeyboard.Render;
end;

procedure SetBoosterKeyboardCallback(Callback: TKeyboardCallback); stdcall;
begin
  if Assigned(GlobalKeyboard) then
    GlobalKeyboard.SetCallback(Callback);
end;

function GetBoosterKeyboardCommand: string; stdcall;
begin
  if Assigned(GlobalKeyboard) then
    Result := GlobalKeyboard.CommandBuffer
  else
    Result := '';
end;

procedure ResetBoosterKeyboard; stdcall;
begin
  if Assigned(GlobalKeyboard) then
    GlobalKeyboard.Reset;
end;

procedure SetBoosterKeyboardEnabled(Enabled: Boolean); stdcall;
begin
  if Assigned(GlobalKeyboard) then
    GlobalKeyboard.SetEnabled(Enabled);
end;

// ===== ИНИЦИАЛИЗАЦИЯ И ОЧИСТКА =====

initialization
  GlobalKeyboard := nil;

finalization
  if Assigned(GlobalKeyboard) then
  begin
    GlobalKeyboard.Free;
    GlobalKeyboard := nil;
  end;

end.
