//----------------------------------------------------------------------------//
// RA3HintOverlay.pas — система подсказок-плашек в правом нижнем углу.        //
//                                                                            //
// Используется когда какая-то функция РА-3 заблокирована и игрок должен      //
// что-то сделать (например, запустить дизели). Каждый кадр модуль рисует     //
// чёрные прямоугольники с белым текстом, стек по вертикали снизу вверх.      //
//                                                                            //
// Жизненный цикл (в DrawRA3 каждый кадр):                                    //
//   1. ResetHints       — очищаем список подсказок                           //
//   2. AddHint('...')   — добавляем по одной для каждого активного блока     //
//   3. RenderHints      — отрисовка (один раз в конце DrawRA3)               //
//                                                                            //
// Размеры/позиция вычисляются от InitResX/InitResY (обновляются в            //
// EngineCore при ресайзе окна), поэтому overlay масштабируется под           //
// разрешение окна.                                                           //
//----------------------------------------------------------------------------//
unit RA3HintOverlay;

interface

procedure ResetHints;
procedure AddHint(const Text: string);
procedure RenderHints;
function  HintCount: Integer;

implementation

uses
  SysUtils, Classes, Variables, DrawFunc2D;

const
  // Параметры внешнего вида плашки
  HINT_FONT_SCALE      = 0.65;        // Размер шрифта (см. CheatMenu — там 0.65 стандарт)
  HINT_PADDING_X       = 12;          // Отступ слева/справа внутри плашки
  HINT_PADDING_Y       = 8;           // Отступ сверху/снизу внутри плашки
  HINT_MARGIN_RIGHT    = 16;          // Отступ от правого края экрана
  HINT_MARGIN_BOTTOM   = 16;          // Отступ от нижнего края экрана
  HINT_GAP             = 6;           // Зазор между плашками когда их несколько
  HINT_BG_COLOR        = $000000;     // Чёрный фон
  HINT_BG_ALPHA        = 220;         // Полупрозрачный (220/255)
  HINT_BORDER_COLOR    = $FFFFFF;     // Белая рамка
  HINT_BORDER_ALPHA    = 90;          // Едва заметная
  HINT_TEXT_COLOR      = $FFFFFF;     // Белый текст
  HINT_TEXT_ALPHA      = 255;         // Полностью непрозрачный
  HINT_SHADOW_COLOR    = $000000;     // Тень текста для читабельности
  HINT_SHADOW_ALPHA    = 200;
  HINT_TEXT_HEIGHT_PX  = 16;          // Базовая высота строки до scale
  HINT_MAX             = 16;          // Максимум подсказок одновременно

var
  Hints: array[0..HINT_MAX - 1] of string;
  HintsCount: Integer = 0;

procedure ResetHints;
begin
  HintsCount := 0;
end;

procedure AddHint(const Text: string);
begin
  if HintsCount >= HINT_MAX then Exit;
  if Text = '' then Exit;
  Hints[HintsCount] := Text;
  Inc(HintsCount);
end;

function HintCount: Integer;
begin
  Result := HintsCount;
end;

// Высота одной плашки (зависит от scale).
function HintBoxHeight: Integer;
begin
  Result := Round(HINT_TEXT_HEIGHT_PX * HINT_FONT_SCALE) + HINT_PADDING_Y * 2;
end;

procedure RenderHints;
var
  i: Integer;
  scrW, scrH: Integer;
  textW, boxW, boxH: Integer;
  boxX, boxY: Integer;
  textX, textY: Integer;
begin
  if HintsCount <= 0 then Exit;

  scrW := Integer(InitResX);
  scrH := Integer(InitResY);
  if (scrW <= 0) or (scrH <= 0) then Exit;

  boxH := HintBoxHeight;

  Begin2D;
  try
    // Рисуем плашки снизу вверх: i=0 — самая нижняя, i=HintsCount-1 — самая верхняя.
    for i := 0 to HintsCount - 1 do
    begin
      // Ширина плашки = ширина текста + горизонтальные паддинги.
      textW := GetTextWidth(0, Hints[i], HINT_FONT_SCALE);
      boxW  := textW + HINT_PADDING_X * 2;

      // Позиция: правый край с margin'ом от края экрана,
      // вертикально стек снизу вверх с зазором.
      boxX := scrW - boxW - HINT_MARGIN_RIGHT;
      boxY := scrH - HINT_MARGIN_BOTTOM - (i + 1) * boxH - i * HINT_GAP;

      // Защита от отрицательных координат на маленьких разрешениях.
      if boxX < 0 then boxX := 0;
      if boxY < 0 then boxY := 0;

      // Чёрный фон
      DrawRectangle2D(boxX, boxY, boxW, boxH,
                      HINT_BG_COLOR, HINT_BG_ALPHA, True);

      // Тонкая белая рамка
      DrawRectangle2D(boxX, boxY, boxW, boxH,
                      HINT_BORDER_COLOR, HINT_BORDER_ALPHA, False);

      // Текст с тенью (чтобы читался на любом фоне через прозрачность).
      textX := boxX + HINT_PADDING_X;
      textY := boxY + HINT_PADDING_Y;

      DrawText2D(0, textX + 1, textY + 1, Hints[i],
                 HINT_SHADOW_COLOR, HINT_SHADOW_ALPHA, HINT_FONT_SCALE);
      DrawText2D(0, textX, textY, Hints[i],
                 HINT_TEXT_COLOR, HINT_TEXT_ALPHA, HINT_FONT_SCALE);
    end;
  finally
    End2D;
  end;
end;

end.
