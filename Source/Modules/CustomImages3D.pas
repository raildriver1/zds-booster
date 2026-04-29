//----------------------------------------------------------------------------//
// CustomImages3D.pas                                                         //
//                                                                            //
// ZDS-Booster — пользовательские 3D-картинки в кабине.                       //
//                                                                            //
// Архитектурно — это упрощённый аналог CustomTexts из CheatMenu.pas, но для  //
// плоскостей с произвольной BMP/JPG/TGA текстурой. Каждая картинка имеет     //
// позицию (X,Y,Z), поворот (RX,RY,RZ), размеры (Width,Height) и путь к       //
// текстуре. Рендер использует уже существующие низкоуровневые функции        //
// движка (BeginObj3D / Position3D / RotateX/Y/Z / SetTexture / DrawPlane).   //
//                                                                            //
// Прозрачность: ZDS-стандарт magenta-key ($FF00FF) — пиксели этого цвета    //
// становятся прозрачными при загрузке через LoadTextureFromFile.             //
//                                                                            //
// Интеграция: процедура DrawCustomImages3D вызывается из                     //
// CheatMenu.RenderCustomTextsAndGizmoForFrame (один раз за кадр в            //
// cabin-local матрице — общий для всех hook'ов локомотивов).                //
//----------------------------------------------------------------------------//
unit CustomImages3D;

interface

uses
  Windows, SysUtils, Classes, OpenGL, DrawFunc3D, Textures;

type
  // Один пользовательский 3D-объект-картинка в кабине.
  // Локальные координаты кабины (та же система, что у CustomTexts).
  TCustomImage3D = record
    Name: string;            // произвольное имя элемента (только для UI)
    TexturePath: string;     // полный путь к BMP/JPG/TGA
    TextureID: Cardinal;     // 0 если ещё не загружено или загрузка не удалась
    LoadAttempted: Boolean;  // True после первой попытки загрузки (успешной или нет)
    X, Y, Z: Single;
    RX, RY, RZ: Single;      // углы в градусах
    Width, Height: Single;   // размеры плоскости в локальных единицах кабины
    Visible: Boolean;
    Color: Cardinal;         // tint COLORREF; $FFFFFF = без тонирования
    Alpha: Byte;             // 0..255
  end;

const
  CUSTOM_IMAGE_DEFAULT_W = 0.30;
  CUSTOM_IMAGE_DEFAULT_H = 0.30;
  CUSTOM_IMAGE_DEFAULT_X = 0.0;
  CUSTOM_IMAGE_DEFAULT_Y = 10.6;
  CUSTOM_IMAGE_DEFAULT_Z = 2.7;
  CUSTOM_IMAGE_DEFAULT_RX = 0.0;
  CUSTOM_IMAGE_DEFAULT_RY = 0.0;
  CUSTOM_IMAGE_DEFAULT_RZ = 0.0;
  CUSTOM_IMAGE_TRANSPARENT_KEY = $00FF00FF; // magenta (RGB ff:00:ff) — ZDS-стандарт

var
  CustomImages: array of TCustomImage3D;
  CustomImageSelectedIdx: Integer = -1;

// Добавляет новую картинку с дефолтными параметрами (без текстуры).
// Делает новую картинку выбранной.
procedure AddCustomImage;

// Удаляет текущую выбранную картинку. Сжимает массив, корректирует индекс.
procedure DeleteSelectedCustomImage;

// Меняет путь к текстуре у выбранной картинки и инвалидирует кэш —
// чтобы следующий рендер загрузил BMP заново.
procedure SetSelectedImageTexturePath(const APath: string);

// Полностью сбрасывает массив (например, при смене локомотива).
// Освобождает GPU-текстуры.
procedure ClearAllCustomImages;

// Главная точка рендера. Зовётся один раз за кадр из
// CheatMenu.RenderCustomTextsAndGizmoForFrame (между BeginObj3D-стэком кабины).
procedure DrawCustomImages3D;

// Сохранение в текстовый файл per-loco (формат — ключ: значение, как у
// custom_texts.cfg). Безопасна — сама создаёт директории.
procedure SaveCustomImagesToFile(const Path: string);

// Загрузка из текстового файла. True если файл прочитан.
function  LoadCustomImagesFromFile(const Path: string): Boolean;

implementation

// Локальный helper — преобразует Single в строку с точкой как разделителем
// (TextFile WriteLn по умолчанию использует регистр locale).
function FormatImageValue(V: Single): string;
begin
  Result := FloatToStrF(V, ffFixed, 8, 6);
  // FloatToStrF честно использует FormatSettings — если в локали Windows
  // десятичный разделитель ',', то получим '0,3'. Принудительно правим.
  if Pos(',', Result) > 0 then
    Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
end;

function ParseImageFloat(const S: string; Default_: Single): Single;
var
  Tmp: string;
  Code: Integer;
begin
  Tmp := StringReplace(Trim(S), ',', '.', [rfReplaceAll]);
  Val(Tmp, Result, Code);
  if Code <> 0 then Result := Default_;
end;

// Создаёт директорию для файла (со всеми родителями), если её нет.
function EnsureImageDirExists(const Path: string): Boolean;
var
  Dir: string;
begin
  Result := False;
  Dir := ExtractFilePath(Path);
  if Dir = '' then begin Result := True; Exit; end;
  if DirectoryExists(Dir) then begin Result := True; Exit; end;
  Result := ForceDirectories(Dir);
end;

procedure InitDefaults(var Img: TCustomImage3D);
begin
  Img.Name := '';
  Img.TexturePath := '';
  Img.TextureID := 0;
  Img.LoadAttempted := False;
  Img.X := CUSTOM_IMAGE_DEFAULT_X;
  Img.Y := CUSTOM_IMAGE_DEFAULT_Y;
  Img.Z := CUSTOM_IMAGE_DEFAULT_Z;
  Img.RX := CUSTOM_IMAGE_DEFAULT_RX;
  Img.RY := CUSTOM_IMAGE_DEFAULT_RY;
  Img.RZ := CUSTOM_IMAGE_DEFAULT_RZ;
  Img.Width := CUSTOM_IMAGE_DEFAULT_W;
  Img.Height := CUSTOM_IMAGE_DEFAULT_H;
  Img.Visible := True;
  Img.Color := $FFFFFF;
  Img.Alpha := 255;
end;

// Освобождает GPU-текстуру у одного элемента (если была загружена).
procedure FreeImageTexture(var Img: TCustomImage3D);
begin
  if Img.TextureID <> 0 then
  begin
    try
      FreeTexture(Img.TextureID);
    except
      // Не валим DLL — текстура могла уже быть освобождена движком.
    end;
    Img.TextureID := 0;
  end;
  Img.LoadAttempted := False;
end;

procedure AddCustomImage;
var
  N: Integer;
  Img: TCustomImage3D;
begin
  InitDefaults(Img);
  N := Length(CustomImages);
  SetLength(CustomImages, N + 1);
  CustomImages[N] := Img;
  CustomImageSelectedIdx := N;
end;

procedure DeleteSelectedCustomImage;
var
  i: Integer;
begin
  if (CustomImageSelectedIdx < 0) or (CustomImageSelectedIdx >= Length(CustomImages)) then Exit;
  // Освобождаем GPU-память для удаляемого элемента.
  FreeImageTexture(CustomImages[CustomImageSelectedIdx]);
  for i := CustomImageSelectedIdx to Length(CustomImages) - 2 do
    CustomImages[i] := CustomImages[i + 1];
  SetLength(CustomImages, Length(CustomImages) - 1);
  if Length(CustomImages) = 0 then
    CustomImageSelectedIdx := -1
  else if CustomImageSelectedIdx >= Length(CustomImages) then
    CustomImageSelectedIdx := Length(CustomImages) - 1;
end;

procedure SetSelectedImageTexturePath(const APath: string);
begin
  if (CustomImageSelectedIdx < 0) or (CustomImageSelectedIdx >= Length(CustomImages)) then Exit;
  // Если путь не изменился — ничего не делаем (избегаем лишней перезагрузки).
  if SameText(CustomImages[CustomImageSelectedIdx].TexturePath, APath) then Exit;
  // Иначе — сбрасываем кэш, чтобы рендер при следующем кадре загрузил заново.
  FreeImageTexture(CustomImages[CustomImageSelectedIdx]);
  CustomImages[CustomImageSelectedIdx].TexturePath := APath;
end;

procedure ClearAllCustomImages;
var
  i: Integer;
begin
  for i := 0 to Length(CustomImages) - 1 do
    FreeImageTexture(CustomImages[i]);
  SetLength(CustomImages, 0);
  CustomImageSelectedIdx := -1;
end;

// =====================================================================
//  RENDER
// =====================================================================
procedure DrawCustomImages3D;
var
  i: Integer;
  WasBlend, WasAlphaTest, WasLighting: GLBoolean;
  PrevBlendSrc, PrevBlendDst: Integer;
begin
  if Length(CustomImages) = 0 then Exit;

  // Сохраняем текущее GL-state, чтобы не сломать рендер кабины.
  WasBlend := glIsEnabled(GL_BLEND);
  WasAlphaTest := glIsEnabled(GL_ALPHA_TEST);
  WasLighting := glIsEnabled(GL_LIGHTING);
  glGetIntegerv(GL_BLEND_SRC, @PrevBlendSrc);
  glGetIntegerv(GL_BLEND_DST, @PrevBlendDst);

  for i := 0 to Length(CustomImages) - 1 do
  begin
    if not CustomImages[i].Visible then Continue;

    // Lazy-загрузка текстуры. Если файл указан, но загрузка ещё не пыталась —
    // пробуем сейчас. magenta-key превращает $FF00FF в прозрачный.
    if (not CustomImages[i].LoadAttempted) and (CustomImages[i].TextureID = 0) then
    begin
      CustomImages[i].LoadAttempted := True;
      if (CustomImages[i].TexturePath <> '') and FileExists(CustomImages[i].TexturePath) then
      begin
        try
          CustomImages[i].TextureID :=
            LoadTextureFromFile(CustomImages[i].TexturePath, 0, CUSTOM_IMAGE_TRANSPARENT_KEY);
        except
          CustomImages[i].TextureID := 0;
        end;
      end;
    end;

    if CustomImages[i].TextureID = 0 then Continue;

    BeginObj3D;
    try
      // Прозрачность через blend + alpha-test (alpha=0 у magenta-пикселей
      // после LoadTextureFromFile). Освещение глушим — иначе цвет картинки
      // будет «варьироваться» от cabin-light'а, что некрасиво для дисплеев.
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_ALPHA_TEST);
      glAlphaFunc(GL_GREATER, 0.1);
      glDisable(GL_LIGHTING);

      Position3D(CustomImages[i].X, CustomImages[i].Y, CustomImages[i].Z);
      RotateX(CustomImages[i].RX);
      RotateY(CustomImages[i].RY);
      RotateZ(CustomImages[i].RZ);

      Color3D(Integer(CustomImages[i].Color), CustomImages[i].Alpha, False, 0.0);
      SetTexture(CustomImages[i].TextureID);
      DrawPlane(CustomImages[i].Width, CustomImages[i].Height);
    finally
      EndObj3D;
    end;
  end;

  // Восстанавливаем GL-state до значения «как было до нашего прохода».
  if not Boolean(WasBlend) then glDisable(GL_BLEND);
  if not Boolean(WasAlphaTest) then glDisable(GL_ALPHA_TEST);
  if Boolean(WasLighting) then glEnable(GL_LIGHTING);
  glBlendFunc(PrevBlendSrc, PrevBlendDst);
end;

// =====================================================================
//  SAVE / LOAD (per-loco config)
// =====================================================================
//
// Формат — текстовый, ключ: значение. Тот же стиль, что у custom_texts.cfg.
// Пример:
//   custom_image_count: 2
//   ci0_path: C:\ZDSimulator55.008new\ra3\newcab\displaymfdu.bmp
//   ci0_x: 0.000000
//   ci0_y: 10.638300
//   ci0_z: 2.752210
//   ci0_rx: -90.000000
//   ci0_ry: 0.000000
//   ci0_rz: 0.000000
//   ci0_w: 0.300000
//   ci0_h: 0.300000
//   ci0_color: 16777215
//   ci0_alpha: 255
//   ci0_visible: 1
//
procedure SaveCustomImagesToFile(const Path: string);
var
  F: TextFile;
  i: Integer;
begin
  if Path = '' then Exit;
  if not EnsureImageDirExists(Path) then Exit;
  try
    AssignFile(F, Path);
    Rewrite(F);
    try
      WriteLn(F, '# ZDS-Booster — Custom 3D images for this loco/LocNum.');
      WriteLn(F, '# Auto-generated. Edit via in-game menu (F12).');
      WriteLn(F, 'custom_image_count: ' + IntToStr(Length(CustomImages)));
      for i := 0 to Length(CustomImages) - 1 do
      begin
        WriteLn(F, 'ci' + IntToStr(i) + '_name: '   + CustomImages[i].Name);
        WriteLn(F, 'ci' + IntToStr(i) + '_path: '   + CustomImages[i].TexturePath);
        WriteLn(F, 'ci' + IntToStr(i) + '_x: '      + FormatImageValue(CustomImages[i].X));
        WriteLn(F, 'ci' + IntToStr(i) + '_y: '      + FormatImageValue(CustomImages[i].Y));
        WriteLn(F, 'ci' + IntToStr(i) + '_z: '      + FormatImageValue(CustomImages[i].Z));
        WriteLn(F, 'ci' + IntToStr(i) + '_rx: '     + FormatImageValue(CustomImages[i].RX));
        WriteLn(F, 'ci' + IntToStr(i) + '_ry: '     + FormatImageValue(CustomImages[i].RY));
        WriteLn(F, 'ci' + IntToStr(i) + '_rz: '     + FormatImageValue(CustomImages[i].RZ));
        WriteLn(F, 'ci' + IntToStr(i) + '_w: '      + FormatImageValue(CustomImages[i].Width));
        WriteLn(F, 'ci' + IntToStr(i) + '_h: '      + FormatImageValue(CustomImages[i].Height));
        WriteLn(F, 'ci' + IntToStr(i) + '_color: '  + IntToStr(Integer(CustomImages[i].Color)));
        WriteLn(F, 'ci' + IntToStr(i) + '_alpha: '  + IntToStr(CustomImages[i].Alpha));
        if CustomImages[i].Visible then
          WriteLn(F, 'ci' + IntToStr(i) + '_visible: 1')
        else
          WriteLn(F, 'ci' + IntToStr(i) + '_visible: 0');
      end;
    finally
      CloseFile(F);
    end;
  except
    // Не валим DLL — просто проигнорируем ошибку записи.
  end;
end;

function LoadCustomImagesFromFile(const Path: string): Boolean;
var
  F: TextFile;
  Line, Key, Value, Prefix: string;
  ColonPos, USCorePos, CICount, CIIdx: Integer;
begin
  Result := False;
  if Path = '' then Exit;
  if not FileExists(Path) then Exit;

  // Освобождаем старый набор.
  ClearAllCustomImages;

  try
    AssignFile(F, Path);
    Reset(F);
    try
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        Line := Trim(Line);
        if (Line = '') or (Line[1] = '#') or (Line[1] = ';') then Continue;
        ColonPos := Pos(':', Line);
        if ColonPos < 1 then Continue;
        Key := LowerCase(Trim(Copy(Line, 1, ColonPos - 1)));
        Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));

        if Key = 'custom_image_count' then
        begin
          CICount := StrToIntDef(Value, 0);
          if CICount < 0 then CICount := 0;
          if CICount > 256 then CICount := 256;
          SetLength(CustomImages, CICount);
          for CIIdx := 0 to CICount - 1 do
            InitDefaults(CustomImages[CIIdx]);
        end
        else if (Length(Key) > 2) and (Copy(Key, 1, 2) = 'ci') then
        begin
          USCorePos := Pos('_', Key);
          if USCorePos < 4 then Continue;
          CIIdx := StrToIntDef(Copy(Key, 3, USCorePos - 3), -1);
          if (CIIdx < 0) or (CIIdx >= Length(CustomImages)) then Continue;
          Prefix := Copy(Key, USCorePos + 1, Length(Key));

          if Prefix = 'name' then
            CustomImages[CIIdx].Name := Value
          else if Prefix = 'path' then
          begin
            CustomImages[CIIdx].TexturePath := Value;
            // Кэш сбросим — текстура подгрузится при следующем рендере.
            CustomImages[CIIdx].TextureID := 0;
            CustomImages[CIIdx].LoadAttempted := False;
          end
          else if Prefix = 'x'       then CustomImages[CIIdx].X       := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_X)
          else if Prefix = 'y'       then CustomImages[CIIdx].Y       := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_Y)
          else if Prefix = 'z'       then CustomImages[CIIdx].Z       := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_Z)
          else if Prefix = 'rx'      then CustomImages[CIIdx].RX      := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_RX)
          else if Prefix = 'ry'      then CustomImages[CIIdx].RY      := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_RY)
          else if Prefix = 'rz'      then CustomImages[CIIdx].RZ      := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_RZ)
          else if Prefix = 'w'       then CustomImages[CIIdx].Width   := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_W)
          else if Prefix = 'h'       then CustomImages[CIIdx].Height  := ParseImageFloat(Value, CUSTOM_IMAGE_DEFAULT_H)
          else if Prefix = 'color'   then CustomImages[CIIdx].Color   := Cardinal(StrToIntDef(Value, $FFFFFF))
          else if Prefix = 'alpha'   then CustomImages[CIIdx].Alpha   := Byte(StrToIntDef(Value, 255))
          else if Prefix = 'visible' then CustomImages[CIIdx].Visible := (Value = '1');
        end;
      end;
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    // Игнорим — оставим что прочиталось до ошибки.
  end;

  if (CustomImageSelectedIdx >= Length(CustomImages)) then
    CustomImageSelectedIdx := -1;
end;

end.
