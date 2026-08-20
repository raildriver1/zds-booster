unit BoosterWorld;

interface

procedure InitializeWorld;
procedure ProcessWorld;
procedure DrawWorldSky(X, Y, Z: Single);
procedure DrawWorldSkyRuntime(X, Y, Z: Single);
procedure DrawWorldSkyLayer(TextureID: Cardinal; Alpha: Byte; ModelID: Word);
procedure LoadWorldSkyTextures;
procedure ApplyWorldSkyHook;
procedure InitializeWorldDayNight;
procedure ApplyWorldDayNightTextures;
procedure ProcessWorldDayNight;
procedure ProcessWorldStepForward;
procedure ProcessWorldSkyPatch;
procedure RestoreWorldSkyHook;
procedure ApplyWorldDomePatch;
function FindWorldTextureFile(const FolderPath: string): string;
function GetWorldCurrentSeason: string;
function IsWorldNightTime: Boolean;

implementation

uses
  DrawFunc3D, Variables, Textures, EngineUtils, OpenGL, Windows, MMSystem,
  SysUtils, BoosterExtended;

function FindWorldTextureFile(const FolderPath: string): string;
var
  SearchRec: TSearchRec;
begin
  Result := '';
  if FindFirst(FolderPath + '\*254*395*.bmp', faAnyFile, SearchRec) = 0 then
  begin
    Result := FolderPath + '\' + SearchRec.Name;
    FindClose(SearchRec);
    Exit;
  end;
  FindClose(SearchRec);
  if FindFirst(FolderPath + '\*254*.bmp', faAnyFile, SearchRec) = 0 then
  begin
    Result := FolderPath + '\' + SearchRec.Name;
    FindClose(SearchRec);
    Exit;
  end;
  FindClose(SearchRec);
  if FindFirst(FolderPath + '\*395*.bmp', faAnyFile, SearchRec) = 0 then
    Result := FolderPath + '\' + SearchRec.Name;
  FindClose(SearchRec);
end;

function GetWorldCurrentSeason: string;
begin
  try
    if PByte(Pointer($00400000 + $349968))^ = 1 then
      Result := 'ЗИМА'
    else
      Result := 'ЛЕТО';
  except
    Result := 'НЕИЗВЕСТНО';
  end;
end;

function IsWorldNightTime: Boolean;
var
  Hour, TotalMin, SunriseMin, SunsetMin: Integer;
  Season: Byte;
begin
  Hour := DrawFunc3D.GetCurrentHour;
  if DrawFunc3D.Config_MegaRealism then
  begin
    try
      TotalMin := Hour * 60 + PInteger(Pointer($00400000 + $8C08038))^;
      SunriseMin := DrawFunc3D.GetMegaRealismSunrise;
      SunsetMin := DrawFunc3D.GetMegaRealismSunset;
      Result := (TotalMin >= SunsetMin + 90) or (TotalMin < SunriseMin - 90);
    except
      Result := (Hour >= 21) or (Hour <= 4);
    end;
    Exit;
  end;
  try
    Season := PByte(Pointer($00400000 + $349968))^;
    if Season = 1 then
      Result := (Hour >= 18) or (Hour <= 7)
    else
      Result := (Hour >= 21) or (Hour <= 4);
  except
    Result := (Hour >= 21) or (Hour <= 4);
  end;
end;

procedure InitializeWorld;
begin
  ApplyWorldSkyHook;
end;

procedure ProcessWorld;
begin
  DrawFunc3D.ProcessFreecam;
  DrawFunc3D.ProcessDayNightSystem;
  DrawFunc3D.ProcessStepForwardConfig;
  DrawFunc3D.ApplyMaxVisibleDistance;
end;

procedure DrawWorldSky(X, Y, Z: Single);
begin
  DrawWorldSkyRuntime(X, Y, Z);
end;

// ============================================================================
// Загрузка текстур неба (лето + зима). Перенесено из LoadBoosterSkyTextures.
// ============================================================================
procedure LoadWorldSkyTextures;
var
  routeName: string;
  skyTexturesPath: string;
  sunriseDawnPath, sunsetTwilightPath: string;
  daySnowPath, sunsetSnowPath, sunsetTwilightSnowPath: string;
  nightSnowPath, sunriseDawnSnowPath, sunriseSnowPath: string;
begin
  if DrawFunc3D.BoosterTexturesLoaded then Exit;

  try
    routeName := DrawFunc3D.GetRouteName;
    if routeName = '' then
    begin
      AddToLogFile(EngineLog, 'Route name not found, skipping sky textures loading');
      Exit;
    end;

    skyTexturesPath := 'routes\' + routeName + '\textures\';

    sunriseDawnPath := skyTexturesPath + 'sky_sunriseDawn.bmp';
    sunsetTwilightPath := skyTexturesPath + 'sky_sunsetTwilight.bmp';

    daySnowPath := skyTexturesPath + 'sky_day_snow.bmp';
    sunsetSnowPath := skyTexturesPath + 'sky_sunset_snow.bmp';
    sunsetTwilightSnowPath := skyTexturesPath + 'sky_sunsetTwilight_snow.bmp';
    nightSnowPath := skyTexturesPath + 'sky_night_snow.bmp';
    sunriseDawnSnowPath := skyTexturesPath + 'sky_sunriseDawn_snow.bmp';
    sunriseSnowPath := skyTexturesPath + 'sky_sunrise_snow.bmp';

    AddToLogFile(EngineLog, '=== ЗАГРУЗКА ТЕКСТУР НЕБА (ЛЕТО + ЗИМА) ===');
    AddToLogFile(EngineLog, 'Route: ' + routeName);

    if not (FileExists(sunriseDawnPath) and FileExists(sunsetTwilightPath)) then
    begin
      AddToLogFile(EngineLog, 'Summer additional sky files not found');
      Exit;
    end;

    if not (FileExists(daySnowPath) and FileExists(sunsetSnowPath) and
            FileExists(sunsetTwilightSnowPath) and FileExists(nightSnowPath) and
            FileExists(sunriseDawnSnowPath) and FileExists(sunriseSnowPath)) then
    begin
      AddToLogFile(EngineLog, 'Winter sky files not found');
      Exit;
    end;

    DrawFunc3D.BoosterSunriseDawnTextureID := LoadTextureFromFile(sunriseDawnPath, 0, -1);
    if DrawFunc3D.BoosterSunriseDawnTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded summer sky_sunriseDawn.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterSunriseDawnTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load summer sky_sunriseDawn.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterSunsetTwilightTextureID := LoadTextureFromFile(sunsetTwilightPath, 0, -1);
    if DrawFunc3D.BoosterSunsetTwilightTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded summer sky_sunsetTwilight.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterSunsetTwilightTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load summer sky_sunsetTwilight.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterDaySnowTextureID := LoadTextureFromFile(daySnowPath, 0, -1);
    if DrawFunc3D.BoosterDaySnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_day_snow.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterDaySnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_day_snow.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterSunsetSnowTextureID := LoadTextureFromFile(sunsetSnowPath, 0, -1);
    if DrawFunc3D.BoosterSunsetSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunset_snow.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterSunsetSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunset_snow.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterSunsetTwilightSnowTextureID := LoadTextureFromFile(sunsetTwilightSnowPath, 0, -1);
    if DrawFunc3D.BoosterSunsetTwilightSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunsetTwilight_snow.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterSunsetTwilightSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunsetTwilight_snow.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterNightSnowTextureID := LoadTextureFromFile(nightSnowPath, 0, -1);
    if DrawFunc3D.BoosterNightSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_night_snow.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterNightSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_night_snow.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterSunriseDawnSnowTextureID := LoadTextureFromFile(sunriseDawnSnowPath, 0, -1);
    if DrawFunc3D.BoosterSunriseDawnSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunriseDawn_snow.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterSunriseDawnSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunriseDawn_snow.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterSunriseSnowTextureID := LoadTextureFromFile(sunriseSnowPath, 0, -1);
    if DrawFunc3D.BoosterSunriseSnowTextureID > 0 then
      AddToLogFile(EngineLog, 'Loaded winter sky_sunrise_snow.bmp, ID: ' + IntToStr(DrawFunc3D.BoosterSunriseSnowTextureID))
    else
    begin
      AddToLogFile(EngineLog, 'Failed to load winter sky_sunrise_snow.bmp');
      Exit;
    end;

    DrawFunc3D.BoosterTexturesLoaded := True;
    AddToLogFile(EngineLog, 'Route sky textures (summer + winter) loaded successfully');

  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error loading route sky textures: ' + E.Message);
      if DrawFunc3D.BoosterSunriseDawnTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterSunriseDawnTextureID);
        DrawFunc3D.BoosterSunriseDawnTextureID := 0;
      end;
      if DrawFunc3D.BoosterSunsetTwilightTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterSunsetTwilightTextureID);
        DrawFunc3D.BoosterSunsetTwilightTextureID := 0;
      end;
      if DrawFunc3D.BoosterDaySnowTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterDaySnowTextureID);
        DrawFunc3D.BoosterDaySnowTextureID := 0;
      end;
      if DrawFunc3D.BoosterSunsetSnowTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterSunsetSnowTextureID);
        DrawFunc3D.BoosterSunsetSnowTextureID := 0;
      end;
      if DrawFunc3D.BoosterSunsetTwilightSnowTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterSunsetTwilightSnowTextureID);
        DrawFunc3D.BoosterSunsetTwilightSnowTextureID := 0;
      end;
      if DrawFunc3D.BoosterNightSnowTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterNightSnowTextureID);
        DrawFunc3D.BoosterNightSnowTextureID := 0;
      end;
      if DrawFunc3D.BoosterSunriseDawnSnowTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterSunriseDawnSnowTextureID);
        DrawFunc3D.BoosterSunriseDawnSnowTextureID := 0;
      end;
      if DrawFunc3D.BoosterSunriseSnowTextureID > 0 then
      begin
        FreeTexture(DrawFunc3D.BoosterSunriseSnowTextureID);
        DrawFunc3D.BoosterSunriseSnowTextureID := 0;
      end;
    end;
  end;
end;

// ============================================================================
// Отрисовка одного слоя неба. Перенесено из DrawSkyLayer.
// ============================================================================
procedure DrawWorldSkyLayer(TextureID: Cardinal; Alpha: Byte; ModelID: Word);
begin
  try
    if (TextureID > 0) and (ModelID > 0) and (Alpha > 0) then
    begin
      DrawFunc3D.Color3D($FFFFFF, Alpha, False, 0.0);
      DrawFunc3D.SetTexture(TextureID);
      DrawFunc3D.DrawModel(ModelID, 0, True);
    end;
  except
    // Безопасный fallback - пропускаем проблемную текстуру
  end;
end;

// ============================================================================
// Отрисовка неба (day/night/sunset/sunrise + MegaRealism фазы).
// Перенесено из DrawSky.
// ============================================================================
procedure DrawWorldSkyRuntime(X, Y, Z: Single);
var
  v5: Single;
  alpha: Byte;
  currentHour, currentMinute: Integer;
  lightingCheck: Byte;
  modelAddr, textureAddr: Pointer;
  modelID: Word;
  textureID: Word;
  timePtr: PInteger;
  v7: Pointer;
  a1: Boolean;

  dayTextureID, sunsetTextureID, nightTextureID, sunriseTextureID: Word;

  totalMinutes: Integer;

  isWinter: Boolean;

  mrSR, mrSS: Integer;
  mrP0, mrP1, mrP2, mrP3, mrP4, mrP5, mrP6, mrP7, mrP8, mrP9, mrP10, mrP11: Integer;
  mrHandled: Boolean;
  FogWasEnabled: BYTEBOOL;
  FogStateCaptured: Boolean;

begin
  FogWasEnabled := GL_FALSE;
  FogStateCaptured := False;
  try
    if not DrawFunc3D.BoosterTexturesLoaded then
      LoadWorldSkyTextures;

    try
      isWinter := PByte(Pointer($00400000 + $349968))^ = 1;
    except
      isWinter := False;
    end;

    try
      v7 := PPointer(Pointer($00400000 + $34B5F0))^;
    except
      v7 := nil;
    end;

    DrawFunc3D.BeginObj3D;

    glGetBooleanv(GL_FOG, @FogWasEnabled);
    FogStateCaptured := True;
    glDisable(GL_FOG);

    DrawFunc3D.DeactiveLight(-1);

    DrawFunc3D.Position3D(Z, Y, X);

    try
      v5 := PDouble(Pointer($0538D920))^ * 15.0 / 3600.0 - 180.0;
      DrawFunc3D.RotateZ(v5);
    except
      v5 := 0.0;
    end;

    try
      lightingCheck := PByte(Pointer($090043A4))^;
      if lightingCheck = 0 then
        DrawFunc3D.Scale3D(1.2);
    except
      DrawFunc3D.Scale3D(1.2);
    end;

    DrawFunc3D.Scale3D((maxvisibledistance - 1500) / 1500.0 + 1.0);

    try
      if v7 <> nil then
      begin
        currentHour := PInteger(v7)^;
        currentMinute := PInteger(Pointer($00400000 + $8C08038))^;
      end
      else
      begin
        currentHour := 12;
        currentMinute := 0;
      end;
    except
      currentHour := 12;
      currentMinute := 0;
    end;

    try
      modelAddr := Pointer(PCardinal(Pointer($09110D70))^ + $02);
      modelID := PWord(modelAddr)^;
      if modelID = 0 then modelID := 1;
    except
      modelID := 1;
    end;

    try
      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $42);
      dayTextureID := PWord(textureAddr)^;
      if dayTextureID = 0 then dayTextureID := 1;

      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $44);
      sunsetTextureID := PWord(textureAddr)^;
      if sunsetTextureID = 0 then sunsetTextureID := 1;

      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $02);
      nightTextureID := PWord(textureAddr)^;
      if nightTextureID = 0 then nightTextureID := 1;

      textureAddr := Pointer(PCardinal(Pointer($09110D60))^ + $40);
      sunriseTextureID := PWord(textureAddr)^;
      if sunriseTextureID = 0 then sunriseTextureID := 1;
    except
      dayTextureID := 1;
      sunsetTextureID := 1;
      nightTextureID := 1;
      sunriseTextureID := 1;
    end;

    a1 := False;
    totalMinutes := currentHour * 60 + currentMinute;

    if not DrawFunc3D.SkyDrawLoggedOnce then
    begin
      DrawFunc3D.SkyDrawLoggedOnce := True;
      AddToLogFile(EngineLog, Format('=== DrawSky CALLED: hour=%d min=%d modelID=%d day=%d sunset=%d night=%d sunrise=%d isWinter=%s ===',
        [currentHour, currentMinute, modelID, dayTextureID, sunsetTextureID, nightTextureID, sunriseTextureID, BoolToStr(isWinter, True)]));
      AddToLogFile(EngineLog, Format('=== DrawSky Booster: dawn=%d twilight=%d daySnow=%d sunsetSnow=%d twilightSnow=%d nightSnow=%d dawnSnow=%d sunriseSnow=%d loaded=%s ===',
        [DrawFunc3D.BoosterSunriseDawnTextureID, DrawFunc3D.BoosterSunsetTwilightTextureID, DrawFunc3D.BoosterDaySnowTextureID,
         DrawFunc3D.BoosterSunsetSnowTextureID, DrawFunc3D.BoosterSunsetTwilightSnowTextureID, DrawFunc3D.BoosterNightSnowTextureID,
         DrawFunc3D.BoosterSunriseDawnSnowTextureID, DrawFunc3D.BoosterSunriseSnowTextureID,
         BoolToStr(DrawFunc3D.BoosterTexturesLoaded, True)]));
      AddToLogFile(EngineLog, Format('=== DrawSky pos: x=%.2f y=%.2f z=%.2f rot=%.2f Config_NewSky=%s Config_MegaRealism=%s ===',
        [X, Y, Z, v5, BoolToStr(DrawFunc3D.Config_NewSky, True), BoolToStr(DrawFunc3D.Config_MegaRealism, True)]));
    end;

    // ===== MEGA REALISM: динамические фазы неба по городу и месяцу =====
    mrHandled := False;
    if DrawFunc3D.Config_MegaRealism and DrawFunc3D.Config_NewSky and DrawFunc3D.BoosterTexturesLoaded then
    begin
      mrSR := DrawFunc3D.GetMegaRealismSunrise;
      mrSS := DrawFunc3D.GetMegaRealismSunset;

      mrP0  := mrSR - 120;
      mrP1  := mrSR - 60;
      mrP2  := mrSR - 30;
      mrP3  := mrSR;
      mrP4  := mrSR + 60;
      mrP5  := mrSR + 120;
      mrP6  := mrSS - 120;
      mrP7  := mrSS - 60;
      mrP8  := mrSS - 30;
      mrP9  := mrSS;
      mrP10 := mrSS + 60;
      mrP11 := mrSS + 120;

      mrHandled := True;

      if isWinter then
      begin
        if (totalMinutes >= mrP0) and (totalMinutes < mrP1) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterNightSnowTextureID, 240, modelID);
          alpha := Round((totalMinutes - mrP0) * 255 / 60);
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnSnowTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP1) and (totalMinutes < mrP2) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnSnowTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP2) and (totalMinutes < mrP3) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnSnowTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP2) * 255 / 30);
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseSnowTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP3) and (totalMinutes < mrP4) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseSnowTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP4) and (totalMinutes < mrP5) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseSnowTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP4) * 255 / 60);
          DrawWorldSkyLayer(DrawFunc3D.BoosterDaySnowTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP5) and (totalMinutes < mrP6) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterDaySnowTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP6) and (totalMinutes < mrP7) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterDaySnowTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP6) * 255 / 60);
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetSnowTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP7) and (totalMinutes < mrP8) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetSnowTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP8) and (totalMinutes < mrP9) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetSnowTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP8) * 255 / 30);
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightSnowTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP9) and (totalMinutes < mrP10) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightSnowTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP10) and (totalMinutes < mrP11) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightSnowTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP10) * 240 / 60);
          DrawWorldSkyLayer(DrawFunc3D.BoosterNightSnowTextureID, alpha, modelID);
        end
        else
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterNightSnowTextureID, 240, modelID);
        end;
      end
      else
      begin
        if (totalMinutes >= mrP0) and (totalMinutes < mrP1) then
        begin
          DrawWorldSkyLayer(nightTextureID, 240, modelID);
          alpha := Round((totalMinutes - mrP0) * 255 / 60);
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP1) and (totalMinutes < mrP2) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP2) and (totalMinutes < mrP3) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP2) * 255 / 30);
          DrawWorldSkyLayer(sunriseTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP3) and (totalMinutes < mrP4) then
        begin
          DrawWorldSkyLayer(sunriseTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP4) and (totalMinutes < mrP5) then
        begin
          DrawWorldSkyLayer(sunriseTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP4) * 255 / 60);
          DrawWorldSkyLayer(dayTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP5) and (totalMinutes < mrP6) then
        begin
          DrawWorldSkyLayer(dayTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP6) and (totalMinutes < mrP7) then
        begin
          DrawWorldSkyLayer(dayTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP6) * 255 / 60);
          DrawWorldSkyLayer(sunsetTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP7) and (totalMinutes < mrP8) then
        begin
          DrawWorldSkyLayer(sunsetTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP8) and (totalMinutes < mrP9) then
        begin
          DrawWorldSkyLayer(sunsetTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP8) * 255 / 30);
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightTextureID, alpha, modelID);
        end
        else if (totalMinutes >= mrP9) and (totalMinutes < mrP10) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightTextureID, 255, modelID);
        end
        else if (totalMinutes >= mrP10) and (totalMinutes < mrP11) then
        begin
          DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightTextureID, 255, modelID);
          alpha := Round((totalMinutes - mrP10) * 240 / 60);
          DrawWorldSkyLayer(nightTextureID, alpha, modelID);
        end
        else
        begin
          DrawWorldSkyLayer(nightTextureID, 240, modelID);
        end;
      end;
    end;

    // ===== Стандартная логика (если MegaRealism не обработал) =====
    if not mrHandled then
    if isWinter then
    begin
      if (totalMinutes >= 420) and (totalMinutes < 510) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 510) and (totalMinutes < 540) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnSnowTextureID, 255, modelID);
        alpha := Round((totalMinutes - 510) * 255 / 30);
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 540) and (totalMinutes < 630) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 630) and (totalMinutes < 660) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseSnowTextureID, 255, modelID);
        alpha := Round((totalMinutes - 630) * 255 / 30);
        DrawWorldSkyLayer(DrawFunc3D.BoosterDaySnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 660) and (totalMinutes < 930) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterDaySnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 930) and (totalMinutes < 960) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterDaySnowTextureID, 255, modelID);
        alpha := Round((totalMinutes - 930) * 255 / 30);
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 960) and (totalMinutes < 1050) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1050) and (totalMinutes < 1080) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetSnowTextureID, 255, modelID);
        alpha := Round((totalMinutes - 1050) * 255 / 30);
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1080) and (totalMinutes < 1170) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1170) and (totalMinutes < 1200) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightSnowTextureID, 255, modelID);
        alpha := Round((totalMinutes - 1170) * 240 / 30);
        DrawWorldSkyLayer(DrawFunc3D.BoosterNightSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1200) or (totalMinutes < 390) then
      begin
        alpha := 240;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterNightSnowTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 390) and (totalMinutes < 420) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterNightSnowTextureID, 240, modelID);
        alpha := Round((totalMinutes - 390) * 255 / 30);
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnSnowTextureID, alpha, modelID);
      end
      else
      begin
        alpha := 240;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterNightSnowTextureID, alpha, modelID);
      end;
    end
    else
    begin
      if (totalMinutes >= 300) and (totalMinutes < 360) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnTextureID, 255, modelID);
        alpha := Round((totalMinutes - 300) * 255 / 60);
        DrawWorldSkyLayer(sunriseTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 360) and (totalMinutes < 420) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(sunriseTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 420) and (totalMinutes < 480) then
      begin
        DrawWorldSkyLayer(sunriseTextureID, 255, modelID);
        alpha := Round((totalMinutes - 420) * 255 / 60);
        DrawWorldSkyLayer(dayTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 480) and (totalMinutes < 1020) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(dayTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1020) and (totalMinutes < 1080) then
      begin
        DrawWorldSkyLayer(dayTextureID, 255, modelID);
        alpha := Round((totalMinutes - 1020) * 255 / 60);
        DrawWorldSkyLayer(sunsetTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1080) and (totalMinutes < 1140) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(sunsetTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1140) and (totalMinutes < 1200) then
      begin
        DrawWorldSkyLayer(sunsetTextureID, 255, modelID);
        alpha := Round((totalMinutes - 1140) * 255 / 60);
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1200) and (totalMinutes < 1260) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1260) and (totalMinutes < 1320) then
      begin
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunsetTwilightTextureID, 255, modelID);
        alpha := Round((totalMinutes - 1260) * 240 / 60);
        DrawWorldSkyLayer(nightTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 1320) or (totalMinutes < 180) then
      begin
        alpha := 240;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(nightTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 180) and (totalMinutes < 240) then
      begin
        DrawWorldSkyLayer(nightTextureID, 240, modelID);
        alpha := Round((totalMinutes - 180) * 255 / 60);
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnTextureID, alpha, modelID);
      end
      else if (totalMinutes >= 240) and (totalMinutes < 300) then
      begin
        alpha := 255;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(DrawFunc3D.BoosterSunriseDawnTextureID, alpha, modelID);
      end
      else
      begin
        alpha := 240;
        if a1 then alpha := 255;
        DrawWorldSkyLayer(nightTextureID, alpha, modelID);
      end;
    end;

  finally
    if FogStateCaptured then
    begin
      if FogWasEnabled then
        glEnable(GL_FOG)
      else
        glDisable(GL_FOG);
    end;
    DrawFunc3D.EndObj3D;
  end;
end;

// ============================================================================
// Патч вызова DrawSky в игре. Перенесено из PatchDrawSkyCall.
// ============================================================================
procedure ApplyWorldSkyHook;
var
  CallAddress: Cardinal;
  DrawSkyAddress: Cardinal;
  NewOffset: Integer;
  OldProtect: DWORD;
  routeName: string;
  skyTexturesPath: string;
  sunriseDawnPath, sunsetTwilightPath: string;
  daySnowPath, sunsetSnowPath, sunsetTwilightSnowPath: string;
  nightSnowPath, sunriseDawnSnowPath, sunriseSnowPath: string;
begin
  if not DrawFunc3D.Config_NewSky then
  begin
    AddToLogFile(EngineLog, 'Sky patching disabled in config (newsky: 0)');
    Exit;
  end;

  if DrawFunc3D.SkyPatchApplied then
  begin
    AddToLogFile(EngineLog, 'Sky patch already applied');
    Exit;
  end;

  try
    routeName := DrawFunc3D.GetRouteName;
    if routeName = '' then
    begin
      AddToLogFile(EngineLog, 'Route name not found, skipping sky patch');
      Exit;
    end;

    skyTexturesPath := 'routes\' + routeName + '\textures\';

    sunriseDawnPath := skyTexturesPath + 'sky_sunriseDawn.bmp';
    sunsetTwilightPath := skyTexturesPath + 'sky_sunsetTwilight.bmp';

    daySnowPath := skyTexturesPath + 'sky_day_snow.bmp';
    sunsetSnowPath := skyTexturesPath + 'sky_sunset_snow.bmp';
    sunsetTwilightSnowPath := skyTexturesPath + 'sky_sunsetTwilight_snow.bmp';
    nightSnowPath := skyTexturesPath + 'sky_night_snow.bmp';
    sunriseDawnSnowPath := skyTexturesPath + 'sky_sunriseDawn_snow.bmp';
    sunriseSnowPath := skyTexturesPath + 'sky_sunrise_snow.bmp';

    if not (FileExists(sunriseDawnPath) and FileExists(sunsetTwilightPath) and
            FileExists(daySnowPath) and FileExists(sunsetSnowPath) and
            FileExists(sunsetTwilightSnowPath) and FileExists(nightSnowPath) and
            FileExists(sunriseDawnSnowPath) and FileExists(sunriseSnowPath)) then
    begin
      AddToLogFile(EngineLog, 'Sky textures (summer/winter) not found in route, skipping patch');
      Exit;
    end;

    CallAddress := $00400000 + $335FFF;
    DrawSkyAddress := Cardinal(@DrawWorldSkyRuntime);
    NewOffset := Integer(DrawSkyAddress) - Integer(CallAddress + 5);

    if VirtualProtect(Pointer(CallAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      PInteger(CallAddress + 1)^ := NewOffset;
      VirtualProtect(Pointer(CallAddress + 1), 4, OldProtect, OldProtect);
      DrawFunc3D.SkyPatchApplied := True;
      AddToLogFile(EngineLog, 'DrawSky call patched successfully for route: ' + routeName + ' (full summer + winter support)');
    end;

  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error patching DrawSky call: ' + E.Message);
    end;
  end;
end;

// ============================================================================
// Восстановление оригинального вызова неба. Перенесено из RestoreOriginalSkyCall.
// ============================================================================
procedure RestoreWorldSkyHook;
var
  CallAddress: Cardinal;
  OriginalOffset: Integer;
  OldProtect: DWORD;
begin
  if not DrawFunc3D.SkyPatchApplied then
  begin
    AddToLogFile(EngineLog, 'Sky patch not applied, nothing to restore');
    Exit;
  end;

  try
    CallAddress := $00400000 + $335FFF;
    OriginalOffset := $FFD49418;

    if VirtualProtect(Pointer(CallAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      PInteger(CallAddress + 1)^ := OriginalOffset;
      VirtualProtect(Pointer(CallAddress + 1), 4, OldProtect, OldProtect);
      DrawFunc3D.SkyPatchApplied := False;
      AddToLogFile(EngineLog, 'Original DrawSky call restored successfully');
    end;

  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Error restoring original DrawSky call: ' + E.Message);
    end;
  end;
end;

// ============================================================================
// Проверка смены состояния NewSky и применение/откат патча.
// Перенесено из ProcessNewSkyPatch.
// ============================================================================
procedure ProcessWorldSkyPatch;
var
  StateChanged: Boolean;
begin
  StateChanged := not DrawFunc3D.NewSkyInitialized or (newsky <> DrawFunc3D.LastNewSkyState);

  if StateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния NewSky: %s -> %s',
      [BoolToStr(DrawFunc3D.LastNewSkyState, True), BoolToStr(DrawFunc3D.Config_NewSky, True)]));

    if newsky then
    begin
      AddToLogFile(EngineLog, 'Применяем патч неба (newsky: 1)');
      ApplyWorldSkyHook;
    end
    else
    begin
      AddToLogFile(EngineLog, 'Восстанавливаем оригинальный вызов неба (newsky: 0)');
      RestoreWorldSkyHook;
    end;

    DrawFunc3D.LastNewSkyState := DrawFunc3D.Config_NewSky;
    DrawFunc3D.NewSkyInitialized := True;
  end;
end;

// ============================================================================
// Инициализация системы день/ночь (текстуры кабины).
// Перенесено из InitializeDayNightSystem.
// ============================================================================
procedure InitializeWorldDayNight;
var
  locFolder: string;
  directoryPath: string;
  dayFolderPath, nightFolderPath: string;
  cabDayPath, pultDayPath, day254Path, klubDayPath: string;
  cabNightPath, pultNightPath, night254Path, klubNightPath: string;
begin
  if DrawFunc3D.DayNightInitialized then Exit;

  try
    locFolder := DrawFunc3D.GetLocomotiveFolder(DrawFunc3D.GetLocomotiveTypeFromMemory);
    directoryPath := 'data\' + locFolder + '\' + DrawFunc3D.LocNum + '\';

    AddToLogFile(EngineLog, '=== ДИАГНОСТИКА СИСТЕМЫ ДЕНЬ/НОЧЬ ===');
    AddToLogFile(EngineLog, 'LocNum: ' + DrawFunc3D.LocNum);
    AddToLogFile(EngineLog, 'locFolder: ' + locFolder);
    AddToLogFile(EngineLog, 'directoryPath: ' + directoryPath);

    dayFolderPath := directoryPath;
    nightFolderPath := directoryPath + 'night';

    AddToLogFile(EngineLog, 'dayFolderPath: ' + dayFolderPath);
    AddToLogFile(EngineLog, 'nightFolderPath: ' + nightFolderPath);
    AddToLogFile(EngineLog, 'day folder exists: ' + BoolToStr(DirectoryExists(dayFolderPath), True));
    AddToLogFile(EngineLog, 'night folder exists: ' + BoolToStr(DirectoryExists(nightFolderPath), True));

    DrawFunc3D.HasDayNightFolders := DirectoryExists(dayFolderPath) and DirectoryExists(nightFolderPath);

    if DrawFunc3D.HasDayNightFolders then
    begin
      AddToLogFile(EngineLog, 'Найдены папки day/night, инициализируем систему смены текстур');

      cabDayPath := dayFolderPath + '\cab.bmp';
      pultDayPath := dayFolderPath + '\pult.bmp';
      day254Path := FindWorldTextureFile(dayFolderPath);
      klubDayPath := dayFolderPath + '\klub_bil.bmp';

      cabNightPath := nightFolderPath + '\cab.bmp';
      pultNightPath := nightFolderPath + '\pult.bmp';
      night254Path := FindWorldTextureFile(nightFolderPath);
      klubNightPath := nightFolderPath + '\klub_bil.bmp';

      AddToLogFile(EngineLog, '=== ДИАГНОСТИКА KLUB ФАЙЛОВ ===');
      AddToLogFile(EngineLog, 'klubDayPath: ' + klubDayPath);
      AddToLogFile(EngineLog, 'klubNightPath: ' + klubNightPath);
      AddToLogFile(EngineLog, 'Day klub file exists: ' + BoolToStr(FileExists(klubDayPath), True));
      AddToLogFile(EngineLog, 'Night klub file exists: ' + BoolToStr(FileExists(klubNightPath), True));

      if FileExists(klubDayPath) then
      begin
        try
          AddToLogFile(EngineLog, 'Попытка загрузки дневной klub текстуры...');
          DrawFunc3D.DayKlubTextureID := LoadTextureFromFile(klubDayPath, 0, -1);
          if DrawFunc3D.DayKlubTextureID > 0 then
            AddToLogFile(EngineLog, 'УСПЕШНО: Загружена дневная текстура klub: ' + IntToStr(DrawFunc3D.DayKlubTextureID))
          else
            AddToLogFile(EngineLog, 'ОШИБКА: LoadTextureFromFile вернул 0 для дневной klub текстуры');
        except
          on E: Exception do
            AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке дневной klub текстуры: ' + E.Message);
        end;
      end
      else
      begin
        AddToLogFile(EngineLog, 'ФАЙЛ НЕ НАЙДЕН: ' + klubDayPath);
      end;

      if FileExists(klubNightPath) then
      begin
        try
          AddToLogFile(EngineLog, 'Попытка загрузки ночной klub текстуры...');
          DrawFunc3D.NightKlubTextureID := LoadTextureFromFile(klubNightPath, 0, -1);
          if DrawFunc3D.NightKlubTextureID > 0 then
            AddToLogFile(EngineLog, 'УСПЕШНО: Загружена ночная текстура klub: ' + IntToStr(DrawFunc3D.NightKlubTextureID))
          else
            AddToLogFile(EngineLog, 'ОШИБКА: LoadTextureFromFile вернул 0 для ночной klub текстуры');
        except
          on E: Exception do
            AddToLogFile(EngineLog, 'ИСКЛЮЧЕНИЕ при загрузке ночной klub текстуры: ' + E.Message);
        end;
      end
      else
      begin
        AddToLogFile(EngineLog, 'ФАЙЛ НЕ НАЙДЕН: ' + klubNightPath);
      end;

      if FileExists(cabDayPath) then
      begin
        DrawFunc3D.DayCabTextureID := LoadTextureFromFile(cabDayPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена дневная текстура cab: ' + IntToStr(DrawFunc3D.DayCabTextureID));
      end;

      if FileExists(pultDayPath) then
      begin
        DrawFunc3D.DayPultTextureID := LoadTextureFromFile(pultDayPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена дневная текстура pult: ' + IntToStr(DrawFunc3D.DayPultTextureID));
      end;

      if day254Path <> '' then
      begin
        DrawFunc3D.Day254TextureID := LoadTextureFromFile(day254Path, 0, -1);
        AddToLogFile(EngineLog, 'Загружена дневная текстура 254: ' + IntToStr(DrawFunc3D.Day254TextureID));
      end;

      if FileExists(cabNightPath) then
      begin
        DrawFunc3D.NightCabTextureID := LoadTextureFromFile(cabNightPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена ночная текстура cab: ' + IntToStr(DrawFunc3D.NightCabTextureID));
      end;

      if FileExists(pultNightPath) then
      begin
        DrawFunc3D.NightPultTextureID := LoadTextureFromFile(pultNightPath, 0, -1);
        AddToLogFile(EngineLog, 'Загружена ночная текстура pult: ' + IntToStr(DrawFunc3D.NightPultTextureID));
      end;

      if night254Path <> '' then
      begin
        DrawFunc3D.Night254TextureID := LoadTextureFromFile(night254Path, 0, -1);
        AddToLogFile(EngineLog, 'Загружена ночная текстура 254: ' + IntToStr(DrawFunc3D.Night254TextureID));
      end;

      AddToLogFile(EngineLog, '=== ИТОГИ ИНИЦИАЛИЗАЦИИ ===');
      AddToLogFile(EngineLog, 'DayKlubTextureID: ' + IntToStr(DrawFunc3D.DayKlubTextureID));
      AddToLogFile(EngineLog, 'NightKlubTextureID: ' + IntToStr(DrawFunc3D.NightKlubTextureID));
      AddToLogFile(EngineLog, 'Система день/ночь инициализирована успешно');
    end
    else
    begin
      AddToLogFile(EngineLog, 'Папки day/night не найдены, система день/ночь отключена');
    end;

  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'КРИТИЧЕСКАЯ ОШИБКА инициализации системы день/ночь: ' + E.Message);
      DrawFunc3D.HasDayNightFolders := False;
    end;
  end;

  DrawFunc3D.DayNightInitialized := True;
end;

// ============================================================================
// Применение дневных/ночных текстур кабины. Перенесено из ApplyDayNightTextures.
// ============================================================================
procedure ApplyWorldDayNightTextures;
var
  isNight: Boolean;
  newTimeMode: Integer;
  textureAddr: Pointer;
  cabTextureID, pultTextureID, texture254ID, klubTextureID: Cardinal;
  OldProtect: DWORD;
begin
  if not DrawFunc3D.HasDayNightFolders then Exit;

  try
    isNight := IsWorldNightTime;
    newTimeMode := Integer(isNight);

    if newTimeMode = DrawFunc3D.CurrentTimeMode then Exit;

    DrawFunc3D.CurrentTimeMode := newTimeMode;
    DrawFunc3D.CurrentIsNight := isNight;

    if isNight then
    begin
      AddToLogFile(EngineLog, 'Переключение на ночные текстуры (' + GetWorldCurrentSeason + ', час: ' + IntToStr(DrawFunc3D.GetCurrentHour) + ')');
      cabTextureID := DrawFunc3D.NightCabTextureID;
      pultTextureID := DrawFunc3D.NightPultTextureID;
      texture254ID := DrawFunc3D.Night254TextureID;
      klubTextureID := DrawFunc3D.NightKlubTextureID;
    end
    else
    begin
      AddToLogFile(EngineLog, 'Переключение на дневные текстуры (' + GetWorldCurrentSeason + ', час: ' + IntToStr(DrawFunc3D.GetCurrentHour) + ')');
      cabTextureID := DrawFunc3D.DayCabTextureID;
      pultTextureID := DrawFunc3D.DayPultTextureID;
      texture254ID := DrawFunc3D.Day254TextureID;
      klubTextureID := DrawFunc3D.DayKlubTextureID;
    end;

    if cabTextureID > 0 then
    begin
      try
        textureAddr := Pointer(PCardinal(Pointer($91D427C))^ + $06);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(cabTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена cab текстура ID: ' + IntToStr(cabTextureID));
        end;
      except
        AddToLogFile(EngineLog, 'Ошибка установки cab текстуры');
      end;
    end;

    if pultTextureID > 0 then
    begin
      try
        textureAddr := Pointer(PCardinal(Pointer($91D427C))^ + $08);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(pultTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена pult текстура ID: ' + IntToStr(pultTextureID));
        end;
      except
        AddToLogFile(EngineLog, 'Ошибка установки pult текстуры');
      end;
    end;

    if texture254ID > 0 then
    begin
      try
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + $38);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(texture254ID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена 254 текстура ID: ' + IntToStr(texture254ID));
        end;
      except
        AddToLogFile(EngineLog, 'Ошибка установки 254 текстуры');
      end;
    end;

    if klubTextureID > 0 then
    begin
      try
        textureAddr := Pointer(PCardinal(Pointer($9110D60))^ + $34);
        if VirtualProtect(textureAddr, SizeOf(Word), PAGE_EXECUTE_READWRITE, OldProtect) then
        begin
          PWord(textureAddr)^ := Word(klubTextureID);
          VirtualProtect(textureAddr, SizeOf(Word), OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'Установлена klub текстура ID: ' + IntToStr(klubTextureID) + ' по адресу: ' + IntToHex(Cardinal(textureAddr), 8));
        end;
      except
        AddToLogFile(EngineLog, 'Ошибка установки klub текстуры');
      end;
    end;

  except
    on E: Exception do
      AddToLogFile(EngineLog, 'Ошибка применения текстур день/ночь: ' + E.Message);
  end;
end;

// ============================================================================
// Периодическая проверка системы день/ночь. Перенесено из ProcessDayNightSystem.
// ============================================================================
procedure ProcessWorldDayNight;
var
  currentTime: Cardinal;
  StateChanged: Boolean;
begin
  currentTime := timeGetTime;

  StateChanged := not DrawFunc3D.NewSkyInitialized or (DrawFunc3D.Config_NewSky <> DrawFunc3D.LastNewSkyState);

  if StateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния NewSky: %s -> %s',
      [BoolToStr(DrawFunc3D.LastNewSkyState, True), BoolToStr(DrawFunc3D.Config_NewSky, True)]));

    DrawFunc3D.LastNewSkyState := DrawFunc3D.Config_NewSky;
    DrawFunc3D.NewSkyInitialized := True;
  end;

  if (currentTime - DrawFunc3D.LastTimeCheck) > DrawFunc3D.TimeCheckInterval1 then
  begin
    if not DrawFunc3D.DayNightInitialized and DrawFunc3D.Config_NewSky then
    begin
      AddToLogFile(EngineLog, 'Инициализируем систему день/ночь');
      InitializeWorldDayNight;
    end;

    if DrawFunc3D.Config_NewSky and DrawFunc3D.HasDayNightFolders then
    begin
      ApplyWorldDayNightTextures;
    end
    else if StateChanged and not DrawFunc3D.Config_NewSky then
    begin
      AddToLogFile(EngineLog, 'Система день/ночь отключена в конфиге');
    end;

    DrawFunc3D.LastTimeCheck := currentTime;
  end;
end;

// ============================================================================
// Запись 80-bit Extended по адресу stepForward. Перенесено из
// WriteStepForwardToMemory (SingleToExtended80 -> BoosterExtended).
// ============================================================================
procedure WriteWorldStepForwardToMemory(Value: Single);
const
  TARGET_ADDRESS = $00725C24;
var
  ExtBytes: TBoosterExtendedBytes;
  OldProtect: DWORD;
  i: Integer;
begin
  try
    ExtBytes := SingleToBoosterExtended80(Value);

    if VirtualProtect(Pointer(TARGET_ADDRESS), 10, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      for i := 0 to 9 do
        PByte(TARGET_ADDRESS + i)^ := ExtBytes[i];

      VirtualProtect(Pointer(TARGET_ADDRESS), 10, OldProtect, OldProtect);
    end
    else
    begin
      AddToLogFile(EngineLog, 'Ошибка изменения защиты памяти для stepForward');
    end;

  except
    on E: Exception do
      AddToLogFile(EngineLog, 'Ошибка записи stepForward в память: ' + E.Message);
  end;
end;

// ============================================================================
// Периодическая проверка stepForward (MainCamera). Перенесено из
// ProcessStepForwardConfig.
// ============================================================================
procedure ProcessWorldStepForward;
var
  currentTime: Cardinal;
  StateChanged: Boolean;
begin
  currentTime := timeGetTime;

  StateChanged := not DrawFunc3D.MainCameraInitialized or (DrawFunc3D.Config_MainCamera <> DrawFunc3D.LastMainCameraState);

  if StateChanged then
  begin
    AddToLogFile(EngineLog, Format('Изменение состояния MainCamera: %s -> %s',
      [BoolToStr(DrawFunc3D.LastMainCameraState, True), BoolToStr(DrawFunc3D.Config_MainCamera, True)]));
  end;

  if DrawFunc3D.Config_MainCamera then
  begin
    if (currentTime - DrawFunc3D.LastStepForwardCheck) > DrawFunc3D.StepForwardCheckInterval then
    begin
      WriteWorldStepForwardToMemory(stepforward);

      if StateChanged then
        AddToLogFile(EngineLog, Format('stepForward применен: %.6f', [stepforward]));

      DrawFunc3D.LastStepForwardCheck := currentTime;
    end;
  end
  else
  begin
    if StateChanged then
    begin
      AddToLogFile(EngineLog, 'Восстанавливаем оригинальный stepForward: 0.1');
      WriteWorldStepForwardToMemory(DrawFunc3D.OriginalStepForwardValue);
    end;
  end;

  DrawFunc3D.LastMainCameraState := DrawFunc3D.Config_MainCamera;
  DrawFunc3D.MainCameraInitialized := True;
end;

// ============================================================================
// Снятие невидимого купола (zFar 1500 -> 8000). Перенесено из ApplyDomePatch.
// ============================================================================
procedure ApplyWorldDomePatch;
const
  ZFAR_TARGET_M = 8000.0;
var
  ZFarBuf: Double;
  ZFarHigh: Cardinal;
  InstructionAddress: PByte;
  OldProtect: DWORD;
  CurHighDWord: Cardinal;
begin
  try
    ZFarBuf := ZFAR_TARGET_M;
    ZFarHigh := PCardinal(Cardinal(@ZFarBuf) + 4)^;

    InstructionAddress := Pointer($00723B18);
    try
      CurHighDWord := PCardinal(InstructionAddress)^;
    except
      CurHighDWord := 0;
    end;
    if CurHighDWord = ZFarHigh then Exit;

    if VirtualProtect(InstructionAddress, 4, PAGE_EXECUTE_READWRITE, @OldProtect) then
    begin
      PCardinal(InstructionAddress)^ := ZFarHigh;
      FlushInstructionCache(GetCurrentProcess, InstructionAddress, 4);
      VirtualProtect(InstructionAddress, 4, OldProtect, @OldProtect);
      AddToLogFile(EngineLog,
        'DomePatch: $00723B18 ' + IntToHex(CurHighDWord, 8) +
        ' -> ' + IntToHex(ZFarHigh, 8) + ' (zFar=' + FloatToStr(ZFarBuf) + 'm)');
    end
    else
    begin
      AddToLogFile(EngineLog,
        'DomePatch: VirtualProtect FAILED for $00723B18: ' +
        SysErrorMessage(GetLastError));
    end;
  except
    on E: Exception do
      AddToLogFile(EngineLog, 'DomePatch: исключение - ' + E.Message);
  end;
end;

end.