unit BoosterTrafficRuntime;

interface

uses
  Classes, SysUtils, Windows, Math;

function GetSignalSequenceRuntime(AlsAddr: Cardinal): string;
procedure InitializeTrafficLightSystemRuntime(const ARouteName: string);
procedure FreeTrafficRuntime;

implementation

var
  s1, s2: TStringList;
  TrafficSystemInitialized: Boolean = False;
  CurrentAlsAddr: Cardinal = 0;
  CurrentRouteName: string = '';

function SignalColor(code: Integer): Char;
begin
  case code of
    0: Result := Char($D7);
    1: Result := Char($CA);
    2: Result := Char($C6);
    3: Result := Char($C7);
    else Result := '?';
  end;
end;

function LoadDataFile(filename: string): TStringList;
var
  f: TextFile;
  line: string;
  piketNum: Integer;
begin
  Result := TStringList.Create;
  if FileExists(filename) then
  begin
    try
      AssignFile(f, filename);
      Reset(f);
      while not Eof(f) do
      begin
        ReadLn(f, line);
        line := Trim(line);
        if line <> '' then
        begin
          piketNum := StrToInt(Copy(line, 1, Pos(#9, line + #9) - 1));
          Result.Add(IntToStr(piketNum));
        end;
      end;
      CloseFile(f);
    except
      // РРіРЅРѕСЂРёСЂСѓРµРј РѕС€РёР±РєРё
    end;
  end;
end;

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToInt(List[Index1]) - StrToInt(List[Index2]);
end;


// Р”РѕР±Р°РІР»СЏРµРј РЅРµРґРѕСЃС‚Р°СЋС‰СѓСЋ С„СѓРЅРєС†РёСЋ ArtificialSignalColor
function ArtificialSignalColor(code: Integer): string;
begin
  case code of
    0: Result := #$C2;
    1: Result := #$C1;
    2: Result := #$CA;
    3: Result := #$CA + #$C6;
    4: Result := #$C6;
    5: Result := #$C7;
    else Result := '?';
  end;
end;

function GetSignalSequence: string;
var
  i, j, piketNum, signalState, currentPiket, closestIndex, minDistance: Integer;
  offset, baseAddr, currentPiketAddr, trafficLightAddr: Cardinal;
  oneDirection: Boolean;
  trafficLightState: Byte;
  piketData: array of record
    offset: Cardinal;
    piketNum: Integer;
    signalState: Integer;
  end;
  filteredPikets, uniquePikets: TStringList;
  displayStart, displayEnd: Integer;
  tempIndex: Integer;
  existingOffset, newOffset: Cardinal;
begin
  Result := '';
  
  baseAddr := $900805C;
  currentPiketAddr := $749A0C;
  trafficLightAddr := CurrentAlsAddr;
  oneDirection := PByte(Pointer($749818))^ = 1;
  currentPiket := PInteger(Pointer(currentPiketAddr))^;
  trafficLightState := PByte(Pointer(trafficLightAddr))^;
  
  // РЎРѕР±РёСЂР°РµРј РґР°РЅРЅС‹Рµ Рѕ РїРёРєРµС‚Р°С…
  SetLength(piketData, s1.Count + s2.Count);
  for i := 0 to High(piketData) do
  begin
    offset := baseAddr + $858 * Cardinal(i);
    piketData[i].offset := offset;
    piketData[i].piketNum := PInteger(Pointer(offset))^;
    piketData[i].signalState := PInteger(Pointer(offset + $70))^;
  end;
  
  // Р¤РёР»СЊС‚СЂСѓРµРј РїРѕ РЅР°РїСЂР°РІР»РµРЅРёСЋ
  filteredPikets := TStringList.Create;
  uniquePikets := TStringList.Create;
  try
    // РЎРЅР°С‡Р°Р»Р° С„РёР»СЊС‚СЂСѓРµРј РїРѕ РЅР°РїСЂР°РІР»РµРЅРёСЋ
    for i := 0 to High(piketData) do
    begin
      if oneDirection then
      begin
        if s1.IndexOf(IntToStr(piketData[i].piketNum)) >= 0 then
          filteredPikets.AddObject(IntToStr(piketData[i].piketNum), TObject(i));
      end
      else
      begin
        if s2.IndexOf(IntToStr(piketData[i].piketNum)) >= 0 then
          filteredPikets.AddObject(IntToStr(piketData[i].piketNum), TObject(i));
      end;
    end;
    
    // Р’РђР–РќРћ: РЈР±РёСЂР°РµРј РґСѓР±Р»РёРєР°С‚С‹, РІС‹Р±РёСЂР°СЏ РЅСѓР¶РЅС‹Р№ Р°РґСЂРµСЃ РІ Р·Р°РІРёСЃРёРјРѕСЃС‚Рё РѕС‚ РЅР°РїСЂР°РІР»РµРЅРёСЏ
    for i := 0 to filteredPikets.Count - 1 do
    begin
      piketNum := StrToInt(filteredPikets[i]);
      tempIndex := Integer(filteredPikets.Objects[i]);
      newOffset := piketData[tempIndex].offset;
      
      j := uniquePikets.IndexOf(IntToStr(piketNum));
      if j = -1 then
      begin
        // РџРёРєРµС‚Р° РµС‰Рµ РЅРµС‚ - РґРѕР±Р°РІР»СЏРµРј
        uniquePikets.AddObject(IntToStr(piketNum), TObject(tempIndex));
      end
      else
      begin
        // РџРёРєРµС‚ СѓР¶Рµ РµСЃС‚СЊ - РІС‹Р±РёСЂР°РµРј РЅСѓР¶РЅС‹Р№ Р°РґСЂРµСЃ
        existingOffset := piketData[Integer(uniquePikets.Objects[j])].offset;
        
        if oneDirection then
        begin
          // Р”Р»СЏ РїСЂСЏРјРѕРіРѕ РЅР°РїСЂР°РІР»РµРЅРёСЏ РІС‹Р±РёСЂР°РµРј РјРµРЅСЊС€РёР№ Р°РґСЂРµСЃ
          if newOffset < existingOffset then
            uniquePikets.Objects[j] := TObject(tempIndex);
        end
        else
        begin
          // Р”Р»СЏ РѕР±СЂР°С‚РЅРѕРіРѕ РЅР°РїСЂР°РІР»РµРЅРёСЏ РІС‹Р±РёСЂР°РµРј Р±РѕР»СЊС€РёР№ Р°РґСЂРµСЃ
          if newOffset > existingOffset then
            uniquePikets.Objects[j] := TObject(tempIndex);
        end;
      end;
    end;
    
    // РЎРѕСЂС‚РёСЂСѓРµРј РїРѕ РЅРѕРјРµСЂСѓ РїРёРєРµС‚Р°
    uniquePikets.CustomSort(CompareStrings);
    
    // РќР°С…РѕРґРёРј Р±Р»РёР¶Р°Р№С€РёР№ СЃР»РµРґСѓСЋС‰РёР№ РїРёРєРµС‚
    closestIndex := -1;
    minDistance := MaxInt;
    
    for i := 0 to uniquePikets.Count - 1 do
    begin
      piketNum := StrToInt(uniquePikets[i]);
      if oneDirection then
      begin
        if (piketNum > currentPiket) and (piketNum - currentPiket < minDistance) then
        begin
          closestIndex := i;
          minDistance := piketNum - currentPiket;
        end;
      end
      else
      begin
        if (piketNum < currentPiket) and (currentPiket - piketNum < minDistance) then
        begin
          closestIndex := i;
          minDistance := currentPiket - piketNum;
        end;
      end;
    end;
    
    if closestIndex >= 0 then
    begin
      // РћРїСЂРµРґРµР»СЏРµРј РґРёР°РїР°Р·РѕРЅ РґР»СЏ Р°РЅР°Р»РёР·Р° (РєР°Рє РІ Python)
      if oneDirection then
      begin
        displayStart := Max(closestIndex - 1, 0);
        displayEnd := Min(closestIndex + 4, uniquePikets.Count - 1);
      end
      else
      begin
        displayStart := Max(closestIndex - 5, 0);
        displayEnd := closestIndex;
      end;
      
      // Р¤РѕСЂРјРёСЂСѓРµРј РїРѕСЃР»РµРґРѕРІР°С‚РµР»СЊРЅРѕСЃС‚СЊ СЃРёРіРЅР°Р»РѕРІ
      if oneDirection then
      begin
        // РџСЂСЏРјРѕРµ РЅР°РїСЂР°РІР»РµРЅРёРµ - РєР°Рє РµСЃС‚СЊ
        for i := displayStart to displayEnd do
        begin
          j := Integer(uniquePikets.Objects[i]);
          Result := Result + SignalColor(piketData[j].signalState);
          // РћСЃС‚Р°РЅР°РІР»РёРІР°РµРјСЃСЏ РЅР° С‡РµСЂРЅРѕРј СЃРёРіРЅР°Р»Рµ
          if piketData[j].signalState = 0 then Break;
        end;
      end
      else
      begin
        // РћР±СЂР°С‚РЅРѕРµ РЅР°РїСЂР°РІР»РµРЅРёРµ - РІ РѕР±СЂР°С‚РЅРѕРј РїРѕСЂСЏРґРєРµ (РєР°Рє [::-1] РІ Python)
        for i := displayEnd downto displayStart do
        begin
          j := Integer(uniquePikets.Objects[i]);
          Result := Result + SignalColor(piketData[j].signalState);
          // РћСЃС‚Р°РЅР°РІР»РёРІР°РµРјСЃСЏ РЅР° С‡РµСЂРЅРѕРј СЃРёРіРЅР°Р»Рµ
          if piketData[j].signalState = 0 then Break;
        end;
      end;
    end;
    
    // Р”РѕР±Р°РІР»СЏРµРј РїСЂРµС„РёРєСЃ РёСЃРєСѓСЃСЃС‚РІРµРЅРЅРѕРіРѕ СЃРІРµС‚РѕС„РѕСЂР° РґР»СЏ РѕР±СЂР°С‚РЅРѕРіРѕ РЅР°РїСЂР°РІР»РµРЅРёСЏ (РєР°Рє РІ Python)
    if not oneDirection then
    begin
      Result := ArtificialSignalColor(trafficLightState) + Result;
    end;
    
  finally
    filteredPikets.Free;
    uniquePikets.Free;
  end;
end;

procedure InitializeTrafficLightSystem;
var
  routeName: string;
  filePath1, filePath2: string;
begin
  if TrafficSystemInitialized then Exit;
  
  // РџРѕР»СѓС‡Р°РµРј РЅР°Р·РІР°РЅРёРµ РјР°СЂС€СЂСѓС‚Р°
  routeName := CurrentRouteName;
  
  // Р¤РѕСЂРјРёСЂСѓРµРј РїСѓС‚Рё Рє С„Р°Р№Р»Р°Рј
  if routeName <> '' then
  begin
    filePath1 := 'routes\' + routeName + '\svetofor1.dat';
    filePath2 := 'routes\' + routeName + '\svetofor2.dat';
  end
  else
  begin
    // Fallback РЅР° СЃС‚Р°РЅРґР°СЂС‚РЅС‹Рµ С„Р°Р№Р»С‹
    filePath1 := 'svetofor1.dat';
    filePath2 := 'svetofor2.dat';
  end;
  
  // РРЅРёС†РёР°Р»РёР·РёСЂСѓРµРј СЃРїРёСЃРєРё РїРёРєРµС‚РѕРІ
  if not Assigned(s1) then s1 := LoadDataFile(filePath1);
  if not Assigned(s2) then s2 := LoadDataFile(filePath2);
  
  //AddToLogFile(EngineLog, 'Р—Р°РіСЂСѓР¶РµРЅ С„Р°Р№Р» svetofor1: ' + filePath1);
  //AddToLogFile(EngineLog, 'Р—Р°РіСЂСѓР¶РµРЅ С„Р°Р№Р» svetofor2: ' + filePath2);
  
  TrafficSystemInitialized := True;
end;

// Р¤СѓРЅРєС†РёСЏ РґР»СЏ РїСЂРѕРІРµСЂРєРё, РЅСѓР¶РЅРѕ Р»Рё РїРѕРєР°Р·С‹РІР°С‚СЊ С†РёС„СЂСѓ float

function GetSignalSequenceRuntime(AlsAddr: Cardinal): string;
begin
  CurrentAlsAddr := AlsAddr;
  Result := GetSignalSequence;
end;

procedure InitializeTrafficLightSystemRuntime(const ARouteName: string);
begin
  CurrentRouteName := ARouteName;
  InitializeTrafficLightSystem;
end;

procedure FreeTrafficRuntime;
begin
  FreeAndNil(s1);
  FreeAndNil(s2);
  TrafficSystemInitialized := False;
  CurrentRouteName := '';
  CurrentAlsAddr := 0;
end;

end.
