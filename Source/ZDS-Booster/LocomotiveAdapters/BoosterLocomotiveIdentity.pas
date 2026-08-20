unit BoosterLocomotiveIdentity;

interface

function ReadLocomotiveNumberFromSettings: string;
function ReadLocomotiveTypeFromSettings: Integer;
function ReadLocomotiveTypeFromMemory: Integer;

implementation

uses SysUtils;

function ReadLocomotiveNumberFromSettings: string;
var
  F: TextFile;
  Line, Name, Value: string;
  P: Integer;
begin
  Result := '068';
  if not FileExists('settings.ini') then Exit;
  try
    AssignFile(F, 'settings.ini');
    Reset(F);
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      Line := Trim(Line);
      if (Line = '') or (Line[1] = '#') or (Line[1] = ';') then Continue;
      P := Pos('=', Line);
      if P <= 0 then Continue;
      Name := LowerCase(Trim(Copy(Line, 1, P - 1)));
      Value := Trim(Copy(Line, P + 1, Length(Line)));
      if Name = 'locnum' then Result := Value;
    end;
    CloseFile(F);
  except
    try CloseFile(F); except end;
  end;
end;

function ReadLocomotiveTypeFromSettings: Integer;
var
  F: TextFile;
  Line, Name, Value: string;
  P: Integer;
begin
  Result := 822;
  if not FileExists('settings.ini') then Exit;
  try
    AssignFile(F, 'settings.ini');
    Reset(F);
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      Line := Trim(Line);
      if (Line = '') or (Line[1] = '#') or (Line[1] = ';') then Continue;
      P := Pos('=', Line);
      if P <= 0 then Continue;
      Name := LowerCase(Trim(Copy(Line, 1, P - 1)));
      Value := Trim(Copy(Line, P + 1, Length(Line)));
      if Name = 'locomotivetype' then
        Result := StrToIntDef(Value, Result);
    end;
    CloseFile(F);
  except
    try CloseFile(F); except end;
  end;
end;

function ReadLocomotiveTypeFromMemory: Integer;
begin
  try
    Result := PInteger(Pointer($00400000 + $4F8D93C))^;
  except
    Result := 822;
  end;
end;

end.
