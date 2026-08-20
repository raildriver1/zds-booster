unit BoosterConfigText;

interface

function FormatBoosterFloat(Value: Single): string;
function BoosterLineHasKey(const Line, Key: string): Boolean;

implementation

uses SysUtils;

function FormatBoosterFloat(Value: Single): string;
var OldSeparator: Char;
begin
  OldSeparator := DecimalSeparator;
  try
    DecimalSeparator := '.';
    Result := Format('%.2f', [Value]);
  finally
    DecimalSeparator := OldSeparator;
  end;
end;

function BoosterLineHasKey(const Line, Key: string): Boolean;
var Trimmed: string; ColonPos: Integer;
begin
  Trimmed := Trim(Line);
  ColonPos := Pos(':', Trimmed);
  Result := (ColonPos > 0) and
    (Trim(Copy(Trimmed, 1, ColonPos - 1)) = Key);
end;

end.
