unit BoosterExtended;

interface

type
  TBoosterExtendedBytes = array[0..9] of Byte;

function SingleToBoosterExtended80(Value: Single): TBoosterExtendedBytes;

implementation

function SingleToBoosterExtended80(Value: Single): TBoosterExtendedBytes;
var
  ExtValue: Extended;
begin
  ExtValue := Value;
  Move(ExtValue, Result, 10);
end;

end.
