unit BoosterMemory;

interface

function WriteBoosterAndVerify(Address: Cardinal; Value: Integer;
  MaxAttempts: Integer = 10): Boolean;
procedure NopMemory(Address: Pointer; Size: Cardinal);
function ReadMemorySingle(Address: Cardinal): Single;
procedure WriteMemorySingle(Address: Cardinal; Value: Single);
procedure WriteMemoryDouble(Address: Cardinal; Value: Double);
procedure WriteByteToMemory(Address: Pointer; Value: Byte);
procedure WriteDWordToMemory(Address: Pointer; Value: LongWord);

implementation

uses Windows;

function WriteBoosterAndVerify(Address: Cardinal; Value: Integer;
  MaxAttempts: Integer): Boolean;
var
  Addr: Pointer;
  OldProtect: DWORD;
  CurrentValue, Attempt: Integer;
begin
  Result := False;
  Addr := Pointer(Address);
  Attempt := 0;
  while Attempt < MaxAttempts do
  begin
    Inc(Attempt);
    try
      if VirtualProtect(Addr, SizeOf(Integer), PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        PInteger(Addr)^ := Value;
        VirtualProtect(Addr, SizeOf(Integer), OldProtect, OldProtect);
        CurrentValue := PInteger(Addr)^;
        if CurrentValue = Value then
        begin
          Result := True;
          Exit;
        end;
      end;
    except
    end;
    Sleep(1);
  end;
end;

procedure NopMemory(Address: Pointer; Size: Cardinal);
var OldProtect: DWORD; I: Integer;
begin
  if VirtualProtect(Address, Size, PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    for I := 0 to Size - 1 do PByte(NativeUInt(Address) + I)^ := $90;
    VirtualProtect(Address, Size, OldProtect, OldProtect);
  end;
end;

function ReadMemorySingle(Address: Cardinal): Single;
begin
  try Result := PSingle(Pointer(Address))^; except Result := 0.0; end;
end;

procedure WriteMemorySingle(Address: Cardinal; Value: Single);
begin
  try PSingle(Pointer(Address))^ := Value; except end;
end;

procedure WriteMemoryDouble(Address: Cardinal; Value: Double);
begin
  try PDouble(Pointer(Address))^ := Value; except end;
end;

procedure WriteByteToMemory(Address: Pointer; Value: Byte);
begin
  try PByte(Address)^ := Value; except end;
end;

procedure WriteDWordToMemory(Address: Pointer; Value: LongWord);
begin
  try PLongWord(Address)^ := Value; except end;
end;

end.
