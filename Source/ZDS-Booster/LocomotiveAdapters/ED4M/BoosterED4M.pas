unit BoosterED4M;

interface

procedure WriteHookAddressED4MRuntime; stdcall;

implementation

uses DrawFunc3D, Windows, EngineUtils, Variables, SysUtils;

procedure WriteHookAddressED4MRuntime; stdcall;
var
  HookAddr: Cardinal;
  CallAddress: Cardinal;
  NewOffset: Integer;
  OldProtect: DWORD;

  function SafeVirtualProtect(Address: Pointer; Size: Cardinal; NewProtect: DWORD; var OldProtect: DWORD): Boolean;
  var
    Attempts: Integer;
  begin
    Result := False;
    Attempts := 0;
    
    repeat
      try
        Result := VirtualProtect(Address, Size, NewProtect, OldProtect);
        if Result then Break;
        
        Inc(Attempts);
        if Attempts > 10 then Break;
          
      except
        Inc(Attempts);
        if Attempts > 5 then Break;
      end;
    until False;
  end;

begin
  try
    // Патчим HookKLUB для ED4M
    try
      HookAddr := Cardinal(@HookKLUB);
      CallAddress := $006296F6; // ← АДРЕС ДЛЯ ЭД4М
      NewOffset := Integer(HookAddr) - Integer(CallAddress + 5);
      
      if SafeVirtualProtect(Pointer(CallAddress + 1), 4, PAGE_EXECUTE_READWRITE, OldProtect) then
      begin
        try
          PInteger(CallAddress + 1)^ := NewOffset;
          SafeVirtualProtect(Pointer(CallAddress + 1), 4, OldProtect, OldProtect);
          AddToLogFile(EngineLog, 'ЭД4М: HookKLUB патч применен по адресу $' + IntToHex(CallAddress, 8));
        except
          // Игнорируем ошибки записи
        end;
      end;
    except
      // Игнорируем ошибки
    end;
    
  except
    on E: Exception do
    begin
      AddToLogFile(EngineLog, 'Ошибка в WriteHookAddressED4M: ' + E.Message);
    end;
  end;
end;

end.