unit LocomotiveHookRegistry;

interface

function LocoPatchOffset(TypeId: Integer): Cardinal;
function KPD3PatchOffset(TypeId: Integer): Cardinal;
function BLOKPatchOffset(TypeId: Integer): Cardinal;

implementation

function LocoPatchOffset(TypeId: Integer): Cardinal;
begin
  case TypeId of
    812: Result := $7245D8;
    822: Result := $7245EF;
    882: Result := $724606;
    880: Result := $72461D;
    621: Result := $724634;
    523: Result := $724643;
    524: Result := $724662;
    2070: Result := $724679;
    2071: Result := $724690;
    1462: Result := $7246A7;
    21014: Result := $7246BE;
    3154: Result := $7246D5;
    3159: Result := $7246EC;
    23152: Result := $724703;
    23142: Result := $724717;
    343: Result := $72472B;
    31714: Result := $72473F;
    811: Result := $724753;
    885: Result := $724767;
    201318: Result := $72477B;
    else Result := $3246D5;
  end;
end;

function KPD3PatchOffset(TypeId: Integer): Cardinal;
begin
  case TypeId of
    524: Result := $1254F4;
    822: Result := $27795A;
    812: Result := $D5A85;
    811: Result := $2BB937;
    882: Result := $1461D5;
    880: Result := $18D236;
    2070: Result := $281156;
    21014: Result := $20F90F;
    1462: Result := $1C842B;
    else Result := 0;
  end;
end;

function BLOKPatchOffset(TypeId: Integer): Cardinal;
begin
  case TypeId of
    621: Result := $5DD854;
    880: Result := $58D217;
    2070: Result := $681137;
    885: Result := $6C2FBB;
    else Result := 0;
  end;
end;

end.
