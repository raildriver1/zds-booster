unit BoosterKPD3;

interface

function InstallKPD3: Boolean;

implementation

uses DrawFunc3D;

function InstallKPD3: Boolean;
begin
  Result := ApplyKPD3Patch;
end;

end.
