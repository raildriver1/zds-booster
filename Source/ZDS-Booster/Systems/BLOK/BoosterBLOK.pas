unit BoosterBLOK;

interface

function InstallBLOK: Boolean;
function HandleBLOKClick(MouseX, MouseY: Integer): Boolean;

implementation

uses DrawFunc3D;

function InstallBLOK: Boolean;
begin
  Result := ApplyBLOKPatch;
end;

function HandleBLOKClick(MouseX, MouseY: Integer): Boolean;
begin
  Result := HandleBlockKeyboardClick(MouseX, MouseY);
end;

end.
