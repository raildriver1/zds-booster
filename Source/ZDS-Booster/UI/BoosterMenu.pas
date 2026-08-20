unit BoosterMenu;

interface

procedure SyncBoosterMenu(Freecam, MainCamera, MaxDistance, NewSky: Boolean);
function BoosterMenuFreecam: Boolean;
function BoosterMenuMainCamera: Boolean;
function BoosterMenuMaxDistance: Boolean;
function BoosterMenuNewSky: Boolean;

implementation

uses DrawFunc3D;

procedure SyncBoosterMenu(Freecam, MainCamera, MaxDistance, NewSky: Boolean);
begin
  SyncConfigFromMenu(Freecam, MainCamera, MaxDistance, NewSky);
end;

function BoosterMenuFreecam: Boolean;
begin
  Result := GetConfigFreecam;
end;

function BoosterMenuMainCamera: Boolean;
begin
  Result := GetConfigMainCamera;
end;

function BoosterMenuMaxDistance: Boolean;
begin
  Result := GetConfigMaxDistance;
end;

function BoosterMenuNewSky: Boolean;
begin
  Result := GetConfigNewSky;
end;

end.
