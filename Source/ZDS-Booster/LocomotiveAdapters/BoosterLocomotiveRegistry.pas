unit BoosterLocomotiveRegistry;

interface

type
  TBoosterSystem = (bsKPD3, bsBLOK, bsKLUB);
  TBoosterSystemSet = set of TBoosterSystem;

  TLocomotiveAdapter = record
    TypeId: Integer;
    Folder: string;
    DisplayName: string;
    Systems: TBoosterSystemSet;
  end;

function FindLocomotiveAdapter(TypeId: Integer;
  out Adapter: TLocomotiveAdapter): Boolean;
function LocomotiveFolder(TypeId: Integer): string;

implementation

const
  AdapterTable: array[0..19] of TLocomotiveAdapter = (
    (TypeId: 812;    Folder: 'chs8';    DisplayName: 'CHS8';    Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 822;    Folder: 'chs7';    DisplayName: 'CHS7';    Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 882;    Folder: 'vl82';    DisplayName: 'VL82M';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 880;    Folder: 'vl80t';   DisplayName: 'VL80T';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 523;    Folder: 'chs4';    DisplayName: 'CHS4';    Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 524;    Folder: 'chs4kvr'; DisplayName: 'CHS4KVR'; Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 621;    Folder: 'chs4t';   DisplayName: 'CHS4T';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 2070;   Folder: 'tep70';   DisplayName: 'TEP70';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 2071;   Folder: 'tep70bs'; DisplayName: 'TEP70BS'; Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 3154;   Folder: 'ed4m';    DisplayName: 'ED4M';    Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 3159;   Folder: 'ed9m';    DisplayName: 'ED9M';    Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 23152;  Folder: 'es5k';    DisplayName: '2ES5K';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 23142;  Folder: 'es4k';    DisplayName: '2ES4K';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 343;    Folder: 'chs2k';   DisplayName: 'CHS2K';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 31714;  Folder: 'ep1m';    DisplayName: 'EP1M';    Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 811;    Folder: 'vl11m';   DisplayName: 'VL11M';   Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 885;    Folder: 'vl85';    DisplayName: 'VL85';    Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 1462;   Folder: 'm62';     DisplayName: 'M62';     Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 21014;  Folder: '2te10u';  DisplayName: '2TE10U';  Systems: [bsKPD3, bsBLOK, bsKLUB]),
    (TypeId: 201318; Folder: 'tem18dm'; DisplayName: 'TEM18DM'; Systems: [bsKPD3, bsBLOK, bsKLUB])
  );

function FindLocomotiveAdapter(TypeId: Integer;
  out Adapter: TLocomotiveAdapter): Boolean;
var
  I: Integer;
begin
  Result := False;
  FillChar(Adapter, SizeOf(Adapter), 0);
  for I := Low(AdapterTable) to High(AdapterTable) do
    if AdapterTable[I].TypeId = TypeId then
    begin
      Adapter := AdapterTable[I];
      Result := True;
      Exit;
    end;
end;

function LocomotiveFolder(TypeId: Integer): string;
var
  Adapter: TLocomotiveAdapter;
begin
  if FindLocomotiveAdapter(TypeId, Adapter) then
    Result := Adapter.Folder
  else
    Result := 'chs7';
end;

end.
