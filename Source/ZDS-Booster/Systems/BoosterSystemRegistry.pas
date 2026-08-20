unit BoosterSystemRegistry;

interface

type
  TBoosterSystemId = (bsiKPD3, bsiBLOK, bsiKLUB);
  TBoosterSystemDescriptor = record
    Id: TBoosterSystemId;
    Name: string;
    CodeUnit: string;
  end;

function GetSystemDescriptor(Id: TBoosterSystemId): TBoosterSystemDescriptor;
function SystemName(Id: TBoosterSystemId): string;

implementation

const
  SystemDescriptors: array[TBoosterSystemId] of TBoosterSystemDescriptor = (
    (Id: bsiKPD3; Name: 'KPD-3'; CodeUnit: 'Systems.KPD3'),
    (Id: bsiBLOK; Name: 'BLOK';  CodeUnit: 'Systems.BLOK'),
    (Id: bsiKLUB; Name: 'KLUB';  CodeUnit: 'Systems.KLUB')
  );

function GetSystemDescriptor(Id: TBoosterSystemId): TBoosterSystemDescriptor;
begin
  Result := SystemDescriptors[Id];
end;

function SystemName(Id: TBoosterSystemId): string;
begin
  Result := SystemDescriptors[Id].Name;
end;

end.
