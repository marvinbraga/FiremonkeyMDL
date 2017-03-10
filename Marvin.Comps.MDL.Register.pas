{ /------------------------------------------------------------------/
  / Por Marcus Vinicius Braga - Marvinbraga Youtube Channel - Brazil /
  /------------------------------------------------------------------/  }

unit Marvin.Comps.MDL.Register;

interface

procedure Register;

implementation

uses
  System.Classes,
  Marvin.Comps.MDL.ProgressCount,
  Marvin.Comps.MDL.Buttons;

procedure Register;
begin
  RegisterComponents('Marvin', [TMRVProgressCount]);
  RegisterComponents('Marvin', [TMRVButtonMDL]);
end;

end.
