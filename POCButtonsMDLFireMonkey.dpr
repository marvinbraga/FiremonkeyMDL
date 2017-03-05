program POCButtonsMDLFireMonkey;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMDLButtons in 'uMDLButtons.pas' {FormPOCButtonsMDL};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPOCButtonsMDL, FormPOCButtonsMDL);
  Application.Run;
end.
