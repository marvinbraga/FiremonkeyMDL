program POCButtonsMDLFireMonkey;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMDLButtons in 'uMDLButtons.pas' {FormPOCButtonsMDL};

{$R *.res}

begin
  {$IFDEF DEBUG}
    ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TFormPOCButtonsMDL, FormPOCButtonsMDL);
  Application.Run;
end.
