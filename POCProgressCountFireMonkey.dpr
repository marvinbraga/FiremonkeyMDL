program POCProgressCountFireMonkey;

uses
  System.StartUpCopy,
  FMX.Forms,
  uProgressCount in 'uProgressCount.pas' {FormProgressCountApplicationPOC};

{$R *.res}

begin
  {$IFDEF DEBUG}
    ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TFormProgressCountApplicationPOC, FormProgressCountApplicationPOC);
  Application.Run;
end.
