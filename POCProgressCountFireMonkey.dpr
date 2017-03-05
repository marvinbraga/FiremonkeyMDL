program POCProgressCountFireMonkey;

uses
  System.StartUpCopy,
  FMX.Forms,
  uProgressCount in 'uProgressCount.pas' {FormProgressCountApplicationPOC};

{$R *.res}

begin
  {$WARN SYMBOL_PLATFORM OFF}
  //ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$WARN SYMBOL_PLATFORM ON}
  Application.Initialize;
  Application.CreateForm(TFormProgressCountApplicationPOC, FormProgressCountApplicationPOC);
  Application.Run;
end.
