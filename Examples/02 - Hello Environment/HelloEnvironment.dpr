program HelloEnvironment;

uses
  Forms,
  main in 'main.pas' {frmHelloWorld},
  rsCompiler in '..\..\source\rsCompiler.pas',
  rsDefsBackend in '..\..\source\rsDefsBackend.pas',
  rsExec in '..\..\source\rsExec.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHelloWorld, frmHelloWorld);
  Application.Run;
end.
