program HelloEnvironment;

uses
  Forms,
  main in 'main.pas' {frmHelloWorld},
  rsCompiler in '..\..\rsCompiler.pas',
  rsExec in '..\..\rsExec.pas',
  rsDefsBackend in '..\..\rsDefsBackend.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHelloWorld, frmHelloWorld);
  Application.Run;
end.
