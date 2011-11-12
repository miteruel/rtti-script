program HelloWorld;

uses
  Forms,
  main in 'main.pas' {frmHelloWorld};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHelloWorld, frmHelloWorld);
  Application.Run;
end.
