program Ackermann;

uses
  Forms,
  main in 'main.pas' {frmAckermann};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAckermann, frmAckermann);
  Application.Run;
end.
