unit main;

interface

uses
  Forms, Classes, Controls, StdCtrls,
  rsCompiler, rsExec;

type
  TfrmHelloWorld = class(TForm)
    btnRun: TButton;
    procedure btnRunClick(Sender: TObject);
  end;

  TEnvironment = class
  public
      procedure ShowMessage(const msg: string);
  end;

var
  frmHelloWorld: TfrmHelloWorld;

implementation
uses
   Dialogs,
   rsDefsBackend;

{$R *.dfm}

procedure TEnvironment.ShowMessage(const msg: string);
begin
   Application.MessageBox(PChar(msg), 'RTTI Script message');
end;

procedure TfrmHelloWorld.btnRunClick(Sender: TObject);
const
   CRLF = #13#10;
   SCRIPT =
      'unit hello;' + CRLF +
      'procedure Test;' + CRLF +
      'begin' + CRLF +
      '   ShowMessage(''Hello Environment!'');' + CRLF +
      'end;' + CRLF +
      'end.';
var
   compiler: TrsCompiler;
   prog: TrsProgram;
   exec: TrsExec;
   env: TEnvironment;
begin
   compiler := TrsCompiler.Create;
   exec := TrsExec.Create;
   env := TEnvironment.Create;
   prog := nil;
   try
      compiler.RegisterEnvironment(TEnvironment);
      prog := compiler.Compile(SCRIPT);
      exec.SetEnvironment(env);
      exec.Load(prog);
      exec.RunProc('Test', []);
   finally
      compiler.Free;
      prog.Free;
      env.Free;
   end;
end;

end.
