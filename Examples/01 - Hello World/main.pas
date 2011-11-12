unit main;

interface

uses
  Forms, Classes, Controls, StdCtrls,
  rsCompiler, rsExec;

type
  TfrmHelloWorld = class(TForm)
    btnRun: TButton;
    procedure btnRunClick(Sender: TObject);
  private
    procedure RegisterSystemC(input: TRsTypeImporter);
    procedure RegisterSystemE(RegisterFunction: TExecImportCall);
  public
  end;

var
  frmHelloWorld: TfrmHelloWorld;

implementation
uses
   Dialogs,
   rsDefsBackend;

{$R *.dfm}

procedure ShowMessage(const msg: string);
begin
   Application.MessageBox(PChar(msg), 'RTTI Script message');
end;

procedure TfrmHelloWorld.RegisterSystemC(input: TRsTypeImporter);
begin
   input.ImportFunction('procedure ShowMessage(const msg: string);');
end;

procedure TfrmHelloWorld.RegisterSystemE(RegisterFunction: TExecImportCall);
begin
   RegisterFunction('ShowMessage', @ShowMessage);
end;

procedure TfrmHelloWorld.btnRunClick(Sender: TObject);
const
   CRLF = #13#10;
   SCRIPT =
      'unit hello;' + CRLF +
      'procedure Test;' + CRLF +
      'begin' + CRLF +
      '   ShowMessage(''Hello World!'');' + CRLF +
      'end;' + CRLF +
      'end.';
var
   compiler: TrsCompiler;
   prog: TrsProgram;
   exec: TrsExec;
begin
   compiler := TrsCompiler.Create;
   exec := TrsExec.Create;
   prog := nil;
   try
      compiler.RegisterStandardUnit('system', RegisterSystemC);
      exec.RegisterStandardUnit('system', RegisterSystemE);
      prog := compiler.Compile(SCRIPT);
      exec.Load(prog);
      exec.RunProc('Test', []);
   finally
      compiler.Free;
      prog.Free;
   end;
end;

end.
