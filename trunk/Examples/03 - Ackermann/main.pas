unit main;

interface

uses
  Forms, Classes, Controls, StdCtrls,
  rsCompiler, rsExec, Spin;

type
  TfrmAckermann = class(TForm)
    btnRun: TButton;
    spnInput: TSpinEdit;
    StaticText1: TStaticText;
    procedure btnRunClick(Sender: TObject);
  private
    procedure RegisterSystemC(input: TRsTypeImporter);
    procedure RegisterSystemE(RegisterFunction: TExecImportCall);
  public
  end;

var
  frmAckermann: TfrmAckermann;

implementation
uses
   Dialogs, SysUtils,
   rsDefsBackend;

{$R *.dfm}

procedure ShowMessage(const msg: string);
begin
   Application.MessageBox(PChar(msg), 'RTTI Script message');
end;

procedure TfrmAckermann.RegisterSystemC(input: TRsTypeImporter);
begin
   input.ImportFunction('procedure ShowMessage(const msg: string);');
   input.ImportFunction('function IntToStr(value: integer): string;');
end;

procedure TfrmAckermann.RegisterSystemE(RegisterFunction: TExecImportCall);
begin
   RegisterFunction('ShowMessage', @ShowMessage);
   RegisterFunction('IntToStr', @IntToStr);
end;

procedure TfrmAckermann.btnRunClick(Sender: TObject);
const
   CRLF = #13#10;
   SCRIPT =
      'unit u_Ackermann;' + CRLF +
      CRLF +
      'function Ackermann(n, x, y: Integer): Integer;' + CRLF +
      'begin' + CRLF +
      '  if n = 0 then' + CRLF +
      '    Result := x+1' + CRLF +
      '  else if y = 0 then' + CRLF +
      '  begin' + CRLF +
      '    if n = 1 then Result := x' + CRLF +
      '    else if n = 2 then Result := 0' + CRLF +
      '    else if n = 3 then Result := 1' + CRLF +
      '    else if n >= 4 then Result := 2' + CRLF +
      '  end' + CRLF +
      '  else' + CRLF +
      '    Result := Ackermann(n - 1, Ackermann(n, x, y - 1), x);' + CRLF +
      'end;' + CRLF +
      CRLF +
      'function F(Input: Integer): Integer;' + CRLF +
      'begin' + CRLF +
      '  ShowMessage(IntToStr(Ackermann(Input, Input, Input)));' + CRLF +
      'end;' + CRLF +
      CRLF +
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
      exec.RunProc('F', [spnInput.Value]);
   finally
      compiler.Free;
      prog.Free;
      exec.free;
   end;
end;

end.
