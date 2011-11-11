{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* tech.turbu-rpg.com.
*****************************************************************************}

unit rsExec;

interface
uses
   SysUtils, RTTI, PrivateHeap, Generics.Collections,
   newClass, rsDefsBackend;

type
   TrsExec = class;

   TExecImportCall = reference to procedure(const name: string; address: pointer);
   TrsExecImportProc = reference to procedure(RegisterFunction: TExecImportCall);

   TrsExec = class
   private type
      TVmContext = record
         locals: array of TValue;
         ar: PValue;  //array reg
         br: boolean; //bool reg
         ip: integer; //instruction pointer
      end;

      TsrPair = TPair<TValue, integer>;
      TSimpleParamList = class
      private
         FValues: TArray<TValue>;
         FCount: integer;
      public
         constructor Create(size: integer);
         procedure Add(const value: TValue);
         property ToArray: TArray<TValue> read FValues;
      end;

   private
      //debug routine for until I get an actual debugger set up
      procedure RegState; virtual;
   private
      FProgram: TrsProgram;
      FHeap: TPrivateHeap;
      FRunOnLoad: boolean;
      FProcMap: TMultimap<string, TRttiMethod>;
      FExtUnits: TDictionary<string, TrsExecImportProc>;
      FExtRoutines: TArray<TRttiMethod>;
      FImpls: TObjectList<TMethodImplementation>;
      FDepth: integer;
      FStack: TStack<TVmContext>;
      FSrStack: TStack<TsrPair>;
      FContext: TVmContext;
      FParamLists: TObjectStack<TSimpleParamList>;
      FNewClasses: TList<TClass>;
      FConstants: array of TValue;
      FLastImport: TPair<string, TDictionary<string, pointer>>;
      FText: TArray<TrsAsmInstruction>;

      function GetProc(const name: string; count: integer): TRttiMethod;
      procedure CreateMethodPointer(method: TRttiMethod; var address: pointer);
      procedure RegisterMethodPointer(method: TRttiMethod; var address: pointer);
      procedure MethodImplementation(UserData: Pointer;
        const Args: TArray<TValue>; out Result: TValue);
      function InvokeCode(index: integer; const Args: TArray<TValue>): TValue;
      function RunLoop(resultIndex: integer; expected: TrsOpcode): TValue;
      procedure InitializeRegs(const op: TrsAsmInstruction;
        const Args: TArray<TValue>);
      procedure RegisterProcs(const unitName: string; method: TRttiMethod; var address: pointer);
      procedure LoadProcs;
      procedure LoadConstants;
      function DoImport(const unitName: string): TDictionary<string, pointer>;

      function GetSR: TValue;
      procedure AddRegs(l, r: integer);
      procedure SubRegs(l, r: integer);
      procedure MulRegs(l, r: integer);
      procedure DivRegs(l, r: integer);
      procedure FDivRegs(l, r: integer);
      procedure ModRegs(l, r: integer);
      procedure AndRegs(l, r: integer);
      procedure OrRegs(l, r: integer);
      procedure XorRegs(l, r: integer);
      procedure ShlRegs(l, r: integer);
      procedure ShrRegs(l, r: integer);
      procedure MovRegs(l, r: integer);
      procedure MovConst(l, r: integer);
      procedure MovInt(l, r: integer);
      procedure NegRegister(l, r: integer);
      procedure NotRegister(l, r: integer);
      procedure IncRegister(l: integer);
      procedure DecRegister(l: integer);
      procedure CompGte(l, r: integer);
      procedure CompLte(l, r: integer);
      procedure CompGt(l, r: integer);
      procedure CompLt(l, r: integer);
      procedure CompEq(l, r: integer);
      procedure CompNeq(l, r: integer);
      function RegAsBoolean(index: integer): boolean;
      procedure XorbRegs(l, r: integer);
      procedure NewList(l: integer);
      procedure PushValue(l: integer);
      procedure Call(l, r: integer);
      procedure Callx(l, r: integer);
      procedure ArrayLoad(l: integer);
      procedure ArrayElemMove(l, r: integer);
      procedure CompEqi(l, r: integer);
      procedure CompGtei(l, r: integer);
      procedure CompGti(l, r: integer);
      procedure CompLtei(l, r: integer);
      procedure CompLti(l, r: integer);
      procedure CompNeqi(l, r: integer);
      procedure PushConst(l: integer);
      procedure PushInt(l: integer);
      procedure ArrayElemAssignC(l, r: integer);
      procedure ArrayElemAssign(l, r: integer);
      procedure LoadSelfRegister(l: integer);
      procedure StringConcat(l, r: integer);
      procedure TruncReg(l, r: integer);
      procedure AssignIntToFloat(l, r: integer);
   public
      constructor Create;
      destructor Destroy; override;
      procedure Load(prog: TrsProgram);
      procedure Run;
      function RunProc(const name: string; const params: array of TValue): TValue;
      function GetProcAddress(const name: string): pointer;

      procedure RegisterStandardUnit(const name: string; const proc: TrsExecImportProc);

      property RunOnLoad: boolean read FRunOnLoad write FRunOnLoad;
   end;

   ErsRuntimeError = class(Exception);

implementation
uses
   StrUtils, Types, TypInfo, Windows,
   rsEnex,
   vmtStructure;

var
   ctx: TRttiContext;

procedure CorruptError;
begin
   raise ErsRuntimeError.Create('Invalid instruction');
end;

procedure NotImplemented;
begin
   raise ErsRuntimeError.Create('Not Implemented Error');
end;

{ TrsExec }

constructor TrsExec.Create;
begin
   FHeap := TPrivateHeap.Create;
   FProcMap := TMultimap<string, TRttiMethod>.Create;
   FParamLists := TObjectStack<TSimpleParamList>.Create;
   FParamLists.OwnsObjects := true;
   FNewClasses := TList<TClass>.Create;
   FImpls := TObjectList<TMethodImplementation>.Create;
   FImpls.OwnsObjects := true;
   FExtUnits := TDictionary<string, TrsExecImportProc>.Create;
   FStack := TStack<TVmContext>.Create;
   FsrStack := TStack<TsrPair>.Create;
end;

destructor TrsExec.Destroy;
begin
   FsrStack.Free;
   FProcMap.Free;
   FStack.Free;
   FLastImport.Value.Free;
   FExtUnits.Free;
   FImpls.Free;
   FNewClasses.Free;
   FParamLists.Free;
   FHeap.Free;
   inherited Destroy;
end;

procedure TrsExec.AddRegs(l, r: integer);
begin
   case FContext.locals[l].Kind of
      tkInteger: FContext.locals[l] := FContext.locals[l].AsInteger + FContext.locals[r].AsInteger;
      tkFloat: FContext.locals[l] := FContext.locals[l].AsExtended + FContext.locals[r].AsExtended;
      else CorruptError;
   end;
end;

procedure TrsExec.SubRegs(l, r: integer);
begin
   case FContext.locals[l].Kind of
      tkInteger: FContext.locals[l] := FContext.locals[l].AsInteger - FContext.locals[r].AsInteger;
      tkFloat: FContext.locals[l] := FContext.locals[l].AsExtended - FContext.locals[r].AsExtended;
      else CorruptError;
   end;
end;

procedure TrsExec.MulRegs(l, r: integer);
begin
   if (FContext.locals[l].Kind = tkInteger) and (FContext.locals[r].Kind = tkInteger) then
      FContext.locals[l] := FContext.locals[l].AsInteger * FContext.locals[r].AsInteger
   else FContext.locals[l] := FContext.locals[l].AsExtended * FContext.locals[r].AsExtended;
end;

procedure TrsExec.DivRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger div FContext.locals[r].AsInteger
end;

procedure TrsExec.FdivRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsExtended / FContext.locals[r].AsExtended
end;

procedure TrsExec.ModRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger mod FContext.locals[r].AsInteger
end;

procedure TrsExec.AndRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger and FContext.locals[r].AsInteger
end;

procedure TrsExec.OrRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger or FContext.locals[r].AsInteger
end;

procedure TrsExec.XorRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger xor FContext.locals[r].AsInteger
end;

procedure TrsExec.ShlRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger shl FContext.locals[r].AsInteger
end;

procedure TrsExec.ShrRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger shr FContext.locals[r].AsInteger
end;

procedure TrsExec.StringConcat(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsString + FContext.locals[r].AsString;
end;

procedure TrsExec.MovRegs(l, r: integer);
begin
   FContext.locals[l] := FContext.locals[r];
end;

procedure TrsExec.MovConst(l, r: integer);
begin
   FContext.locals[l] := FConstants[r];
end;

procedure TrsExec.MovInt(l, r: integer);
begin
   FContext.locals[l] := r;
end;

procedure TrsExec.NegRegister(l, r: integer);
begin
   case FContext.locals[r].Kind of
      tkInteger: FContext.locals[l] := -FContext.locals[r].AsInteger;
      tkFloat: FContext.locals[l] := -FContext.locals[r].AsExtended;
      else CorruptError;
   end;
end;

procedure TrsExec.NotRegister(l, r: integer);
begin
   if l = 0 then
      FContext.br := not FContext.br
   else begin
      FContext.br := not FContext.locals[r].AsBoolean;
      FContext.locals[l] := FContext.br;
   end;
end;

procedure TrsExec.IncRegister(l: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger + 1;
end;

procedure TrsExec.DecRegister(l: integer);
begin
   FContext.locals[l] := FContext.locals[l].AsInteger - 1;
end;

procedure TrsExec.CompGte(l, r: integer);
begin
   case FContext.locals[l].Kind of
      tkInteger: FContext.br := FContext.locals[l].AsInteger >= FContext.locals[r].AsInteger;
      tkFloat: FContext.br := FContext.locals[l].AsExtended >= FContext.locals[r].AsExtended;
      tkString, tkChar: FContext.br := FContext.locals[l].AsString >= FContext.locals[r].AsString;
      else CorruptError;
   end;
end;

procedure TrsExec.CompLte(l, r: integer);
begin
   case FContext.locals[l].Kind of
      tkInteger: FContext.br := FContext.locals[l].AsInteger <= FContext.locals[r].AsInteger;
      tkFloat: FContext.br := FContext.locals[l].AsExtended <= FContext.locals[r].AsExtended;
      tkString, tkChar: FContext.br := FContext.locals[l].AsString <= FContext.locals[r].AsString;
      else CorruptError;
   end;
end;

procedure TrsExec.CompGt(l, r: integer);
begin
   CompLte(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsExec.CompLt(l, r: integer);
begin
   CompGte(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsExec.CompEq(l, r: integer);
begin
   case FContext.locals[l].Kind of
      tkInteger: FContext.br := FContext.locals[l].AsInteger = FContext.locals[r].AsInteger;
      tkFloat: FContext.br := FContext.locals[l].AsExtended = FContext.locals[r].AsExtended;
      tkString, tkChar: FContext.br := FContext.locals[l].AsString = FContext.locals[r].AsString;
      else CorruptError;
   end;
end;

procedure TrsExec.CompGtei(l, r: integer);
begin
   FContext.br := FContext.locals[l].AsInteger >= r;
end;

procedure TrsExec.CompLtei(l, r: integer);
begin
   FContext.br := FContext.locals[l].AsInteger <= r;
end;

procedure TrsExec.CompGti(l, r: integer);
begin
   FContext.br := FContext.locals[l].AsInteger > r;
end;

procedure TrsExec.CompLti(l, r: integer);
begin
   FContext.br := FContext.locals[l].AsInteger < r;
end;

procedure TrsExec.CompEqi(l, r: integer);
begin
   FContext.br := FContext.locals[l].AsInteger = r;
end;

procedure TrsExec.CompNeqi(l, r: integer);
begin
   FContext.br := FContext.locals[l].AsInteger <> r;
end;

procedure TrsExec.CompNeq(l, r: integer);
begin
   CompEq(l, r);
   FContext.br := not FContext.br;
end;

function TrsExec.RegAsBoolean(index: integer): boolean;
begin
   if index = 0 then
      result := FContext.br
   else result := FContext.locals[index].AsBoolean;
end;

procedure TrsExec.XorbRegs(l, r: integer);
begin
   FContext.br := RegAsBoolean(l) xor RegAsBoolean(r);
end;

procedure TrsExec.NewList(l: integer);
begin
   FParamLists.Push(TSimpleParamList.Create(l));
end;

procedure TrsExec.PushValue(l: integer);
begin
   FParamLists.peek.Add(FContext.locals[l]);
end;

procedure TrsExec.PushInt(l: integer);
begin
   FParamLists.peek.Add(l);
end;

procedure TrsExec.PushConst(l: integer);
begin
   FParamLists.Peek.Add(FConstants[l]);
end;

procedure TrsExec.Call(l, r: integer);
var
   args: TArray<TValue>;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   FStack.Push(FContext);
   FContext.locals[l] := InvokeCode(FContext.ip + r, args);
   FContext := FStack.Pop;
end;

procedure TrsExec.Callx(l, r: integer);
var
   args: TArray<TValue>;
   method: TRttiMethod;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   method := FExtRoutines[r];
   if method.IsStatic then
      FContext.locals[l] := RTTI.Invoke(method.CodeAddress, args, method.CallingConvention, method.ReturnType.Handle)
   else FContext.locals[l] := method.Invoke(GetSR, args);
end;

procedure TrsExec.ArrayLoad(l: integer);
begin
   if not (FContext.locals[l].IsArray) then
      CorruptError;
   FContext.ar := @FContext.locals[l];
end;

procedure TrsExec.AssignIntToFloat(l: integer; r: integer);
begin
   FContext.locals[l] := FContext.locals[r].AsExtended;
end;

procedure TrsExec.ArrayElemMove(l: integer; r: integer);
begin
   if not (assigned(FContext.ar) and (FContext.ar.IsArray)) then
      CorruptError;
   FContext.locals[l] := FContext.ar.GetArrayElement(r);
end;

procedure TrsExec.ArrayElemAssignC(l: integer; r: integer);
begin
   if not (assigned(FContext.ar) and (FContext.ar.IsArray)) then
      CorruptError;
   FContext.ar.SetArrayElement(l, FContext.locals[r]);
end;

procedure TrsExec.ArrayElemAssign(l: integer; r: integer);
begin
   if FContext.locals[l].kind <> tkInteger then
      CorruptError;
   ArrayElemAssignC(FContext.locals[l].AsInteger, r);
end;

procedure TrsExec.LoadSelfRegister(l: integer);
begin
   if not (FContext.locals[l].IsObject) then
      CorruptError;
   FsrStack.Push(TsrPair.Create(FContext.locals[l], FDepth));
end;

procedure TrsExec.TruncReg(l, r: integer);
begin
   if FContext.locals[r].Kind = tkInteger then
      FContext.locals[l] := FContext.locals[r]
   else FContext.locals[l] := trunc(FContext.locals[r].AsExtended);
end;

function TrsExec.RunLoop(resultIndex: integer; expected: TrsOpcode): TValue;
var
   op: ^TrsAsmInstruction;
begin
   repeat
      inc(FContext.ip);
      op := @FText[FContext.ip];
      case op.op of
         OP_NOP:  ;
         OP_ADD:  AddRegs(op.left, op.right);
         OP_SUB:  SubRegs(op.left, op.right);
         OP_MUL:  MulRegs(op.left, op.right);
         OP_DIV:  DivRegs(op.left, op.right);
         OP_FDIV: FdivRegs(op.left, op.right);
         OP_MOD:  ModRegs(op.left, op.right);
         OP_AND:  AndRegs(op.left, op.right);
         OP_OR:   OrRegs(op.left, op.right);
         OP_XOR:  XorRegs(op.left, op.right);
         OP_SHL:  ShlRegs(op.left, op.right);
         OP_SHR:  ShrRegs(op.left, op.right);
         OP_AS:   NotImplemented;
         OP_SCAT: StringConcat(op.left, op.right);
         OP_MOV:  MovRegs(op.left, op.right);
         OP_MOVC: MovConst(op.left, op.right);
         OP_MOVI: MovInt(op.left, op.right);
         OP_MOVF: NotImplemented;
         OP_MOVP: NotImplemented;
         OP_NEG:  NegRegister(op.left, op.right);
         OP_NOT:  NotRegister(op.left, op.right);
         OP_INC:  IncRegister(op.left);
         OP_DEC:  DecRegister(op.left);
         OP_GTE:  CompGte(op.left, op.right);
         OP_LTE:  CompLte(op.left, op.right);
         OP_GT:   CompGt(op.left, op.right);
         OP_LT:   CompLt(op.left, op.right);
         OP_EQ:   CompEq(op.left, op.right);
         OP_NEQ:  CompNeq(op.left, op.right);
         OP_GTEI: CompGtei(op.left, op.right);
         OP_LTEI: CompLtei(op.left, op.right);
         OP_GTI:  CompGti(op.left, op.right);
         OP_LTI:  CompLti(op.left, op.right);
         OP_EQI:  CompEqi(op.left, op.right);
         OP_NEQI: CompNeqi(op.left, op.right);
         OP_IN:   NotImplemented;
         OP_IS:   NotImplemented;
         OP_XORB: XorbRegs(op.left, op.right);
         OP_LIST: NewList(op.left);
         OP_PUSH: PushValue(op.left);
         OP_PSHI: PushInt(op.left);
         OP_PSHC: PushConst(op.left);
         OP_CALL: Call(op.left, op.right);
         OP_CALX: CallX(op.left, op.right);
         OP_INIT: CorruptError;
         OP_RET:  Break;
                   //-1 because IP will increment after the loop
         OP_JUMP: inc(FContext.ip, op.left - 1);
         OP_FJMP: if not FContext.br then
                     inc(FContext.ip, op.left - 1);
         OP_TJMP: if FContext.br then
                     inc(FContext.ip, op.left - 1);
         OP_ARYL: ArrayLoad(op.left);
         OP_ELEM: ArrayElemMove(op.left, op.right);
         OP_VASN: MovRegs(op.left, op.right);
         OP_AITF: AssignIntToFloat(op.left, op.right);
         OP_EASN: ArrayElemAssign(op.left, op.right);
         OP_EASC: ArrayElemAssignC(op.left, op.right);
         OP_FASN: NotImplemented;
         OP_PASN: NotImplemented;
         OP_TRYF: NotImplemented;
         OP_TRYE: NotImplemented;
         OP_TRYC: NotImplemented;
         OP_CTRY: Break;
         OP_EXIS: NotImplemented;
         OP_EXLD: NotImplemented;
         OP_RAIS: NotImplemented;
         OP_SRLD: LoadSelfRegister(op.left);
         OP_MCLS: NotImplemented;
         OP_TRNC: TruncReg(op.left, op.right)
         else CorruptError;
      end;
   until false;
   if op.op <> expected then
      CorruptError;
   result := FContext.Locals[resultIndex];
end;

procedure TrsExec.InitializeRegs(const op:  TrsAsmInstruction; const Args: TArray<TValue>);
var
   i: integer;
begin
   if op.op <> OP_INIT then
      raise ErsRuntimeError.Create('Invalid function entry point.');
   SetLength(FContext.locals, op.left + 1);
   FContext.ar := nil;
   FContext.br := false;
   for i := 0 to high(args) do
      FContext.locals[i + 1] := args[i];
end;

function TrsExec.InvokeCode(index: integer; const Args: TArray<TValue>): TValue;
begin
   inc(FDepth);
   try
      FContext.ip := index;
      InitializeRegs(FProgram.Text[FContext.ip], args);
      result := RunLoop(length(args) + 1, OP_RET); //TODO: don't do this if it's a procedure
   finally
      dec(FDepth);
   end;
end;

procedure TrsExec.MethodImplementation(UserData: Pointer; const Args: TArray<TValue>; out Result: TValue);
begin
   result := InvokeCode(NativeInt(UserData), args);
end;

procedure TrsExec.CreateMethodPointer(method: TRttiMethod; var address: pointer);
var
   impl: TMethodImplementation;
begin
   impl := method.CreateImplementation(address, self.MethodImplementation);
   address := impl.CodeAddress;
   FImpls.Add(impl);
   FProcMap.Add(method.Name, method)
end;

function TrsExec.DoImport(const unitName: string): TDictionary<string, pointer>;
var
   proc: TrsExecImportProc;
   procImporter: TExecImportCall;
   dict: TDictionary<string, pointer>;
begin
   if FLastImport.Key <> unitName then
   begin
      if not FExtUnits.TryGetValue(unitName, proc) then
         raise ErsRuntimeError.CreateFmt('Import proc for unit %s not found', [UnitName]);
      result := TDictionary<string, pointer>.Create;
      dict := result;
      procImporter :=
         procedure(const name: string; address: pointer)
         begin
            dict.Add(UpperCase(name), address);
         end;
      proc(procImporter);
      FLastImport.Value.Free;
      FLastImport.Key := unitName;
      FLastImport.Value := dict;
   end;
   result := FLastImport.Value;
end;

procedure TrsExec.RegisterMethodPointer(method: TRttiMethod; var address: pointer);
var
   classname: string;
   importMap: TDictionary<string, pointer>;
   index: nativeInt;
begin
   classname := method.Parent.Name;
   importMap := DoImport(copy(classname, pos('*', className) + 1, 255));

   index := NativeInt(address);
   if not importMap.TryGetValue(UpperCase(method.Name), address) then
      raise ErsRuntimeError.CreateFmt('No import provided for procedure %s', [method.Name]);

   if index > high(FExtRoutines) then
      SetLength(FExtRoutines, index + 1);
   FExtRoutines[index] := (method);
end;

procedure TrsExec.RegisterProcs(const unitName: string; method: TRttiMethod; var address: pointer);
begin
   if pos('EXT*', method.parent.name) = 0 then
      CreateMethodPointer(method, address)
   else RegisterMethodPointer(method, address);
end;

procedure TrsExec.LoadProcs;
begin
   FPRogram.InstallPackage(self.RegisterProcs);
end;

procedure TrsExec.LoadConstants;

   function DeserializeConstant(const value: string; info: PTypeInfo): TValue;
   begin
     case info.Kind of
       tkEnumeration: result := TValue.FromOrdinal(info, TypInfo.GetEnumValue(info, value));
       tkInteger: result := StrToInt(value);
       tkInt64: result := StrToInt64(value);
       tkUString, tkString, tkWString, tkChar, tkWChar: result := value;
       tkFloat: result := StrToFloat(value);
       tkSet: TValue.Make(StringToSet(info, value), info, result);
       else raise Exception.Create('Unknown constant type');
     end;
   end;

var
   i: integer;
begin
   SetLength(FConstants, FProgram.Constants.Count);
   for i := 0 to FProgram.Constants.Count - 1 do
      FConstants[i] := DeserializeConstant(FProgram.Constants[i], pointer(FProgram.Constants.Objects[i]))
end;

procedure TrsExec.Load(prog: TrsProgram);
begin
   assert(FProgram = nil); //fix this after changing to interfaces
   FProgram := prog;
   FText := FProgram.Text;
   LoadProcs;
   LoadConstants;
   if FRunOnLoad then
      Run;
end;

procedure TrsExec.Run;
begin
   //not implemented yet
end;

function TrsExec.GetProc(const name: string; count: integer): TRttiMethod;
var
   methods: TList<TRttiMethod>;
begin
   if not FProcMap.ContainsKey(name) then
      raise ErsRuntimeError.CreateFmt('Procedure %s not found', [name]);
   methods := FProcMap[name];
   if methods.Count = 1 then
      exit(methods[0]);

   methods := TEnex.Where<TRttiMethod>(methods,
     function(method: TRttiMethod): boolean
     begin
        result := length(method.GetParameters) = count
     end);
   try
      if methods.Count = 0 then
         raise ErsRuntimeError.CreateFmt('Cannot find procedure %s with %d arguments', [name, count])
      else if methods.Count > 1 then
         raise ErsRuntimeError.CreateFmt('Found more than one procedure %s with %d arguments.  Fully qualified name is required.', [name, count])
      else result := methods[0];
   finally
      methods.free;
   end;
end;

function TrsExec.GetProcAddress(const name: string): pointer;
var
   methods: TList<TRttiMethod>;
begin
   if not FProcMap.ContainsKey(name) then
      raise ErsRuntimeError.CreateFmt('Procedure %s not found', [name]);
   methods := FProcMap[name];
   if methods.Count = 1 then
      result := methods[0].CodeAddress
   else raise ErsRuntimeError.CreateFmt('Found more than one procedure %s.  Fully qualified name is required.', [name]);
end;

function TrsExec.GetSR: TValue;
begin
   if (FsrStack.Count = 0) or (FsrStack.Peek.Value <> FDepth) then
      CorruptError;
   result := FsrStack.Pop.Key;
end;

function TrsExec.RunProc(const name: string; const params: array of TValue): TValue;
var
   method: TRttiMethod;
begin
   method := GetProc(name, length(params));
   result := method.Invoke(nil, params);
end;

procedure TrsExec.RegisterStandardUnit(const name: string; const proc: TrsExecImportProc);
begin
   FExtUnits.Add(UpperCase(name), proc);
end;

procedure TrsExec.RegState;
var
   i: integer;
begin
   for i := 0 to high(FContext.locals) do
      Writeln(format('%d: %s', [i, FContext.locals[i].ToString]));
end;

{ TrsExec.TSimpleParamList }

constructor TrsExec.TSimpleParamList.Create(size: integer);
begin
   SetLength(FValues, size);
end;

procedure TrsExec.TSimpleParamList.Add(const value: TValue);
begin
   FValues[FCount] := value;
   inc(FCount);
end;

end.
