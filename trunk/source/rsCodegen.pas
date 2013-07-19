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

unit rsCodegen;

interface
uses
   Classes, RTTI, Generics.Collections,
   rsDefs, rsDefsBackend;

type
   IrsOptimizer = interface
      procedure Process(proc: TProcSymbol);
   end;

   TrsMethodFlattener = class(TInterfacedObject, IrsOptimizer)
   private
      procedure Process(proc: TProcSymbol);
   end;

   TrsCodegen = class
   private
      FJumpTable: TDictionary<string, integer>;
      FUnresolvedJumpTable: TList<TPair<string, integer>>;
      FConstTable: TStringList;
      FLocals: TStringList;
      FCurrent: TrsScriptUnit;
      FCurrentUnit: TUnitSymbol;
      FFreeReg: integer;
      FTempReg: integer;
      FTempList: TList<integer>;
      FTempQueue: TQueue<integer>;
      FTryStack: TStack<TPair<TSyntaxKind, integer>>;
      FUnresStack: TStack<TPair<string, TReferenceType>>;
      FDebug: boolean;
      FCurrentLine: integer;

      function NextReg(temp: integer = 0): integer;
      procedure Write(const opcode: TrsAsmInstruction);
      procedure WriteOp(opcode: TrsOpcode; left: integer = 0; right: integer = 0);

      function UnresolvedJump(const name: string): integer;
      function ResolveJump(const name: string): integer;

      function Eval(value: TTypedSyntax): integer;
      function WriteBinCalc(value: TBinOpSyntax): TrsAsmInstruction;
      function WriteIntBinCalc(value: TBinOpSyntax): TrsAsmInstruction;
      procedure ProcessProc(proc: TProcSymbol);
      function WriteConst(value: TValueSyntax): TrsAsmInstruction;
      function WriteUnCalc(value: TUnOpSyntax): TrsAsmInstruction;
      function WriteBoolCalc(value: TBoolOpSyntax): TrsAsmInstruction;
      function WriteVariable(value: TVariableSyntax): TrsAsmInstruction;
      function WriteCall(value: TCallSyntax): TrsAsmInstruction;
      function WriteElem(value: TElemSyntax): TrsAsmInstruction;
      procedure WriteJump(value: TJumpSyntax);
      procedure WriteAssign(value: TAssignmentSyntax);
      procedure AssignLValue(lValue: TTypedSyntax; rValue: integer);
      procedure AssignElem(lValue: TElemSyntax; rValue: integer);
      procedure AssignDot(lValue: TDotSyntax; rValue: integer);
      procedure AssignProp(selfVal, rValue: integer; prop: TVariableSyntax);
      procedure AssignArrayProp(lValue: TArrayPropSyntax; rValue: integer);
      procedure AssignElemProp(selfVal, rValue: integer; prop: TElemSyntax);
{      procedure AssignField(lValue: TFieldSyntax; rValue: integer);
      procedure AssignProp(lValue: TPropSyntax; rValue: integer); }
      procedure WriteTryBlock(opcode: TrsOpcode; opType: TSyntaxKind);
      procedure CloseTryBlock(kind: TSyntaxKind; const ret: string);
      function VariableIndex(sym: TVarSymbol): integer;
      procedure WriteTryCallBlock(syntax: TTryCallSyntax);
      procedure WriteRaise(syntax: TRaiseSyntax);
      function WriteDotChain(value: TDotSyntax): TrsAsmInstruction;
      function WriteClassRef(value: TTypeRefSyntax): TrsAsmInstruction;
      function WriteCast(value: TCastSyntax): TrsAsmInstruction;
      function ClassIndex(sym: TClassTypeSymbol): integer;

      function CreateProcInfo(proc: TProcSymbol; start: integer; ext, standalone: boolean): TrsProcInfo;
      procedure CreateUnitClass(&unit: TUnitSymbol);
      procedure ResolveJumps;
      procedure ChangeLeftValue(index, value: integer);
      procedure ChangeRightValue(index, value: integer);
      function ResolveCall(const name: string; position: integer): integer;
      procedure ResolveUnitLocalCalls(&unit: TUnitSymbol);
      procedure SetupUnitGlobals(&unit: TUnitSymbol; rsu: TrsScriptUnit);
      procedure SetupUnitProperties(&unit: TUnitSymbol; rsu: TrsScriptUnit);
      procedure SetupLocals(proc: TProcSymbol);
      procedure SetupUnitExternalClasses(&unit: TUnitSymbol; rsu: TrsScriptUnit);
      procedure PopUnres;
      function ShortCircuitEval(value: TBoolOpSyntax): TrsAsmInstruction;
      procedure AssignConst(lvalue: TVariableSyntax; rvalue: TValueSyntax);
      procedure AssignSelfBinOp(lvalue: TVariableSyntax; rvalue: TBinOpSyntax);
      procedure PushParam(param: TTypedSyntax);
      function WriteArrayPropRead(value: TArrayPropSyntax): TrsAsmInstruction;
      function WritePropertyValue(symbol: TPropSymbol;
        sem: integer): TrsAsmInstruction;
      function SerializeArray(const value: TValue): string;
      function SerializeConstant(value: TValueSyntax): string;
      procedure AssignmentPeephole(start: integer);
      procedure LoadSR(l: integer);
      procedure ChangeLeft(index, value: integer);
      procedure ChangeLastOp(value: TrsOpcode);
   public
      constructor Create;
      destructor Destroy; override;
      function Process(&unit: TUnitSymbol): TrsScriptUnit;
      property constants: TStringList read FConstTable;
   end;

implementation
uses
   Windows, Math,
   SysUtils, StrUtils, TypInfo,
   rsEnex, rsImport,
   vmtBuilder, newClass;

{ TrsMethodFlattener }

procedure TrsMethodFlattener.Process(proc: TProcSymbol);
var
   block: TSyntaxList;
   i, j: integer;
   sub: TBlockSyntax;
begin
   block := proc.syntax.children;
   i := 0;
   while i < block.Count do
   begin
      if block[i].kind = skBlock then
      begin
         sub := TBlockSyntax(block[i]);
         for j := sub.children.count - 1 downto 0 do
            block.Insert(i + 1, sub.children[j]);
         TSyntaxList(sub.children).OwnsObjects := false;
         assert(block[i] = sub);
         block.Delete(i);
      end
      else inc(i);
   end;
end;

{ TrsCodegen }

constructor TrsCodegen.Create;
begin
   FJumpTable := TDictionary<string, integer>.Create;
   FTryStack := TStack<TPair<TSyntaxKind, integer>>.Create;
   FConstTable := TStringList.Create;
   FConstTable.AddObject('False', TypeInfo(boolean));
   FConstTable.AddObject('True', TypeInfo(boolean));
   FLocals := TStringList.Create;
   FUnresolvedJumpTable := TList<TPair<string, integer>>.Create;
   FUnresStack := TStack<TPair<string, TReferenceType>>.Create;
   FTempList := TList<integer>.Create;
   FTempQueue := TQueue<integer>.Create;
end;

destructor TrsCodegen.Destroy;
begin
   FTempQueue.Free;
   FTempList.Free;
   FJumpTable.Free;
   FUnresStack.Free;
   FUnresolvedJumpTable.Free;
   FTryStack.Free;
   FLocals.Free;
   inherited;
end;

procedure TrsCodegen.Write(const opcode: TrsAsmInstruction);
begin
   assert(opcode.op in [low(TrsOpcode)..High(TrsOpcode)]);
   FCurrent.Text.Add(opcode);
   FCurrent.OpcodeMap.Add(FCurrentLine);
end;

procedure TrsCodegen.WriteOp(opcode: TrsOpcode; left, right: integer);
var
   op: TrsAsmInstruction;
begin
   op.op := opcode;
   op.left := left;
   op.right := right;
   write(op);
end;

function TrsCodegen.SerializeArray(const value: TValue): string;
var
   sl: TStringList;
   i: integer;
begin
   sl := TStringList.Create;
   try
      for i := 0 to value.GetArrayLength - 1 do
         sl.add(value.GetArrayElement(i).ToString);
      result := '[' + sl.CommaText + ']';
   finally
      sl.free;
   end;
end;

function TrsCodegen.WriteConst(value: TValueSyntax): TrsAsmInstruction;
begin
   result.left := NextReg(value.sem);
   if value.value.Kind = tkInteger then
   begin
      result.op := OP_MOVI;
      result.right := value.value.AsInteger;
   end
   else begin
      result.op := OP_MOVC;
      result.right := FConstTable.AddObject(SerializeConstant(value), pointer(value.value.TypeInfo))
   end;
end;

function TrsCodegen.WriteIntBinCalc(value: TBinOpSyntax): TrsAsmInstruction;
const OPCODES: array[TBinOpKind] of TrsOpcode =
  (OP_INC, OP_DEC, OP_MULI, OP_DIVI, OP_NOP, OP_MODI, OP_ANDI, OP_ORI, OP_XORI, OP_SHLI, OP_SHRI, OP_NOP);
var
   left, new: integer;
begin
   left := Eval(value.left);
   if left = -1 then
      popUnres;
   if left > FLocals.Count then
      result.left := left
   else begin
      new := NextReg(value.sem);
      WriteOp(OP_MOV, new, left);
      result.left := new;
   end;
   result.right := (value.right as TValueSyntax).value.AsInteger;
   result.op := OPCODES[value.op];
   assert(result.op <> OP_NOP);
end;

function TrsCodegen.WriteBinCalc(value: TBinOpSyntax): TrsAsmInstruction;
const OPCODES: array[TBinOpKind] of TrsOpcode =
  (OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_FDIV, OP_MOD, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR, OP_AS);
var
   left, new: integer;
begin
   if not (value.op in [opDivide, opAs]) and (value.right.kind = skValue)
      and (TValueSyntax(value.right).value.Kind = tkInteger)
      and (value.left.&type.TypeInfo.Kind = tkInteger) then
      Exit(WriteIntBinCalc(value));

   result.right := Eval(value.right);

   left := Eval(value.left);
   if left = -1 then
      popUnres;
   if left > FLocals.Count then
      result.left := left
   else begin
      new := NextReg(value.sem);
      WriteOp(OP_MOV, new, left);
      result.left := new;
   end;
   if result.right = -1 then
      popUnres;
   result.op := OPCODES[value.op];
   if (result.op = OP_ADD) and (value.left.&type.TypeInfo.Kind in [tkUString, tkWString, tkWChar]) then
      result.op := OP_SCAT;
end;

function TrsCodegen.WriteUnCalc(value: TUnOpSyntax): TrsAsmInstruction;
const OPCODES: array[TUnOpKind] of TrsOpcode = (OP_NEG, OP_NOT, OP_INC, OP_DEC, OP_EXIS);
begin
   if value.op in [opNeg, opNot] then
   begin
      result.right := Eval(value.sub);
      if (value.op = opNot) and (result.right = 0) then
         result.left := 0
      else result.left := NextReg(value.sem);
   end
   else begin
      result.left := Eval(value.sub);
      result.right := 1;
   end;
   result.op := OPCODES[value.op];
end;

function TrsCodegen.ShortCircuitEval(value: TBoolOpSyntax): TrsAsmInstruction;
var
   location: integer;
begin
   assert(value.op in [opBoolAnd, opBoolOr]);
   result.left := 0;
   result.op := OP_NOP;
   location := FCurrent.Text.Count;
   if value.op = opBoolAnd then
      WriteOp(OP_FJMP, -1)
   else WriteOp(OP_TJMP, -1);
   Eval(value.right);
   ChangeLeftValue(location, FCurrent.Text.Count - location);
end;

function TrsCodegen.WriteBoolCalc(value: TBoolOpSyntax): TrsAsmInstruction;
const
   OPCODES: array[TBoolOpKind] of TrsOpcode =
     (OP_GTE, OP_LTE, OP_GT, OP_LT, OP_EQ, OP_NEQ, OP_IN, OP_IS, OP_NOP, OP_NOP, OP_XORB);
   OPCODES_I: array[opGreaterEqual..opNotEqual] of TrsOpcode =
     (OP_GTEI, OP_LTEI, OP_GTI, OP_LTI, OP_EQI, OP_NEQI);
begin
   result.left := Eval(value.left);

   //special-case opimization
   if (value.op in [opGreaterEqual..opNotEqual]) and (value.right.kind = skValue)
     and (TValueSyntax(value.right).value.Kind = tkInteger) then
   begin
      result.right := TValueSyntax(value.right).value.AsInteger;
      result.op := OPCODES_I[value.op];
   end
   else if value.op in [opBoolAnd, opBoolOr] then
      result := ShortCircuitEval(value)
   else begin
      result.right := Eval(value.right);
      result.op := OPCODES[value.op];
      if (result.left = 0) and (value.op in [opEquals, opNotEqual]) then
         inc(result.op, ord(OP_EQB) - ord(OP_EQ));
   end;
end;

function MakeArrayPropGetter(const value: string): string;
begin
   result := StringReplace(value, '.', '.Get*', []);
end;

function TrsCodegen.WritePropertyValue(symbol: TPropSymbol; sem: integer): TrsAsmInstruction;
var
   typ: TTypeSymbol;
   isArray: boolean;
   refType: TReferenceType;
begin
   typ := symbol.&Type;
   isArray := AnsiStartsStr('ARRAYOF*', typ.name);
   if isArray then
      result.op := OP_MVAP
   else result.op := OP_MOVP;
   result.left := NextReg(sem);
   result.right := -1;

   if isArray then
      FCurrent.Unresolved.Add(TUnresolvedReference.Create(MakeArrayPropGetter(symbol.fullName), FCurrent.Text.Count, rtArrayProp))
   else FCurrent.Unresolved.Add(TUnresolvedReference.Create(symbol.fullName, FCurrent.Text.Count, rtProp));
end;

function TrsCodegen.WriteVariable(value: TVariableSyntax): TrsAsmInstruction;
begin
   if value.symbol is TFieldSymbol then
   begin
      result.op := OP_MOVF;
      result.left := NextReg(value.sem);
      result.right := TFieldSymbol(value.symbol).index;
   end
   else if value.symbol is TPropSymbol then
      result := WritePropertyValue(TPropSymbol(value.symbol), value.sem)
   else if value.&type = BooleanType then
   begin
      result.op := OP_MOV;
      result.left := 0;
      result.right := VariableIndex(value.symbol);
   end
   else begin
      result.op := OP_NOP;
      result.left := VariableIndex(value.symbol);
   end
end;

function TrsCodegen.UnresolvedJump(const name: string): integer;
begin
   result := -1;
   FUnresolvedJumpTable.Add(TPair<string, integer>.Create(name, FCurrent.Text.Count));
end;

function TrsCodegen.ResolveJump(const name: string): integer;
begin
   if FJumpTable.TryGetValue(name, result) then
      result := result - FCurrent.Text.Count
   else result := UnresolvedJump(name);
end;

function TrsCodegen.ResolveCall(const name: string; position: integer): integer;
var
   table: TUnresList;
begin
   if FCurrent.Routines.ContainsKey(name) then
      result := FCurrent.Routines[name].index - position
   else begin
      result := -1;
      if AnsiStartsText(FCurrent.namedot, name) then
         table := FCurrent.UnresolvedCalls
      else table := FCurrent.Unresolved;
      table.Add(TUnresolvedReference.Create(name, position, rtCall));
   end;
end;

function TrsCodegen.SerializeConstant(value: TValueSyntax): string;
begin
   if value.value.Kind in [tkArray, tkDynArray] then
      result := SerializeArray(value.value)
   else result := value.value.ToString;
end;

procedure TrsCodegen.PushParam(param: TTypedSyntax);
var
   left: integer;
   last: TrsAsmInstruction;
begin
   if param.kind = skValue then
   begin
      if TValueSyntax(param).value.Kind = tkInteger then
         WriteOp(OP_PSHI, TValueSyntax(param).value.AsInteger)
      else if TValueSyntax(param).value.TypeInfo = TypeInfo(boolean) then
         WriteOp(OP_PSHC, ord(TValueSyntax(param).value.AsBoolean))
      else WriteOp(OP_PSHC, FConstTable.AddObject(serializeConstant(TValueSyntax(param)), pointer(TValueSyntax(param).value.TypeInfo)));
   end
   else begin
      left := Eval(param);
      if left = -1 then
         PopUnres;
      last := FCurrent.Text.Last;
      if last.op = OP_CALL then
      begin
         FCurrent.Text.Delete(FCurrent.Text.Count - 1);
         WriteOp(OP_PCAL, last.left, last.right);
      end
      else WriteOp(OP_PUSH, left);
   end;
end;

function TrsCodegen.WriteCall(value: TCallSyntax): TrsAsmInstruction;
var
   param: TTypedSyntax;
   left: Integer;
begin
   WriteOp(OP_LIST, value.params.Count);
   for param in value.params do
      PushParam(param);
   if assigned(value.SelfSymbol) then
   begin
      left := Eval(value.SelfSymbol);
      if left = -1 then
         PopUnres;
      LoadSR(left);
   end;
   result.op := OP_CALL;
   result.right := ResolveCall(value.proc.fullName, FCurrent.Text.Count);
   if assigned(value.proc.&Type) then
   begin
      if value.proc.&type = BooleanType then
         result.left := 0
      else result.left := NextReg(value.sem);
   end
   else result.left := -1;
end;

procedure TrsCodegen.PopUnres;
var
   pair: TPair<string, TReferenceType>;
   ref: TUnresolvedReference;
begin
   pair := FUnresStack.Pop;
   ref := TUnresolvedReference.Create(pair.Key, FCurrent.Text.Count, pair.Value);
   assert(pair.Value <> rtCall);
   FCurrent.Unresolved.Add(ref);
end;

function TrsCodegen.WriteElem(value: TElemSyntax): TrsAsmInstruction;
var
   left: integer;
begin
   result.left := NextReg(value.sem);
   result.right := Eval(value.Right);
   result.op := OP_ELEM;
   if result.right = -1 then
      popUnres;

   left := Eval(value.Left);
   if left = -1 then
      popUnres;
   WriteOp(OP_ARYL, left);
end;

function TrsCodegen.WriteDotChain(value: TDotSyntax): TrsAsmInstruction;
var
   left: integer;
begin
   left := Eval(value.Left);
   if left = -1 then
      popUnres;
   LoadSR(left);

   result.op := OP_NOP;
   result.left := Eval(value.Right);
end;

function TrsCodegen.ClassIndex(sym: TClassTypeSymbol): integer;
begin
   assert(false); //TODO: implement this
end;

function TrsCodegen.WriteClassRef(value: TTypeRefSyntax): TrsAsmInstruction;
begin
   WriteOp(OP_MCLS, NextReg, ClassIndex(value.&class));
end;

function TrsCodegen.WriteCast(value: TCastSyntax): TrsAsmInstruction;
begin
   if (value.Base.&type.TypeInfo.Kind = tkInteger)  and (value.&type.TypeInfo.Kind = tkFloat) then
   begin
      result.op := OP_AITF;
      result.left := NextReg(value.sem);
      result.right := Eval(value.Base);
   end
   else raise EParseError.Create('Corrupt parse tree');
end;

function TrsCodegen.WriteArrayPropRead(value: TArrayPropSyntax): TrsAsmInstruction;
var
   base: TDotSyntax;
   left: integer;
   param: TTypedSyntax;
begin
   WriteOp(OP_LIST, value.params.Count);
   for param in value.params do
      PushParam(param);

   base := value.base;
   left := Eval(base.Left);
   if left = -1 then
      popUnres;
   LoadSR(left);

   result.op := OP_MVAP;
   result.left := NextReg(value.sem);
   result.right := -1;
   FCurrent.Unresolved.Add(TUnresolvedReference.Create(
     ((Base.Right as TVariableSyntax).Symbol as TPropSymbol).readSpec.fullName,
     FCurrent.Text.Count, rtArrayProp));
end;

function TrsCodegen.Eval(value: TTypedSyntax): integer;
var
   op: TrsAsmInstruction;
begin
   case value.kind of
      skValue: op := WriteConst(TValueSyntax(value));
      skBinOp: op := WriteBinCalc(TBinOpSyntax(value));
      skUnOp:  op := WriteUnCalc(TUnOpSyntax(value));
      skBoolOp: op := WriteBoolCalc(TBoolOpSyntax(value));
      skVariable: op := WriteVariable(TVariableSyntax(value));
      skCall: op := WriteCall(TCallSyntax(value));
      skElem: op := WriteElem(TElemSyntax(value));
      skDot: op := WriteDotChain(TDotSyntax(value));
      skType: op := WriteClassRef(TTypeRefSyntax(value));
      skCast: op := WriteCast(TCastSyntax(value));
      skArrayProp: op := WriteArrayPropRead(TArrayPropSyntax(value));
      else raise EParseError.Create('Corrupt parse tree');
   end;
   if (op.op <> OP_NOP) or FDebug then
      write(op);
   if value.kind <> skBoolOp then
      result := op.left
   else result := 0;
end;

procedure TrsCodegen.LoadSR(l: integer);
var
   prev: TrsAsmInstruction;
begin
   prev := FCurrent.Text.Last;
   if (prev.op = OP_MOVP) and (prev.left = l) then
      ChangeLastOp(OP_MOVPSR)
   else if (prev.op = OP_MVAP) and (prev.left = l) then
      ChangeLastOp(OP_MVAPSR)
   else writeOp(OP_SRLD, l);
end;

function TrsCodegen.NextReg(temp: integer): integer;
var
   reg: integer;
begin
   if temp > 0 then
   begin
      if temp <> FTempReg then
      begin
         FTempReg := temp;
         for reg in FTempList do
            FTempQueue.Enqueue(reg);
         FTempList.Clear;
      end;
      if FTempQueue.Count > 0 then
      begin
         result := FTempQueue.Dequeue;
         FTempList.Add(result);
         FFreeReg := max(FFreeReg, result);
         Exit;
      end;
   end;
   inc(FFreeReg);
   result := FFreeReg;
   if temp > 0 then
      FTempList.Add(result);
end;

procedure TrsCodegen.WriteJump(value: TJumpSyntax);
var
   left: integer;
begin
   left := ResolveJump(value.name);
   if value.conditional then
      WriteOp(OP_FJMP, left)
   else WriteOp(OP_JUMP, left);
end;

procedure TrsCodegen.AssignElem(lValue: TElemSyntax; rValue: integer);
var
   op: TrsAsmInstruction;
begin
   WriteOp(OP_ARYL, Eval(lValue.Left));
   op.op := OP_EASN;
   //minor optimization for a common case
   if (lValue.Right.kind = skValue) and (TValueSyntax(lValue.Right).value.Kind = tkInteger) then
   begin
      op.op := OP_EASC;
      op.left := TValueSyntax(lValue.Right).value.AsInteger;
   end
   else op.left := Eval(lValue.Right);
   op.right := rValue;
   Write(op);
end;

procedure TrsCodegen.AssignArrayProp(lValue: TArrayPropSyntax; rValue: integer);
var
   sr: integer;
   param: TTypedSyntax;
begin
   WriteOp(OP_LIST, lValue.params.Count);
   for param in lValue.params do
      PushParam(param);
   sr := Eval(lValue.base.left);
   if sr = -1 then
      PopUnres;
   LoadSR(sr);

   FCurrent.Unresolved.Add(TUnresolvedReference.Create(((lValue.base.Right as TVariableSyntax).Symbol as TPropSymbol).WriteSpec.fullName, FCurrent.Text.Count, rtArrayProp));
   WriteOp(OP_APSN, -1, rValue);
end;

{
procedure TrsCodegen.AssignField(lValue: TFieldSyntax; rValue: integer);
begin
   WriteOp(OP_LIST);
   WriteOp(OP_PUSH, Eval(lValue.Left));
   WriteOp(OP_FASN, Eval(lValue.Right), rValue);
end;
}

procedure TrsCodegen.AssignProp(selfVal, rValue: integer; prop: TVariableSyntax);
var
   propSym: TPropSymbol;
begin
   LoadSR(selfVal);
   propSym := prop.symbol as TPropSymbol;
   FCurrent.Unresolved.Add(TUnresolvedReference.Create(propSym.fullName, FCurrent.Text.Count, rtProp));
   WriteOp(OP_PASN, -1, rValue);
end;

procedure TrsCodegen.AssignElemProp(selfVal, rValue: integer; prop: TElemSyntax);
begin
   LoadSR(selfVal);
   AssignElem(prop, rValue);
end;

procedure TrsCodegen.AssignDot(lValue: TDotSyntax; rValue: integer);
var
   selfVal: integer;
begin
   selfVal := eval(lValue.left);
   if selfVal = -1 then
      PopUnres;
   if lValue.Right.kind = skElem then
      AssignElemProp(selfVal, rValue, lValue.Right as TElemSyntax)
   else case (lValue.right as TVariableSyntax).symbol.kind of
      syProp: AssignProp(selfVal, rValue, TVariableSyntax(lValue.right));
//      syVariable: AssignField(selfVal, dotVal, rValue);
      else assert(false);
   end;
end;

procedure TrsCodegen.AssignLValue(lValue: TTypedSyntax; rValue: integer);
var
   left: integer;
begin
   case lValue.kind of
      skVariable:
      begin
         left := VariableIndex(TVariableSyntax(lValue).symbol);
         if left = -1 then
            PopUnres;
         WriteOp(OP_VASN, left, rValue);
      end;
      skElem: AssignElem(TElemSyntax(lValue), rValue);
      skDot: AssignDot(TDotSyntax(lValue), rValue);
      skArrayProp: AssignArrayProp(TArrayPropSyntax(lValue), rValue);
{      skField: AssignField(TFieldSyntax(lValue), rValue);
      skProp: AssignProp(TPropSyntax(lValue), rValue); }
      else raise EParseError.Create('Corrupt parse tree');
   end;
end;

procedure TrsCodegen.AssignConst(lvalue: TVariableSyntax; rvalue: TValueSyntax);
var
   left: integer;
begin
   left := VariableIndex(lValue.symbol);
   if rvalue.value.Kind = tkInteger then
      WriteOp(OP_MOVI, left, rValue.value.AsInteger)
   else if rValue.value.TypeInfo = TypeInfo(boolean) then
      WriteOp(OP_MOVC, left, ord(rValue.value.AsBoolean))
   else WriteOp(OP_MOVC, left, FConstTable.AddObject(SerializeConstant(rvalue), pointer(rvalue.value.TypeInfo)));
end;

procedure TrsCodegen.AssignSelfBinOp(lvalue: TVariableSyntax; rvalue: TBinOpSyntax);
const OPCODES: array[TBinOpKind] of TrsOpcode =
  (OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_FDIV, OP_MOD, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR, OP_AS);
var
   left, right: integer;
begin
   assert(lValue.symbol = TVariableSyntax(rvalue.left).symbol);
   right := Eval(rValue.right);
   left := Eval(lValue);
   WriteOp(OPCODES[rValue.op], left, right);
end;

procedure TrsCodegen.ChangeLastOp(value: TrsOpcode);
var
   index: integer;
   op: TrsAsmInstruction;
begin
   index := FCurrent.text.count - 1;
   op := FCurrent.Text[index];
   op.op := value;
   FCurrent.Text[index] := op;
end;

procedure TrsCodegen.ChangeLeft(index, value: integer);
var
   op: TrsAsmInstruction;
begin
   op := FCurrent.Text[index];
   op.left := value;
   FCurrent.Text[index] := op;
end;

procedure TrsCodegen.AssignmentPeephole(start: integer);
var
   i, j: integer;
   mov, ml, mr: integer;
   valid: boolean;
   op: TrsAsmInstruction;
begin
   mov := -1; ml := -1; mr := -1;
   valid := false;
   i := start;
   while i < FCurrent.Text.Count do
   begin
      op := FCurrent.Text[i];
      if op.op = OP_MOV then
      begin
         mov := i;
         ml := op.left;
         mr := op.right;
         valid := true;
      end
      else if valid and (op.op = OP_VASN) and (op.left = mr) and (op.right = ml) then
      begin
         FCurrent.Text.Delete(mov);
         dec(i);
         for j := mov to i do
            if FCurrent.Text[j].left = ml then
               ChangeLeft(j, mr);
         FCurrent.text.Delete(i);
         dec(i);
         valid := false;
         if ml = FFreeReg then
            dec(FFreeReg);
      end
      else if valid and not ((op.op in [OP_ADD..OP_SHR, OP_MULI..OP_SHRI]) and (op.left = ml)) then
         valid := false;
      inc(i);
   end;
end;

procedure TrsCodegen.WriteAssign(value: TAssignmentSyntax);
var
   current: integer;
begin
   // x := 5;
   if (value.lvalue.kind = skVariable) and (value.rvalue.kind = skValue) then
      AssignConst(TVariableSyntax(value.lValue), TValueSyntax(value.rValue))
   //x := x * 5;
   else if (value.lvalue.kind = skVariable) and (value.rvalue.kind = skBinOp) and
      (TBinOpSyntax(value.rValue).left.kind = skVariable) and
      (TVariableSyntax(TBinOpSyntax(value.rValue).left).symbol = TVariableSyntax(value.lValue).symbol) then
      AssignSelfBinOp(TVariableSyntax(value.lValue), TBinOpSyntax(value.rValue))
   //general case
   else begin
      current := FCurrent.Text.Count;
      AssignLValue(value.lValue, Eval(value.rValue));
      AssignmentPeephole(current);
   end;
end;

procedure TrsCodegen.WriteTryBlock(opcode: TrsOpcode; opType: TSyntaxKind);
begin
   WriteOp(opcode);
   FTryStack.Push(TPair<TSyntaxKind, integer>.Create(opType, FCurrent.Text.Count));
end;

procedure TrsCodegen.CloseTryBlock(kind: TSyntaxKind; const ret: string);
var
   top: TPair<TSyntaxKind, integer>;
   tryOp: TrsAsmInstruction;
begin
   top := FTryStack.Pop;
   if top.Key <> kind then
      raise EParseError.Create('Corrupt parse tree');
   tryOp := FCurrent.Text[top.Value];
   tryOp.left := FCurrent.Text.Count - top.Value;
   FCurrent.Text[top.Value] := tryOp;
   WriteOp(OP_CTRY, ResolveJump(ret));
end;

function TrsCodegen.VariableIndex(sym: TVarSymbol): integer;
begin
   if assigned(FLocals) then
      result := FLocals.IndexOf(UpperCase(sym.name))
   else result := -1;
   if result = -1 then
      FUnresStack.push(TPair<string, TReferenceType>.Create(sym.fullName, rtVar));
end;

procedure TrsCodegen.WriteTryCallBlock(syntax: TTryCallSyntax);
begin
   WriteOp(OP_TRYC, ResolveJump(syntax.jump));
   WriteOp(OP_JUMP, ResolveJump(syntax.ret));
end;

procedure TrsCodegen.WriteRaise(syntax: TRaiseSyntax);
begin
   if syntax.obj = nil then
      WriteOp(OP_RAIS)
   else WriteOp(OP_RAIS, Eval(syntax.obj));
end;

function TrsCodegen.CreateProcInfo(proc: TProcSymbol; start: integer; ext, standalone: boolean): TrsProcInfo;
var
   param: TParamSymbol;
   params: TList<IBuffer>;
   info: PTypeInfo;
begin
   result.index := start;
   params := TList<IBuffer>.Create;
   try
      if assigned(proc.paramList) then
         for param in proc.paramList do
            params.Add(vmtBuilder.CreateMethodParam(param.flags, param.&Type.TypeInfo, 0, param.name));
      if assigned(proc.&type) then
         info := proc.&Type.TypeInfo
      else info := nil;
      result.info := vmtBuilder.CreateMethodInfo(pointer(start), proc.name, ccReg,
         info, params.ToArray);
      result.isExternal := ext;
      result.standalone := standalone;
   finally
      params.Free;
   end;
end;

procedure TrsCodegen.ChangeLeftValue(index, value: integer);
var
   op: TrsAsmInstruction;
begin
   op := FCurrent.Text[index];
   op.left := value;
   FCurrent.Text[index] := op;
end;

procedure TrsCodegen.ChangeRightValue(index, value: integer);
var
   op: TrsAsmInstruction;
begin
   op := FCurrent.Text[index];
   op.right:= value;
   FCurrent.Text[index] := op;
end;

procedure TrsCodegen.ResolveJumps;
var
   pair: TPair<string, integer>;
begin
   for pair in FUnresolvedJumpTable do
   begin
      if not FJumpTable.ContainsKey(pair.Key) then
         raise EParseError.CreateFmt('Internal error: Invalid jump "%s"', [pair.key]);
      ChangeLeftValue(pair.Value, FJumpTable[pair.Key] - pair.Value);
   end;
   FUnresolvedJumpTable.Clear;
   FJumpTable.Clear;
end;

procedure TrsCodegen.SetupLocals(proc: TProcSymbol);
var
   locals: TStringList;
   local: string;
   i: integer;
begin
   FLocals.Clear;
   locals := proc.symbolTable.KeysWhereValue(
      function(value: TSymbol): boolean
      begin
         result := value.kind = syVariable
      end);
   try
      for local in locals do
         FLocals.Add(local);
   finally
      locals.Free;
   end;
   if assigned(proc.paramList) then
      for i := 0 to proc.paramList.Count - 1 do
      begin
         FLocals.Delete(FLocals.IndexOf(proc.paramList[i].name));
         FLocals.Insert(i, UpperCase(proc.paramList[i].name));
      end;
   if assigned(proc.&Type) then
   begin
      FLocals.Delete(FLocals.IndexOf('RESULT'));
      FLocals.Insert(proc.paramList.count, 'RESULT');
   end;
   FLocals.Insert(0, '');
   FTempQueue.Clear;
   FTempList.Clear;
end;

procedure TrsCodegen.ProcessProc(proc: TProcSymbol);
var
   syntax: TSyntax;
   start: integer;
   dummy: integer;
begin
   SetupLocals(proc);
   start := FCurrent.Text.Count;
   WriteOp(OP_INIT);
   FFreeReg := FLocals.Count;
   for syntax in proc.syntax.children do
   begin
      if FCurrentUnit.LineMap.TryGetValue(syntax, dummy) then
         FCurrentLine := dummy;
      if syntax is TTypedSyntax then
         Eval(TTypedSyntax(syntax))
      else case syntax.kind of
         skReturn: WriteOp(OP_RET);
         skLabel: FJumpTable.Add(TLabelSyntax(syntax).name, FCurrent.Text.Count);
         skJump: WriteJump(TJumpSyntax(syntax));
         skAssign: WriteAssign(TAssignmentSyntax(syntax));
         skTryF: WriteTryBlock(OP_TRYF, skFinally);
         skTryE: WriteTryBlock(OP_TRYE, skExcept);
         skFinally: CloseTryBlock(skFinally, TFinallySyntax(syntax).ret);
         skExcept: CloseTryBlock(skExcept, TExceptSyntax(syntax).ret);
         skELoad: WriteOp(OP_EXLD, VariableIndex(TExceptionLoadSyntax(syntax).symbol));
         skTryCall: WriteTryCallBlock(TTryCallSyntax(syntax));
         skRaise: WriteRaise(TRaiseSyntax(syntax));
         else raise EParseError.Create('Corrupt parse tree');
      end;
   end;
   ChangeLeftValue(start, FFreeReg);
   ResolveJumps;
   assert(FUnresStack.Count = 0);
end;

procedure TrsCodegen.CreateUnitClass(&unit: TUnitSymbol);
var
   newClass: TNewClass;
   info: TrsProcInfo;
   prefix: string;
begin
   if &unit.IsExternal then
      prefix := 'rsUNITEXT*'
   else prefix := 'rsUNIT*';
   newClass := TNewClass.Create(TObject, prefix + &unit.name);
   FCurrent.&Unit.AddClass(newClass);
   for info in FCurrent.Routines.Values do
      if info.standalone then
         newClass.AddMethod(info.info, 0, 0);
end;

procedure TrsCodegen.ResolveUnitLocalCalls(&unit: TUnitSymbol);
var
   i: integer;
   left: integer;
   table: TUnresList;
   ref: TUnresolvedReference;
   unitName: string;
begin
   table := FCurrent.UnresolvedCalls;
   unitName := &unit.name + '.';
   for i := table.Count - 1 downto 0 do
   begin
      ref := table[i];
      begin
         left := ResolveCall(ref.name, ref.location);
         assert(left <> -1);
         ChangeRightValue(ref.location, left);
      end;
   end;
   table.Clear;
end;

procedure TrsCodegen.SetupUnitProperties(&unit: TUnitSymbol; rsu: TrsScriptUnit);
var
   proplist: TList<TPair<string, TSymbol>>;
   pair: TPair<string, TSymbol>;
   symbol: TSymbol;
   cls: TClassTypeSymbol;
   props: TArray<TRttiProperty>;
   prop, found: TRttiProperty;
   propSym: TPropSymbol;
   ctx: TRttiContext;
begin
   ctx := TRttiContext.Create;
   for symbol in &unit.privates.Values do
      if (symbol.kind = syType) and (symbol is TClassTypeSymbol) then
      begin
         cls := TClassTypeSymbol(symbol);
         proplist := cls.GetSymbolTable.Where(
            function(name: string; value: TSymbol): boolean
            begin result := (value.kind = syProp) end);
         props := ctx.GetType(cls.metaclass.ClassInfo).GetDeclaredProperties;
         try
            for pair in propList do
            begin
               propSym := TPropSymbol(pair.Value);
               if assigned(propSym.paramList) and (propSym.paramList.Count > 0) then
               begin
                  if assigned(propSym.readSpec) then
                     rsu.ArrayProps.Add(propSym.readSpec.fullName);
                  if assigned(propSym.writeSpec) then
                     rsu.ArrayProps.Add(propSym.writeSpec.fullName);
               end
               else begin
                  found := nil;
                  for prop in props do
                     if AnsiSameText(prop.Name, propSym.name)then
                     begin
                        found := prop;
                        Break;
                     end;
                  assert(assigned(found));
                  rsu.properties.AddObject(propSym.fullName, found);
               end;
            end;
         finally
            proplist.Free;
         end;
      end;
end;

procedure TrsCodegen.SetupUnitGlobals(&unit: TUnitSymbol; rsu: TrsScriptUnit);
var
   list: TList<TPair<string, TSymbol>>;
   pair: TPair<string, TSymbol>;
begin
   list := &unit.privates.Where(
      function(name: string; value: TSymbol): boolean
      begin
         result := value.kind = syVariable
      end);
   try
      for pair in list do
         rsu.Globals.AddObject(pair.Value.fullName, pointer(TVarSymbol(pair.Value).&Type.TypeInfo));
   finally
      list.free;
   end;
end;

procedure TrsCodegen.SetupUnitExternalClasses(&unit: TUnitSymbol; rsu: TrsScriptUnit);
var
   list: TList<TPair<string, TSymbol>>;
   pair: TPair<string, TSymbol>;
begin
   list := &unit.privates.Where(
      function(name: string; value: TSymbol): boolean
      begin
         result := (value.kind = syType) and (TTypeSymbol(value) is TExternalClassType)
      end);
   try
      for pair in list do
         rsu.ExtClasses.Add(TExternalClassType(pair.Value).metaclass);
   finally
      list.free;
   end;
end;

function TrsCodegen.Process(&unit: TUnitSymbol): TrsScriptUnit;
var
   procs: TList<TProcSymbol>;
   proc: TProcSymbol;
   index: integer;
begin
//OutputDebugString('SAMPLING ON');
   result := TrsScriptUnit.Create(&unit.name, &unit.IsExternal);
   try
      FCurrent := result;
      FCurrentUnit := &unit;
      index := 0;
      procs := &unit.procs;
      try
         for proc in procs do
         begin
            if not &unit.IsExternal then
            begin
               index := FCurrent.Text.Count;
               processProc(proc);
            end
            else inc(index);
            FCurrent.Routines.Add(proc.fullName, CreateProcInfo(proc, index, &unit.IsExternal,
                                                                proc.parent = &unit));
         end;
         SetupUnitGlobals(&unit, result);
         SetupUnitProperties(&unit, result);
         SetupUnitExternalClasses(&unit, result);
         if not &unit.IsExternal then
            ResolveUnitLocalCalls(&unit);
      finally
         procs.Free;
      end;
      CreateUnitClass(&unit);
   except
      result.Free;
      raise;
   end;
//OutputDebugString('SAMPLING OFF');
end;

end.
