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

unit rsParser;

interface
uses
   SysUtils, RTTI, TypInfo,
   Classes, Generics.Collections,
   rsDefs, rsLexer;

type
   TUseUnitProc = reference to function(const name: string; out &unit: TUnitSymbol): boolean;
   TVerifyAttrProc = reference to function(const name: string; out ctr: TProcSymbol; out &self: TVarSymbol): boolean;
   
   TrsParser = class
   private type
      TTokenSet = set of TTokenKind;

      TProcInfo = record
         name: string;
         retval: TTypeSymbol;
         parent: TTypeSymbol;
         params: IParamList;
         forwarded: boolean;
      end;

      TAttrList = TObjectList<TCallSyntax>;
   private
      FBranchCount: integer;
      FCondCounter: integer;
      function NewBranch: string;
      function NextCondName: string;
      procedure SetQueue(value: TTokenQueue);
   private
      FCurrent: TToken;
      FPublicSymbols: ISymbolTable;
      FPrivateSymbols: ISymbolTable;
      FSubExprNamespace: ISymbolTable;
      FEnvironment: ISymbolTable;
      FUnitDict: TDictionary<string, TUnitSymbol>;
      FScopeStack: TStack<ISymbolTable>;
      FScopeStackA: TArray<ISymbolTable>;
      FQueue: TTokenQueue;
      FLexer: TrsLexer;
      FCurrentUnit: TUnitSymbol;
      FCurrentProc: TProcSymbol;
      FLastSelf: TVarSymbol;
      FEnvironmentSymbol: TVarSymbol;
      FOnUses: TUseUnitProc;
      FOnVerifyAttribute: TVerifyAttrProc;
      FFinallyDepth: integer;
      FExceptDepth: integer;
      FInImplementation: boolean;
      FEofExpected: boolean;
      FSemCount: integer;

      procedure Expect(kind: TTokenKind); overload;
      function Expect(kinds: TTokenSet): TTokenKind; overload;
      procedure Verify(kind: TTokenKind); overload;
      function Verify(kinds: TTokenSet): TTokenKind; overload;
      function Check(kind: TTokenKind): boolean;
      procedure EnsureScopeArray;
      procedure ScopeStackChange(Sender: TObject; const Item: ISymbolTable; Action: TCollectionNotification);
      function TrySymbolLookup(const name: string; out value: TSymbol): boolean;
      function SymbolLookup(const name: string): TSymbol; overload;
      function AncestorSymbolLookup(const name: string; const namespace: ISymbolTable): TSymbol;
      function SymbolLookup(const name: string; const namespace: ISymbolTable): TSymbol; overload;
      function ConstSymbolLookup(const name: string): TConstSymbol;
      function TypeSymbolLookup(const name: string): TTypeSymbol;

      function TokenToBinOp: TBinOpKind;
      procedure AddConst;
      procedure ParseIntfDelcarations;
      procedure ParseInterface;
      procedure ParseImplementation;
      function ParseUnit(const ExpectedName: string): TUnitSymbol;
      procedure Next;
      function DoParse(const ExpectedName: string): TUnitSymbol;
      procedure ParseUses;
      procedure Use(const name: string);
      function ReadStringConst: TTypedSyntax;
      function ReadConst: TTypedSyntax;
      function ReadSubExpression: TTypedSyntax;
      function ReadTerm: TTypedSyntax;
      function ReadFactor: TTypedSyntax;
      procedure UnknownTokenError;
      function IsCompilerMagic(): boolean;
      function ReadCompilerMagic: TTypedSyntax;
      function ReadIdentifier: TSyntax;
      function ReadTypeIdentifier: TTypedSyntax;
      function ReadParenExpression: TTypedSyntax;
      function ReadBracketExpression: TTypedSyntax;
      function ReadExpression: TTypedSyntax; overload;
      function ReadExpression(following: TTokenKind): TTypedSyntax; overload;
      function ReadUnOp: TTypedSyntax;
      procedure EvalExpression(var value: TTypedSyntax; constOnly: boolean);
      procedure EvalBinOp(var value: TTypedSyntax; constOnly: boolean);
      procedure EvalArraySyntax(var value: TTypedSyntax; constOnly: boolean);
      function CombineValues(const left, right: TValue; op: TBinOpKind): TValueSyntax; overload;
      function CombineValues(const value: TValue; op: TUnOpKind): TValueSyntax; overload;
      function CombineInts(const left, right: integer; op: TBinOpKind): TValue;
      function CombineFloats(const left, right: extended; op: TBinOpKind): TValue;
      function CombineStrings(const left, right: string; op: TBinOpKind): TValue;
      function CombineBools(const left, right: boolean; op: TBinOpKind): TValue;
      procedure EvalUnOp(var value: TTypedSyntax; constOnly: boolean);
      function NegateValue(const value: TValue): TValue;
      function NotValue(const value: TValue): TValue;
      procedure AddSymbol(const name: string; value: TSymbol);
      procedure ParseBlock(Adder: TProc);
      procedure AddType;
      function ReadType(const name: string): TTypeSymbol;
      function ReadClass(const name: string): TClassTypeSymbol;
      function ReadClassDefP(const name: string): TClassTypeSymbol;
      function ReadClassDef(const name: string; parent: TClassTypeSymbol): TClassTypeSymbol;
      procedure ReadClassDefSection(value: TClassTypeSymbol);
      procedure ReadField(value: TClassTypeSymbol; sectionType: TMemberVisibility);
      function TryTypeSymbolLookup(const name: string; out value: TTypeSymbol): boolean;
      procedure ReadPostField(value: TClassTypeSymbol;
        sectionType: TMemberVisibility);
      procedure ReadProperty(value: TClassTypeSymbol;
        sectionType: TMemberVisibility; classScope: boolean);
      function ReadParamList: IParamList;
      function ReadVarType(var flags: TParamFlags): TTypeSymbol;
      function ReadFlagToken: TParamFlags;
      function ReadProcDecl(inIntf: boolean): TProcInfo;
      function AddProc(const name: string; retval: TTypeSymbol;
        const ParamList: IParamList; forwarded: boolean): TProcSymbol;
      function ReadProcType(const name: string): TProcTypeSymbol;
      procedure AddVar;
      function ReadIdList(flags: TParamFlags): IParamList;
      procedure ReadMethodDecl(value: TClassTypeSymbol;
         sectionType: TMemberVisibility; classScope: boolean; info: TProcInfo);
      function ReadForwardProc(add: boolean = true): TProcSymbol;
      procedure ParseImplDeclarations;
      procedure ReadImplProc;
      procedure ReadProcName(var name: string; var parent: TTypeSymbol;
         inIntf: boolean);
      procedure ReadProcLocals(proc: TProcSymbol);
      procedure BeginProc(proc: TProcSymbol);
      procedure EndProc;
      procedure AddLabel;
      procedure ReadProcBody(proc: TProcSymbol);
      function ParseCompoundStatement(const breakTo, continueTo, exitTo: string): TBlockSyntax;
      function ParseStatementList(const breakTo, continueTo, exitTo: string): TSyntaxList;
      function ParseAssignmentExpr(lValue: TTypedSyntax): TAssignmentSyntax;
      function ParseLabel(sym: TLabelSymbol): TSyntax;
      function ParseCall(sym: TProcSymbol; selfSymbol: TVarSymbol): TCallSyntax;
      procedure CheckParamList(const expected: IParamList;
        const passed: TList<TTypedSyntax>);
      function ArrayToSet(value: TTypedSyntax; setType: PTypeInfo): TTypedSyntax;
      function ConstArrayToSet(value: TValueSyntax; setType: PTypeInfo): TValueSyntax;
      function ConstArrayTypeToSet(value: TArraySyntax; setType: PTypeInfo): TValueSyntax;
      function ParseGoto: TJumpSyntax;
      function ParseIf(const breakTo, continueTo, exitTo: string): TBlockSyntax;
      function TokenToBoolOp: TBoolOpKind;
      function ReadLineOrBlock(const breakTo, continueTo, exitTo: string): TSyntax;
      function ParseStatement(const breakTo, continueTo, exitTo: string): TSyntax;
      function ParseCase(const breakTo, continueTo, exitTo: string): TBlockSyntax;
      function ReadCaseLabels(value: TVarSymbol): TTypedSyntax;
      function ParseFor(const exitTo: string): TBlockSyntax;
      function DoBreak(const where: string): TJumpSyntax;
      function ParseForToHeader(index: TVarSymbol; out cond, step: TTypedSyntax): TBlockSyntax;
      function ParseForInHeader(index: TVarSymbol; out cond,
        step: TTypedSyntax): TSyntax;
      function ReadLoopBody(cond, step: TTypedSyntax; const exitTo: string): TBlockSyntax;
      function ParseWhile(const exitTo: string): TBlockSyntax;
      function ParseRepeat(const exitTo: string): TBlockSyntax;
      procedure CheckBooleanCondition(var cond: TTypedSyntax);
      function DoExit(const where: string): TSyntax;
      function ParseTry(const breakTo, continueTo, exitTo: string): TBlockSyntax;
      function ReadFinallyBlock(const breakTo, continueTo, exitTo, internalBreak,
        internalCont, internalExit: string): TBlockSyntax;
      function CreateTryEndingNavigation(const entrance, breakTo, continueTo,
        exitTo, internalBreak, internalCont, internalExit: string): TBlockSyntax;
      function ReadExceptBlock(const breakTo, continueTo, exitTo, internalBreak,
        internalCont, internalExit: string): TBlockSyntax;
      function ReadExceptionHandler(const ret, cont: string): TBlockSyntax;
      function ParseRaise: TRaiseSyntax;
      procedure AddScope(&unit: TUnitSymbol);
      function ReadAttributeList: TAttrList;
      function ReadAttribute: TCallSyntax;
      procedure ParseImplBody;
      function ReadArrayType: TTypeSymbol;
      function ReadSubIdentifier: TSyntax;
   public
      constructor Create(sysUnit: TUnitSymbol; const onUses: TUseUnitProc;
        const onVerifyAttr: TVerifyAttrProc);
      destructor Destroy; override;

      function Parse(const input: TTokenQueue; const ExpectedName: string): TUnitSymbol; overload;
      function Parse(const input, ExpectedName: string): TUnitSymbol; overload;
      function ParseExternalHeader(const input: TTokenQueue; parent: TUnitSymbol;
        add: boolean): TProcSymbol;
      procedure Reset;
      procedure SetEnvironment(env: TClassTypeSymbol);
   end;

implementation
uses
   Windows, Math;

type
//minor hack
   real = extended;

{ TrsParser }

constructor TrsParser.Create(sysUnit: TUnitSymbol; const onUses: TUseUnitProc;
  const onVerifyAttr: TVerifyAttrProc);
begin
   FScopeStack := TStack<ISymbolTable>.Create;
   FScopeStack.OnNotify := self.ScopeStackChange;
   FUnitDict := TDictionary<string, TUnitSymbol>.Create;
   assert(sysUnit.name = 'SYSTEM');
   FPublicSymbols := CreateSymbolTable(false);
   FPrivateSymbols := CreateSymbolTable(true);
   AddScope(sysUnit);
   FOnUses := OnUses;
   FOnVerifyAttribute := onVerifyAttr;
end;

destructor TrsParser.Destroy;
var
   name: string;
begin
   for name in FUnitDict.Keys do
      FPrivateSymbols.ExtractPair(name);
   FUnitDict.Free;
   FScopeStack.Free;
   FQueue.Free;
   FLexer.Free;
   FEnvironmentSymbol.Free;
end;

procedure TrsParser.AddScope(&unit: TUnitSymbol);
begin
   FScopeStack.Push(&unit.publics);
   FUnitDict.Add(&unit.name, &unit);
   FPrivateSymbols.Add(&unit.name, &unit)
end;

procedure TrsParser.Next;
begin
   if FCurrent.kind = tkSem then
      inc(FSemCount);
   if assigned(FLexer) then
      FLexer.Lex(FCurrent)
   else FCurrent := FQueue.Extract;
   if (FCurrent.kind = tkEOF) and not FEofExpected then
      raise EParseError.Create('Unexpected end of file.');
end;

procedure TrsParser.Verify(kind: TTokenKind);
begin
   if not (FCurrent.kind = kind) then
      raise EParseError.CreateFmt('Unexpected token "%s" at %d,%d', [FCurrent.origText, FCurrent.row, FCurrent.Column]);
end;

function TrsParser.Verify(kinds: TTokenSet): TTokenKind;
begin
   if not (FCurrent.kind in kinds) then
      raise EParseError.CreateFmt('Unexpected token "%s" at %d, %d', [FCurrent.origText, FCurrent.row, FCurrent.Column]);
   result := FCurrent.kind;
end;

procedure TrsParser.Expect(kind: TTokenKind);
begin
   Verify(kind);
   Next;
end;

function TrsParser.Expect(kinds: TTokenSet): TTokenKind;
begin
   if FCurrent.kind in kinds then
   begin
      result := FCurrent.kind;
      Next;
   end
   else raise EParseError.CreateFmt('Unexpected token "%s" at %d,%d', [FCurrent.origText, FCurrent.row, FCurrent.Column]);
end;

function TrsParser.Check(kind: TTokenKind): boolean;
begin
   result := FCurrent.kind = kind;
   if result then
      Next;
end;

procedure TrsParser.ScopeStackChange(Sender: TObject; const Item: ISymbolTable;
  Action: TCollectionNotification);
begin
   SetLength(FScopeStackA, 0);
end;

procedure TrsParser.EnsureScopeArray;
begin
   if length(FScopeStackA) = 0 then
      FScopeStackA := FScopeStack.ToArray;
end;

function TrsParser.TrySymbolLookup(const name: string; out value: TSymbol): boolean;
var
   i: integer;
   uName: string;
begin
   uName := UpperCase(name);
   EnsureScopeArray;
   for i := high(FScopeStackA) downto 0 do
      if FScopeStackA[i].TryGetValue(uName, value) then
         Exit(true);
   if assigned(FEnvironment) then
   begin
      result := FEnvironment.TryGetValue(uName, value);
      if result then
         FLastSelf := FEnvironmentSymbol;
   end
   else result := false;
end;

function TrsParser.SymbolLookup(const name: string): TSymbol;
begin
   if not TrySymbolLookup(name, result) then
      raise EParseError.CreateFmt('Symbol "%s" not found', [name])
end;

procedure TrsParser.SetEnvironment(env: TClassTypeSymbol);
begin
   FEnvironment := env.GetSymbolTable;
   FEnvironmentSymbol := TVarSymbol.Create('ENVIRONMENT*self', env, nil);
end;

procedure TrsParser.SetQueue(value: TTokenQueue);
begin
   FQueue.Free;
   FQueue := value;
   Next;
end;

function TrsParser.AncestorSymbolLookup(const name: string; const namespace: ISymbolTable): TSymbol;
var
   local: ISymbolTable;
begin
   local := (namespace['*PARENT'] as TFieldSymbol).&Type.GetSymbolTable;
   if not namespace.TryGetValue(name, result) then
      if local.ContainsKey('*PARENT') then
         result := AncestorSymbolLookup(name, local)
      else raise EParseError.CreateFmt('Symbol "%s" not found', [name]);
end;

function TrsParser.SymbolLookup(const name: string; const namespace: ISymbolTable): TSymbol;
begin
   if namespace = nil then
      result := symbolLookup(name)
   else if not namespace.TryGetValue(name, result) then
      if namespace.ContainsKey('*PARENT') then
         result := AncestorSymbolLookup(name, namespace)
      else raise EParseError.CreateFmt('Symbol "%s" not found', [name])
end;

procedure TrsParser.AddSymbol(const name: string; value: TSymbol);
var
   namespace: ISymbolTable;
   dummy: TSymbol;
begin
   namespace := FScopeStack.Peek;
   //for working with forward-declared types
   if namespace.TryGetValue(name, dummy) and (dummy = value) then
      Exit;

   namespace.Add(name, value);
   if (namespace = FPrivateSymbols) and (not FInImplementation) then
      FPublicSymbols.Add(name, value);
end;

function TrsParser.ConstSymbolLookup(const name: string): TConstSymbol;
var
   sym: TSymbol;
begin
   sym := SymbolLookup(name);
   if sym.kind <> syConst then
      raise EParseError.CreateFmt('"%s" is not a constant', [name]);
   result := TConstSymbol(sym);
end;

function TrsParser.TryTypeSymbolLookup(const name: string; out value: TTypeSymbol): boolean;
var
   symbol: TSymbol;
begin
   if not TrySymbolLookup(name, symbol) then
      result := false
   else if symbol is TTypeSymbol then
   begin
      result := true;
      value := TTypeSymbol(symbol);
   end
   else raise EParseError.CreateFmt('"%s" is not a type identifier', [name]);
end;

function TrsParser.TypeSymbolLookup(const name: string): TTypeSymbol;
begin
   if not TryTypeSymbolLookup(name, result) then
      raise EParseError.CreateFmt('Symbol "%s" not found', [name]);
end;

function TrsParser.TokenToBinOp: TBinOpKind;
begin
   case FCurrent.kind of
      tkPlus: result := opPlus;
      tkMinus: result := opMinus;
      tkTimes: result := opMult;
      tkDiv: result := opDiv;
      tkDivide: result := opDivide;
      tkMod: result := opMod;
      tkAnd: result := opAnd;
      tkOr: result := opOr;
      tkXor: result := opXor;
      tkShl: result := opShl;
      tkShr: result := opShr;
      tkAs: result := opAs;
      else raise EParseError.CreateFmt('Unknown op token "%s"', [FCurrent.origText]);
   end;
   Next;
end;

function TrsParser.TokenToBoolOp: TBoolOpKind;
begin
   case FCurrent.kind of
      tkGreaterEqual: result := opGreaterEqual;
      tkLessEqual: result := opLessEqual;
      tkGreaterThan: result := opGreaterThan;
      tkLessThan: result := opLessThan;
      tkEquals: result := opEquals;
      tkNotEqual: result := opNotEqual;
      tkIn: result := opIn;
      tkIs: result := opIs;
      tkAnd: result := opBoolAnd;
      tkOr: result := opBoolOr;
      tkXor: result := opBoolXor;
      else raise EParseError.CreateFmt('Unknown op token "%s"', [FCurrent.origText]);
   end;
   Next;
end;

procedure TrsParser.Use(const name: string);
var
   symbol: TUnitSymbol;
begin
   if FPrivateSymbols.ContainsKey(name) then
      raise EParseError.CreateFmt('Identifier redeclared: "%s"', [name]);
   if not FOnUses(name, symbol) then
      raise EParseError.CreateFmt('Used unit "%s" not found', [name]);
   assert(symbol.name = name);
   AddScope(symbol);
end;

procedure TrsParser.ParseUses;
begin
   repeat
      Verify(tkIdentifier);
      Use(FCurrent.GetText);
      Next;
   until not check(tkComma);
   expect(tkSem);
end;

function TrsParser.ReadStringConst: TTypedSyntax;
var
   rValue: TValueSyntax;
begin
   result := TValueSyntax.Create(FCurrent.GetText);
   Next;
   while FCurrent.kind in [tkChar, tkPlus] do
   begin
      if FCurrent.kind = tkPlus then
      begin
         Next;
         Verify([tkChar, tkString, tkIdentifier]);
      end;
      if FCurrent.kind <> tkIdentifier then
         rValue := TValueSyntax.Create(FCurrent.GetText)
      else rValue := TValueSyntax.Create(ConstSymbolLookup(FCurrent.GetText).value);
      result := TBinOpSyntax.Create(opPlus, result, rValue);
      Next;
   end;
   Expect(tkSem);
end;

function TrsParser.IsCompilerMagic(): boolean;
begin
   result := false; //TODO: Implement this
end;

function TrsParser.ReadCompilerMagic: TTypedSyntax;
begin
   result := nil;
   assert(false); //TODO: Implement this
end;

function TrsParser.ReadTypeIdentifier: TTypedSyntax;
begin
   raise EParseError.Create('Class and type references are not implemented yet.'); //TODO: Implement this
end;

function TrsParser.ReadSubIdentifier: TSyntax;
var
   symbol: TSymbol;
begin
   symbol := SymbolLookup(FCurrent.GetText, FSubExprNamespace);
   FSubExprNamespace := nil;
   result := nil;
   repeat
      case symbol.kind of
         syConst, syEnum: result := TValueSyntax.Create(TConstSymbol(symbol).value);
         syVariable, syProp:
         begin
            result := TVariableSyntax.Create(TVarSymbol(symbol));
            if assigned(FEnvironmentSymbol) and (FLastSelf = FEnvironmentSymbol) then
            begin
               result := TDotSyntax.Create(TVariableSyntax.Create(FLastSelf), TTypedSyntax(result));
               FLastSelf := nil;
            end;
         end;
         syType: result := ReadTypeIdentifier;
         syProc:
         begin
            result := ParseCall(TProcSymbol(symbol), FLastSelf);
            FLastSelf := nil;
            Exit;
         end;
         syUnit:
         begin
            Next;
            Expect(tkDot);
            symbol := TUnitSymbol(symbol).SymbolLookup(FCurrent.GetText);
         end;
         syLabel: Exit(ParseLabel(TLabelSymbol(symbol)))
      end;
   until assigned(result);
   Next;
end;

function TrsParser.ReadIdentifier: TSyntax;
var
   synType: TTypeSymbol;
   arrayFlag: boolean;
   second: TSyntax;
begin
   result := ReadSubIdentifier;
   try
      while FCurrent.kind in [tkDot, tkOpenBracket] do
      begin
         if not (result is TTypedSyntax) then
            raise EParseError.CreateFmt('The "%s" operator is not valid here', [FCurrent.origText]);
         FLastSelf := TTypedSyntax(result).GetSelfValue;
         synType := TTypedSyntax(result).&type;
         if (synType = nil) then
         begin
            assert(result is TCallSyntax);
            raise EParseError.Create('Procedures have no result type');
         end;
         if ((FCurrent.kind = tkDot) and not synType.CanDerefDot) or
            ((FCurrent.kind = tkOpenBracket) and not synType.CanDerefArray) then
            raise EParseError.CreateFmt('The "%s" operator cannot be used on an expression of type "%s".', [FCurrent.origText, synType.name]);
         arrayFlag := FCurrent.kind = tkOpenBracket;
         Next;
         if arrayFlag then
         begin
            if TTypedSyntax(result).&type is TClassTypeSymbol then
               result := TDotSyntax.Create(TTypedSyntax(result),
                 TVariableSyntax.Create(TClassTypeSymbol(TTypedSyntax(result).&type).DefaultProperty));
            FSubExprNamespace := nil;
            second := ReadExpression;
            Expect(tkCloseBracket);
         end
         else begin
            FSubExprNamespace := synType.GetSymbolTable;
            second := ReadIdentifier;
         end;
         if not (second is TTypedSyntax) then
            raise EParseError.Create('Typed expression expected');
         if arrayFlag then
         begin
            if result.kind = skDot then
               result := TArrayPropSyntax.Create(TDotSyntax(result), TTypedSyntax(second))
            else result := TElemSyntax.Create(TTypedSyntax(result), TTypedSyntax(second))
         end
         else begin
            if assigned(FLastSelf) then
            begin
               assert(TArraypropSyntax(result).GetSelfValue = FLastSelf);
               FLastSelf := nil;
            end;
            result := TDotSyntax.Create(TTypedSyntax(result), TTypedSyntax(second));
         end;
      end;
   except
      result.free;
      raise;
   end;
end;

function TrsParser.ReadParenExpression: TTypedSyntax;
begin
   Next;
   result := ReadExpression(tkCloseParen);
end;

function TrsParser.ReadUnOp: TTypedSyntax;
var
   op: TUnOpKind;
begin
   case Expect([tkMinus, tkNot]) of
      tkMinus: op := opNeg;
      tkNot: op := opNot;
      else raise EParseError.CreateFmt('Incorrect operator type: "%s"', [FCurrent.origText]);
   end;
   result := TUnOpSyntax.Create(op, readTerm);

   //minor optimization to avoid negating constants in output
   if TUnOpSyntax(result).sub.kind = skValue then
      EvalUnOp(result, false);
   result.sem := FSemCount;
end;

function ConstArrayFromArray(value: TArraySyntax; destType: PTypeInfo = nil): TValueSyntax;
var
   vals: TArray<TValue>;
   i: integer;
   children: TSyntaxList;
begin
   if (destType = nil) then
      if value.&type.HasTypeInfo  then
         destType := value.&type.TypeInfo
      else Exit(nil);

   children := value.subvalues.children;
   if children.count = 0 then
      Exit(nil);
   setLength(vals, children.Count);
   for i := 0 to high(vals) do
   begin
      if children[i].kind <> skValue then
         Exit(nil);
      vals[i] := TValueSyntax(children[i]).value;
   end;
   result := TValueSyntax.Create(TValue.FromArray(destType, vals))
end;

function TrsParser.ReadBracketExpression: TTypedSyntax;
var
   simplified: TValueSyntax;
begin
   Next;
   result := TArraySyntax.Create;
   if not check(tkCloseBracket) then
      repeat
         TArraySyntax(result).add(ReadExpression);
      until Expect([tkComma, tkCloseBracket]) = tkCloseBracket;
   TArraySyntax(result).Finalize;
   simplified := ConstArrayFromArray(TArraySyntax(result));
   if assigned(simplified) then
   begin
      result.Free;
      result := simplified;
   end;
end;

procedure TrsParser.UnknownTokenError;
begin
   raise EParseError.CreateFmt('Unknown Token "%s"', [FCurrent.origText]);
end;

function TrsParser.ReadFactor: TTypedSyntax;
begin
   result := nil;
   case FCurrent.kind of
      tkIdentifier:
      begin
         if IsCompilerMagic then
            result := ReadCompilerMagic
         else Exit(ReadIdentifier as TTypedSyntax); //don't call Next after this
      end;
      tkPlus:
      begin
         Next; //meaningless noise; skip this
         result := ReadTerm;
      end;
      tkMinus, tkNot: Exit(ReadUnOp);
      tkOpenParen: Exit(ReadParenExpression);
      tkOpenBracket: Exit(ReadBracketExpression);
      tkInt, tkFloat, tkChar, tkString: result := TValueSyntax.Create(FCurrent);
      tkNil: result := TValueSyntax.Create;
      else UnknownTokenError;
   end;
   Next;
end;

function TrsParser.ReadTerm: TTypedSyntax;
var
   op: TBinOpKind;
begin
   result := ReadFactor;
   while FCurrent.kind in [tkTimes, tkDivide, tkDiv, tkMod, tkAnd, tkShl, tkShr, tkAs] do
   try
      if result.&type = BooleanType then
         Break;
      op := TokenToBinOp;
      result := TBinOpSyntax.Create(op, result, ReadFactor);
      TBinOpSyntax(result).left.sem := FSemCount;
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ReadSubExpression: TTypedSyntax;
var
   op: TBinOpKind;
begin
   result := ReadTerm;
   while FCurrent.kind in [tkPlus, tkMinus, tkOr, tkXor] do
   try
      if result.&type = BooleanType then
         Break;
      op := TokenToBinOp;
      result := TBinOpSyntax.Create(op, result, ReadTerm);
      TBinOpSyntax(result).left.sem := FSemCount;
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ReadExpression: TTypedSyntax;
var
   op: TBoolOpKind;
   sub: TTypedSyntax;
begin
   result := ReadSubExpression;
   while FCurrent.kind in [tkGreaterEqual, tkLessEqual, tkGreaterThan, tkLessThan,
                           tkEquals, tkNotEqual, tkIn, tkIs, tkAnd, tkOr, tkXor] do
   try
      op := TokenToBoolOp;
      sub := ReadSubExpression;
      if (op in [opBoolAnd..opBoolXor]) and not ((result.&type = BooleanType) and (sub.&type = BooleanType)) then
      begin
         sub.Free;
         raise EParseError.Create('Boolean expression required');
      end;
      result := TBoolOpSyntax.Create(op, result, sub);
      TBoolOpSyntax(result).left.sem := FSemCount;
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ReadExpression(following: TTokenKind): TTypedSyntax;
begin
   result := ReadExpression;
   try
      Expect(following);
   except
      result.Free;
      raise;
   end;
end;

{$WARN USE_BEFORE_DEF OFF}
function TrsParser.CombineInts(const left, right: integer; op: TBinOpKind): TValue;
var
   v: integer;
begin
   case op of
      opPlus: v := left + right;
      opMinus: v := left - right;
      opMult: v := left * right;
      opDiv: v := left div right;
      opDivide: Exit(left / right);
      opMod: v := left mod right;
      opAnd: v := left and right;
      opOr: v := left or right;
      opXor: v := left xor right;
      opShl: v := left shl right;
      opShr: v := left shr right;
      opAs: Abort;
   end;
   result := v;
end;

function TrsParser.CombineFloats(const left, right: extended; op: TBinOpKind): TValue;
var
   v: real;
begin
   case op of
      opPlus: v := left + right;
      opMinus: v := left - right;
      opMult: v := left * right;
      opDivide: v := left / right;
      else Abort;
   end;
   result := v;
end;

function TrsParser.CombineBools(const left, right: boolean; op: TBinOpKind): TValue;
var
   v: boolean;
begin
   case op of
      opAnd: v := left and right;
      opOr: v := left or right;
      opXor: v := left xor right;
      else Abort;
   end;
   result := v;
end;
{$WARN USE_BEFORE_DEF ON}

function TrsParser.CombineStrings(const left, right: string; op: TBinOpKind): TValue;
begin
   if op = opPlus then
      result := left + right
   else raise EParseError.Create('Invalid operation');
end;

function TrsParser.CombineValues(const left, right: TValue; op: TBinOpKind): TValueSyntax;
var
   val: TValue;
begin
   if (left.Kind = tkInteger) and (right.Kind = tkInteger) then
      val := CombineInts(left.AsInteger, right.AsInteger, op)
   else if (left.Kind in [tkInteger, typInfo.tkFloat]) and (right.Kind in [tkInteger, typInfo.tkFloat]) then
      val := CombineFloats(left.AsExtended, right.AsExtended, op)
   else if (left.Kind in [tkWChar, tkUString, tkWString]) and (right.Kind in [tkWChar, tkUString, tkWString]) then
      val := CombineStrings(left.AsString, right.AsString, op)
   else if (left.TypeInfo = TypeInfo(boolean)) and (right.TypeInfo = TypeInfo(boolean)) then
      val := CombineBools(left.AsBoolean, right.AsBoolean, op)
   else Abort;
   result := TValueSyntax.Create(val);
end;

procedure TrsParser.EvalArraySyntax(var value: TTypedSyntax; constOnly: boolean);
var
   isConst: boolean;
   subscript: TSyntax;
   values: array of TValue;
   newVal: TValueSyntax;
   i: integer;
begin
   isConst := true;
   for subscript in (value as TArraySyntax).subvalues.children do
      if subscript.kind <> skValue then
      begin
         isConst := false;
         break;
      end;

   if isConst then
   begin
      setLength(values, TArraySyntax(value).subvalues.children.Count);
      for i := 0 to high(values) do
         values[i] := TValueSyntax(TArraySyntax(value).subvalues.children[i]).value;
      newVal := TValueSyntax.Create(TValue.FromArray(MakeArrayPropType(value.&type).TypeInfo, values));
      value.Free;
      value := newVal;
   end;
end;

procedure TrsParser.EvalBinOp(var value: TTypedSyntax; constOnly: boolean);
var
   left, right, new: TTypedSyntax;
begin
   left := TBinOpSyntax(value).left;
   right := TBinOpSyntax(value).right;
   EvalExpression(left, constOnly);
   EvalExpression(right, constOnly);
   if (left.kind = skValue) and (right.kind = skValue) then
   begin
      try
         new := CombineValues(TValueSyntax(left).value, TValueSyntax(right).value, TBinOpSyntax(value).op);
      except
         raise EParseError.Create('Invalid operation');
      end;
      value.Free;
      value := new;
   end;
end;

function TrsParser.NegateValue(const value: TValue): TValue;
begin
   if value.Kind = tkInteger then
      result := -(value.AsInteger)
   else if value.Kind = typInfo.tkFloat then
      result := -(value.AsExtended)
   else Abort;
end;

function TrsParser.NotValue(const value: TValue): TValue;
begin
   if value.Kind = tkInteger then
      result := not (value.AsInteger)
   else if value.TypeInfo = TypeInfo(boolean) then
      result := not (value.AsBoolean)
   else Abort;
end;

function TrsParser.CombineValues(const value: TValue; op: TUnOpKind): TValueSyntax;
var
   v: TValue;
begin
   case op of
      opNeg: v := negateValue(value);
      opNot: v := notValue(value);
   end;
   result := TValueSyntax.Create(v);
end;

procedure TrsParser.EvalUnOp(var value: TTypedSyntax; constOnly: boolean);
var
   sub, new: TTypedSyntax;
   main: TUnOpSyntax;
begin
   main := value as TUnOpSyntax;
   sub := main.sub;
   evalExpression(sub, constOnly);
   if not (sub.kind in [skValue, skBinOp, skUnOp, skVariable]) then
      raise EParseError.Create('Invalid operation');
   if sub.kind = skValue then
   begin
      try
         new := CombineValues(TValueSyntax(sub).value, main.op);
      except
         raise EParseError.Create('Invalid operation');
      end;
      value.Free;
      value := new;
   end;
end;

procedure TrsParser.EvalExpression(var value: TTypedSyntax; constOnly: boolean);
begin
   case value.kind of
      skBinOp: EvalBinOp(value, constOnly);
      skUnOp: EvalUnOp(value, constOnly);
      skArray: EvalArraySyntax(value, constOnly);
   end;
   value.sem := FSemCount;

   if constOnly and (value.kind <> skValue) then
      raise EParseError.Create('Constant expression required');
end;

function TrsParser.ReadConst: TTypedSyntax;
begin
   if FCurrent.kind in [tkString, tkChar] then
      result := ReadStringConst
   else result := ReadExpression(tkSem);
   EvalExpression(result, true);
end;

procedure TrsParser.AddConst;
var
   name: string;
   value: TSyntax;
begin
   Verify(tkIdentifier);
   name := FCurrent.origText;
   Next;
   expect(tkEquals); //TODO: Implement typed constants
   value := ReadConst;
   AddSymbol(UpperCase(name), TConstSymbol.Create(name, (value as TValueSyntax).value));
   value.Free;
end;

procedure TrsParser.ReadField(value: TClassTypeSymbol; sectionType: TMemberVisibility);
var
   name: string;
   &type: TTypeSymbol;
begin
   name := FCurrent.origText;
   Next;
   expect(tkColon);
   Verify(tkIdentifier);
   &type := Self.TypeSymbolLookup(FCurrent.OrigText);
   value.AddField(name, &type, sectionType);
end;

function TrsParser.ReadArrayType: TTypeSymbol;
var
   lBound, uBound: integer;
   typeSym: TTypeSymbol;
begin
   Next; //Exepect(tkArray);
   if Check(tkOpenBracket) then
   begin
      case Verify([tkInt, tkIdentifier]) of
         tkInt:
         begin
            lBound := StrToInt(FCurrent.GetText);
            Next;
            Expect(tkDotDot);
            Verify(tkInt);
            uBound := StrToInt(FCurrent.GetText);
            Next;
         end;
         //TODO: implement this
         tkIdentifier: raise EParseError.Create('Array [type] syntax is not supported yet');
         else raise EParseError.Create('Invalid array type');
      end;
      Expect(tkCloseBracket);
   end
   else begin
      lBound := 0;
      uBound := 0;
   end;
   Expect(tkOf);
   typeSym := TypeSymbolLookup(FCurrent.GetText);
   Result := TArrayTypeSymbol.Create(lBound, uBound, typeSym);
   FScopeStack.Peek.Add(result.name, result);
end;

function TrsParser.ReadVarType(var flags: TParamFlags): TTypeSymbol;
begin
   Expect(tkColon);
   case FCurrent.kind of
      tkIdentifier: result := TypeSymbolLookup(FCurrent.OrigText);
      tkArray: result := ReadArrayType;
//      tkSet: ;
      else raise EParseError.Create('Invalid type');
   end;
   Next;
end;

function TrsParser.ReadFlagToken: TParamFlags;
begin
   case FCurrent.kind of
      tkVar: result := [pfVar];
      tkConst: result := [pfConst];
      tkOut: result := [pfOut];
      else result := [];
   end;
   if result <> [] then
      Next;
end;

function TrsParser.ReadIdList(flags: TParamFlags): IParamList;
var
   names: TStringList;
   name: string;
   &type: TTypeSymbol;
begin
   result := EmptyParamList(false);
   names := TStringList.Create;
   try
      repeat
         verify(tkIdentifier);
         names.Add(FCurrent.origText);
         Next;
         if not Check(tkComma) then
            Verify(tkColon);
      until FCurrent.kind = tkColon;
      &type := ReadVarType(flags);
      for name in names do
         result.Add(TParamSymbol.Create(name, &type, flags));
   finally
      names.Free;
   end;
end;

function TrsParser.ReadParamList: IParamList;
var
   flags: TParamFlags;
   list: IParamList;
begin
   result := EmptyParamList;
   while FCurrent.kind in [tkVar, tkConst, tkOut, tkIdentifier] do
   begin
      flags := ReadFlagToken;
      list := ReadIdList(flags);
      list.OwnsObjects := false;
      result.AddRange(list);
      if FCurrent.kind = tkSem then
      begin
         Next;
         Verify([tkVar, tkConst, tkOut, tkIdentifier]);
      end;
   end;
end;

procedure TrsParser.ReadProperty(value: TClassTypeSymbol; sectionType: TMemberVisibility;
  classScope: boolean);
var
   name: string;
   &type: TTypeSymbol;
   dummy: TParamFlags; //TODO: implement this
   paramList: IParamList;
   readSpec, writeSpec: TSymbol;
begin
   readSpec := nil;
   writeSpec := nil;
   Next; //property keyword
   name := FCurrent.origText;
   Next;
   if check(tkOpenBracket) then
   begin
      paramList := ReadParamList;
      if paramList.Count < 1 then
         raise EParseError.Create('Indexed properties must contain at least one index parameter');
      Expect(tkCloseBracket);
   end;
   &type := ReadVarType(dummy);
   repeat
      verify(tkIdentifier);
      if FCurrent.GetText = 'READ' then
      begin
         if assigned(readSpec) then
            raise EParseError.Create('Read specifier redeclared')
         else readSpec := SymbolLookup(FCurrent.GetText);
      end
      else if FCurrent.GetText = 'WRITE' then
      begin
         if assigned(writeSpec) then
            raise EParseError.Create('Write specifier redeclared')
         else writeSpec := SymbolLookup(FCurrent.GetText);
      end
      else raise EParseError.CreateFmt('Expected "read" or "write" but found "%s".', [FCurrent.origText]);
      Next;
   until FCurrent.kind = tkSem;
   value.addProp(name, &type, sectionType, classScope, paramList, readSpec, writeSpec);
   Next;
end;

function TrsParser.AddProc(const name: string; retval: TTypeSymbol;
  const ParamList: IParamList; forwarded: boolean): TProcSymbol;
var
   existing: TSymbol;
begin
   if FPrivateSymbols.ContainsKey(UpperCase(name)) then
   begin
      existing := FPrivateSymbols[UpperCase(name)];
      if (existing is TProcSymbol) and TProcSymbol(existing).forwarded and
         not forwarded then
      begin
         result := TProcSymbol(existing);
         result.Define;
      end
      else raise EParseError.CreateFmt('Identifier redeclared: "%s"', [name]);
   end
   else begin
      result := TProcSymbol.Create(name, retval, mvPublic, false, paramList, forwarded, FCurrentUnit);
      self.AddSymbol(UpperCase(name), result);
   end;
end;

procedure TrsParser.ReadProcName(var name: string; var parent: TTypeSymbol; inIntf: boolean);
var
   lname: string;
begin
   lName := FCurrent.OrigText;
   Next;
   if check(tkDot) then //TODO: turn this into a while loop to allow for nested classes
   begin
      if inIntf then
         raise EParseError.Create('Expected ''(''.');
      parent := TypeSymbolLookup(lName);
      if not (parent is TClassTypeSymbol) then //TODO: allow record types here
         raise EParseError.CreateFmt('"%s" is not a class type.', [name]);
      lName := FCurrent.GetText;
      Next;
   end
   else parent := nil;
   name := lName;
end;

function TrsParser.ReadProcDecl(inIntf: boolean): TProcInfo;
var
   isFunction: boolean;
   forwarded: boolean;
begin
   forwarded := inIntf;
   isFunction := FCurrent.kind = tkFunction;
   Next;
   Verify(tkIdentifier);
   ReadProcName(result.name, result.parent, inIntf);
   if check(tkOpenParen) then
   begin
      result.params := ReadParamList;
      Expect(tkCloseParen);
   end;
   if isFunction then
   begin
      Expect(tkColon);
      Verify(tkIdentifier);
      result.retval := TypeSymbolLookup(FCurrent.OrigText);
      Next;
   end
   else result.retval := nil;
   expect(tkSem);
   if (Check(tkForward)) then
   begin
      Expect(tkSem);
      forwarded := true;
   end;
   result.forwarded := forwarded or inIntf;
end;

procedure TrsParser.ReadMethodDecl(value: TClassTypeSymbol; sectionType: TMemberVisibility;
  classScope: boolean; info: TProcInfo);
begin
   value.AddMethod(info.name, info.retval, sectionType, classScope, info.params);
end;

procedure TrsParser.ReadPostField(value: TClassTypeSymbol; sectionType: TMemberVisibility);
var
   classScope: boolean;
begin
   if FCurrent.kind = tkClass then
   begin
      classScope := true;
      Next;
      //TODO: implement class constructors and destructors
      Verify([tkProcedure, tkFunction, tkProperty{, tkConstructor, tkDestructor}]);
   end
   else classScope := false;
   if FCurrent.kind = tkProperty then
      ReadProperty(value, sectionType, classScope)
   else
      ReadMethodDecl(value, sectionType, classScope, ReadProcDecl(true));
end;

{$WARN USE_BEFORE_DEF OFF}
procedure TrsParser.ReadClassDefSection(value: TClassTypeSymbol);
var
   sectionType: TMemberVisibility;
begin
   //TODO: Add support for vars, class vars, class methods and subtypes
   case Expect([tkPrivate, tkProtected, tkPublic]) of
      tkPrivate: sectionType := mvPrivate;
      tkProtected: sectionType := mvProtected;
      tkPublic: sectionType := mvPublic;
   end;
   while FCurrent.kind = tkIdentifier do
      ReadField(value, sectionType);
   while FCurrent.kind in [tkClass, tkProcedure, tkFunction, tkProperty, tkConstructor, tkDestructor] do
      ReadPostField(value, sectionType)
end;
{$WARN USE_BEFORE_DEF ON}

function TrsParser.ReadClassDef(const name: string; parent: TClassTypeSymbol): TClassTypeSymbol;
var
   symbol: TTypeSymbol;
begin
   if TryTypeSymbolLookup(name, symbol) then
   begin
      if symbol is TClassTypeSymbol then
         result := TClassTypeSymbol(symbol)
      else raise EParseError.CreateFmt('Type "%s" is not a class type', [name]);
      if result.forwarded then
         result.Define
      else raise EParseError.CreateFmt('Class "%s" redefined', [name]);
   end
   else result := TClassTypeSymbol.Create(name);
   AddSymbol(name, result);
   while FCurrent.kind in [tkPrivate, tkProtected, tkPublic] do
      ReadClassDefSection(result);
   Expect(tkEnd);
   Expect(tkSem);
end;

function TrsParser.ReadClassDefP(const name: string): TClassTypeSymbol;
var
   parent: TSymbol;
begin
   Next; //skip paren
   Verify(tkIdentifier);
   parent := SymbolLookup(FCurrent.GetText);
   if not (parent is TClassTypeSymbol) then
      raise EParseError.CreateFmt('"%s" is not a class type', [FCurrent.origText]);
   Next;
   Expect(tkCloseParen); //TODO: implement interface inheritance
   result := ReadClassDef(name, TClassTypeSymbol(parent));
end;

function TrsParser.ReadClass(const name: string): TClassTypeSymbol;
begin
   Next;
   case FCurrent.kind of
      tkSem: result := TClassTypeSymbol.CreateFwd(name);
      tkOpenParen: result := ReadClassDefP(name);
      tkPrivate, tkProtected, tkPublic: result := ReadClassDef(name, nil);
      else raise EParseError.CreateFmt('Illegal type "%s"', [FCurrent.origText]);
   end;
end;

function TrsParser.ReadProcType(const name: string): TProcTypeSymbol;
var
   isFunction: boolean;
   isMethod: boolean;
   paramList: IParamList;
   retval: TTypeSymbol;
begin
   isFunction := FCurrent.kind = tkFunction;
   Next;
   if check(tkOpenParen) then
   begin
      paramList := ReadParamList;
      Expect(tkCloseParen);
   end;
   if isFunction then
   begin
      Expect(tkColon);
      Verify(tkIdentifier);
      retval := TypeSymbolLookup(FCurrent.OrigText);
      Next;
   end
   else retval := nil;
   if FCurrent.kind = tkOf then
   begin
      Next;
      Expect(tkObject);
      isMethod := true;
   end
   else isMethod := false;
   expect(tkSem);
   result := TProcTypeSymbol.Create(name, retval, paramList, isMethod);
end;

function TrsParser.ReadType(const name: string): TTypeSymbol;
begin
   case FCurrent.kind of
      tkClass: result := ReadClass(name);
      tkProcedure, tkFunction: result := ReadProcType(name);
//TODO: implement the rest
{      tkIdentifier: ; //if this is "string" then check for string types
      tkOpenParen: ;
      tkArray: ;
      tkRecord: ;
      tkSet: ; }
      else raise EParseError.CreateFmt('Illegal type "%s"', [FCurrent.origText]);
   end;
end;

function TrsParser.ReadAttribute: TCallSyntax;
var
   name: string;
   attrType: TVarSymbol;
   ctr: TProcSymbol;
begin
   name := FCurrent.GetText;
   if not FOnVerifyAttribute(name, ctr, attrType) then
      raise EParseError.CreateFmt('Unknown attribute type "%s"', [FCurrent.origText]);
   result := ParseCall(ctr, attrType);
   Expect(tkCloseBracket);
end;

function TrsParser.ReadAttributeList: TAttrList;
var
   list: TObjectList<TCallSyntax>;
begin
   list := TObjectList<TCallSyntax>.Create;
   list.OwnsObjects := true;
   result := list;
   while Check(tkOpenBracket) do
      result.Add(ReadAttribute);
end;

procedure TrsParser.AddType;
var
   name: string;
   list: TAttrList;
begin
   list := ReadAttributeList;
   list.Free; //TODO: Actually apply the attributes instead of discarding them
   Verify(tkIdentifier);
   name := FCurrent.origText;
   Next;
   expect(tkEquals);
   AddSymbol(name, ReadType(name));
end;

procedure TrsParser.AddVar;
var
   param: TParamSymbol;
begin
   for param in ReadIdList([]) do
      self.AddSymbol(UpperCase(param.name), param);
   Expect(tkSem);
end;

procedure TrsParser.BeginProc(proc: TProcSymbol);
begin
   FScopeStack.Push(proc.SymbolTable);
   FCurrentProc := proc;
end;

procedure TrsParser.EndProc;
begin
   FCurrentProc := nil;
   FScopeStack.Pop;
end;

procedure TrsParser.ParseBlock(Adder: TProc);
begin
   Next;
   repeat
      Adder;
   until FCurrent.kind <> tkIdentifier;
end;

function TrsParser.ReadForwardProc(add: boolean): TProcSymbol;
var
   info: TProcInfo;
begin
   info := ReadProcDecl(true);
   if add then
      result := AddProc(info.name, info.retval, info.params, true)
   else result := TProcSymbol.Create(info.name, info.retval, mvPublic, false, info.params, true, nil);
end;

procedure TrsParser.ParseIntfDelcarations;
begin
   FScopeStack.Push(FPrivateSymbols);
   while FCurrent.kind in [tkConst, tkType, tkVar, tkProcedure, tkFunction] do
      case FCurrent.kind of
         tkConst: ParseBlock(AddConst);
         tkType: ParseBlock(AddType);
         tkVar: ParseBlock(AddVar);
         tkProcedure, tkFunction: ReadForwardProc;
      end;
   FScopeStack.Pop;
end;

procedure TrsParser.ParseInterface;
begin
   Expect(tkInterface);
   if check(tkUses) then
      ParseUses;
   ParseIntfDelcarations;
end;

procedure TrsParser.AddLabel;
var
   list: TStringList;
   name: string;
begin
   Next; //expect(tkLabel);
   Verify(tkIdentifier);
   list := TStringList.Create;
   try
      repeat
         list.Add(FCurrent.GetText);
         Next;
         if Check(tkComma) then
            Verify(tkIdentifier);
      until FCurrent.kind = tkSem;
      Next;
      for name in list do
         AddSymbol(name, TLabelSymbol.Create(name));
   finally
      list.Free;
   end;
end;

procedure TrsParser.ReadProcLocals(proc: TProcSymbol);
var
   param: TParamSymbol;
begin
   if assigned(proc.&Type) then
      proc.symbolTable.Add('RESULT', TVarSymbol.Create('RESULT', proc.&Type, proc));
   if assigned(proc.paramList) then
   begin
      for param in proc.paramList do
         proc.symbolTable.Add(UpperCase(param.name), param);
      proc.paramList.OwnsObjects := false;
   end;
   while FCurrent.kind in [tkConst, tkVar, tkLabel] do //TODO: allow sub-procedures
      case FCurrent.kind of
         tkConst: ParseBlock(AddConst);
         tkVar: ParseBlock(AddVar);
         tkLabel: AddLabel;
      end;
end;

function TrsParser.ParseAssignmentExpr(lValue: TTypedSyntax): TAssignmentSyntax;
var
   rValue: TTypedSyntax;
begin
   try
      rValue := ReadExpression;
      EnsureCompatibleTypes(lValue.&type, rValue);
      result := TAssignmentSyntax.Create(lValue, rValue);
   except
      lValue.Free;
      raise;
   end;
end;

function TrsParser.ParseLabel(sym: TLabelSymbol): TSyntax;
begin
   Next;
   Expect(tkColon);
   sym.Use;
   result := TLabelSyntax.Create(sym.name);
end;

procedure TrsParser.CheckBooleanCondition(var cond: TTypedSyntax);
begin
   try
      CheckCompatibleTypes(BooleanType, cond.&type, true);
      EvalExpression(cond, false);
   except
      FreeAndNil(cond);
      raise;
   end;
end;

function TrsParser.ConstArrayToSet(value: TValueSyntax; setType: PTypeInfo): TValueSyntax;
var
   intSet: set of 0..31;
   input, output, subvalue: TValue;
   i: integer;
begin
   intSet := [];
   input := value.value;
   for i := 0 to input.GetArrayLength - 1 do
   begin
      subvalue := input.GetArrayElement(i);
      if (subvalue.AsOrdinal < 0) or (subvalue.AsOrdinal > 31) then
         raise ERangeError.CreateFmt('Set value %s falls outside acceptable boundaries', [subvalue.ToString]);
      if subValue.AsOrdinal in intSet then
         raise EParseError.CreateFmt('Value %s is repeated in a set', [subvalue.ToString]);
      include(intSet, subValue.AsOrdinal);
   end;
   TValue.Make(@intSet, setType, output);
   result := TValueSyntax.Create(output);
end;

function TrsParser.ConstArrayTypeToSet(value: TArraySyntax; setType: PTypeInfo): TValueSyntax;
var
   intSet: set of 0..31;
   output, subvalue: TValue;
   i: integer;
begin
   intSet := [];
   for i := 0 to value.subvalues.children.Count - 1 do
   begin
      subvalue := (value.subvalues.children[i] as TValueSyntax).value;
      if (subvalue.AsOrdinal < 0) or (subvalue.AsOrdinal > 31) then
         raise ERangeError.CreateFmt('Set value %s falls outside acceptable boundaries', [subvalue.ToString]);
      if subValue.AsOrdinal in intSet then
         raise EParseError.CreateFmt('Value %s is repeated in a set', [subvalue.ToString]);
      include(intSet, subValue.AsOrdinal);
   end;
   TValue.Make(@intSet, setType, output);
   result := TValueSyntax.Create(output);
end;

function TrsParser.ArrayToSet(value: TTypedSyntax; setType: PTypeInfo): TTypedSyntax;
var
   arr: TArraySyntax;
begin
   if value.kind = skValue then
      Exit(ConstArrayToSet(TValueSyntax(value), setType));
   arr := value as TArraySyntax;
   if arr.subvalues.children.Count = 0 then
      result := TValueSyntax.Create(TValue.Empty.Cast(setType))
   else if arr.Constant then
      result := ConstArrayTypeToSet(arr, setType)
   else begin
      raise ENotImplemented.Create('Non-constant sets not implemented yet');
   end;
end;

procedure TrsParser.CheckParamList(const expected: IParamList; const passed: TList<TTypedSyntax>);
var
   i, stop, exc: integer;
   info: PTypeInfo;
   param: TTypedSyntax;
begin
   if assigned(expected) then
      exc := expected.count
   else exc := 0;
   stop := min(exc, passed.Count);
   i := 0;
   while i < stop do
   begin
      info := expected[i].&Type.TypeInfo;
      param := passed[i];
      if (info.kind = TypInfo.tkSet) and (param.kind in [skValue, skArray]) then
      begin
         param := ArrayToSet(param, expected[i].&Type.TypeInfo);
         passed.Delete(i);
         passed.Insert(i, param);
      end
      else if (info.kind = tkDynArray) and (param.kind = skArray) and (TArraySyntax(param).constant) then
      begin
         param := ConstArrayFromArray(TArraySyntax(param), info);
         passed.Delete(i);
         passed.Insert(i, param);
      end;
      CheckCompatibleTypes(expected[i].&Type, param.&type, (pfVar in expected[i].flags));
      inc(i);
   end;
   if i < exc then
      raise EParseError.Create('Not enough parameters') //TODO: expand this to deal with default params
   else if i < passed.Count then
      raise EParseError.Create('Too many parameters');
end;

function TrsParser.ParseCall(sym: TProcSymbol; selfSymbol: TVarSymbol): TCallSyntax;
var
   param: TTypedSyntax;
   params: TObjectList<TTypedSyntax>;
begin
   Next; //skip function name
   params := TObjectList<TTypedSyntax>.Create;
   params.OwnsObjects := true;
   try
      if Check(tkOpenParen) and not Check(tkCloseParen) then
         repeat
            param := ReadExpression;
            EvalExpression(param, false);
            params.Add(param)
         until Expect([tkCloseParen, tkComma]) = tkCloseParen;
      if assigned(selfSymbol) then
         params.Insert(0, TVariableSyntax.Create(selfSymbol));
      CheckParamList(sym.paramList, params);
   except
      params.Free;
      raise;
   end;
   result := TCallSyntax.Create(sym, params);
   result.sem := FSemCount;
   if assigned(sym.paramList) and
      (sym.paramList.Count > 0) and
      AnsiSameText(sym.paramList[0].name, 'self') then
         result.SelfSymbol := params.Extract(params.First);
end;

function TrsParser.ParseGoto: TJumpSyntax;
begin
   Next; //expect(tkGoto);
   Verify(tkIdentifier);
   if not (SymbolLookup(FCurrent.GetText).kind = syLabel) then
      raise EParseError.CreateFmt('"%s" is not a label', [FCurrent.origText]);
   result := TJumpSyntax.Create(FCurrent.GetText);
   Next;
end;

function TrsParser.ReadLineOrBlock(const breakTo, continueTo, exitTo: string): TSyntax;
begin
   if FCurrent.kind = tkBegin then
      result := ParseCompoundStatement(breakTo, continueTo, exitTo)
   else result := ParseStatement(breakTo, continueTo, exitTo);
end;

function TrsParser.ParseIf(const breakTo, continueTo, exitTo: string): TBlockSyntax;
var
   cond: TTypedSyntax;
   branch, branch2: string;
begin
   Next; //expect(tkIf)
   cond := ReadExpression(tkThen);
   CheckBooleanCondition(cond);
   result := TBlockSyntax.Create;
   try
      branch := NewBranch;
      result.Add(cond);
      branch2 := NewBranch;
      result.Add(TJumpSyntax.Create(branch2, true));
      if FCurrent.kind <> tkElse then
         result.Add(ReadLineOrBlock(breakTo, continueTo, exitTo));
      if Check(tkElse) and not (FCurrent.kind in [tkEnd, tkSem]) then
      begin
         result.Add(TJumpSyntax.Create(branch, false));
         result.Add(TLabelSyntax.Create(branch2));
         result.Add(ReadLineOrBlock(breakTo, continueTo, exitTo));
      end
      else result.Add(TLabelSyntax.Create(branch2));
      result.Add(TLabelSyntax.Create(branch));
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ReadCaseLabels(value: TVarSymbol): TTypedSyntax;
var
   left, right, sub: TTypedSyntax;
begin
   result := nil;
   try
      repeat
         left := ReadExpression;
         EvalExpression(left, true);
         if Check(tkDotDot) then
         begin
            right := ReadExpression;
            EvalExpression(right, true);
            sub := TBoolOpSyntax.Create(opGreaterEqual, TVariableSyntax.Create(value), left);
            sub := TBoolOpSyntax.Create(opBoolOr, sub,
              TBoolOpSyntax.Create(opLessEqual, TVariableSyntax.Create(value), right));
         end
         else sub := TBoolOpSyntax.Create(opEquals, TVariableSyntax.Create(value), left);
         if result = nil then
            result := sub
         else result := TBoolOpSyntax.Create(opBoolOr, result, sub);
      until not Check(tkComma);
      Expect(tkColon);
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.NewBranch: string;
begin
   inc(FBranchCount);
   result := 'BRANCH*' + intToStr(FBranchCount);
end;

function TrsParser.NextCondName: string;
begin
   inc(FCondCounter);
   result := 'COND*' + IntToStr(FCondCounter);
end;

function TrsParser.ParseCase(const breakTo, continueTo, exitTo: string): TBlockSyntax;
var
   cond: TTypedSyntax;
   action: TBlockSyntax;
   value: TVarSymbol;
   endpoint, nextCase: string;
begin
   Next; //Expect(tkCase);
   cond := ReadExpression(tkOf);
   EvalExpression(cond, false);
   result := TBlockSyntax.Create;
   try
      value := TVarSymbol.Create(NextCondName, cond.&type, FCurrentProc);
      AddSymbol(value.name, value);
      result.Add(TAssignmentSyntax.Create(TVariableSyntax.Create(value), cond));
      nextCase := '';
      endpoint := NewBranch;
      repeat //TODO: optimize CASE codegen
         result.Add(ReadCaseLabels(value));
         action := TBlockSyntax.Create;
         result.Add(action);
         if nextCase <> '' then
            action.Add(TJumpSyntax.Create(nextCase));
         if FCurrent.kind <> tkSem then
            action.add(ReadLineOrBlock(breakTo, continueTo, exitTo));
         if not (FCurrent.kind in [tkElse, tkEnd, tkSem]) then
            raise EParseError.Create('Semicolon expected');
         Check(tkSem);
         action.Add(TJumpSyntax.Create(endpoint));
         if nextcase <> '' then
            result.add(TLabelSyntax.Create(nextcase));
         nextcase := NewBranch;
      until FCurrent.kind in [tkElse, tkEnd];
      result.add(TLabelSyntax.Create(nextcase));
      if Check(tkElse) and not (FCurrent.kind in [tkEnd, tkSem]) then
      begin
         action.Add(TJumpSyntax.Create(nextCase));
         result.Add(ReadLineOrBlock(breakTo, continueTo, exitTo));
         check(tkSem);
      end;
      result.Add(TLabelSyntax.Create(endpoint));
      Expect(tkEnd);
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ParseForToHeader(index: TVarSymbol; out cond, step: TTypedSyntax): TBlockSyntax;
const OPS: array[boolean] of TBoolOpKind = (opLessEqual, opGreaterEqual);
var
   lBound, uBound: TTypedSyntax;
   uSymbol: TVarSymbol;
   down: boolean;
begin
   //TODO: Check to be sure index is an ordinal type
   uBound := nil;
   result := nil;
   lBound := ReadExpression;
   try
      result := TBlockSyntax.Create;
      down := Expect([tkTo, tkDownto]) = tkDownto;
      uBound := ReadExpression(tkDo);
      EvalExpression(lBound, false);
      EvalExpression(uBound, false);
      result.Add(TAssignmentSyntax.Create(TVariableSyntax.Create(index), lBound));

      //Make sure that the upper bound is only evaluated at the top of the loop
      //and not on every trip through it
      if uBound.kind = skValue then
         cond := TBoolOpSyntax.Create(OPS[down], TVariableSyntax.Create(index), uBound)
      else begin
         uSymbol := TVarSymbol.Create(NextCondName, index.&Type, FCurrentProc);
         AddSymbol(uSymbol.name, uSymbol);
         result.Add(TAssignmentSyntax.Create(TVariableSyntax.Create(uSymbol), uBound));
         cond := TBoolOpSyntax.Create(OPS[down], TVariableSyntax.Create(index), TVariableSyntax.Create(uSymbol))
      end;
      CheckBooleanCondition(cond);
      cond.sem := 0;
      TBoolOpSyntax(cond).left.sem := 0;
   except
      lBound.Free;
      uBound.Free;
      result.Free;
      raise;
   end;
   if down then
      step := TUnOpSyntax.Create(opDec, TVariableSyntax.Create(index))
   else step := TUnOpSyntax.Create(opInc, TVariableSyntax.Create(index));
end;

function TrsParser.ParseForInHeader(index: TVarSymbol; out cond, step: TTypedSyntax): TSyntax;
begin
   raise EParseError.Create('FOR-IN is not yet implemented');
end;

function TrsParser.ReadLoopBody(cond, step: TTypedSyntax; const exitTo: string): TBlockSyntax;
var
   breakTo, continueTo: string;
begin
   BreakTo := NewBranch;
   ContinueTo := NewBranch;
   result := TBlockSyntax.Create;
   try
      result.Add(TLabelSyntax.Create(ContinueTo));
      result.Add(cond);
      result.Add(TJumpSyntax.Create(breakTo, true));
      try
         if FCurrent.kind <> tkSem then
            result.Add(ReadLineOrBlock(breakTo, continueTo, exitTo));
      except
         step.Free;
         raise;
      end;
      if assigned(step) then
         result.Add(step);
      result.Add(TJumpSyntax.Create(ContinueTo));
      result.Add(TLabelSyntax.Create(BreakTo));
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ParseFor(const exitTo: string): TBlockSyntax;
var
   index: TSymbol;
   cond, step: TTypedSyntax;
begin
   Next; //Expect(tkFor)
   Verify(tkIdentifier);
   index := SymbolLookup(FCurrent.GetText);
   if not (index is TVarSymbol) then
      raise EParseError.Create('Variable expected');
   Next;
   result := TBlockSyntax.Create;
   try
      case Expect([tkAssign, tkIn]) of
         tkAssign: result.Add(ParseForToHeader(TVarSymbol(index), cond, step));
         tkIn: result.Add(ParseForInHeader(TVarSymbol(index), cond, step));
      end;
      result.Add(ReadLoopBody(cond, step, exitTo));
   except
      result.Free;
      raise;
   end;   
end;

function TrsParser.DoBreak(const where: string): TJumpSyntax;
begin
   if where = '' then
   begin
      if FFinallyDepth > 0 then
         raise EParseError.CreateFmt('Cannot use "%s" to break out of a FINALLY block', [FCurrent.origText])
      else raise EParseError.CreateFmt('"%s" can only be used inside a loop', [FCurrent.origText])
   end
   else result := TJumpSyntax.Create(where);
   Next;
end;

function TrsParser.ParseWhile(const exitTo: string): TBlockSyntax;
var
   cond: TTypedSyntax;
begin
   Next; //Expect(tkWhile)
   cond := ReadExpression(tkDo);
   CheckBooleanCondition(cond);
   cond.sem := 0;
   if cond.kind = skBoolOp then
      TBoolOpSyntax(cond).left.sem := 0
   else if cond.kind = skBinOp then
      TBinOpSyntax(cond).left.sem := 0;
   result := (ReadLoopBody(cond, nil, exitTo));
end;

function TrsParser.ParseRepeat(const exitTo: string): TBlockSyntax;
var
   cond: TTypedSyntax;
   breakTo, continueTo: string;
begin
   Next; //Expect(tkRepeat);
   result := TBlockSyntax.Create;
   try
      breakTo := NewBranch;
      continueTo := NewBranch;
      result.Add(TLabelSyntax.Create(continueTo));
      while check(tkSem) do ;
      while not check(tkUntil) do
      begin
         result.add(ReadLineOrBlock(breakTo, continueTo, exitTo));
         while check(tkSem) do ;
      end;
      cond := ReadExpression;
      EvalExpression(cond, false);
      CheckBooleanCondition(cond);
      result.Add(cond);
      result.Add(TJumpSyntax.Create(continueTo, true));
      result.Add(TLabelSyntax.Create(breakTo));
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.DoExit(const where: string): TSyntax;
var
   sym: TVarSymbol;
   value: TTypedSyntax;
begin
   Next;
   if check(tkOpenParen) then
   begin
      try
         sym := SymbolLookup('RESULT') as TVarSymbol;
      except
         on E: EParseError do
         begin
            E.Message := 'Procedure cannot have a return value';
            raise;
         end;
      end;
      value := ReadExpression;
      EvalExpression(value, false);
      try
         EnsureCompatibleTypes(sym.&Type, value);
      except
         value.Free;
         raise;
      end;
      result := TBlockSyntax.Create;
      TBlockSyntax(result).Add(TAssignmentSyntax.Create(TVariableSyntax.Create(sym), value));
      TBlockSyntax(result).Add(TJumpSyntax.Create(where));
   end
   else result := TJumpSyntax.Create(where);
end;

function TrsParser.CreateTryEndingNavigation(const entrance, breakTo, continueTo,
  exitTo, internalBreak, internalCont, internalExit: string): TBlockSyntax;

  procedure AddNav(const cond, internal: string);
  begin
      if cond <> '' then
      begin
         result.Add(TLabelSyntax.Create(internal));
         result.Add(TTryCallSyntax.Create(entrance, cond));         
      end;  
  end;
  
begin
   result := TBlockSyntax.Create;
   try
      AddNav(breakTo, internalBreak);
      AddNav(continueTo, internalCont);
      AddNav(exitTo, internalExit);
   except
      result.Free;
   end;
end;

function TrsParser.ReadFinallyBlock(const breakTo, continueTo, exitTo,
  internalBreak, internalCont, internalExit: string): TBlockSyntax;
var
   start, finish: string;
begin
   inc(FFinallyDepth);
   result := TBlockSyntax.Create;
   try
      try
         start := NewBranch;
         finish := NewBranch;
         result.Add(TLabelSyntax.Create(start));
         result.Add(TFinallySyntax.Create(finish));
         while not check(tkEnd) do
            result.Add(ReadLineOrBlock('', '', ''));
         result.Add(TSyntax.Create(skReturn));
         result.Add(CreateTryEndingNavigation(start, breakTo, continueTo,
           exitTo, internalBreak, internalCont, internalExit));
         result.Add(TLabelSyntax.Create(finish));
      except
         result.Free;
         raise;
      end;
   finally
      dec(FFinallyDepth);
   end;
end;

function TrsParser.ReadExceptionHandler(const ret, cont: string): TBlockSyntax;
var
   name: string;
   sym: TVarSymbol;
   &type: TTypeSymbol;
begin
   Next; //Expect(tkOn);
   Verify(tkIdentifier);
   if (not TryTypeSymbolLookup(FCurrent.GetText, &type)) then
   begin
      name := FCurrent.origText;
      Next;
      Expect(tkColon);
      &type := TypeSymbolLookup(FCurrent.OrigText);
      sym := TVarSymbol.Create(name, &type, FCurrentProc);
      AddSymbol(UpperCase(name), sym);
   end
   else sym := nil;
   if not ((&type is TClassTypeSymbol) and (TClassTypeSymbol(&type).DescendsFrom(Exception))) then
      raise EParseError.CreateFmt('"%s" is not an exception class', [FCurrent.GetText]);
   Next;
   expect(tkDo);
   result := TBlockSyntax.Create;
   try
      result.Add(TUnOpSyntax.Create(opExis, TTypeRefSyntax.Create(TClassTypeSymbol(&type))));
      result.Add(TJumpSyntax.Create(cont, true));
      if assigned(sym) then
         result.Add(TExceptionLoadSyntax.Create(sym));
      result.Add(ReadLineOrBlock('', '', '')); //TODO: implement breaks somehow
      result.Add(TSyntax.Create(skSwallow));
      result.Add(TJumpSyntax.Create(ret));
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ReadExceptBlock(const breakTo, continueTo, exitTo,
  internalBreak, internalCont, internalExit: string): TBlockSyntax;
var
   start, finish, endHandler, next: string;
   exc: TExceptSyntax;
   elseBlock: TBlockSyntax;
begin
   inc(FExceptDepth);
   result := TBlockSyntax.Create;
   try
      try
         start := NewBranch;
         finish := NewBranch;
         endHandler := NewBranch;
         result.Add(TLabelSyntax.Create(start));
         exc := TExceptSyntax.Create(finish);
         elseBlock := nil;
         result.Add(Exc);
         if FCurrent.kind = tkOn then
         begin
            while Check(tkOn) do
            begin
               next := NewBranch;
               Result.Add(ReadExceptionHandler(endHandler, next));
               Result.Add(TLabelSyntax.Create(next));
            end;
            if check(tkElse) then
               elseBlock := TBlockSyntax.Create;
         end
         else elseBlock := TBlockSyntax.Create;
         if assigned(elseBlock) then
         begin
            while not check(tkEnd) do
               elseBlock.Add(ReadLineOrBlock('', '', '')); //TODO: implement breaks somehow
            elseBlock.Add(TSyntax.Create(skSwallow));
            result.Add(ElseBlock);
         end
         else expect(tkEnd);
         result.Add(TLabelSyntax.Create(endHandler));         
         result.Add(TSyntax.Create(skReturn));
         result.Add(CreateTryEndingNavigation(start, breakTo, continueTo,
           exitTo, internalBreak, internalCont, internalExit));
         result.Add(TLabelSyntax.Create(finish));
      except
         result.Free;
         raise;
      end;
   finally
      dec(FExceptDepth);
   end;
end;

function TrsParser.ParseTry(const breakTo, continueTo, exitTo: string): TBlockSyntax;
var
   internalBreak, internalCont, internalExit: string;
begin
   Next; //Expect(tkTry);
   result := TBlockSyntax.Create;
   try
      if breakTo = '' then
         internalBreak := ''
      else internalBreak := NewBranch;
      if continueTo = '' then
         internalCont := ''
      else internalCont := NewBranch;
      internalExit := NewBranch;
      while not (FCurrent.kind in [tkFinally, tkExcept]) do
         result.Add(ReadLineOrBlock(internalBreak, internalCont, internalExit));
      case Expect([tkFinally, tkExcept]) of
         tkFinally:
         begin
            result.Add(ReadFinallyBlock(breakTo, continueTo, exitTo, internalBreak, internalCont, internalExit));
            result.children.Insert(0, TSyntax.Create(skTryF));
         end;
         tkExcept:
         begin
            result.add(ReadExceptBlock(breakTo, continueTo, exitTo, internalBreak, internalCont, internalExit));
            result.children.Insert(0, TSyntax.Create(skTryE));
         end;
      end;
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.ParseRaise: TRaiseSyntax;
var
   expr: TTypedSyntax;
   &type: TTypeSymbol;
begin
   Next; //expect(tkRaise);
   if (Check(tkSem) or (FCurrent.kind = tkEnd)) then
   begin
      if (FExceptDepth = 0) then
         raise EParseError.Create('Re-raising an exception is only allowed inside an EXCEPT block.')
      else Exit(TRaiseSyntax.Create(nil));
   end;
   expr := ReadExpression;
   if not (expr is TVariableSyntax) then
      raise EParseError.Create('Exception variable expected');
   try
      &type := expr.&type;
      if not ((&type is TClassTypeSymbol) and (TClassTypeSymbol(&type).DescendsFrom(Exception))) then
         raise EParseError.Create('RAISE can only be used with an exception');
   except
      expr.Free;
      raise
   end;
   result := TRaiseSyntax.Create(TVariableSyntax(expr));
end;
   
function TrsParser.ParseStatement(const breakTo, continueTo, exitTo: string): TSyntax;
begin
   case FCurrent.kind of
      tkIdentifier:
      begin
         result := ReadIdentifier;
         if (result is TTypedSyntax) and Check(tkAssign) then
            result := ParseAssignmentExpr(TTypedSyntax(result));
      end;
      tkGoto: result := ParseGoto;
      tkBegin: result := ParseCompoundStatement(breakTo, continueTo, exitTo);
      tkIf: result := ParseIf(breakTo, continueTo, exitTo);
      tkCase: result := ParseCase(breakTo, continueTo, exitTo);
      tkFor: result := ParseFor(exitTo);
      tkWhile: result := ParseWhile(exitTo);
      tkRepeat: result := ParseRepeat(exitTo);
      tkTry: result := ParseTry(breakTo, continueTo, exitTo);
      tkRaise: result := ParseRaise;
      tkBreak: result := DoBreak(breakTo);
      tkContinue: result := DoBreak(continueTo);
      tkExit: result := DoExit(exitTo);
      else raise EParseError.Create('Statement expected');
   end;
end;

function TrsParser.ParseStatementList(const breakTo, continueTo, exitTo: string): TSyntaxList;
const STATEMENTS = [tkIdentifier, tkGoto, tkBegin, tkIf, tkCase, tkFor, tkWhile,
                    tkRepeat, tkTry, tkRaise, tkSem, tkBreak, tkContinue, tkExit];
var
   new: TSyntax;
   isIf: boolean;
begin
   result := TSyntaxList.Create(false);
   try
      if FCurrent.kind in STATEMENTS then
      repeat
         while Check(tkSem) do ; //skip any number of blank sem "statements"
         isIf := FCurrent.kind = tkIf;
         new := ParseStatement(breakTo, continueTo, exitTo);
         result.Add(new);
         if (FCurrent.kind = tkSem) then
            Next
         else if (new.kind <> skLabel) and not isIf then
            Break;
      until Check(tkEnd);
   except
      result.OwnsObjects := true;
      result.Free;
      raise;
   end;
end;

function TrsParser.ParseCompoundStatement(const breakTo, continueTo, exitTo: string): TBlockSyntax;
var
   list: TSyntaxList;
   new: TSyntax;
begin
   Next; //Expect(tkBegin)
   result := TBlockSyntax.Create;
   try
      list := ParseStatementList(breakTo, continueTo, exitTo);
      try
         if list.count = 0 then
            Expect(tkEnd);
         for new in list do
            result.Add(new);
      finally
         list.Free;
      end;
   except
      result.Free;
      raise;
   end;
end;

procedure TrsParser.ReadProcBody(proc: TProcSymbol);
var
   exitTo: string;
begin
   exitTo := NewBranch;
   proc.syntax := ParseCompoundStatement('', '', exitTo);
   expect(tkSem);
   proc.syntax.Add(TLabelSyntax.Create(exitTo));
   proc.syntax.Add(TSyntax.Create(skReturn));
end;

procedure TrsParser.ReadImplProc;
var
   info: TProcInfo;
   proc: TProcSymbol;
   list: TAttrList;
begin
   list := ReadAttributeList;
   list.Free; //TODO: Actually apply the attributes instead of discarding them
   info := ReadProcDecl(false);
   if info.forwarded then
   begin
      self.AddProc(info.name, info.retval, info.params, true);
      Exit;
   end;
   if assigned(info.parent) then
      proc := (info.parent as TClassTypeSymbol).DefineMethod(info.name)
   else proc := self.AddProc(info.name, info.retval,info.params, false);
   BeginProc(proc);
   ReadProcLocals(proc);
   ReadProcBody(proc);
   EndProc;
end;

procedure TrsParser.ParseImplDeclarations;
begin
   FScopeStack.Push(FPrivateSymbols);
   while FCurrent.kind in [tkConst, tkType, tkVar, tkProcedure, tkFunction, tkOpenBracket] do
      case FCurrent.kind of
         tkConst: ParseBlock(AddConst);
         tkType: ParseBlock(AddType);
         tkVar: ParseBlock(AddVar);
         tkProcedure, tkFunction, tkOpenBracket: ReadImplProc;
      end;
end;

procedure TrsParser.ParseImplBody;
begin
   if check(tkUses) then
      ParseUses;
   ParseImplDeclarations;
end;

procedure TrsParser.ParseImplementation;
begin
   Expect(tkImplementation);
   FInImplementation := true;
   ParseImplBody;
end;

function TrsParser.ParseUnit(const ExpectedName: string): TUnitSymbol;
begin
   Verify(tkIdentifier);
   if (ExpectedName <> '') and (not AnsiSameText(ExpectedName, FCurrent.origText)) then
      raise EParseError.CreateFmt('Unit name "%s" does not match filename "%s"', [FCurrent.origText, ExpectedName]);
   result := TUnitSymbol.Create(FCurrent.origText, FPublicSymbols, FPrivateSymbols);
   FCurrentUnit := result;
   try
      Next;
      Expect(tkSem);
      if FCurrent.kind = tkInterface then
      begin
         ParseInterface;
         ParseImplementation;
      end
      else ParseImplBody;
      Expect(tkEnd);
      FEofExpected := true;
      Expect(tkDot);
   except
      result.Free;
      raise;
   end;
end;

function TrsParser.DoParse(const ExpectedName: string): TUnitSymbol;
begin
//OutputDebugString('SAMPLING ON');
   try
      FSemCount := 0;
      case Expect([tkProgram, tkUnit]) of
         tkProgram: raise EParseError.Create('Program parsing is not supported yet'); //TODO: support this
         tkUnit: result := ParseUnit(ExpectedName);
         else raise EParseError.CreateFmt('Unknown token: %s', [FCurrent.origText]);
      end;
   except
      on E: Exception do
      begin
         E.Message := format('Parse error: [%s] at line %d, column %d', [E.Message, FCurrent.row, FCurrent.Column]);
         raise;
      end;
   end;
//OutputDebugString('SAMPLING OFF');
end;

procedure TrsParser.Reset;
begin
   FPrivateSymbols.clear;
   FPublicSymbols.Clear;
end;

function TrsParser.Parse(const input: TTokenQueue; const ExpectedName: string): TUnitSymbol;
begin
   FreeAndNil(FLexer);
   setQueue(input);
   try
      result := DoParse(expectedName);
   finally
      FScopeStack.Clear;
   end;
end;

function TrsParser.Parse(const input, ExpectedName: string): TUnitSymbol;
begin
   FreeAndNil(FQueue);
   FLexer := TRsLexer.Create;
   FLexer.Load(input);
   Next;
   try
      result := DoParse(expectedName);
   finally
      FScopeStack.Clear;
   end;
end;

function TrsParser.ParseExternalHeader(const input: TTokenQueue;
  parent: TUnitSymbol; add: boolean): TProcSymbol;
begin
   setQueue(input);
   FEofExpected := true;
   FCurrentUnit := parent;
   FScopeStack.Push(parent.privates);
   try
      result := ReadForwardProc(add);
   finally
      FScopeStack.Pop;
   end;
end;

end.
