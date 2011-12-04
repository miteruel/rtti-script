unit rsDefs;

interface
uses
   SysUtils, Classes, TypInfo, RTTI, Generics.Collections;

type
   TTokenKind = (
      tkNone,

      //symbols
      tkIdentifier, tkAssign, tkColon, tkSem, tkEquals, tkNotEqual,
      tkLessThan, tkLessEqual, tkGreaterThan, tkGreaterEqual, tkPlus, tkMinus,
      tkTimes, tkDivide, tkComma, tkDot, tkDotDot, tkOpenParen, tkCloseParen,
      tkOpenBracket, tkCloseBracket, tkInt, tkFloat, tkChar, tkString,

      //keywords
      tkAnd, tkArray, tkAs, tkBegin, tkBreak, tkCase, tkClass, tkConst,
      tkContinue, tkConstructor, tkDefault, tkDestructor, tkDiv, tkDo, tkDownTo,
      tkElse, tkEnd, tkExcept, tkExit, {tkExternal,} tkFinally, tkFor,
      tkForward, tkFunction, tkGoto, tkIf, tkIn, tkImplementation, tkInherited,
      tkInterface, tkIs, tkLabel, tkMod, tkNil, tkNot, tkObject, tkOf, tkOn,
      tkOr, tkOut, tkOverride, tkPrivate, tkProcedure, tkProperty, tkProtected,
      tkPublic, tkProgram, tkRaise, tkRepeat, tkRecord, tkSet, tkShl, tkShr,
      tkThen, tkTo, tkTry, tkType, tkUnit, tkUntil, tkUses, tkVar, tkVirtual,
      tkWhile, tkXor,

      tkEof, tkError);

   TToken = record
   private
      FKind: TTokenKind;
//      FText: string;
//      FOrigText: string;
      FRow: integer;
      FColumn: integer;
      FStart: PChar;
      FLength: integer;
      function GetOrigText: string;
   public
      constructor Create(kind: TTokenKind; row, column, length: integer; start: PChar);
      function GetText: string;
      property kind: TTokenKind read FKind write FKind;
//      property text: string read GetText;
      property origText: string read GetOrigText;
      property row: integer read FRow;
      property Column: integer read FColumn;
   end;

   TKeywordPair = record
      keyword: string;
      token: TTokenKind;
   end;

   TSyntaxKind = (skValue, skBinOp, skUnOp, skBoolOp, skVariable, skType, skCall,
                  skDeref, skReturn, skLabel, skJump, skBlock, skAssign, skTryF,
                  skTryE, skFinally, skExcept, skELoad, skTryCall, skRaise,
                  skSwallow, skElem, skDot, skCast, skArrayProp, skArray);
   TBinOpKind = (opPlus, opMinus, opMult, opDiv, opDivide, opMod, opAnd, opOr,
                  opXor, opShl, opShr, opAs);
   TBoolOpKind = (opGreaterEqual, opLessEqual, opGreaterThan, opLessThan, opEquals,
                  opNotEqual, opIn, opIs, opBoolAnd, opBoolOr, opBoolXor);
   TUnOpKind = (opNeg, opNot, opInc, opDec, opExis);
   TSymbolKind = (syConst, syVariable, syLabel, syProp, syProc, syType, syEnum, syUnit);

   TBlockSyntax = class;

   TSymbol = class
   private
      function GetFullName: string;
   protected
      FName: string;
      FFullName: string;
      FKind: TSymbolKind;
      //in case any descendants need interfaces
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;

      function ResolveFullName: string; virtual;
      constructor Create(const name: string; kind: TSymbolKind);
   public
      property name: string read FName;
      property fullName: string read GetFullName;
      property kind: TSymbolKind read FKind;
   end;

   ISymbolTable = interface
      function GetItem(const Key: string): TSymbol;
      procedure SetItem(const Key: string; const Value: TSymbol);
      procedure Add(const Key: string; const Value: TSymbol);
      function Values: TEnumerable<TSymbol>;
      function TryGetValue(const Key: string; out Value: TSymbol): Boolean;
      function ContainsKey(const Key: string): Boolean;
      function ExtractPair(const Key: string): TPair<string,TSymbol>;
      function KeysWhereValue(filter: TPredicate<TSymbol>): TStringList;
      function Where(filter: TFunc<string, TSymbol, boolean>): TList<TPair<string, TSymbol>>;
      procedure Clear;
      function Count: integer;
      property Items[const Key: string]: TSymbol read GetItem write SetItem; default;
   end;

   TConstSymbol = class(TSymbol)
   private
      FValue: TValue;
   public
      constructor Create(const name: string; const value: TValue);
      property value: TValue read FValue;
   end;

   TTypeSymbol = class(TSymbol)
   private
      FTypeInfo: PTypeInfo;
   protected
      function GetTypeInfo: PTypeInfo; virtual;
   public
      constructor Create(const name: string);
      function CanDerefDot: boolean; virtual;
      function CanDerefArray: boolean; virtual;
      function GetSymbolTable: ISymbolTable; virtual;
      function HasTypeInfo: boolean;
      property TypeInfo: PTypeInfo read GetTypeInfo;
   end;

   TVarSymbol = class(TSymbol)
   private
      FType: TTypeSymbol;
      FIndex: integer;
      FParent: TSymbol;
   protected
      function ResolveFullName: string; override;
   public
      constructor Create(const name: string; aType: TTypeSymbol; parent: TSymbol);
      property &Type: TTypeSymbol read FType;
      property index: integer read FIndex;
      property parent: TSymbol read FParent;
   end;

   TFieldSymbol = class(TVarSymbol)
   private
      FVisibility: TMemberVisibility;
   public
      constructor Create(const name: string; aType: TTypeSymbol; visibility: TMemberVisibility;
        parent: TSymbol);
      property visibility: TMemberVisibility read FVisibility;
   end;

   TParamSymbol = class(TVarSymbol)
   private
      FFlags: TParamFlags;
   public
      constructor Create(const name: string; aType: TTypeSymbol; flags: TParamFlags);
      destructor Destroy; override;
      property flags: TParamFlags read FFlags;
   end;

   IParamList = interface
      procedure Insert(Index: Integer; const Value: TParamSymbol);
      function Count: integer;
      function Last: TParamSymbol;
      procedure SetOwnsObjects(value: boolean);
      function GetItem(Index: Integer): TParamSymbol;
      procedure SetItem(Index: Integer; const Value: TParamSymbol);
      function GetEnumerator: TEnumerator<TParamSymbol>;
      function Add(const Value: TParamSymbol): Integer;
      procedure AddRange(Collection: IParamList);

      property OwnsObjects: boolean write SetOwnsObjects;
      property Items[Index: Integer]: TParamSymbol read GetItem write SetItem; default;
   end;

   TPropSymbol = class(TVarSymbol)
   private
      FVisibility: TMemberVisibility;
      FClassScope: boolean;
      FParamList: IParamList;
      FReadSpec: TSymbol;
      FWriteSpec: TSymbol;
      FPropInfo: PPropInfo;
      procedure VerifyReadWriteCompat;
   public
      constructor Create(const name: string; &type: TTypeSymbol; visibility: TMemberVisibility;
        classScope: boolean; const paramList: IParamList; readSpec, writeSpec: TSymbol;
        parent: TSymbol); overload;
      constructor Create(const name: string; visibility: TMemberVisibility;
        info: PPropInfo; parent: TSymbol); overload;
      property visibility: TMemberVisibility read FVisibility;
      property classScope: boolean read FClassScope;
      property paramList: IParamList read FParamList;
      property readSpec: TSymbol read FReadSpec;
      property writeSpec: TSymbol read FWriteSpec;
   end;

   TProcSymbol = class(TVarSymbol)
   private
      FVisibility: TMemberVisibility;
      FClassScope: boolean;
      FParamList: IParamList;
      FForwarded: boolean;
      FSymbolTable: ISymbolTable;
      FSyntax: TBlockSyntax;
      procedure SetSyntax(const Value: TBlockSyntax);
   public
      constructor Create(const name: string; &type: TTypeSymbol; visibility: TMemberVisibility;
        classScope: boolean; const paramList: IParamList; forwarded: boolean; parent: TSymbol);
      destructor Destroy; override;
      procedure Define;
      property visibility: TMemberVisibility read FVisibility;
      property classScope: boolean read FClassScope;
      property paramList: IParamList read FParamList;
      property forwarded: boolean read FForwarded;
      property symbolTable: ISymbolTable read FSymbolTable;
      property syntax: TBlockSyntax read FSyntax write SetSyntax;
   end;

   TClassTypeSymbol = class(TTypeSymbol)
   private
      FForward: boolean;
      FParentType: TClassTypeSymbol;
      FSymbols: ISymbolTable;
      FExportSymbols: ISymbolTable;
      FMetaclass: TClass;
      procedure SetParent(const Value: TClassTypeSymbol);
      procedure CheckUnique(const name, kind: string);
      procedure AddSymbol(const name: string; visibility: TMemberVisibility;
        value: TVarSymbol; var counter: integer);
      procedure SetMetaclass(const Value: TClass);
   protected
      FFieldCount: integer;
      FPropCount: integer;
      FMethodCount: integer;
      FDefaultProperty: TPropSymbol;
   public
      constructor Create(const name: string);
      constructor CreateFwd(const name: string);
      procedure Define;
      procedure AddField(const name: string; &type: TTypeSymbol; visibility: TMemberVisibility);
      procedure AddProp(const name: string; &type: TTypeSymbol; visibility: TMemberVisibility;
        classScope: boolean; paramList: IParamList; readSpec, writeSpec: TSymbol); overload;
      procedure AddProp(info: TRttiInstanceProperty); overload;
      procedure AddMethod(const name: string; &type: TTypeSymbol; visibility: TMemberVisibility;
        classScope: boolean; paramList: IParamList);
      function DefineMethod(const name: string): TProcSymbol;
      function SymbolLookup(const name: string): TSymbol;
      function DescendsFrom(ancestor: TClassTypeSymbol): boolean; overload;
      function DescendsFrom(metaclass: TClass): boolean; overload;
      function CanDerefDot: boolean; override;
      function CanDerefArray: boolean; override;
      function GetSymbolTable: ISymbolTable; override;

      property forwarded: boolean read FForward;
      property parent: TClassTypeSymbol read FParentType write SetParent;
      property metaclass: TClass read FMetaclass write SetMetaclass;
      property fieldCount: integer read FFieldCount;
      property propCount: integer read FPropCount;
      property methodCount: integer read FMethodCount;
      property defaultProperty: TPropSymbol read FDefaultProperty;
   end;

   TArrayTypeSymbol = class(TTypeSymbol)
   private
      FBaseType: TTypeSymbol;
      FUBound: integer;
      FLBound: integer;
   public
      constructor Create(lBound, uBound: integer; baseType: TTypeSymbol);
      function CanDerefArray: boolean; override;
      property lBound: integer read FLBound;
      property ubound: integer read FUBound;
      property baseType: TTypeSymbol read FBaseType;
   end;

   TProcTypeSymbol = class(TTypeSymbol)
   private
      FParams: IParamList;
      FRetval: TTypeSymbol;
      FMethod: boolean;
   public
      constructor Create(const name: string; retval: TTypeSymbol;
        const params: IParamList; isMethod: boolean);
      property retval: TTypeSymbol read FRetval;
      property params: IParamList read FParams;
      property isMethod: boolean read FMethod;
   end;

   TLabelSymbol = class(TSymbol)
   private
      FUsed: boolean;
   public
      constructor Create(const name: string);
      procedure Use;
      property Used: boolean read FUsed;
   end;

   TUnitSymbol = class(TSymbol)
   private
      FPublicTable: ISymbolTable;
      FPrivateTable: ISymbolTable;
      FExternal: boolean;
   public
      constructor Create(name: string; const publicTable, privateTable: ISymbolTable; isExternal: boolean = false);
      destructor Destroy; override;
      function SymbolLookup(const name: string): TSymbol;
      function Procs: TList<TProcSymbol>;
      property publics: ISymbolTable read FPublicTable;
      property privates: ISymbolTable read FPrivateTable;
      property IsExternal: boolean read FExternal;
   end;

   TSyntax = class
   private
      FKind: TSyntaxKind;
   public
      constructor Create(kind: TSyntaxKind);
      property kind: TSyntaxKind read FKind;
   end;

   TTypedSyntax = class(TSyntax)
   private
      FSem: integer;
   protected
      function GetType: TTypeSymbol; virtual; abstract;
   public
      function GetSelfValue: TVarSymbol; virtual;
      property &type: TTypeSymbol read GetType;
      property sem: integer read FSem write FSem;
   end;

   TValueSyntax = class(TTypedSyntax)
   private
      FValue: TValue;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(const value: TValue); overload;
      constructor Create(const value: TToken); overload;
      constructor Create(); overload;
      property value: TValue read FValue;
   end;

   TBinOpSyntax = class(TTypedSyntax)
   private
      FOp: TBinOpKind;
      FRight: TTypedSyntax;
      FLeft: TTypedSyntax;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(op: TBinOpKind; left, right: TTypedSyntax);
      destructor Destroy; override;
      procedure ResetLeft(value: TTypedSyntax);
      procedure ResetRight(value: TTypedSyntax);
      property op: TBinOpKind read FOp;
      property left: TTypedSyntax read FLeft;
      property right: TTypedSyntax read FRight;
   end;

   TBoolOpSyntax = class(TTypedSyntax)
   private
      FOp: TBoolOpKind;
      FRight: TTypedSyntax;
      FLeft: TTypedSyntax;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(op: TBoolOpKind; left, right: TTypedSyntax);
      destructor Destroy; override;
      property op: TBoolOpKind read FOp;
      property left: TTypedSyntax read FLeft;
      property right: TTypedSyntax read FRight;
   end;

   TUnOpSyntax = class(TTypedSyntax)
   private
      FOp: TUnOpKind;
      FSub: TTypedSyntax;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(op: TUnOpKind; sub: TTypedSyntax);
      destructor Destroy; override;
      procedure resetSub(value: TTypedSyntax);
      property op: TUnOpKind read FOp;
      property sub: TTypedSyntax read FSub;
   end;

   TVariableSyntax = class(TTypedSyntax)
   private
      FSymbol: TVarSymbol;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(symbol: TVarSymbol);
      function GetSelfValue: TVarSymbol; override;
      property symbol: TVarSymbol read FSymbol;
   end;

   TTypeRefSyntax = class(TTypedSyntax)
   private
      FClass: TClassTypeSymbol;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(base: TClassTypeSymbol);
      property &class: TClassTypeSymbol read FClass;
   end;

   TSyntaxList = class(TObjectList<TSyntax>);

   TBlockSyntax = class(TSyntax)
   private
      FChildren: TSyntaxList;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Add(value: TSyntax);
      property children: TSyntaxList read FChildren;
   end;

   TAssignmentSyntax = class(TSyntax)
   private
      FRValue: TTypedSyntax;
      FLValue: TTypedSyntax;
   public
      constructor Create(lValue, rValue: TTypedSyntax);
      destructor Destroy; override;
      property lValue: TTypedSyntax read FLValue;
      property rValue: TTypedSyntax read fRValue;
   end;

   TLabelSyntax = class(TSyntax)
   private
      FName: string;
   public
      constructor Create(const name: string);
      property name: string read FName;
   end;

   TJumpSyntax = class(TSyntax)
   private
      FName: string;
      FConditional: boolean;
   public
      constructor Create(const name: string; conditional: boolean = false);
      property name: string read FName;
      property conditional: boolean read FConditional;
      //conditional jump jumps if BR = *false*, because that means that the
      //check failed and we're jumping over the success case
   end;

   TCallSyntax = class(TTypedSyntax)
   private
      FProc: TProcSymbol;
      FParams: TList<TTypedSyntax>;
      FSelfSymbol: TTypedSyntax;
      procedure SetSelf(const Value: TTypedSyntax);
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(proc: TProcSymbol; const params: TList<TTypedSyntax>);
      destructor Destroy; override;
      function GetSelfValue: TVarSymbol; override;
      property proc: TProcSymbol read FProc;
      property params: TList<TTypedSyntax> read FParams;
      property SelfSymbol: TTypedSyntax read FSelfSymbol write SetSelf;
   end;

   TDotSyntax = class(TTypedSyntax)
   private
      FLeft: TTypedSyntax;
      FRight: TTypedSyntax;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(left, right: TTypedSyntax);
      destructor Destroy; override;
      function GetSelfValue: TVarSymbol; override;
      property left: TTypedSyntax read FLeft;
      property right: TTypedSyntax read FRight;
   end;

   TArrayPropSyntax = class(TTypedSyntax)
   private
      FSymbol: TVarSymbol;
      FBase: TDotSyntax;
      FParams: TList<TTypedSyntax>;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(base: TDotSyntax; subscript: TTypedSyntax);
      destructor Destroy; override;
      function GetSelfValue: TVarSymbol; override;
      property base: TDotSyntax read FBase;
      property params: TList<TTypedSyntax> read FParams;
   end;

   TTryCallSyntax = class(TSyntax)
   private
      FJump: string;
      FRet: string;
   public
      constructor Create(const jump, ret: string);
      property jump: string read FJump;
      property ret: string read FRet;
   end;

   TFinallySyntax = class(TSyntax)
   private
      FRet: string;
   public
      constructor Create(const ret: string);
      property ret: string read FRet;
   end;

   TExceptBlockData = record
      cls: TClassTypeSymbol;
      sym: TVarSymbol;
      block: TBlockSyntax;
   end;

   TExceptSyntax = class(TSyntax)
   private
      FRet: string;
   public
      constructor Create(const ret: string);
      property ret: string read FRet;
   end;

   TExceptionLoadSyntax = class(TSyntax)
   private
      FSymbol: TVarSymbol;
   public
      constructor Create(sym: TVarSymbol);
      property symbol: TVarSymbol read FSymbol;
   end;

   TRaiseSyntax = class(TSyntax)
   private
      FObject: TVariableSyntax;
   public
      constructor Create(obj: TVariableSyntax);
      property obj: TVariableSyntax read FObject;
   end;

   TElemSyntax = class(TTypedSyntax)
   private
      FSymbol: TVarSymbol;
      FLeft: TTypedSyntax;
      FRight: TTypedSyntax;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(left, right: TTypedSyntax);
      destructor Destroy; override;
      function GetSelfValue: TVarSymbol; override;
      property Left: TTypedSyntax read FLeft;
      property Right: TTypedSyntax read FRight;
      property symbol: TVarSymbol read FSymbol;
   end;

   TCastSyntax = class(TTypedSyntax)
   private
      FBase: TTypedSyntax;
      FType: TTypeSymbol;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create(base: TTypedSyntax; &type: TTypeSymbol);
      destructor Destroy; override;
      property Base: TTypedSyntax read FBase;
   end;

   TArraySyntax = class(TTypedSyntax)
   private
      FChildren: TBlockSyntax;
      FType: TTypeSymbol;
    function IsConstant: boolean;
   protected
      function GetType: TTypeSymbol; override;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Add(value: TTypedSyntax);
      procedure Finalize;
      property subvalues: TBlockSyntax read FChildren;
      property constant: boolean read IsConstant;
   end;

   EParseError = class(Exception);
   TTokenQueue = TQueue<TToken>;
   TBlockList = class(TObjectList<TBlockSyntax>);

function GetKeyword(const name: string): TTokenKind;
function CreateSymbolTable(ownsObjects: boolean): ISymbolTable;

function EmptyParamList(owns: boolean = true): IParamList;

procedure AddNativeType(nativeType: TRttiType; new: TTypeSymbol);
function NativeTypeDefined(value: TRttiType): boolean;
function TypeOfNativeType(value: PTypeInfo): TTypeSymbol;
function TypeOfRttiType(value: TRttiType): TTypeSymbol;
function TypeOfValue(const value: TValue): TTypeSymbol;
function BooleanType: TTypeSymbol;

function FindNativeType(const name: string): TTypeSymbol;
function MakeArrayPropType(base: TTypeSymbol): TTypeSymbol;
procedure AddArrayType(value: TArrayTypeSymbol);

procedure CheckCompatibleTypes(left, right: TTypeSymbol; exact: boolean = false);
procedure EnsureCompatibleTypes(left: TTypeSymbol; var right: TTypedSyntax; exact: boolean = false);

implementation
uses
   SyncObjs,
   rsEnex;

var
   ParserFormatSettings: TFormatSettings;

//minor hack
type real = extended;

function CheckCompatibleArrayTypes(left, right: TTypeSymbol): TTypeSymbol; forward;
function MakeArrayType(base: TTypeSymbol): TTypeSymbol; forward;

{ TToken }

constructor TToken.Create(kind: TTokenKind; row, column, length: integer; start: PChar);
begin
   FKind := kind;
   FRow := row;
   FColumn := column;
   FStart := start;
   FLength := length;
end;

function TToken.GetOrigText: string;
begin
   SetLength(result, FLength);
   Move(FStart^, result[1], FLength * sizeof(char));
end;

function TToken.GetText: string;
var
   otext: string;
begin
   oText := GetOrigText;
   case kind of
      tkIdentifier: result := UpperCase(oText);
      tkChar: result := oText;
      tkString: result := AnsiDequotedStr(oText, '''');
      else result := oText;
   end;
end;

{ TSyntax }

constructor TSyntax.Create(kind: TSyntaxKind);
begin
   FKind := kind;
end;

{ TValueSyntax }

constructor TValueSyntax.Create(const value: TValue);
begin
   inherited create(skValue);
   FValue := value;
end;

constructor TValueSyntax.Create(const value: TToken);
begin
   case value.kind of
      tkInt: Create(StrToInt(value.GetText));
      tkFloat: Create(TValue.From<Real>(StrToFloat(value.GetText, ParserFormatSettings)));
      tkChar, tkString: Create(value.GetText);
      else assert(false);
   end;
end;

constructor TValueSyntax.Create;
begin
   Create(TValue.Empty);
end;

function TValueSyntax.GetType: TTypeSymbol;
begin
   result := TypeOfValue(FValue);
end;

{ TBinOpSyntax }

constructor TBinOpSyntax.Create(op: TBinOpKind; left, right: TTypedSyntax);
begin
   if (left.kind = skValue) and (right.kind <> skValue) and (op in [opPlus, opMult, opAnd, opOr, opXor])
      and (left.&type.TypeInfo.Kind <> tkUString) then
      Create(op, right, left)
   else begin
      inherited Create(skBinOp);
      FOp := op;
      FLeft := left;
      FRight := right;
   end;
end;

destructor TBinOpSyntax.Destroy;
begin
   FLeft.Free;
   FRight.Free;
   inherited Destroy;
end;

function TBinOpSyntax.GetType: TTypeSymbol;
begin
   result := FLeft.GetType; //TODO: check for left: integer, right: float case
end;

procedure TBinOpSyntax.ResetLeft(value: TTypedSyntax);
begin
   FLeft := value;
end;

procedure TBinOpSyntax.ResetRight(value: TTypedSyntax);
begin
   FRight := value;
end;

{ TBoolOpSyntax }

function InvertBoolOp(op: TBoolOpKind): TBoolOpKind;
begin
   case op of
      opGreaterEqual: result := opLessThan;
      opLessEqual: result := opGreaterThan;
      opGreaterThan: result := opLessEqual;
      opLessThan: result := opGreaterEqual;
      opEquals: result := opNotEqual;
      opNotEqual: result := opEquals;
      opBoolXor: result := opBoolXor;
      else raise EParseError.Create('Invalid boolean operation');
   end;
end;

constructor TBoolOpSyntax.Create(op: TBoolOpKind; left, right: TTypedSyntax);
begin
   if (left.kind = skValue) and (right.kind <> skValue) and
      (op in [opGreaterEqual, opLessEqual, opGreaterThan, opLessThan, opEquals, opNotEqual, opBoolXor]) then
      Create(InvertBoolOp(op), right, left)
   else begin
      inherited Create(skBoolOp);
      FOp := op;
      FLeft := left;
      FRight := right;
   end;
end;

destructor TBoolOpSyntax.Destroy;
begin
   FLeft.Free;
   FRight.Free;
   inherited Destroy;
end;

function TBoolOpSyntax.GetType: TTypeSymbol;
begin
   result := BooleanType;
end;

{ TSymbol }

constructor TSymbol.Create(const name: string; kind: TSymbolKind);
begin
   FName := name;
   FKind := kind;
end;

function TSymbol.GetFullName: string;
begin
   if FFullName = '' then
      FFullName := resolveFullName;
   result := FFullName
end;

function TSymbol.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSymbol.ResolveFullName: string;
begin
   result := FName;
end;

function TSymbol._AddRef: Integer;
begin
   result := -1;
end;

function TSymbol._Release: Integer;
begin
   result := -1;
end;

{ TConstSymbol }

constructor TConstSymbol.Create(const name: string; const value: TValue);
begin
   if value.Kind = tkEnumeration then
      inherited Create(name, syEnum)
   else inherited Create(name, syConst);
   FValue := value;
end;

{ TVarSymbol }

constructor TVarSymbol.Create(const name: string; aType: TTypeSymbol; parent: TSymbol);
begin
   inherited Create(name, syVariable);
   FName := name;
   FType := aType;
   FParent := parent;
end;

function TVarSymbol.ResolveFullName: string;
begin
   if assigned(FParent) then
      result := FParent.fullName + '.' + UpperCase(FName)
   else result := UpperCase(FName);
end;

{ TVariableSyntax }

constructor TVariableSyntax.Create(symbol: TVarSymbol);
begin
   assert(assigned(symbol));
   inherited Create(skVariable);
   FSymbol := symbol;
end;

function TVariableSyntax.GetSelfValue: TVarSymbol;
begin
   if FSymbol.&Type is TClassTypeSymbol then
      result := FSymbol
{   else if FSymbol.parent is TClassTypeSymbol then
      result := FSymbol.parent }
   else result := nil;
end;

function TVariableSyntax.GetType: TTypeSymbol;
begin
   result := FSymbol.FType;
end;

{ TUnitSymbol }

constructor TUnitSymbol.Create(name: string; const publicTable, privateTable: ISymbolTable; isExternal: boolean = false);
begin
   inherited Create(name, syUnit);
   FPublicTable := publicTable;
   FPrivateTable := privateTable;
   FExternal := isExternal;
end;

destructor TUnitSymbol.Destroy;
begin
   inherited;
end;

function TUnitSymbol.Procs: TList<TProcSymbol>;
var
   cls: TClassTypeSymbol;
   classes: TList<TClassTypeSymbol>;
   exportSymbols: TList<TProcSymbol>;
begin
   result := TEnex.Select<TSymbol, TProcSymbol>(self.privates.Values);
   classes := TEnex.Select<TSymbol, TClassTypeSymbol>(self.privates.Values);
   try
      for cls in classes do
      begin
         exportSymbols := TEnex.Select<TSymbol, TProcSymbol>(cls.FExportSymbols.Values);
         try
            result.addRange(exportSymbols);
         finally
            exportSymbols.Free;
         end;
      end;
   finally
      classes.Free;
   end;
end;

function TUnitSymbol.SymbolLookup(const name: string): TSymbol;
begin
   if not FPublicTable.TryGetValue(name, result) then
      raise EParseError.CreateFmt('Symbol %s not found', [name])
end;

{ TUnOpSyntax }

constructor TUnOpSyntax.Create(op: TUnOpKind; sub: TTypedSyntax);
begin
   inherited Create(skUnOp);
   FOp := op;
   FSub := sub;
end;

destructor TUnOpSyntax.Destroy;
begin
   FSub.Free;
   inherited Destroy;
end;

function TUnOpSyntax.GetType: TTypeSymbol;
begin
   result := FSub.GetType;
end;

procedure TUnOpSyntax.resetSub(value: TTypedSyntax);
begin
   FSub := value;
end;

{ TTypeSymbol }

function TTypeSymbol.CanDerefArray: boolean;
begin
   result := false;
end;

function TTypeSymbol.CanDerefDot: boolean;
begin
   result := false;
end;

constructor TTypeSymbol.Create(const name: string);
begin
   inherited Create(name, syType);
end;

function TTypeSymbol.GetSymbolTable: ISymbolTable;
begin
   raise EParseError.CreateFmt('Internal error: no symbol table available for type "%s"', [FName]);
end;

function TTypeSymbol.GetTypeInfo: PTypeInfo;
begin
   if FTypeInfo = nil then
      raise EInsufficientRtti.CreateFmt('TypeInfo has not been generated for %s yet.', [FName])
   else result := FTypeInfo;
end;

function TTypeSymbol.HasTypeInfo: boolean;
begin
   result := assigned(FTypeInfo);
end;

{ TClassTypeSymbol }

constructor TClassTypeSymbol.Create(const name: string);
begin
   inherited Create(name);
   FExportSymbols := CreateSymbolTable(false);
   FSymbols := CreateSymbolTable(true);
end;

constructor TClassTypeSymbol.CreateFwd(const name: string);
begin
   Create(name);
   FForward := true;
end;

function TClassTypeSymbol.CanDerefArray: boolean;
begin
   result := assigned(FDefaultProperty);
end;

function TClassTypeSymbol.CanDerefDot: boolean;
begin
   result := true;
end;

procedure TClassTypeSymbol.CheckUnique(const name, kind: string);
begin
   if FSymbols.ContainsKey(name) then
      raise EParseError.CreateFmt('Duplicate %s name %s', [kind, name]);
end;

procedure TClassTypeSymbol.AddSymbol(const name: string; visibility: TMemberVisibility;
  value: TVarSymbol; var counter: integer);
var
   uName: string;
begin
   uName := UpperCase(name);
   FSymbols.Add(uName, value);
   if visibility <> mvPrivate then
      FExportSymbols.Add(uName, value);
   inc(counter);
   value.FIndex := counter;
   value.FParent := self;
end;

procedure TClassTypeSymbol.AddField(const name: string; &type: TTypeSymbol;
  visibility: TMemberVisibility);
begin
   CheckUnique(name, 'field');
   AddSymbol(name, visibility, TFieldSymbol.Create(name, &type, visibility, self), FFieldCount);
end;

procedure TClassTypeSymbol.AddMethod(const name: string; &type: TTypeSymbol;
  visibility: TMemberVisibility; classScope: boolean; paramList: IParamList);
var
   selfType: TTypeSymbol;
begin
   CheckUnique(name, 'method'); //TODO: modify this to allow for overloading
   if classScope then
      selfType := nil
   else selfType := self;
   if paramList = nil then
      ParamList := EmptyParamList;
   paramList.Insert(0, TParamSymbol.Create('self', selfType, []));
   AddSymbol(name, visibility,
     TProcSymbol.Create(name, &type, visibility, classScope, paramList, true, self), FMethodCount);
end;

procedure TClassTypeSymbol.AddProp(info: TRttiInstanceProperty);
begin
   CheckUnique(name, 'property');
   AddSymbol(info.Name, info.Visibility,
     TPropSymbol.Create(info.Name, info.Visibility, info.PropInfo, self), FPropCount);
end;

procedure TClassTypeSymbol.AddProp(const name: string; &type: TTypeSymbol;
  visibility: TMemberVisibility; classScope: boolean; paramList: IParamList;
  readSpec, writeSpec: TSymbol);
begin
   CheckUnique(name, 'property');
   AddSymbol(name, visibility,
     TPropSymbol.Create(name, &type, visibility, classScope, paramList,
                        readSpec, writeSpec, self),
     FPropCount);
end;

procedure TClassTypeSymbol.Define;
begin
   assert(FForward);
   FForward := false;
end;

function TClassTypeSymbol.DefineMethod(const name: string): TProcSymbol;
begin
   try
      result := FSymbols[name] as TProcSymbol;
   except
      raise EParseError.CreateFmt('Undefined method: %s.%s.', [FName, name]);
   end;
   if result.forwarded then
      result.Define
   else raise EParseError.CreateFmt('Method redeclared: %s.%s.', [FName, name]);
end;

function TClassTypeSymbol.DescendsFrom(ancestor: TClassTypeSymbol): boolean;
var
   current: TClassTypeSymbol;
begin
   current := self;
   while (current <> ancestor) and assigned(current) do
      current := current.FParentType;
   result := current = ancestor;
end;

function TClassTypeSymbol.DescendsFrom(metaclass: TClass): boolean;
var
   current: TClassTypeSymbol;
begin
   current := self;
   while (current.metaclass <> metaclass) and assigned(current) do
      current := current.FParentType;
   result := current.metaclass = metaclass;
end;

function TClassTypeSymbol.GetSymbolTable: ISymbolTable;
begin
   result := FSymbols;
end;

procedure TClassTypeSymbol.SetMetaclass(const Value: TClass);
begin
   assert(FMetaclass = nil);
   FMetaclass := Value;
end;

procedure TClassTypeSymbol.SetParent(const Value: TClassTypeSymbol);
begin
   assert(FParentType = nil);
   FParentType := Value;
   FFieldCount := value.FFieldCount;
   FPropCount := value.FPropCount;
   FMethodCount := value.FMethodCount;
end;

function TClassTypeSymbol.SymbolLookup(const name: string): TSymbol;
begin
   if not FSymbols.TryGetValue(name, result) then
      raise EParseError.CreateFmt('Symbol %s not found', [name])
end;

{ TFieldSymbol }

constructor TFieldSymbol.Create(const name: string; aType: TTypeSymbol;
  visibility: TMemberVisibility; parent: TSymbol);
begin
   FVisibility := visibility;
   inherited Create(name, aType, parent);
end;

{ TParamSymbol }

constructor TParamSymbol.Create(const name: string; aType: TTypeSymbol;
  flags: TParamFlags);
begin
   inherited Create(name, aType, nil);
   FFlags := flags;
end;

destructor TParamSymbol.Destroy;
begin
   inherited;
end;

{ TPropSymbol }

constructor TPropSymbol.Create(const name: string; &type: TTypeSymbol;
  visibility: TMemberVisibility; classScope: boolean; const paramList: IParamList;
  readSpec, writeSpec: TSymbol; parent: TSymbol);
begin
   inherited Create(name, &type, parent);
   FKind := syProp;
   FVisibility := visibility;
   FClassScope := classScope;
   FParamList := paramList;
   FReadSpec := readSpec;
   FWriteSpec := writeSpec;
   VerifyReadWriteCompat;
end;

constructor TPropSymbol.Create(const name: string; visibility: TMemberVisibility;
  info: PPropInfo; parent: TSymbol);
begin
   inherited Create(name, TypeOfNativeType(info.PropType^), parent);
   FVisibility := visibility;
   FKind := syProp;
   FPropinfo := info;
end;

procedure TPropSymbol.VerifyReadWriteCompat;
var
   match: boolean;
   i: integer;
   left, right: TParamSymbol;
begin
   assert(assigned(FReadSpec) or (assigned(FWriteSpec)));
   if assigned(FReadSpec) then
   begin
      if FParamList.Count > 0 then
      begin
         match := (FReadSpec is TProcSymbol) and
           (TProcSymbol(FReadSpec).FParamList.Count = FParamList.Count + 1) and
           AnsiSameText(TProcSymbol(FReadSpec).FParamList[0].name, 'self');
         if Match then
         begin
            for i := 0 to FParamList.Count - 1 do
            begin
               left := FParamList[i];
               right:= TProcSymbol(FReadSpec).FParamList[i + 1];
               CheckCompatibleTypes(left.FType, right.FType, true);
               match := match and (left.flags = right.flags);
            end;
            match := match and (TProcSymbol(FReadSpec).&type = (self.&type as TArrayTypeSymbol).baseType);
         end;
      end
      else if FReadSpec is TFieldSymbol then
         match := TFieldSymbol(FReadSpec).&Type = self.&Type
      else match := false; //TODO: implement this
      if not match then
         raise EParseError.Create('Read specifier must match property type');
   end;
   if assigned(FWriteSpec) then
   begin
      if FParamList.Count > 0 then
      begin
         match := (FWriteSpec is TProcSymbol) and
           (TProcSymbol(FWriteSpec).FParamList.Count = FParamList.Count + 2) and
           AnsiSameText(TProcSymbol(FWriteSpec).FParamList[0].name, 'self');
         if Match then
         begin
            for i := 0 to FParamList.Count - 1 do
            begin
               left := FParamList[i];
               right:= TProcSymbol(FWriteSpec).FParamList[i + 1];
               CheckCompatibleTypes(left.FType, right.FType, true);
               match := match and (left.flags = right.flags);
            end;
            match := match and (TProcSymbol(FWriteSpec).paramList.Last.&Type = (self.&type as TArrayTypeSymbol).baseType);
         end;
      end
      else if FWriteSpec is TFieldSymbol then
         match := TFieldSymbol(FWriteSpec).&Type = self.&Type
      else match := false; //TODO: implement this
      if not match then
         raise EParseError.Create('Write specifier must match property type');
   end;
end;

{ TProcSymbol }

constructor TProcSymbol.Create(const name: string; &type: TTypeSymbol;
  visibility: TMemberVisibility; classScope: boolean; const paramList: IParamList;
  forwarded: boolean; parent: TSymbol);
var
   param: TParamSymbol;
begin
   inherited Create(name, &type, parent);
   FKind := syProc;
   FVisibility := visibility;
   FClassScope := classScope;
   if assigned(paramList) then
      for param in paramList do
         param.FParent := self;
   FParamList := paramList;
   FSymbolTable := CreateSymbolTable(true);
   FForwarded := forwarded;
end;

destructor TProcSymbol.Destroy;
begin
   FSyntax.Free;
   inherited Destroy;
end;

procedure TProcSymbol.Define;
begin
   assert(FForwarded);
   FForwarded := false;
end;

procedure TProcSymbol.SetSyntax(const Value: TBlockSyntax);
begin
   assert(FSyntax = nil);
   FSyntax := Value;
end;

{ TArrayTypeSymbol }

function TArrayTypeSymbol.CanDerefArray: boolean;
begin
   result := true;
end;

constructor TArrayTypeSymbol.Create(lBound, uBound: integer;
  baseType: TTypeSymbol);
begin
   inherited Create('ARRAYOF*' + baseType.name);
   FLBound := lBound;
   FUBound := uBound;
   FBaseType := baseType;
end;

{ TLabelSymbol }

constructor TLabelSymbol.Create(const name: string);
begin
   inherited Create(name, syLabel);
end;

procedure TLabelSymbol.Use;
begin
   if FUsed then
      raise EParseError.CreateFmt('Label %s reused', [FName]);
   FUsed := true;
end;

{ TProcTypeSymbol }

constructor TProcTypeSymbol.Create(const name: string; retval: TTypeSymbol;
  const params: IParamList; isMethod: boolean);
begin
   inherited Create(name);
   FRetval := retval;
   FParams := params;
   FMethod := isMethod;
end;

{ TTypedSyntax }

function TTypedSyntax.GetSelfValue: TVarSymbol;
begin
   result := nil;
end;

{ TBlockSyntax }

constructor TBlockSyntax.Create;
begin
   inherited Create(skBlock);
   FChildren := TSyntaxList.Create;
   FChildren.OwnsObjects := true;
end;

destructor TBlockSyntax.Destroy;
begin
   FChildren.free;
   inherited Destroy;
end;

procedure TBlockSyntax.Add(value: TSyntax);
begin
   FChildren.Add(value);
end;

{ TAssignmentSyntax }

constructor TAssignmentSyntax.Create(lValue, rValue: TTypedSyntax);
begin
   inherited Create(skAssign);
   FLValue := lValue;
   FRValue := rValue;
end;

destructor TAssignmentSyntax.Destroy;
begin
   FLValue.Free;
   FRValue.Free;
   inherited;
end;

{ TLabelSyntax }

constructor TLabelSyntax.Create(const name: string);
begin
   inherited Create(skLabel);
   FName := name;
end;

{ TJumpSyntax }

constructor TJumpSyntax.Create(const name: string; conditional: boolean = false);
begin
   inherited Create(skJump);
   FName := name;
   FConditional := conditional;
end;

{ TCallSyntax }

constructor TCallSyntax.Create(proc: TProcSymbol; const params: TList<TTypedSyntax>);
begin
   inherited Create(skCall);
   FProc := proc;
   FParams := params;
end;

destructor TCallSyntax.Destroy;
begin
   FSelfSymbol.Free;
   FParams.Free;
   inherited;
end;

function TCallSyntax.GetSelfValue: TVarSymbol;
begin
   if FProc.FType is TClassTypeSymbol then
      result := FProc
   else result := nil;
end;

function TCallSyntax.GetType: TTypeSymbol;
begin
   result := FProc.FType;
end;

procedure TCallSyntax.SetSelf(const Value: TTypedSyntax);
begin
   assert(FSelfSymbol = nil);
   FSelfSymbol := Value;
end;

{ TTryCallSyntax }

constructor TTryCallSyntax.Create(const jump, ret: string);
begin
   inherited Create(skTryCall);
   FJump := jump;
   FRet := ret;
end;

{ TFinallySyntax }

constructor TFinallySyntax.Create(const ret: string);
begin
   inherited Create(skFinally);
   FRet := ret;
end;

{ TExceptSyntax }

constructor TExceptSyntax.Create(const ret: string);
begin
   inherited Create(skExcept);
   FRet := ret;
end;

{ TRaiseSyntax }

constructor TRaiseSyntax.Create(obj: TVariableSyntax);
begin
   inherited Create(skRaise);
   FObject := obj;
end;

{ TElemSyntax }

constructor TElemSyntax.Create(left, right: TTypedSyntax);
var
   lName, rName: string;
begin
   assert(left.kind in [skVariable, skDot]);

   inherited Create(skElem);
   FLeft := left;
   FRight := right;
   if (left.kind = skVariable) then
      lName := TVariableSyntax(FLeft).FSymbol.name
   else lName := (TDotSyntax(FLeft).FRight as TVariableSyntax).FSymbol.name;
   case right.kind of
      skVariable: rName := TVariableSyntax(FRight).FSymbol.name;
      skValue: rName := TValueSyntax(FRight).FValue.ToString;
      skElem: rName := TElemSyntax(FRight).FSymbol.name;
      skArrayProp: rName := TArrayPropSyntax(FRight).FSymbol.name;
      else assert(false);
   end;
   FSymbol := TVarSymbol.Create(format('%s[%s]', [lName, rName]), self.GetType, nil);
end;

destructor TElemSyntax.Destroy;
begin
   FSymbol.Free;
   FLeft.Free;
   FRight.Free;
   inherited Destroy;
end;

function TElemSyntax.GetSelfValue: TVarSymbol;
begin
   if FSymbol.FType is TClassTypeSymbol then
      result := FSymbol
   else result := nil;
end;

function TElemSyntax.GetType: TTypeSymbol;
begin
   result := (FLeft.&type as TArrayTypeSymbol).FBaseType
end;

{ TTypeRefSyntax }

constructor TTypeRefSyntax.Create(base: TClassTypeSymbol);
begin
   inherited Create(skType);
   FClass := base;
end;

function TTypeRefSyntax.GetType: TTypeSymbol;
begin
   result := FClass;
end;

{ TExceptionLoadSyntax }

constructor TExceptionLoadSyntax.Create(sym: TVarSymbol);
begin
   inherited Create(skELoad);
   FSymbol := sym;
end;

{ TDotSyntax }

constructor TDotSyntax.Create(left, right: TTypedSyntax);
begin
   inherited Create(skDot);
   FLeft := left;
   FRight := right;
end;

destructor TDotSyntax.Destroy;
begin
   FLeft.Free;
   FRight.Free;
   inherited;
end;

function TDotSyntax.GetSelfValue: TVarSymbol;
begin
   result := FRight.GetSelfValue;
end;

function TDotSyntax.GetType: TTypeSymbol;
begin
   result := FRight.&type;
end;

{ TCastSyntax }

constructor TCastSyntax.Create(base: TTypedSyntax; &type: TTypeSymbol);
begin
   inherited Create(skCast);
   FBase := base;
   FType := &type;
end;

destructor TCastSyntax.Destroy;
begin
   FBase.Free;
   inherited Destroy;
end;

function TCastSyntax.GetType: TTypeSymbol;
begin
   result := FType;
end;

{ TArrayPropSyntax }

constructor TArrayPropSyntax.Create(base: TDotSyntax; subscript: TTypedSyntax);
var
   lName, rName: string;
begin
   inherited Create(skArrayProp);
   FBase := base;
   FParams := TObjectList<TTypedSyntax>.Create;
   FParams.Add(subscript);

   lName := (base.FRight as TVariableSyntax).FSymbol.name;
   case subscript.kind of
      skVariable: rName := TVariableSyntax(subscript).FSymbol.name;
      skValue: rName := TValueSyntax(subscript).FValue.ToString;
      skElem: rName := TElemSyntax(subscript).FSymbol.name;
      skArrayProp: rName := TArrayPropSyntax(subscript).FSymbol.name;
      else assert(false);
   end;
   FSymbol := TVarSymbol.Create(format('%s[%s]', [lName, rName]), self.GetType, nil);
end;

destructor TArrayPropSyntax.Destroy;
begin
   FSymbol.Free;
   FParams.Free;
   FBase.Free;
   inherited Destroy;
end;

function TArrayPropSyntax.GetSelfValue: TVarSymbol;
begin
   result := FSymbol;
end;

function TArrayPropSyntax.GetType: TTypeSymbol;
begin
   result := (((FBase.FRight as TVariableSyntax).FSymbol as TPropSymbol).FType as TArrayTypeSymbol).basetype;
end;

{ TArraySyntax }

constructor TArraySyntax.Create;
begin
   inherited Create(skArray);
   FChildren := TBlockSyntax.Create;
end;

destructor TArraySyntax.Destroy;
begin
   FChildren.Free;
   inherited Destroy;
end;

procedure TArraySyntax.Finalize;
var
   baseType, newType: TTypeSymbol;
   i: integer;
begin
   if FChildren.children.count = 0 then
   begin
      FType := TypeOfNativeType(nil);
      Exit;
   end;
   baseType := (FChildren.children[0] as TTypedSyntax).&type;
   for i := 1 to FChildren.children.Count - 1 do
   begin
      newType := CheckCompatibleArrayTypes(baseType, (FChildren.children[i] as TTypedSyntax).&type);
      if newType = nil then
         raise EParseError.CreateFmt('Incompatible types: %s and %s', [baseType, (FChildren.children[i] as TTypedSyntax).&type])
      else baseType := newType;
   end;
   FType := MakeArrayType(baseType);
end;

function TArraySyntax.GetType: TTypeSymbol;
begin
   assert(assigned(FType));
   result := FType;
end;

function TArraySyntax.IsConstant: boolean;
var
   i: integer;
begin
   result := true;
   for i := 0 to FChildren.children.Count - 1 do
      result := result and (FChildren.children[i].kind = skValue);
end;

procedure TArraySyntax.Add(value: TTypedSyntax);
begin
   FChildren.Add(value);
end;

{ Classless }

type
   TSymbolTable = class(TObjectDictionary<string, TSymbol>, ISymbolTable)
   private
      FRefCount: integer;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;

      function GetItem(const Key: string): TSymbol;
      procedure SetItem(const Key: string; const Value: TSymbol);
      function Values: TEnumerable<TSymbol>;
      function Where(filter: TFunc<string, TSymbol, boolean>): TList<TPair<string, TSymbol>>;
      function KeysWhereValue(filter: TPredicate<TSymbol>): TStringList;
      function Count: integer;
   public
      destructor Destroy; override;
   end;

   TParamList = class(TObjectList<TParamSymbol>, IParamList)
   private
      FRefCount: integer;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;

      function Count: integer;
      procedure SetOwnsObjects(value: boolean);
      function GetItem(Index: Integer): TParamSymbol;
      procedure SetItem(Index: Integer; const Value: TParamSymbol);
      function GetEnumerator: TEnumerator<TParamSymbol>;
      procedure AddRange(Collection: IParamList);
   end;


function CreateSymbolTable(ownsObjects: boolean): ISymbolTable;
begin
   if ownsObjects then
      result := TSymbolTable.Create([doOwnsValues])
   else result := TSymbolTable.Create;
end;

function EmptyParamList(owns: boolean = true): IParamList;
begin
   result := TParamList.Create;
   result.OwnsObjects := owns;
end;

function CheckSubtype(left, right: TTypeSymbol): boolean;
begin
   result := false;
   if (left = TypeOfNativeType(TypeInfo(Real))) and (right = TypeOfNativeType(TypeInfo(Integer))) then
      result := true
   else if (left is TClassTypeSymbol) then
   begin
      if (right is TClassTypeSymbol) then
         result := TClassTypeSymbol(right).DescendsFrom(TClassTypeSymbol(left))
      else if right = TypeOfNativeType(nil) then
         result := true;
   end;
   //TODO: add more rules
end;

function CheckCompatibleArrayTypes(left, right: TTypeSymbol): TTypeSymbol;
begin
   if (left = right) or CheckSubtype(left, right) then
      result := left
   else if CheckSubtype(right, Left) then
      result := right
   else result := nil;
end;

procedure CheckCompatibleTypes(left, right: TTypeSymbol; exact: boolean);
var
   good: boolean;
begin
   assert(assigned(left));
   if right = nil then
      raise EParseError.CreateFmt('Incompatible types: "%s" and "Procedure or untyped value"', [left.name]);
   good := false;
   if left = right then
      good := true
   else if not exact then
      good := CheckSubtype(left, right);
   if not good then
      raise EParseError.CreateFmt('Incompatible types: "%s" and "%s"', [left.name, right.name]);
end;

procedure EnsureCompatibleTypes(left: TTypeSymbol; var right: TTypedSyntax; exact: boolean = false);
begin
   CheckCompatibleTypes(left, right.&type, exact);
   if exact then
      Exit;

   if left <> right.&type then
      right := TCastSyntax.Create(right, left);
end;

var
   LContext: TRttiContext;
   Sync: TMultiReadExclusiveWriteSynchronizer;
   LTypeTable: TDictionary<TRttiType, TTypeSymbol>;
   LArrayPropTable: TObjectDictionary<string, TTypeSymbol>;
   LArrayTypeTable: TDictionary<string, TTypeSymbol>;
   LFreeList: TObjectList<TTypeSymbol>;

procedure AddNativeType(nativeType: TRttiType; new: TTypeSymbol);
begin
   LTypeTable.Add(nativeType, new);
end;

function NativeTypeDefined(value: TRttiType): boolean;
begin
   result := LTypeTable.ContainsKey(value);
end;

function TypeOfRttiType(value: TRttiType): TTypeSymbol;
begin
      result := LTypeTable[value];
end;

function TypeOfNativeType(value: PTypeInfo): TTypeSymbol;
begin
   result := TypeOfRttiType(LContext.GetType(value));
end;

function TypeOfValue(const value: TValue): TTypeSymbol;
begin
   result := TypeOfNativeType(value.TypeInfo);
end;

var
   LBoolType: TTypeSymbol = nil;

function BooleanType: TTypeSymbol;
begin
   if not assigned(LBoolType) then
      LBoolType := TypeOfNativeType(TypeInfo(boolean));
   result := LBoolType;
end;

function FindNativeType(const name: string): TTypeSymbol;
begin
   result := TEnex.FirstWhereOrDefault<TTypeSymbol>(LTypeTable.Values,
     function(value: TTypeSymbol): boolean
     begin
        result := AnsiSameText(value.Name, name);
     end,
     nil);
   if result = nil then
      raise EParseError.CreateFmt('Type "%s" has not been imported.', [name]);
end;

procedure AddArrayType(value: TArrayTypeSymbol);
begin
   if LArrayTypeTable.ContainsKey(UpperCase(value.name)) then
      raise EParseError.Create('Duplicate array type');
   LArrayTypeTable.Add(UpperCase(value.name), value);
end;

function MakeArrayType(base: TTypeSymbol): TTypeSymbol;
var
   name: string;
begin
   name := 'ARRAYOF*' + UpperCase(base.name);
   TMonitor.Enter(LArrayTypeTable);
   try
      if not LArrayTypeTable.TryGetValue(name, Result) then
      begin
         result := TArrayTypeSymbol.Create(0, 0, base);
         LArrayTypeTable.Add(name, result);
         LFreeList.Add(result);
      end;
   finally
      TMonitor.Exit(LArrayTypeTable);
   end;
end;

function MakeArrayPropType(base: TTypeSymbol): TTypeSymbol;
var
   name: string;
begin
   name := 'ARRAYPROP*' + UpperCase(base.name);
   TMonitor.Enter(LArrayPropTable);
   try
      if not LArrayPropTable.TryGetValue(name, Result) then
      begin
         result := TArrayTypeSymbol.Create(0, 0, base);
         LArrayPropTable.Add(name, result)
      end;
   finally
      TMonitor.Exit(LArrayPropTable);
   end;
end;

const
   KEYWORD_COUNT = 63;
   KEYWORD_TABLE: array[1..KEYWORD_COUNT] of TKeywordPair = (
      (keyword: 'AND'; token: tkAnd), (keyword: 'ARRAY'; token: tkArray),
      (keyword: 'AS'; token: tkAs), (keyword: 'BEGIN'; token: tkBegin),
      (keyword: 'BREAK'; token: tkBreak), (keyword: 'CASE'; token: tkCase),
      (keyword: 'CLASS'; token: tkClass), (keyword: 'CONST'; token: tkConst),
      (keyword: 'CONSTRUCTOR'; token: tkConstructor), (keyword: 'CONTINUE'; token: tkContinue),
      (keyword: 'DESTRUCTOR'; token: tkDestructor), (keyword: 'DIV'; token: tkDiv),
      (keyword: 'DO'; token: tkDo), (keyword: 'DOWNTO'; token: tkDownTo),
      (keyword: 'ELSE'; token: tkElse), (keyword: 'END'; token: tkEnd),
      (keyword: 'EXCEPT'; token: tkExcept), (keyword: 'EXIT'; token: tkExit),
      {(keyword: 'EXTERNAL'; token: tkExternal),} (keyword: 'FINALLY'; token: tkFinally),
      (keyword: 'FOR'; token: tkFor), (keyword: 'FORWARD'; token: tkForward),
      (keyword: 'FUNCTION'; token: tkFunction), (keyword: 'GOTO'; token: tkGoto),
      (keyword: 'IF'; token: tkIf), (keyword: 'IMPLEMENTATION'; token: tkImplementation),
      (keyword: 'IN'; token: tkIn), (keyword: 'INHERITED'; token: tkInherited),
      (keyword: 'INTERFACE'; token: tkInterface), (keyword: 'IS'; token: tkIs),
      (keyword: 'LABEL'; token: tkLabel), (keyword: 'MOD'; token: tkMod),
      (keyword: 'NIL'; token: tkNil), (keyword: 'NOT'; token: tkNot),
      (keyword: 'OBJECT'; token: tkObject), (keyword: 'OF'; token: tkOf),
      (keyword: 'ON'; token: tkOn), (keyword: 'OR'; token: tkOr),
      (keyword: 'OUT'; token: tkOut), (keyword: 'OVERRIDE'; token: tkOverride),
      (keyword: 'DEFAULT'; token: tkDefault), (keyword: 'PRIVATE'; token: tkPrivate),
      (keyword: 'PROCEDURE'; token: tkProcedure), (keyword: 'PROPERTY'; token: tkProperty),
      (keyword: 'PROTECTED'; token: tkProtected), (keyword: 'PUBLIC'; token: tkPublic),
      (keyword: 'RAISE'; token: tkRaise),
      (keyword: 'RECORD'; token: tkRecord), (keyword: 'REPEAT'; token: tkRepeat),
      (keyword: 'SCRIPT'; token: tkProgram), (keyword: 'SET'; token: tkSet),
      (keyword: 'SHL'; token: tkShl), (keyword: 'SHR'; token: tkShr),
      (keyword: 'THEN'; token: tkThen), (keyword: 'TO'; token: tkTo),
      (keyword: 'TRY'; token: tkTry), (keyword: 'TYPE'; token: tkType),
      (keyword: 'UNIT'; token: tkUnit), (keyword: 'UNTIL'; token: tkUntil),
      (keyword: 'USES'; token: tkUses), (keyword: 'VAR'; token: tkVar),
      (keyword: 'VIRTUAL'; token: tkVirtual), (keyword: 'WHILE'; token: tkWhile),
      (keyword: 'XOR'; token: tkXor));
//turned "keywords" chr and ord into compiler magic functions

var
   LKeywords: TDictionary<string, TTokenKind>;

function GetKeyword(const name: string): TTokenKind;
begin
   if not LKeywords.TryGetValue(name, result) then
      result := tkIdentifier;
end;

var
   pair: TKeywordPair;

{ TSymbolTable }

function TSymbolTable.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TSymbolTable._AddRef: Integer;
begin
  Result := TInterlocked.Increment(FRefCount);
end;

function TSymbolTable._Release: Integer;
begin
  Result := TInterlocked.Decrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TSymbolTable.Count: integer;
begin
   result := inherited Count;
end;

destructor TSymbolTable.Destroy;
var
   list: TStringList;
   key: string;
   pair: TPair<string, TSymbol>;
begin
   list := TStringList.Create;
   try
      for pair in self do
         if pair.Value.FKind = syType then
            list.Add(pair.Key);
      for key in list do
         self.ExtractPair(key);
   finally
      list.Free;
   end;
   inherited;
end;

function TSymbolTable.GetItem(const Key: string): TSymbol;
begin
   result := self[key];
end;

procedure TSymbolTable.SetItem(const Key: string; const Value: TSymbol);
begin
   self[key] := value;
end;

function TSymbolTable.Values: TEnumerable<TSymbol>;
begin
   result := inherited Values;
end;

function TSymbolTable.KeysWhereValue(filter: TPredicate<TSymbol>): TStringList;
var
   pair: TPair<string, TSymbol>;
begin
   result := TStringList.Create;
   try
      for pair in self do
         if filter(pair.Value) then
            result.Add(pair.Key);
   except
      result.Free;
      raise;
   end;
end;

function TSymbolTable.Where(
  filter: TFunc<string, TSymbol, boolean>): TList<TPair<string, TSymbol>>;
var
   pair: TPair<string, TSymbol>;
begin
   result := TList<TPair<string, TSymbol>>.Create;
   try
      for pair in self do
         if filter(pair.Key, pair.Value) then
            result.Add(pair);
   except
      result.Free;
      raise;
   end;
end;

{ TParamList }

procedure TParamList.AddRange(Collection: IParamList);
var
   param: TParamSymbol;
begin
   for param in collection do
      add(param);
end;

function TParamList.Count: integer;
begin
   result := inherited Count;
end;

function TParamList.GetEnumerator: TEnumerator<TParamSymbol>;
begin
   result := inherited GetEnumerator;
end;

function TParamList.GetItem(Index: Integer): TParamSymbol;
begin
   result := self[index];
end;

procedure TParamList.SetItem(Index: Integer; const Value: TParamSymbol);
begin
   self[index] := value;
end;

procedure TParamList.SetOwnsObjects(value: boolean);
begin
   self.OwnsObjects := value;
end;

function TParamList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TParamList._AddRef: Integer;
begin
  Result := TInterlocked.Increment(FRefCount);
end;

function TParamList._Release: Integer;
begin
  Result := TInterlocked.Decrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

initialization
   LKeywords := TDictionary<string, TTokenKind>.Create;
   for pair in KEYWORD_TABLE do
      lKeywords.Add(pair.keyword, pair.token);
   LTypeTable := TObjectDictionary<TRttiType, TTypeSymbol>.Create([doOwnsValues]);
   LArrayPropTable := TObjectDictionary<string, TTypeSymbol>.Create([doOwnsValues]);
   LArrayTypeTable := TDictionary<string, TTypeSymbol>.Create();
   Sync := TMultiReadExclusiveWriteSynchronizer.Create;
   LFreeList := TObjectList<TTypeSymbol>.Create(true);
   ParserFormatSettings := FormatSettings;
   ParserFormatSettings.DecimalSeparator := '.';
finalization
   Sync.Free;
   lKeywords.Free;
   LTypeTable.Free;
   LArrayTypeTable.Free;
   LArrayPropTable.Free;
   LFreeList.Free;
end.
