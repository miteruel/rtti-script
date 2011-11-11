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

unit rsDefsBackend;

interface
uses
   TypInfo, rtti, Classes, Generics.Collections,
   newClass, rttiPackage;

type
{$MINENUMSIZE 4}
   TrsOpcode = (
      OP_NOP,  //NOP
      OP_ADD,  //addition operator
      OP_SUB,  //subtraction operator
      OP_MUL,  //multiplication operator
      OP_DIV,  //integer division operator
      OP_FDIV, //floating point division operator
      OP_MOD,  //mod operator
      OP_AND,  //bitwise and operator
      OP_OR,   //bitwise or operator
      OP_XOR,  //bitwise xor operator
      OP_SHL,  //shl operator
      OP_SHR,  //shr operator
      OP_AS,   //as operator
      OP_SCAT, //string concatenation
      OP_MOV,  //load value into register
      OP_MOVC, //mov constant
      OP_MOVI, //mov integer
      OP_MOVF, //mov field
      OP_MOVP, //mov property
      OP_MVAP, //mov array property
      OP_NEG,  //negation
      OP_NOT,  //not operator
      OP_INC,  //inc function
      OP_DEC,  //dec function
      OP_GTE,  // >= operator
      OP_LTE,  // <= operator
      OP_GT,   // > operator
      OP_LT,   // < operator
      OP_EQ,   // = operator
      OP_NEQ,  // <> operator
      OP_GTEI, // reg >= integer constant
      OP_LTEI, // reg <= integer constant
      OP_GTI,  // reg > integer constant
      OP_LTI,  // reg < integer constant
      OP_EQI,  // reg = integer constant
      OP_NEQI, // reg <> integer constant
      OP_IN,   //in operator
      OP_IS,   //is operator
      OP_XORB, //boolean xor
      OP_LIST, //create a param list
      OP_PUSH, //Add a param to the param list
      OP_PSHI, //Add an integer constant to the param list
      OP_PSHC, //Add a constant to the param list
      OP_CALL, //call with the current param list
      OP_CALX, //call an external function with the current param list
      OP_PCAL, //call and push result
      OP_PCLX, //call external and push result
      OP_INIT, //Initialize new function call with a certain number of temp variables
      OP_RET,  //return
      OP_JUMP, //unconditional jump
      OP_FJMP, //conditional jump if bool register = false
      OP_TJMP, //conditional jump if bool register = true
      OP_ARYL, //load array into array register
      OP_ELEM, //read array element
      OP_VASN, //assign to variable
      OP_AITF, //assign an integer to a float variable
      OP_EASN, //array element assign (register subscript)
      OP_EASC, //array element assign (const integer subscript)
      OP_FASN, //field assign
      OP_PASN, //property assign
      OP_APSN, //array property assign
      OP_TRYF, //open try-finally block
      OP_TRYE, //open try-except block
      OP_TRYC, //TryCall, a special call used to resolve Break, Continue and Exit within a try
      OP_CTRY, //close (return from) try block; left = return position
      OP_EXIS, //performs an IS on the exception object
      OP_EXLD, //loads exception object into a variable
      OP_RAIS, //if left = 0, "raise;" else raise exception in register
      OP_SRLD, //pushes a self value onto the SR (self register) stack; used for method calls and prop/field invocation
      OP_MCLS, //load a class reference
      OP_TRNC //Intrinsic TRUNC
   );

   TrsAsmInstruction = record
      op: TrsOpcode;
      left, right: integer;
   end;

   TrsProcInfo = record
      index: integer;
      info: PVmtMethodEntry;
      isExternal, standalone: boolean;
   end;

   TReferenceType = (rtVar, rtCall, rtType);
   TUnresolvedReference = class
   private
      FName: string;
      FLocation: integer;
      FRefType: TReferenceType;
   public
      constructor Create(const name: string; location: integer; refType: TReferenceType);
      property name: string read FName;
      property location: integer read FLocation;
      property refType: TReferenceType read FRefType;
   end;

   TUnresList = class(TObjectList<TUnresolvedReference>);

   TrsScriptUnit = class
   private
      FName, FNameDot: string;
      FText: TList<TrsAsmInstruction>;
      FRoutines: TDictionary<string, TrsProcInfo>;
      FUnresolved: TUnresList;
      FUnresolvedCalls: TUnresList;
      FExternal: boolean;
      FGlobals: TStringList;
      FUnit: INewUnit;
   public
      constructor Create(const name: string; isExternal: boolean);
      destructor Destroy; override;
      property name: string read FName;
      property nameDot: string read FNameDot; //performance optimization for codegen and link time
      property Text: TList<TrsAsmInstruction> read FText;
      property Unresolved: TUnresList read FUnresolved;
      property UnresolvedCalls: TUnresList read FUnresolvedCalls;
      property Routines: TDictionary<string, TrsProcInfo> read FRoutines;
      property IsExternal: boolean read FExternal;
      property Globals: TStringList read FGlobals;
      property &Unit: INewUnit read FUnit;
   end;

   TrsProgram = class
   private
      FText: TArray<TrsAsmInstruction>;
      FRoutines: TDictionary<string, TrsProcInfo>;
      FGlobals: TStringList;
      FConstants: TStringList;
      FScriptClasses: TList<TNewClass>;
      FPackage: IRttiPackage;
      FUnits: TList<INewUnit>;
      procedure SetConstants(const Value: TStringList);
   public
      constructor Create;
      destructor Destroy; override;
      procedure InstallPackage(proc: TGetCodeAddressProc);
      procedure AddText(const value: TList<TrsAsmInstruction>);

      property Text: TArray<TrsAsmInstruction> read FText;
      property Routines: TDictionary<string, TrsProcInfo> read FRoutines;
      property Units: TList<INewUnit> read FUnits;
      property Globals: TStringList read FGlobals;
      property Constants: TStringList read FConstants write SetConstants;
      property ScriptClasses: TList<TNewClass> read FScriptClasses;
   end;

   PValue = ^TValue;

   TUnitList = class(TObjectList<TrsScriptUnit>);

   TMultimap<TKey, TValue> = class(TObjectDictionary<TKey, TList<TValue>>)
   public
      constructor Create;
      procedure Add(const key: TKey; const value: TValue);
   end;

implementation

{ TrsScriptUnit }

constructor TrsScriptUnit.Create(const name: string; isExternal: boolean);
begin
   FName := name;
   FNameDot := name + '.';
   FExternal := isExternal;
   FText := TList<TrsAsmInstruction>.Create;
   FRoutines := TDictionary<string, TrsProcInfo>.Create;
   FUnresolved := TUnresList.Create(true);
   FUnresolvedCalls := TUnresList.Create(true);
   FGlobals := TStringList.Create;
   FUnit := TRttiNewUnit.Create(name);
end;

destructor TrsScriptUnit.Destroy;
begin
   FUnresolvedCalls.Free;
   FUnresolved.Free;
   FGlobals.Free;
   FRoutines.Free;
   FText.Free;
   inherited;
end;

{ TUnresolvedReference }

constructor TUnresolvedReference.Create(const name: string;
  location: integer; refType: TReferenceType);
begin
   FName := name;
   FLocation := location;
   FRefType := refType;
end;

{ TrsProgram }

procedure TrsProgram.AddText(const value: TList<TrsAsmInstruction>);
var
   index: integer;
begin
   index := length(FText);
   SetLength(FText, index + value.Count);
   Move(value.ToArray[0], FText[index], value.Count * sizeof(TrsAsmInstruction));
end;

constructor TrsProgram.Create;
begin
   FRoutines := TDictionary<string, TrsProcInfo>.Create;
   FGlobals := TStringList.Create;
   FGlobals.Add('');
   FScriptClasses := TList<TNewClass>.Create;
   FUnits := TList<INewUnit>.Create;
end;

destructor TrsProgram.Destroy;
begin
   FUnits.Free;
   FScriptClasses.Free;
   FRoutines.Free;
   FGlobals.Free;
   FConstants.Free;
   inherited Destroy;
end;

procedure TrsProgram.InstallPackage(proc: TGetCodeAddressProc);
var
   newUnit: INewUnit;
begin
   if assigned(FPackage) then
      Exit;
   FPackage := TRttiPackage.Create('');
   for newUnit in FUnits do
      FPackage.AddUnit(newUnit);
   FPackage.Install(proc);
end;

procedure TrsProgram.SetConstants(const Value: TStringList);
begin
   assert(FConstants = nil);
   FConstants := Value;
end;

{ TMultimap<TKey, TValue> }

constructor TMultimap<TKey, TValue>.Create;
begin
   inherited Create([doOwnsValues]);
end;

procedure TMultimap<TKey, TValue>.Add(const key: TKey; const value: TValue);
var
   list: TList<TValue>;
begin
   if not self.TryGetValue(key, list) then
   begin
      list := TList<TValue>.Create;
      inherited Add(key, list);
   end;
   list.Add(value);
end;

end.
