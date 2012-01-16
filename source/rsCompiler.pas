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

unit rsCompiler;

interface
uses
   TypInfo, RTTI,
   Generics.Collections,
   rsDefs, rsImport, rsLexer, rsParser, rsCodegen, rsLinker, rsDefsBackend;

type
   TrsCompiler = class;

   TrsAttribute = class(TCustomAttribute)
   public
      procedure ApplyToProcC; virtual;
      procedure ApplyToProcR; virtual;
      procedure ApplyToTypeC; virtual;
      procedure ApplyToTypeR; virtual;
   end;

   TAttributeClass = class of TrsAttribute;

   TrsTypeImporter = class
   private
      FCompiler: TrsCompiler;
      FTable: ISymbolTable;
      FUnit: TUnitSymbol;
      FOwnsSymbols: boolean;
   public
      constructor Create(compiler: TrsCompiler; const name: string); overload;
      constructor Create(compiler: TrsCompiler; &unit: TUnitSymbol); overload;
      destructor Destroy; override;

      function ImportClass(value: TClass): TExternalClassType;
      procedure ImportType(value: PTypeInfo);
      procedure ImportFunction(const signature: string);
      procedure ImportVariable(const name: string; &type: PTypeInfo); overload;
      procedure ImportVariable(const name: string; &type: TClass); overload;
      procedure ImportConstant(const name: string; value: TValue);
      procedure ImportAttribute(value: TAttributeClass);
   end;

   TrsCompilerRegisterProc = reference to procedure(input: TrsTypeImporter);
   TUseUnitFunc = reference to function(const name: string; out text: string): boolean;

   TrsCompiler = class
   private type
      TUseStack = TStack<string>;
      class var ctx: TRttiContext;
   private
      FLexer: TRsLexer;
      FParser: TrsParser;
      FOptimizers: TList<IrsOptimizer>;
      FExtUnits: TDictionary<string, TrsCompilerRegisterProc>;
      FUnitCache: TObjectDictionary<string, TUnitSymbol>;
      FAttributes: TObjectDictionary<string, TTypeSymbol>;
      FAttributeSelf: TObjectDictionary<string, TVarSymbol>;
      FUnitList: TList<TUnitSymbol>;
      FOnUses: TUseUnitFunc;
      FUseStack: TUseStack;
      FSysSetup: boolean;
      FEnvironment: TClass;
      function ProcessUses(const name: string; out &unit: TUnitSymbol): boolean;
      function RecursiveCompile(const name: string; const text: string): TUnitSymbol;
      procedure RecursiveUsesPanic(const name: string);
      function InternalImport(const name: string; const proc: TrsCompilerRegisterProc;
        &unit: TUnitSymbol = nil): TUnitSymbol;
      function ParseExternalHeader(const signature: string; parent: TUnitSymbol;
        add: boolean): TProcSymbol;

      procedure SetupParser(sys: TUnitSymbol);
      procedure AddType(info: PTypeInfo; parent: TUnitSymbol);
      function VerifyAttribute(const name: string; out ctr: TProcSymbol;
         out selfSym: TVarSymbol): boolean;
      procedure EnsureSysUnit;
      function Link(units: TUnitList): TrsProgram;
      procedure Reset;
      procedure InternalCompile(const script, name: string);
   public
      constructor Create;
      destructor Destroy; override;
      function Compile(const value: string): TrsProgram;
      procedure CompileUnit(const value: string);
      procedure RegisterStandardUnit(const name: string; const proc: TrsCompilerRegisterProc);
      function RegisterEnvironment(env: TClass): TExternalClassType;
      function GetUnit(const name: string): TUnitSymbol;
      property OnUses: TUseUnitFunc read FOnUses write FOnUses;
   end;

implementation
uses
Windows,
   SysUtils, Classes,
   rsSystemUnit;

{ TrsCompiler }

constructor TrsCompiler.Create;
var
   sysUnit: TUnitSymbol;
begin
   FExtUnits := TDictionary<string, TrsCompilerRegisterProc>.Create;
   FUnitList := TList<TUnitSymbol>.Create;

   FUnitCache := TObjectDictionary<string, TUnitSymbol>.Create([doOwnsValues]);
   FAttributes := TObjectDictionary<string, TTypeSymbol>.Create([doOwnsValues]);
   FAttributeSelf := TObjectDictionary<string, TVarSymbol>.Create([doOwnsValues]);

   FLexer := TRsLexer.Create;
   sysUnit := BuildSysUnit(self.AddType);
   FUnitCache.Add(SYSNAME, sysUnit);
   Reset;

   FOptimizers := TList<IrsOptimizer>.Create;
   FOptimizers.Add(TrsMethodFlattener.Create);
end;

destructor TrsCompiler.Destroy;
begin
   FExtUnits.Free;
   FUnitCache.Free;
   FAttributes.Free;
   FAttributeSelf.Free;
   FOptimizers.Free;
   FUnitList.Free;
   FParser.Free;
   FLexer.Free;
   inherited;
end;

procedure TrsCompiler.EnsureSysUnit;
begin
   if not FSysSetup then
   begin
      if FExtUnits.ContainsKey(SYSNAME) then
         InternalImport(SYSNAME, FExtUnits[SYSNAME], FUnitCache[SYSNAME]);
      FSysSetup := true;
   end;
end;

function TrsCompiler.GetUnit(const name: string): TUnitSymbol;
begin
   result := FUnitCache[name];
end;

function TrsCompiler.Link(units: TUnitList): TrsProgram;
var
   linker: TrsLinker;
   codegen: TrsCodegen;
   symbol: TUnitSymbol;
   optimizer: IrsOptimizer;
   procs: TList<TProcSymbol>;
   proc: TProcSymbol;
begin
   linker := nil;
   result := nil;
   codegen := TrsCodegen.Create;
   try
      try
         for symbol in FUnitList do
         begin
            if not symbol.IsExternal then
               for optimizer in FOptimizers do
               begin
                  procs := symbol.Procs;
                  try
                     for proc in procs do
                        optimizer.process(proc);
                  finally
                     procs.Free;
                  end;
               end;
            units.Add(codegen.process(symbol));
         end;
         linker := TrsLinker.Create;
         result := linker.Link(units, codegen.constants, FEnvironment);
      except
         codegen.constants.Free;
         raise;
      end;
   finally
      codegen.Free;
      linker.Free;
   end;
end;

procedure TrsCompiler.InternalCompile(const script, name: string);
var
   symbol: TUnitSymbol;
begin
   symbol := FParser.Parse(script, name);
   if FUnitCache.ContainsKey(symbol.name) then
      FUnitList.Remove(FUnitCache[symbol.name]);
   FUnitCache.AddOrSetValue(symbol.name, symbol);
   FUnitList.Add(symbol);
end;

function TrsCompiler.Compile(const value: string): TrsProgram;
var
   name: string;
   units: TUnitList;
begin
//OutputDebugString('SAMPLING ON');
   if assigned(FUseStack) and (FUseStack.Count > 0) then
      name := FUseStack.Peek
   else name := '';
   EnsureSysUnit;
   TRttiContext.Create.GetPackages; //workaround for sorting error
   if assigned(FEnvironment) then
      FParser.SetEnvironment(rsDefs.TypeOfNativeType(FEnvironment.ClassInfo) as TClassTypeSymbol);
   units := TUnitList.Create;
   units.OwnsObjects := true;
   try
      InternalCompile(value, name);
      if (FUseStack = nil) or (FUseStack.Count = 0) then
         result := Link(units)
      else result := nil;
   finally
      units.Free;
      if name = '' then
         Reset;
   end;
//OutputDebugString('SAMPLING OFF');
end;

procedure TrsCompiler.CompileUnit(const value: string);
begin
   EnsureSysUnit;
   if assigned(FEnvironment) then
      FParser.SetEnvironment(rsDefs.TypeOfNativeType(FEnvironment.ClassInfo) as TClassTypeSymbol);
   try
      InternalCompile(value, '');
   finally
      Reset;
   end;
end;

procedure TrsCompiler.AddType(info: PTypeInfo; parent: TUnitSymbol);

   procedure ImportEnumeration(data: PTypeData);
   var
      i: integer;
      name: string;
   begin
      for i := data.MinValue to data.MaxValue do
      begin
         name := GetEnumName(info, i);
         parent.publics.Add(UpperCase(name), TConstSymbol.Create(name, TValue.FromOrdinal(info, i)));
      end;
   end;

var
   rttiType: TRttiType;
   parentType: TRttiType;
   new: TTypeSymbol;
   baseType: TClassTypeSymbol;
begin
   rttiType := ctx.GetType(info);
   if NativeTypeDefined(rttiType) then
   begin
      parent.publics.Add(UpperCase(string(info.Name)), TypeOfNativeType(info));
      Exit;
   end;

   if info.Kind = TypInfo.tkClass then
   begin
      parentType := TRttiInstanceType(rttiType).BaseType;
      if assigned(parentType) and not NativeTypeDefined(parentType) then
         AddType(parentType.Handle, parent);
      if assigned(parentType) then
         baseType := typeOfRttiType(parentType) as TClassTypeSymbol
      else baseType := nil;
      new := TExternalClassType.Create(TRttiInstanceType(rttiType), TypeOfRttiType,
        ParseExternalHeader,
        procedure(value: TRttiType) begin AddType(value.Handle, parent) end,
        parent, baseType);
   end
   else if info.kind in [TypInfo.tkArray, tkDynArray] then
      new := TExternalArrayType.Create(rttiType)
   else new := TExternalType.Create(rttiType);
   parent.publics.Add(UpperCase(string(info.Name)), new);
   if info.kind = tkEnumeration then
      ImportEnumeration(GetTypeData(info))
   else if info.kind = tkDynArray then
      AddArrayType(new as TArrayTypeSymbol);
   AddNativeType(rttiType, new);
end;

function TrsCompiler.InternalImport(const name: string;
  const proc: TrsCompilerRegisterProc; &unit: TUnitSymbol = nil): TUnitSymbol;
var
   importer: TrsTypeImporter;
   lParser: TrsParser;
begin
   if &unit = nil then
      importer := TrsTypeImporter.Create(self, name)
   else begin
      assert(AnsiSameText(&unit.name, name));
      importer := TrsTypeImporter.Create(self, &unit);
   end;
   lParser := FParser;
   try
      SetupParser(FUnitCache[SYSNAME]);
      proc(importer);
      result := importer.FUnit;
   finally
      FParser.Free;
      FParser := lParser;
      importer.Free;
   end;
end;

function TrsCompiler.ParseExternalHeader(const signature: string; parent: TUnitSymbol;
  add: boolean): TProcSymbol;
begin
   result := FParser.ParseExternalHeader(FLexer.Lex(signature + ';'), parent, add);
end;

function TrsCompiler.ProcessUses(const name: string; out &unit: TUnitSymbol): boolean;
var
   proc: TrsCompilerRegisterProc;
   lName: string;
   text: string;
begin
   lName := UpperCase(name);
   if FUnitCache.TryGetValue(lName, &unit) then
   begin
      result := true;
      if not FUnitList.Contains(&unit) then
         FUnitList.Add(&unit);
   end
   else if FExtUnits.TryGetValue(lName, proc) then
   begin
      &unit := InternalImport(name, proc);
      FUnitCache.Add(lName, &unit);
      FUnitList.Add(&unit);
      result := true;
   end
   else if assigned(FOnUses) and FOnUses(lName, text) then
   begin
      &unit := RecursiveCompile(name, text);
      FUnitCache.Add(lName, &unit);
      FUnitList.Add(&unit);
      result := true;
   end
   else result := false;
end;

function TrsCompiler.RecursiveCompile(const name: string; const text: string): TUnitSymbol;
var
   lName, scope: string;
   oldParser: TrsParser;
begin
   lName := UpperCase(name);
   if FUseStack = nil then
      FUseStack := TStack<string>.Create;
   for scope in FUseStack do
      if scope = lName then
         RecursiveUsesPanic(name);
   oldParser := FParser;
   SetupParser(FUnitCache[SYSNAME]);
   try
      self.Compile(text);
   finally
      FParser.Free;
      FParser := oldParser;
   end;
   result := FUnitCache[lName];
end;

procedure TrsCompiler.RecursiveUsesPanic(const name: string);
var
   list: TStringList;
   used: string;
begin
   list := TStringList.Create;
   try
      for used in FUseStack do
         list.Add(used);
      raise EParseError.CreateFmt('Unit "%s" recursively uses itself. Uses stack: %s', [name, list.CommaText]);
   finally
      list.Free;
   end;
end;

function TrsCompiler.RegisterEnvironment(env: TClass): TExternalClassType;
begin
   EnsureSysUnit;
   if not NativeTypeDefined(TRttiContext.Create.GetType(env.ClassInfo)) then
      self.AddType(env.ClassInfo, FUnitCache[SYSNAME]);
   FEnvironment := env;
   result := TypeOfNativeType(env.ClassInfo) as TExternalClassType
end;

procedure TrsCompiler.RegisterStandardUnit(const name: string; const proc: TrsCompilerRegisterProc);
begin
   FExtUnits.Add(UpperCase(name), proc);
end;

procedure TrsCompiler.Reset;
var
   sysunit: TUnitSymbol;
begin
   sysUnit := FUnitCache[SYSNAME];
   FUnitList.Clear;
   FUnitList.Add(sysUnit);
   FParser.Free;
   SetupParser(sysUnit);
end;

function TrsCompiler.VerifyAttribute(const name: string; out ctr: TProcSymbol; out selfSym: TVarSymbol): boolean;
var
   uName: string;
   &type: TTypeSymbol;
begin
   uName := UpperCase(name);
   result := FAttributes.TryGetValue(uName, &type);
   if result then
   begin
      ctr := (&type as TClassTypeSymbol).SymbolLookup('CREATE') as TProcSymbol;
      selfSym := FAttributeSelf[uName];
   end;
end;

procedure TrsCompiler.SetupParser(sys: TUnitSymbol);
begin
   FParser := TrsParser.Create(sys, self.ProcessUses, self.VerifyAttribute);
end;

{ TrsAttribute }

procedure TrsAttribute.ApplyToProcC;
begin
   //does nothing
end;

procedure TrsAttribute.ApplyToProcR;
begin
   //does nothing
end;

procedure TrsAttribute.ApplyToTypeC;
begin
   //does nothing
end;

procedure TrsAttribute.ApplyToTypeR;
begin
   //does nothing
end;

{ TrsTypeImporter }

constructor TrsTypeImporter.Create(compiler: TrsCompiler; const name: string);
begin
   FCompiler := compiler;
   FTable := CreateSymbolTable(true);
   FOwnsSymbols := true;
   FUnit := TUnitSymbol.Create(name, FTable, FTable, true);
end;

constructor TrsTypeImporter.Create(compiler: TrsCompiler; &unit: TUnitSymbol);
begin
   FCompiler := compiler;
   FTable := &unit.publics;
   FUnit := &unit;
end;

destructor TrsTypeImporter.Destroy;
begin
   inherited Destroy;
end;

procedure TrsTypeImporter.ImportAttribute(value: TAttributeClass);
var
   uName: string;
begin
   uName := UpperCase(value.ClassName);
   FCompiler.FAttributes.Add(uName,
     TExternalClassType.Create(FCompiler.ctx.GetType(value) as TRttiInstanceType,
       TypeOfRttiType,
       FCompiler.ParseExternalHeader,
       procedure(value: TRttiType) begin ImportType(value.Handle) end,
       FUnit, nil));
   FCompiler.FAttributeSelf.Add(uName, TVarSymbol.Create('', FCompiler.FAttributes[uName], FUnit));
end;

function TrsTypeImporter.ImportClass(value: TClass): TExternalClassType;
begin
   ImportType(value.ClassInfo);
   result := FTable[UpperCase(value.ClassName)] as TExternalClassType;
end;

procedure TrsTypeImporter.ImportConstant(const name: string; value: TValue);
begin
   FTable.Add(UpperCase(name), TConstSymbol.Create(name, value));
end;

procedure TrsTypeImporter.ImportFunction(const signature: string);
begin
   FCompiler.ParseExternalHeader(signature, FUnit, true);
end;

procedure TrsTypeImporter.ImportType(value: PTypeInfo);
begin
   FCompiler.AddType(value, FUnit);
end;

procedure TrsTypeImporter.ImportVariable(const name: string; &type: PTypeInfo);
var
   rType: TRttiType;
   sym: TVarSymbol;
begin
   rType := FCompiler.ctx.GetType(&type);
   if not NativeTypeDefined(rType) then
      ImportType(&type);
   sym := TVarSymbol.Create(name, TypeOfRttiType(rType), FUnit);
   FTable.Add(UpperCase(name), sym);
end;

procedure TrsTypeImporter.ImportVariable(const name: string; &type: TClass);
begin
   ImportVariable(name, &type.ClassInfo);
end;

end.
