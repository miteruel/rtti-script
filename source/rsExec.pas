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
   SysUtils, TypInfo, RTTI, PrivateHeap, Generics.Collections, Windows,
   newClass, rttiPackage, rsDefsBackend;

type
   TrsExec = class;
   TrsVM = class;

   TExecImportCall = reference to procedure(const name: string; address: pointer);
   TExtFunction = reference to function(const selfValue: TValue; const index: TArray<TValue>): TValue;
   TExtProcedure = reference to procedure(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
   TArrayPropImport = reference to procedure(const classname, name: string; const OnRead: TExtFunction; const OnWrite: TExtProcedure);
   TrsExecImportProc = reference to procedure(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
   TOnLineEvent = reference to procedure(Sender: TrsVM; const line: TrsDebugLineInfo);
   TZeroDivideHandler = reference to function(Sender: TrsVM; l: integer; var handled: boolean): integer;

   TrsVM = class
   private type
      TVmContext = record
         locals: array of TValue;
         ar: PValue;  //array reg
         br: boolean; //bool reg
         ip: PrsAsmInstruction; //instruction pointer
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
      procedure DumpProgram; virtual;
   private
      FParent: TrsExec;
      FDepth: integer;
      FStack: TStack<TVmContext>;
      FSrStack: TStack<TsrPair>;
      FContext: TVmContext;
      FParamLists: TObjectStack<TSimpleParamList>;
      FText: TArray<TrsAsmInstruction>;
      FLastLine: TrsDebugLineInfo;
      FPackage: IRttiPackage;
      FOnDivideByZero: TZeroDivideHandler;

      function InvokeCode(index: integer; const Args: TArray<TValue>): TValue;
      function RunLoop(resultIndex: integer; expected: TrsOpcode): TValue;
      procedure InitializeRegs(const op: TrsAsmInstruction;
        const Args: TArray<TValue>);
      procedure LineCheck;

      function GetValue(i: integer): PValue;
      procedure SetValue(i: integer; const val: TValue); overload;
      procedure SetValue(i: integer; const val: integer); overload;
      procedure SetValue(i: integer; const val: Extended); overload;
      class procedure AssignValue(l, r: PValue); static;
      function GetSR: TValue;
      procedure InitializeReg(value: PValue; info: PTypeInfo);
      function GetIP: integer;

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
      procedure DivInt(l, r: integer);
      procedure AddInt(l, r: integer);
      procedure SubInt(l, r: integer);
      procedure MulInt(l, r: integer);
      procedure ModInt(l, r: integer);
      procedure AndInt(l, r: integer);
      procedure OrInt(l, r: integer);
      procedure XorInt(l, r: integer);
      procedure ShlInt(l, r: integer);
      procedure ShrInt(l, r: integer);
      procedure MovRegs(l, r: integer);
      procedure MovConst(l, r: integer);
      procedure MovBool(l, r: integer);
      procedure MovInt(l, r: integer);
      procedure MovProp(l, r: integer);
      procedure MovArrayProp(l, r: integer);
      procedure MovArrayPropSR(l, r: integer);
      procedure MovPropSR(l, r: integer);
      procedure PropAssign(l, r: integer);
      procedure ArrayPropAssign(l, r: integer);
      procedure NegRegister(l, r: integer);
      procedure NotRegister(l, r: integer);
      procedure IncRegister(l, r: integer);
      procedure DecRegister(l, r: integer);
      procedure CompGte(l, r: integer);
      procedure CompLte(l, r: integer);
      procedure CompGt(l, r: integer);
      procedure CompLt(l, r: integer);
      procedure CompEq(l, r: integer);
      procedure CompNeq(l, r: integer);
      procedure CompEqb(l, r: integer);
      procedure CompNeqb(l, r: integer);
      function RegAsBoolean(index: integer): boolean;
      procedure XorbRegs(l, r: integer);
      procedure NewList(l, r: integer);
      procedure PushValue(l, r: integer);
      procedure Call(l, r: integer);
      procedure Callx(l, r: integer);
      procedure PCall(l, r: integer);
      procedure PCallx(l, r: integer);
      procedure ArrayLoad(l, r: integer);
      procedure ArrayElemMove(l, r: integer);
      procedure CompEqi(l, r: integer);
      procedure CompGtei(l, r: integer);
      procedure CompGti(l, r: integer);
      procedure CompLtei(l, r: integer);
      procedure CompLti(l, r: integer);
      procedure CompNeqi(l, r: integer);
      procedure PushConst(l, r: integer);
      procedure PushInt(l, r: integer);
      procedure ArrayElemAssignC(l, r: integer);
      procedure ArrayElemAssign(l, r: integer);
      procedure LoadSelfRegister(l, r: integer);
      procedure StringConcat(l, r: integer);
      procedure TruncReg(l, r: integer);
      procedure AssignIntToFloat(l, r: integer);
   private
      constructor Create(parent: TrsExec);
   public
      destructor Destroy; override;
   end;

   TrsExec = class
   private
      FProgram: TrsProgram;
      FHeap: TPrivateHeap;
      FGlobals: TArray<TValue>;
      FConstants: TArray<TValue>;
      FRunOnLoad: boolean;
      FProcMap: TMultimap<string, TRttiMethod>;
      FExtUnits: TDictionary<string, TrsExecImportProc>;
      FExtRoutines: TArray<TRttiMethod>;
      FMagicRoutines: TArray<TExtFunction>;
      FImpls: TObjectList<TMethodImplementation>;
      FText: TArray<TrsAsmInstruction>;
      FEnvironment: TObject;
      FNewClasses: TList<TClass>;
      FLastImport: TPair<string, TDictionary<string, pointer>>;
      FMaxStackDepth: integer;
      FVmMap: TObjectDictionary<cardinal, TrsVm>;
      FArrayProcTableR: TArray<TExtFunction>;
      FArrayProcTableW: TArray<TExtProcedure>;
      FSystemLoaded: boolean;
      FOnLine: TOnLineEvent;
      FOnDivideByZero: TZeroDivideHandler;

      class var
      FBlank: TValue;
      FBlankData: IValueData;

      procedure SetExtRoutine(index: integer; method: TRttiMethod);
      function GetProc(const name: string; count: integer): TRttiMethod;
      procedure CreateMethodPointer(method: TRttiMethod; var address: pointer);
      procedure RegisterMethodPointer(method: TRttiMethod; var address: pointer);
      procedure MethodImplementation(UserData: Pointer;
        const Args: TArray<TValue>; out Result: TValue);
      procedure RegisterProcs(const unitName: string; method: TRttiMethod; var address: pointer);
      procedure LoadProcs;
      procedure LoadConstants;
      procedure LoadGlobals;
      procedure LoadEnvironment;
      procedure LoadClass(cls: TClass);
      procedure LoadClasses;
      function DoImport(const unitName: string): TDictionary<string, pointer>;
      function GetVM: TrsVM;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Load(prog: TrsProgram);
      procedure Run;
      function RunProc(const name: string; const params: array of TValue): TValue;
      function GetProcAddress(const name: string): pointer;
      procedure Clean;

      procedure RegisterStandardUnit(const name: string; const proc: TrsExecImportProc);
      procedure SetEnvironment(value: TObject);

      property RunOnLoad: boolean read FRunOnLoad write FRunOnLoad;
      property MaxStackDepth: integer read FMaxStackDepth write FMaxStackDepth;
      property OnLine: TOnLineEvent read FOnLine write FOnLine;
      property OnDivideByZero: TZeroDivideHandler read FOnDivideByZero write FOnDivideByZero;
   end;

   ErsRuntimeError = class(Exception);

implementation
uses
   StrUtils, Types, Math, Classes, tlHelp32, IOUtils,
   rsEnex,
   vmtStructure;

var
   PascalFormatSettings: TFormatSettings;

procedure CorruptError;
begin
   raise ErsRuntimeError.Create('Invalid instruction');
end;

procedure NotImplemented;
begin
   raise ErsRuntimeError.Create('Not Implemented Error');
end;

{$q+}{$r+}{$o+}
{ TrsExec }

constructor TrsExec.Create;
var
   notBlank: TValue;
begin
   FHeap := TPrivateHeap.Create;
   FProcMap := TMultimap<string, TRttiMethod>.Create;
   FNewClasses := TList<TClass>.Create;
   FImpls := TObjectList<TMethodImplementation>.Create;
   FImpls.OwnsObjects := true;
   FExtUnits := TDictionary<string, TrsExecImportProc>.Create;
   FMaxStackDepth := 2048;
   FBlank := TValue.Empty;
   notBlank := FBlank.Cast<integer>;
   FBlankData := TValueData(notBlank).FValueData;
   FVmMap := TObjectDictionary<cardinal, TrsVm>.Create([doOwnsValues]);
end;

destructor TrsExec.Destroy;
begin
   FVmMap.Free;
   FProcMap.Free;
   FLastImport.Value.Free;
   FExtUnits.Free;
   FImpls.Free;
   FNewClasses.Free;
   FHeap.Free;
   inherited Destroy;
end;

function TrsExec.GetVM: TrsVM;
var
   id: cardinal;
begin
   id := TThread.CurrentThread.ThreadID;
   if not FvmMap.TryGetValue(id, result) then
   begin
      result := TrsVM.Create(self);
      FvmMap.Add(id, result);
   end;
end;

procedure TrsExec.MethodImplementation(UserData: Pointer; const Args: TArray<TValue>; out Result: TValue);
var
   vm: TrsVM;
   reentrant: boolean;
begin
   vm := GetVM;
   vm.FPackage := FProgram.package;
{   try
      vm.DumpProgram;
   except
   end;}
   reentrant := assigned(vm.FContext.ip);
   if reentrant then
      vm.FStack.Push(vm.FContext);
   try
      result := vm.InvokeCode(NativeInt(UserData), args);
   finally
      if reentrant then
         vm.FContext := vm.FStack.Pop;
   end;
end;

//thanks to Raymond Chen for the code to find all thread IDs
//http://blogs.msdn.com/b/oldnewthing/archive/2006/02/23/537856.aspx
procedure TrsExec.Clean;
var
   h: THandle;
   id: nativeUInt;
   te: THREADENTRY32;
   threadlist, deadlist: TList;
begin
   threadlist := TList.Create;
   deadlist := TList.Create;
   try
      h := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
      if (h <> INVALID_HANDLE_VALUE) then
      begin
         te.dwSize := sizeof(te);
         if (Thread32First(h, te)) then
         repeat
            assert(te.dwSize >= sizeof(DWORD) * 3);
            threadlist.Add(pointer(te.th32ThreadID));
            te.dwSize := sizeof(te);
         until not (Thread32Next(h, te));
      end;
      CloseHandle(h);
      for id in FVmMap.Keys do
         if threadList.IndexOf(pointer(id)) = -1 then
            deadList.Add(pointer(id));
      if deadList.Count > 0 then
         for id := 0 to deadList.Count - 1 do
            FVmMap.Remove(cardinal(deadList[id]));
   finally
      threadlist.Free;
      deadlist.Free;
   end;
end;

procedure TrsExec.CreateMethodPointer(method: TRttiMethod; var address: pointer);
const RSUNIT_HEADER = 'rsUNIT*.';
var
   impl: TMethodImplementation;
   sl: TStringList;
   unitname: string;
   idx: integer;
begin
   sl := FProgram.unitOffsets;
   unitname := method.parent.name;
   AnsiStartsText(RSUNIT_HEADER, unitName);
   unitName := Copy(unitName, length(RSUNIT_HEADER));
   idx := sl.IndexOf(unitName);
   assert(idx >= 0);
   nativeUint(address) := nativeUInt(Address) + nativeUInt(pointer(sl.Objects[idx]));
   impl := method.CreateImplementation(address, self.MethodImplementation);
   address := impl.CodeAddress;
   FImpls.Add(impl);
   FProcMap.Add(method.Name, method);
   sl.Text;
end;

function TrsExec.DoImport(const unitName: string): TDictionary<string, pointer>;
var
   proc: TrsExecImportProc;
   arrayPropImporter: TArrayPropImport;
   procImporter: TExecImportCall;
   dict: TDictionary<string, pointer>;
begin
   if FLastImport.Key <> unitName then
   begin
      if not FExtUnits.TryGetValue(unitName, proc) then
         raise ErsRuntimeError.CreateFmt('Import proc for unit %s not found', [UnitName]);
      if unitName = 'SYSTEM' then
         FSystemLoaded := true;
      result := TDictionary<string, pointer>.Create;
      dict := result;
      procImporter :=
         procedure(const name: string; address: pointer)
         begin
            dict.Add(UpperCase(name), address);
         end;
      arrayPropImporter :=
         procedure(const classname, name: string; const OnRead: TExtFunction; const OnWrite: TExtProcedure)
         var
            idx: integer;
         begin
            if assigned(OnRead) then
            begin
               idx := FProgram.ArrayProps.IndexOf(format('%s.GET*%s', [classname, name]));
               if idx = -1 then
                  asm int 3 end;
               if idx > high(FArrayProcTableR) then
                  SetLength(FArrayProcTableR, idx + 1);
               FArrayProcTableR[idx] := OnRead;
            end;
            if assigned(OnWrite) then
            begin
               idx := FProgram.ArrayProps.IndexOf(format('%s.SET*%s', [classname, name]));
               if idx = -1 then
                  asm int 3 end;
               if idx > high(FArrayProcTableW) then
                  SetLength(FArrayProcTableW, idx + 1);
               FArrayProcTableW[idx] := OnWrite;
            end;
         end;
      proc(procImporter, arrayPropImporter);
      FLastImport.Value.Free;
      FLastImport.Key := unitName;
      FLastImport.Value := dict;
   end;
   result := FLastImport.Value;
end;

procedure TrsExec.SetExtRoutine(index: integer; method: TRttiMethod);
begin
   if index > high(FExtRoutines) then
      SetLength(FExtRoutines, index + 1);
   if assigned(FExtRoutines[index]) and(FExtRoutines[index] <> method) then
      asm int 3 end;
   FExtRoutines[index] := method;
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

   SetExtRoutine(index, method);
end;

procedure TrsExec.RegisterProcs(const unitName: string; method: TRttiMethod; var address: pointer);
begin
   if pos('EXT*', method.parent.name) = 0 then
      CreateMethodPointer(method, address)
   else RegisterMethodPointer(method, address);
end;

procedure TrsExec.LoadProcs;
begin
   FSystemLoaded := false;
   SetLength(FExtRoutines, 0);
   FProcMap.Clear;
   FProgram.InstallPackage(self.RegisterProcs);
   if (not FSystemLoaded) and (FExtUnits.ContainsKey('SYSTEM')) then
      DoImport('SYSTEM');
end;

function DeserializeConstant(const value: string; info: PTypeInfo): TValue; forward;

function LoadArray(const value: string; info: PTypeInfo): TValue;
var
   arr: TArray<TValue>;
   parser: TStringList;
   elType: PTypeInfo;
   i: integer;
begin
   assert((value[1] = '[') and (value[length(value)] = ']'));
   elType := GetTypeData(info).eltype2^;
   parser := TStringList.Create;
   try
      parser.CommaText := copy(value, 2, length(value) - 2);
      setLength(arr, parser.Count);
      for i := 0 to parser.Count - 1 do
         arr[i] := DeserializeConstant(parser[i], elType);
   finally
      parser.Free;
   end;
   result := TValue.FromArray(info, arr);
end;

function DeserializeConstant(const value: string; info: PTypeInfo): TValue;
begin
   if info = nil then
      result := (TValue.Empty)
  else case info.Kind of
    tkEnumeration: result := TValue.FromOrdinal(info, TypInfo.GetEnumValue(info, value));
    tkInteger: result := StrToInt(value);
    tkInt64: result := StrToInt64(value);
    tkUString, tkString, tkWString, tkChar, tkWChar: result := value;
    tkFloat: result := StrToFloat(value);
{Not ready for this yet; requires support in codegen first}
//       tkFloat: result := StrToFloat(value, PascalFormatSettings);
    tkSet: TValue.Make(StringToSet(info, value), info, result);
    tkArray, tkDynArray: result := LoadArray(value, info);
    else raise Exception.Create('Unknown constant type');
  end;
end;

procedure TrsExec.LoadConstants;
var
   i: integer;
begin
   SetLength(FConstants, FProgram.Constants.Count);
   for i := 0 to FProgram.Constants.Count - 1 do
      FConstants[i] := DeserializeConstant(FProgram.Constants[i], pointer(FProgram.Constants.Objects[i]))
end;

procedure TrsExec.LoadGlobals;
var
   i: integer;
   blank: TValue;
begin
   SetLength(FGlobals, FProgram.Globals.Count);
   blank := TValue.Empty;
   for i := 1 to High(FGlobals) do
      FGlobals[i] := blank.Cast(pointer(FProgram.Globals.Objects[i]));
   if assigned(FEnvironment) then
   begin
      if (length(FGlobals) < 2) or (FProgram.Globals[1] <> 'ENVIRONMENT*SELF') then
         raise ErsRuntimeError.Create('No environment defined for this program')
      else if (FGlobals[1].TypeInfo <> FEnvironment.ClassInfo) then
         raise ErsRuntimeError.CreateFmt('Incorrect environment type for this program; expected %s but %s was loaded.', [FGlobals[1].TypeInfo.Name, FEnvironment.ClassName]);
      FGlobals[1] := FEnvironment;
   end;
end;

procedure TrsExec.LoadClass(cls: TClass);
var
   info: TrsProcInfo;
   methods: TArray<TRttiMethod>;
   method: TRttiMethod;
   classdot: string;

   function NoImport(obj: TRttiObject): boolean;
   var
      attr: TCustomAttribute;
   begin
      for attr in obj.GetAttributes do
         if attr is NoImportAttribute then
            exit(true);
      result := false;
   end;

begin
   classdot := cls.ClassName + '.';
   methods := TRttiContext.Create.GetType(cls.ClassInfo).GetDeclaredMethods;
   for method in methods do
   begin
      if (not NoImport(method)) and (FProgram.Routines.TryGetValue(classdot + UpperCase(method.Name), info)) then
         SetExtRoutine(-info.index, method);
   end;
end;

procedure TrsExec.LoadEnvironment;
begin
   if assigned(FEnvironment) then
      LoadClass(FEnvironment.ClassType);
end;

procedure TrsExec.LoadClasses;
var
   cls: TClass;
begin
   for cls in FProgram.ExtClasses do
      LoadClass(cls);
end;

procedure TrsExec.Load(prog: TrsProgram);
begin
//   assert(FProgram = nil); //fix this after changing to interfaces
   FProgram := prog;
   FText := FProgram.Text;
   LoadEnvironment;
   LoadProcs;
   LoadClasses;
   LoadConstants;
   LoadGlobals;
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

   result := nil;
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

procedure TrsExec.SetEnvironment(value: TObject);
begin
   if assigned(FProgram) then
      raise ErsRuntimeError.Create('Environment must be set before the script program is loaded.');
   FEnvironment := value;
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

{ TrsVM }

constructor TrsVM.Create(parent: TrsExec);
begin
   FParent := parent;
   FParamLists := TObjectStack<TSimpleParamList>.Create;
   FParamLists.OwnsObjects := true;
   FStack := TStack<TVmContext>.Create;
   FsrStack := TStack<TsrPair>.Create;
   FText := parent.FText;
   FOnDivideByZero := parent.FOnDivideByZero;
end;

destructor TrsVM.Destroy;
begin
   FsrStack.Free;
   FStack.Free;
   FParamLists.Free;
   inherited Destroy;
end;

procedure TrsVM.AddRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   case val.Kind of
      tkInteger: SetValue(l, val.AsInteger + GetValue(r).AsInteger);
      tkFloat: SetValue(l, val.AsExtended + GetValue(r).AsExtended);
      else CorruptError;
   end;
end;

procedure TrsVM.SubRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   case val.Kind of
      tkInteger: SetValue(l, val.AsInteger - GetValue(r).AsInteger);
      tkFloat: SetValue(l, val.AsExtended - GetValue(r).AsExtended);
      else CorruptError;
   end;
end;

procedure TrsVM.MulRegs(l, r: integer);
var
   val, val2: PValue;
begin
   val := GetValue(l);
   val2 := GetValue(r);
   if (val.Kind = tkInteger) and (val2.Kind = tkInteger) then
      SetValue(l, val.AsInteger * val2.AsInteger)
   else SetValue(l, val.AsExtended * val2.AsExtended)
end;

procedure TrsVM.DivRegs(l, r: integer);
var
   val: PValue;
   handled: boolean;
   errResult: integer;
begin
   val := GetValue(l);
   try
      SetValue(l, val.AsInteger div GetValue(r).AsInteger);
   except
      on EDivByZero do
      begin
         if assigned(FOnDivideByZero) then
         begin
            handled := false;
            errResult := FOnDivideByZero(self, val.AsInteger, handled);
            if handled then
               SetValue(l, errResult)
            else raise;
         end
         else raise;
      end;
   end;
end;

procedure TrsVM.FdivRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsExtended / GetValue(r).AsExtended);
end;

procedure TrsVM.ModRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger mod GetValue(r).AsInteger);
end;

procedure TrsVM.AndRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger and GetValue(r).AsInteger);
end;

procedure TrsVM.OrRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger or GetValue(r).AsInteger);
end;

procedure TrsVM.XorRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger xor GetValue(r).AsInteger);
end;

procedure TrsVM.ShlRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger shl GetValue(r).AsInteger);
end;

procedure TrsVM.ShrRegs(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger shr GetValue(r).AsInteger);
end;

procedure TrsVM.StringConcat(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsString + GetValue(r).AsString);
end;

procedure TrsVM.AddInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger + r);
end;

procedure TrsVM.SubInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger - r);
end;

procedure TrsVM.MulInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger * r);
end;

procedure TrsVM.DivInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger div r);
end;

procedure TrsVM.ModInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger mod r);
end;

procedure TrsVM.AndInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger and r);
end;

procedure TrsVM.OrInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger or r);
end;

procedure TrsVM.XorInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger xor r);
end;

procedure TrsVM.ShlInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger shl r);
end;

procedure TrsVM.ShrInt(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger shr r);
end;

procedure TrsVM.MovRegs(l, r: integer);
begin
   SetValue(l, GetValue(r)^);
end;

procedure TrsVM.MovConst(l, r: integer);
begin
   SetValue(l, FParent.FConstants[r]);
end;

procedure TrsVM.MovBool(l, r: integer);
begin
   MovConst(l, r);
   FContext.br := boolean(r);
end;

procedure TrsVM.MovInt(l, r: integer);
begin
   SetValue(l, r);
end;

procedure TrsVM.MovProp(l, r: integer);
var
   sr: TObject;
   prop: TRttiProperty;
begin
   prop := FParent.FProgram.Properties.Objects[r] as TRttiProperty;
   sr := GetSR.AsObject;
   SetValue(l, prop.GetValue(sr));
end;

procedure TrsVM.MovArrayProp(l, r: integer);
var
   args: TArray<TValue>;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   SetValue(l, FParent.FArrayProcTableR[r](GetSR, args));
end;

procedure TrsVM.MovPropSR(l, r: integer);
var
   sr: TObject;
   prop: TRttiProperty;
begin
   prop := FParent.FProgram.Properties.Objects[r] as TRttiProperty;
   sr := GetSR.AsObject;
   FSrStack.Push(TsrPair.Create(prop.GetValue(sr), FDepth));
end;

procedure TrsVM.MovArrayPropSR(l, r: integer);
var
   args: TArray<TValue>;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   FSrStack.Push(TsrPair.Create(FParent.FArrayProcTableR[r](GetSR, args), FDepth));
end;

procedure TrsVM.NegRegister(l, r: integer);
var
   val2: PValue;
begin
   val2 := GetValue(r);
   case val2.Kind of
      tkInteger: SetValue(l, -val2.AsInteger);
      tkFloat: SetValue(l,-val2.AsExtended);
      else CorruptError;
   end;
end;

procedure TrsVM.NotRegister(l, r: integer);
begin
   if l = 0 then
      FContext.br := not FContext.br
   else begin
      FContext.br := not FContext.locals[r].AsBoolean;
      SetValue(l, FContext.br);
   end;
end;

procedure TrsVM.IncRegister(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger + r);
end;

procedure TrsVM.DecRegister(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger - r);
end;

procedure TrsVM.CompGte(l, r: integer);
var
   val, val2: PValue;
begin
   val := GetValue(l);
   val2 := GetValue(r);
   case val.Kind of
      tkInteger: FContext.br := val.AsInteger >= val2.AsInteger;
      tkFloat: FContext.br := val.AsExtended >= val2.AsExtended;
      tkString, tkChar: FContext.br := val.AsString >= val2.AsString;
      else CorruptError;
   end;
end;

procedure TrsVM.CompLte(l, r: integer);
var
   val, val2: PValue;
begin
   val := GetValue(l);
   val2 := GetValue(r);
   case val.Kind of
      tkInteger: FContext.br := val.AsInteger <= val2.AsInteger;
      tkFloat: FContext.br := val.AsExtended <= val2.AsExtended;
      tkString, tkChar: FContext.br := val.AsString <= val2.AsString;
      else CorruptError;
   end;
end;

procedure TrsVM.CompGt(l, r: integer);
begin
   CompLte(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsVM.CompLt(l, r: integer);
begin
   CompGte(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsVM.CompEq(l, r: integer);
var
   val, val2: PValue;
begin
   val := GetValue(l);
   val2 := GetValue(r);
   case val.Kind of
      tkInteger: FContext.br := val.AsInteger = val2.AsInteger;
      tkFloat: FContext.br := val.AsExtended = val2.AsExtended;
      tkString, tkChar: FContext.br := val.AsString = val2.AsString;
       tkEnumeration: FContext.br := val.AsOrdinal = val2.AsOrdinal;
      else CorruptError;
   end;
end;

procedure TrsVM.CompEqb(l, r: integer);
var
   val2: PValue;
begin
   val2 := GetValue(r);
   FContext.br := FContext.br = val2.AsBoolean;
end;

procedure TrsVM.CompNeqb(l, r: integer);
begin
   CompEqb(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsVM.CompGtei(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   case val.Kind of
      tkInteger: FContext.br := val.AsInteger >= r;
      tkFloat: FContext.br := val.AsExtended >= r;
      else CorruptError;
   end;
end;

procedure TrsVM.CompLtei(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   case val.Kind of
      tkInteger: FContext.br := val.AsInteger <= r;
      tkFloat: FContext.br := val.AsExtended <= r;
      else CorruptError;
   end;
end;

procedure TrsVM.CompGti(l, r: integer);
begin
   CompLtei(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsVM.CompLti(l, r: integer);
begin
   CompGtei(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsVM.CompEqi(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   case val.Kind of
      tkInteger: FContext.br := val.AsInteger = r;
      tkFloat: FContext.br := val.AsExtended = r;
      else CorruptError;
   end;
end;

procedure TrsVM.CompNeqi(l, r: integer);
begin
   CompEqi(l, r);
   FContext.br := not FContext.br;
end;

procedure TrsVM.CompNeq(l, r: integer);
begin
   CompEq(l, r);
   FContext.br := not FContext.br;
end;

function TrsVM.RegAsBoolean(index: integer): boolean;
begin
   if index = 0 then
      result := FContext.br
   else result := GetValue(index).AsBoolean;
end;

procedure TrsVM.XorbRegs(l, r: integer);
begin
   FContext.br := RegAsBoolean(l) xor RegAsBoolean(r);
end;

procedure TrsVM.NewList(l, r: integer);
begin
   FParamLists.Push(TSimpleParamList.Create(l));
end;

procedure TrsVM.PushValue(l, r: integer);
begin
   FParamLists.peek.Add(GetValue(l)^);
end;

procedure TrsVM.PushInt(l, r: integer);
begin
   FParamLists.peek.Add(l);
end;

procedure TrsVM.PushConst(l, r: integer);
begin
   FParamLists.Peek.Add(FParent.FConstants[l]);
end;

procedure TrsVM.Call(l, r: integer);
var
   args: TArray<TValue>;
   retval: TValue;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   FStack.Push(FContext);
   retval := InvokeCode(GetIP + r, args);
   FContext := FStack.Pop;
   if l <> -1 then
      SetValue(l, retval);
end;

procedure TrsVM.Callx(l, r: integer);
var
   args: TArray<TValue>;
   method: TRttiMethod;
   retval: TValue;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   method := FParent.FExtRoutines[r];
   try
      if method.IsStatic then
         retval := method.Invoke(nil, args)
      else retval := method.Invoke(GetSR, args);
   except
      on EInsufficientRTTI do
         raise ErsRuntimeError.CreateFmt('Tried to call external routine "%s" with no implementation.', [method.Name]);
   end;
   if assigned(method.ReturnType) then
      SetValue(l, retval);
end;

procedure TrsVM.PCall(l, r: integer);
var
   args: TArray<TValue>;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   FStack.Push(FContext);
   FParamLists.peek.Add(InvokeCode(GetIP + r, args));
   FContext := FStack.Pop;
end;

procedure TrsVM.PCallx(l, r: integer);
var
   args: TArray<TValue>;
   method: TRttiMethod;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   method := FParent.FExtRoutines[r];
   if method.IsStatic then
      FParamLists.peek.Add(RTTI.Invoke(method.CodeAddress, args, method.CallingConvention, method.ReturnType.Handle))
   else FParamLists.peek.Add(method.Invoke(GetSR, args));
end;

procedure TrsVM.ArrayLoad(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   if not (val.IsArray) then
      CorruptError;
   FContext.ar := val;
end;

procedure TrsVM.AssignIntToFloat(l: integer; r: integer);
begin
   SetValue(l, GetValue(r).AsExtended);
end;

procedure TrsVM.ArrayElemMove(l: integer; r: integer);
begin
   if not (assigned(FContext.ar) and (FContext.ar.IsArray)) then
      CorruptError;
   SetValue(l, FContext.ar.GetArrayElement(r));
end;

procedure TrsVM.ArrayElemAssignC(l: integer; r: integer);
begin
   if not (assigned(FContext.ar) and (FContext.ar.IsArray)) then
      CorruptError;
   FContext.ar.SetArrayElement(l, GetValue(r)^);
end;

procedure TrsVM.ArrayElemAssign(l: integer; r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   if val.kind <> tkInteger then
      CorruptError;
   ArrayElemAssignC(val.AsInteger, r);
end;

procedure TrsVM.LineCheck;
var
   newLine: TrsDebugLineInfo;
begin
   newLine := FParent.FProgram.OpcodeMap[GetIP];
   if FLastLine <> newLine then
   begin
      if assigned(FParent.FOnLine) then
         FParent.FOnLine(self, newLine);
      FLastLine :=  newLine;
   end;
end;

procedure TrsVM.LoadSelfRegister(l, r: integer);
var
   val: PValue;
begin
   val := GetValue(l);
   if not (val.IsObject) then
      CorruptError;
   FsrStack.Push(TsrPair.Create(val^, FDepth));
end;

procedure TrsVM.TruncReg(l, r: integer);
var
   val2: PValue;
begin
   val2 := GetValue(r);
   if val2.Kind = tkInteger then
      SetValue(l, val2^)
   else SetValue(l, trunc(val2.AsExtended));
end;

procedure TrsVM.PropAssign(l, r: integer);
var
   val2: PValue;
   sr: TObject;
   prop: TRttiProperty;
begin
   val2 := GetValue(r);
   sr := GetSR.AsObject;
   prop := FParent.FProgram.Properties.Objects[l] as TRttiProperty;
   prop.SetValue(sr, val2^);
end;

procedure TrsVM.ArrayPropAssign(l, r: integer);
var
   args: TArray<TValue>;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   FParent.FArrayProcTableW[l](GetSR, args, FContext.locals[r]);
end;

type TOpcodeProc = procedure (Self: TObject; l, r: integer);

{
const
  OP_PROCS: array[TrsOpcode] of pointer =
   (nil,                 //OP_NOP
    @TrsVM.AddRegs,      //OP_ADD
    @TrsVM.SubRegs,      //OP_SUB
    @TrsVM.MulRegs,      //OP_MUL
    @TrsVM.DivRegs,      //OP_DIV
    @TrsVM.FdivRegs,     //OP_FDIV
    @TrsVM.ModRegs,      //OP_MOD
    @TrsVM.AndRegs,      //OP_AND
    @TrsVM.OrRegs,       //OP_OR
    @TrsVM.XorRegs,      //OP_XOR
    @TrsVM.ShlRegs,      //OP_SHL
    @TrsVM.ShrRegs,      //OP_SHR
    nil,                 //OP_AS
    @TrsVM.StringConcat, //OP_SCAT
    @TrsVM.AddInt,       //OP_ADDI
    @TrsVM.SubInt,       //OP_SUBI
    @TrsVM.MulInt,       //OP_MULI
    @TrsVM.DivInt,       //OP_DIVI
    @TrsVM.ModInt,       //OP_MODI
    @TrsVM.AndInt,       //OP_ANDI
    @TrsVM.OrInt,        //OP_ORI
    @TrsVM.XorInt,       //OP_XORI
    @TrsVM.ShlInt,       //OP_SHLI
    @TrsVM.ShrInt,       //OP_SHRI
    @TrsVM.MovRegs,      //OP_MOV
    @TrsVM.MovConst,     //OP_MOVC
    @TrsVM.MovInt,       //OP_MOVI
    nil,                 //OP_MOVF
    @TrsVM.MovProp,      //OP_MOVP
    @TrsVM.MovArrayProp, //OP_MVAP
    @TrsVM.NegRegister,  //OP_NEG
    @TrsVM.NotRegister,  //OP_NOT
    @TrsVM.IncRegister,  //OP_INC
    @TrsVM.DecRegister,  //OP_DEC
    @TrsVM.CompGte,      //OP_GTE
    @TrsVM.CompLte,      //OP_LTE
    @TrsVM.CompGt,       //OP_GT
    @TrsVM.CompLt,       //OP_GT
    @TrsVM.CompEq,       //OP_EQ
    @TrsVM.CompNeq,      //OP_NEQ
    @TrsVM.CompGtei,     //OP_GTEI
    @TrsVM.CompLtei,     //OP_LTEI
    @TrsVM.CompGti,      //OP_GTI
    @TrsVM.CompLti,      //OP_GTI
    @TrsVM.CompEqi,      //OP_EQI
    @TrsVM.CompNeqi,     //OP_NEQI
    nil,                 //OP_IN
    nil,                 //OP_IS
    @TrsVM.XorbRegs,     //OP_XORB
    @TrsVM.NewList,      //OP_LIST
    @TrsVM.PushValue,    //OP_PUSH
    @TrsVM.PushInt,      //OP_PSHI
    @TrsVM.PushConst,    //OP_PSHC
    @TrsVM.Call,         //OP_CALL
    @TrsVM.CallX,        //OP_CALX
    nil,                 //OP_CALM
    @TrsVM.PCall,        //OP_PCAL
    @TrsVM.PCallX,       //OP_PCLX
    nil,                 //OP_PCLM
    nil,                 //OP_INIT
    nil,                 //OP_RET
    nil,                 //OP_JUMP
    nil,                 //OP_FJMP
    nil,                  //OP_TJMP
    @TrsVM.ArrayLoad,    //OP_ARYL
    @TrsVM.ArrayElemMove,//OP_ARYL
    @TrsVM.MovRegs,      //OP_VASN
    @TrsVM.AssignIntToFloat,//OP_AITF
    @TrsVM.ArrayElemAssign, //OP_EASN
    @TrsVM.ArrayElemAssignC,//OP_EASC
    nil,                    //OP_FASN
    @TrsVM.PropAssign,      //OP_PASN
    @TrsVM.ArrayPropAssign, //OP_APSN
    nil,                    //OP_TRYF
    nil,                    //OP_TRYE
    nil,                    //OP_TRYC
    nil,                    //OP_CTRY
    nil,                    //OP_EXIS
    nil,                    //OP_EXLD
    nil,                    //OP_RAIS
    @TrsVM.LoadSelfRegister,//OP_SRLD
    nil,                    //OP_MCLS
    @TrsVM.TruncReg         //OP_TRNC
    );
{
         OP_NOP:  ;
         OP_AS:   NotImplemented;
         OP_MOVF: NotImplemented;
         OP_IN:   NotImplemented;
         OP_IS:   NotImplemented;
         OP_CALM:  NotImplemented;
         OP_PCLM:  NotImplemented;
         OP_INIT: CorruptError;
         OP_RET:  Break;
                   //-1 because IP will increment after the loop
         OP_JUMP: inc(FContext.ip, op.left - 1);
         OP_FJMP: if not FContext.br then
                     inc(FContext.ip, op.left - 1);
         OP_TJMP: if FContext.br then
                     inc(FContext.ip, op.left - 1);
         OP_FASN: NotImplemented;
         OP_TRYF: NotImplemented;
         OP_TRYE: NotImplemented;
         OP_TRYC: NotImplemented;
         OP_CTRY: Break;
         OP_EXIS: NotImplemented;
         OP_EXLD: NotImplemented;
         OP_RAIS: NotImplemented;
         OP_MCLS: NotImplemented;

var
   OPCODES: array[TrsOpcode] of TOpcodeProc absolute OP_PROCS;
}

function TrsVM.RunLoop(resultIndex: integer; expected: TrsOpcode): TValue;
var
   op: PrsAsmInstruction;
begin
   repeat
      inc(FContext.ip);
      op := FContext.ip;
      LineCheck;
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
         OP_MULI: MulInt(op.left, op.right);
         OP_DIVI: DivInt(op.left, op.right);
         OP_MODI: ModInt(op.left, op.right);
         OP_ANDI: AndInt(op.left, op.right);
         OP_ORI:  OrInt(op.left, op.right);
         OP_XORI: XorInt(op.left, op.right);
         OP_SHLI: ShlInt(op.left, op.right);
         OP_SHRI: ShrInt(op.left, op.right);

         OP_MOV:  MovRegs(op.left, op.right);
         OP_MOVC: MovConst(op.left, op.right);
         OP_MOVB: MovBool(op.left, op.right);
         OP_MOVI: MovInt(op.left, op.right);
         OP_MOVF: NotImplemented;
         OP_MOVP: MovProp(op.left, op.right);
         op_MVAP: MovArrayProp(op.left, op.right);
         OP_MOVPSR: MovPropSR(op.left, op.right);
         OP_MVAPSR: MovArrayPropSR(op.left, op.right);
         OP_NEG:  NegRegister(op.left, op.right);
         OP_NOT:  NotRegister(op.left, op.right);
         OP_INC:  IncRegister(op.left, op.right);
         OP_DEC:  DecRegister(op.left, op.right);
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
         OP_EQB:  CompEqb(op.left, op.right);
         OP_NEQB: CompNeqb(op.left, op.right);
         OP_IN:   NotImplemented;
         OP_IS:   NotImplemented;
         OP_XORB: XorbRegs(op.left, op.right);
         OP_LIST: NewList(op.left, 0);
         OP_PUSH: PushValue(op.left, 0);
         OP_PSHI: PushInt(op.left, 0);
         OP_PSHC: PushConst(op.left, 0);
         OP_CALL: Call(op.left, op.right);
         OP_CALX: CallX(op.left, op.right);
         OP_PCAL: PCall(op.left, op.right);
         OP_PCLX: PCallX(op.left, op.right);
         OP_INIT: CorruptError;
         OP_RET:  Break;
                   //-1 because IP will increment after the loop
         OP_JUMP: inc(FContext.ip, op.left - 1);
         OP_FJMP: if not FContext.br then
                     inc(FContext.ip, op.left - 1);
         OP_TJMP: if FContext.br then
                     inc(FContext.ip, op.left - 1);
         OP_ARYL: ArrayLoad(op.left, 0);
         OP_ELEM: ArrayElemMove(op.left, op.right);
         OP_VASN: MovRegs(op.left, op.right);
         OP_AITF: AssignIntToFloat(op.left, op.right);
         OP_EASN: ArrayElemAssign(op.left, op.right);
         OP_EASC: ArrayElemAssignC(op.left, op.right);
         OP_FASN: NotImplemented;
         OP_PASN: PropAssign(op.left, op.right);
         OP_APSN: ArrayPropAssign(op.left, op.right);
         OP_TRYF: NotImplemented;
         OP_TRYE: NotImplemented;
         OP_TRYC: NotImplemented;
         OP_CTRY: Break;
         OP_EXIS: NotImplemented;
         OP_EXLD: NotImplemented;
         OP_RAIS: NotImplemented;
         OP_SRLD: LoadSelfRegister(op.left, 0);
         OP_MCLS: NotImplemented;
         OP_TRNC: TruncReg(op.left, op.right)
         else CorruptError;
      end;
   until false;
   if op.op <> expected then
      CorruptError;
   result := FContext.Locals[resultIndex];
end;

procedure TrsVM.InitializeRegs(const op:  TrsAsmInstruction; const Args: TArray<TValue>);
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

function TrsVM.InvokeCode(index: integer; const Args: TArray<TValue>): TValue;
begin
   inc(FDepth);
   if FDepth > FParent.FMaxStackDepth then
      raise ErsRuntimeError.CreateFmt('Script executor stack overflow at a depth of %d frames', [FDepth]);
   try
      FContext.ip := @FParent.FProgram.Text[index];
      InitializeRegs(FContext.ip^, args);
      result := RunLoop(length(args) + 1, OP_RET); //TODO: don't do this if it's a procedure
   finally
      dec(FDepth);
   end;
end;

function TrsVM.GetIP: integer;
begin
   result := (PByte(FContext.ip) - PByte(@FText[0])) div sizeof(TrsAsmInstruction);
end;

function TrsVM.GetSR: TValue;
begin
   if (FsrStack.Count = 0) or (FsrStack.Peek.Value <> FDepth) then
      CorruptError;
   result := FsrStack.Pop.Key;
end;

function TrsVM.GetValue(i: integer): PValue;
begin
   if i > 0 then
      result := @FContext.locals[i]
   else result := @FParent.FGlobals[-i];
end;

procedure TrsVM.InitializeReg(value: PValue; info: PTypeInfo);
var
   ValData: PValueData absolute value;
begin
   assert(not IsManaged(info));
   if valData.FValueData <> FParent.FBlankData then
   begin
      //minor optimization to avoid calling AddRef and Release if possible
      if assigned(valData.FValueData) then
         ValData.FValueData := FParent.FBlankData
      else pointer(ValData.FValueData) := pointer(FParent.FBlankData);
   end;
   ValData.FTypeInfo := info;
   valData.FAsExtended := 0;
end;

procedure TrsVM.SetValue(i: integer; const val: TValue);
begin
   if i = 0 then
      FContext.br := val.AsBoolean
   else AssignValue(GetValue(i), @val);
end;

procedure TrsVM.SetValue(i: integer; const val: integer);
var
   left: PValueData;
begin
   pointer(left) := GetValue(i);
   if not (assigned(left.FTypeInfo) and (left.FTypeInfo.kind = tkInteger)) then
      InitializeReg(pointer(left), TypeInfo(integer));
   left.FAsSLong := val;
end;

procedure TrsVM.SetValue(i: integer; const val: Extended);
var
   left: PValueData;
begin
   pointer(left) := GetValue(i);
   if not (assigned(left.FTypeInfo) and (left.FTypeInfo.kind = tkFloat)) then
      InitializeReg(pointer(left), TypeInfo(Extended));
   left.FAsExtended := val;
end;

class procedure TrsVM.AssignValue(l, r: PValue);
var
   lData: PValueData absolute l;
   rData: PValueData absolute r;
begin
   lData.FTypeInfo := rData.FTypeInfo;
   lData.FAsExtended := rData.FAsExtended;
   if IsManaged(rData.FTypeInfo) or (assigned(lData.FValueData) and (lData.FValueData <> TrsExec.FBlankData)) then
      lData.FValueData := rData.FValueData
   else pointer(lData.FValueData) := pointer(TrsExec.FBlankData);
end;

procedure TrsVM.RegState;
var
   i: integer;
begin
   for i := 0 to high(FContext.locals) do
      Writeln(format('%d: %s', [i, FContext.locals[i].ToString]));
end;

procedure TrsVM.DumpProgram;
var
   ms: TMemoryStream;
begin
   ms := TMemoryStream.Create;
   try
      ms.Write(FParent.FProgram.Text[0], length(FParent.FProgram.Text) * sizeof(TRSAsmInstruction));
      ms.SaveToFile(TPath.Combine(ExtractFilePath(ParamStr(0)), 'RS Dump.bin'));
   finally
      ms.Free;
   end;
end;

{ TrsVM.TSimpleParamList }

constructor TrsVM.TSimpleParamList.Create(size: integer);
begin
   SetLength(FValues, size);
end;

procedure TrsVM.TSimpleParamList.Add(const value: TValue);
begin
   TrsVM.AssignValue(@FValues[FCount], @value);
   inc(FCount);
end;

initialization
   PascalFormatSettings := FormatSettings;
   PascalFormatSettings.DecimalSeparator := '.';
end.
