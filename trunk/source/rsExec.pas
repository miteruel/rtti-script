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
   newClass, rsDefsBackend;

type
   TrsExec = class;

   TExecImportCall = reference to procedure(const name: string; address: pointer);
   TrsExecImportProc = reference to procedure(RegisterFunction: TExecImportCall);

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
   private
      FParent: TrsExec;
      FDepth: integer;
      FStack: TStack<TVmContext>;
      FSrStack: TStack<TsrPair>;
      FContext: TVmContext;
      FParamLists: TObjectStack<TSimpleParamList>;
      FText: TArray<TrsAsmInstruction>;

      function InvokeCode(index: integer; const Args: TArray<TValue>): TValue;
      function RunLoop(resultIndex: integer; expected: TrsOpcode): TValue;
      procedure InitializeRegs(const op: TrsAsmInstruction;
        const Args: TArray<TValue>);

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
      procedure MulInt(l, r: integer);
      procedure ModInt(l, r: integer);
      procedure AndInt(l, r: integer);
      procedure OrInt(l, r: integer);
      procedure XorInt(l, r: integer);
      procedure ShlInt(l, r: integer);
      procedure ShrInt(l, r: integer);
      procedure MovRegs(l, r: integer);
      procedure MovConst(l, r: integer);
      procedure MovInt(l, r: integer);
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
      function RegAsBoolean(index: integer): boolean;
      procedure XorbRegs(l, r: integer);
      procedure NewList(l: integer);
      procedure PushValue(l: integer);
      procedure Call(l, r: integer);
      procedure Callx(l, r: integer);
      procedure PCall(l, r: integer);
      procedure PCallx(l, r: integer);
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
      FImpls: TObjectList<TMethodImplementation>;
      FText: TArray<TrsAsmInstruction>;
      FEnvironment: TObject;
      FNewClasses: TList<TClass>;
      FLastImport: TPair<string, TDictionary<string, pointer>>;
      FMaxStackDepth: integer;
      FVmMap: TObjectDictionary<cardinal, TrsVm>;

      class var
      FBlank: TValue;
      FBlankData: IValueData;

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
   end;

   ErsRuntimeError = class(Exception);

implementation
uses
   StrUtils, Types, Math, Classes, tlHelp32,
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
   FVmMap := TObjectDictionary<cardinal, TrsVm>.Create;
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
begin
   result := GetVM.InvokeCode(NativeInt(UserData), args);
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
{Not ready for this yet; requires support in codegen first}
//       tkFloat: result := StrToFloat(value, PascalFormatSettings);
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

procedure TrsExec.LoadEnvironment;
var
   info: TrsProcInfo;
   methods: TArray<TRttiMethod>;
   method: TRttiMethod;
   classdot: string;
   i: integer;
begin
   if assigned(FEnvironment) then
   begin
      classdot := FEnvironment.ClassName + '.';
      methods := TRttiContext.Create.GetType(FEnvironment.ClassInfo).GetMethods;
      SetLength(FExtRoutines, length(methods));
      i := 0;
      for method in methods do
         if FProgram.Routines.TryGetValue(classdot + UpperCase(method.Name), info) then
         begin
            i := min(i, info.index);
            FExtRoutines[-info.index] := method;
         end;
      SetLength(FExtRoutines, succ(-i));
   end;
end;

procedure TrsExec.Load(prog: TrsProgram);
begin
   assert(FProgram = nil); //fix this after changing to interfaces
   FProgram := prog;
   FText := FProgram.Text;
   LoadEnvironment;
   LoadProcs;
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
begin
   val := GetValue(l);
   SetValue(l, val.AsInteger div GetValue(r).AsInteger);
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

procedure TrsVM.MovInt(l, r: integer);
begin
   SetValue(l, r);
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
      else CorruptError;
   end;
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

procedure TrsVM.NewList(l: integer);
begin
   FParamLists.Push(TSimpleParamList.Create(l));
end;

procedure TrsVM.PushValue(l: integer);
begin
   FParamLists.peek.Add(GetValue(l)^);
end;

procedure TrsVM.PushInt(l: integer);
begin
   FParamLists.peek.Add(l);
end;

procedure TrsVM.PushConst(l: integer);
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
   //TODO: this will break if a function returns nil. Hooray for the semipredicate problem!  Fix this.
   if not retval.IsEmpty then
      SetValue(l, retval);
end;

procedure TrsVM.Callx(l, r: integer);
var
   args: TArray<TValue>;
   method: TRttiMethod;
   handle: PTypeInfo;
   retval: TValue;
begin
   args := FParamLists.Peek.ToArray;
   FParamLists.Pop;
   method := FParent.FExtRoutines[r];
   if method.IsStatic then
   begin
      if assigned(method.ReturnType) then
         handle := method.ReturnType.Handle
      else handle := nil;
      retval := RTTI.Invoke(method.CodeAddress, args, method.CallingConvention, Handle)
   end
   else retval := method.Invoke(GetSR, args);
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

procedure TrsVM.ArrayLoad(l: integer);
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

procedure TrsVM.LoadSelfRegister(l: integer);
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

function TrsVM.RunLoop(resultIndex: integer; expected: TrsOpcode): TValue;
var
   op: PrsAsmInstruction;
begin
   repeat
      inc(FContext.ip);
      op := FContext.ip;
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
         OP_MOVI: MovInt(op.left, op.right);
         OP_MOVF: NotImplemented;
         OP_MOVP: NotImplemented;
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
         OP_IN:   NotImplemented;
         OP_IS:   NotImplemented;
         OP_XORB: XorbRegs(op.left, op.right);
         OP_LIST: NewList(op.left);
         OP_PUSH: PushValue(op.left);
         OP_PSHI: PushInt(op.left);
         OP_PSHC: PushConst(op.left);
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
   AssignValue(GetValue(i), @val);
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
