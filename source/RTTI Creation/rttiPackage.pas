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

unit rttiPackage;

interface
uses
   Generics.Collections, PrivateHeap, RTTI,
   NewClass;

type
   TGetCodeAddressProc = reference to procedure(const unitName: string; method: TRttiMethod; var address: pointer);

   INewUnit = interface
      procedure AddClass(value: TNewClass);
      function GetEnumerator: TEnumerator<TNewClass>;
      function GetName: string;
      function ClassCount: integer;
      property name: string read GetName;
   end;

   IRttiPackage = interface
      procedure AddUnit(const newUnit: INewUnit);
      procedure Install(const GetCodeAddressProc: TGetCodeAddressProc);
      function GetName: string;
      property Name: string read GetName;
   end;

   TRttiNewUnit = class(TInterfacedObject, INewUnit)
   private
      FName: string;
      FClasses: TObjectList<TNewClass>;
      function GetName: string;
      function ClassCount: integer;
   public
      constructor Create(const name: string);
      destructor Destroy; override;

      procedure AddClass(value: TNewClass);
      function GetEnumerator: TEnumerator<TNewClass>;
      property name: string read GetName;
   end;

   TRttiPackage = class(TInterfacedObject, IRttiPackage)
   private type
      TNewClassMap = class(TObjectDictionary<string, TList<TClass>>)
      public
         procedure Add(const name: string; value: TClass);
      end;
   private
      FName: string;
      FUnits: TList<INewUnit>;
      FHeap: TPrivateHeap;
      FLib: TLibModule;
      FNewClasses: TNewClassMap;
      procedure PrepareLibModule;
      function BuildTypeTable: PPackageTypeInfo;
      function GetTypeCount: integer;
      function BuildUnitNameString: utf8String;
      function GetName: string;
   public
      constructor Create(const name: string);
      destructor Destroy; override;
      procedure AddUnit(const newUnit: INewUnit);
      procedure Install(const GetCodeAddressProc: TGetCodeAddressProc);
      property Name: string read GetName;
   end;

implementation
uses
   SysUtils, TypInfo;

{ TRttiNewUnit }

function TRttiNewUnit.ClassCount: integer;
begin
   result := FClasses.Count;
end;

constructor TRttiNewUnit.Create(const name: string);
begin
   FName := name;
   FClasses := TObjectList<TNewClass>.Create;
end;

destructor TRttiNewUnit.Destroy;
begin
   FClasses.Free;
   inherited Destroy;
end;

function TRttiNewUnit.GetEnumerator: TEnumerator<TNewClass>;
begin
   result := FClasses.GetEnumerator;
end;

function TRttiNewUnit.GetName: string;
begin
   result := FName;
end;

procedure TRttiNewUnit.AddClass(value: TNewClass);
begin
   FClasses.Add(value);
end;

{ TRttiPackage }

constructor TRttiPackage.Create(const name: string);
begin
   FName := name;
   FUnits := TList<INewUnit>.Create;
   FHeap := TPrivateHeap.Create(0, false);
   FNewClasses := TNewClassMap.Create;
end;

destructor TRttiPackage.Destroy;
begin
   if assigned(FLib.TypeInfo) then
      UnregisterModule(@FLib);
   FNewClasses.Free;
   FUnits.Free;
   FHeap.Free;
   inherited Destroy;
end;

function TRttiPackage.GetName: string;
begin
   result := FName;
end;

function TRttiPackage.GetTypeCount: integer;
var
   newUnit: INewUnit;
begin
   result := 0;
   for newUnit in FUnits do
      inc(result, newUnit.classCount);
end;

function TRttiPackage.BuildUnitNameString: utf8String;
var
   newUnit: INewUnit;
   newUnitString: utf8String;
   unitIdx: integer;
begin
   result := '';
   for newUnit in FUnits do
   begin
      //ugly, but avoids potentially-breaky conversions to UnicodeString and back
      unitIdx := length(result) + 1;
      newUnitString := utf8String(newUnit.name);
      setLength(result, length(result) + length(newUnitString) + 1);
      result[unitIdx] := ansiChar(length(newUnitString));
      inc(unitIdx);
      move(newUnitString[1], result[unitIdx], length(newUnitString));
   end;
end;

procedure TRttiPackage.AddUnit(const newUnit: INewUnit);
begin
   FUnits.Add(newUnit);
end;

function TRttiPackage.BuildTypeTable: PPackageTypeInfo;
var
   scriptClass: TNewClass;
   newUnit: INewUnit;
   i: integer;
   newClass: TClass;
   unitNameString: utf8String;
begin
   FHeap.GetMem(result, sizeof(TPackageTypeInfo));
   result.TypeCount := GetTypeCount;
   FHeap.GetMem(result.TypeTable, result.TypeCount * sizeof(pointer));
   i := -1;
   for newUnit in FUnits do
      for scriptClass in newUnit do
      begin
         inc(i);
         newClass := scriptClass.classPtr(FHeap);
         result.TypeTable^[i * 2] := newClass.ClassInfo;
         result.TypeTable^[i * 2 + 1] := pointer(1);
         FNewClasses.Add(newUnit.name, newClass);
      end;
   unitNameString := BuildUnitNameString;
   FHeap.GetMem(result.UnitNames, length(unitNameString));
   result.UnitCount := FUnits.Count;
   Move(unitNameString[1], result.UnitNames^, length(unitNameString));
end;

procedure TRttiPackage.PrepareLibModule;
var
   instance: NativeUInt;
begin
   FLib.Next := nil;
   FLib.TypeInfo := BuildTypeTable;
   instance := NativeUint(FLib.TypeInfo);
   FLib.Instance := instance;
   FLib.CodeInstance := instance;
   FLib.DataInstance := instance;
   FLib.ResInstance := 0;
   FLib.Reserved := 0;
   system.RegisterModule(@FLib);
end;

type
   TPointers = array[0..0] of pointer;
   PPointers = ^TPointers;

procedure TRttiPackage.Install(const GetCodeAddressProc: TGetCodeAddressProc);
var
   ctx: TRttiContext;
   pair: TPair<string, TList<TClass>>;
   cls: TClass;
   method: TRttiMethod;
   vmt: PPointers;
begin
   if assigned(FLib.TypeInfo) then
      raise Exception.CreateFmt('Rtti Package %s is already installed.', [FName]);
   PrepareLibModule;
   ctx := TRttiContext.Create;
   ctx.GetPackages; //workaround for package sorting error
   if assigned(GetCodeAddressProc) then
      for pair in FNewClasses do
         for cls in pair.Value do
         begin
            vmt := pointer(cls);
            for method in ctx.GetType(cls).GetDeclaredMethods do
            begin
               GetCodeAddressProc(pair.Key, method, PVmtMethodExEntry(method.Handle).Entry.CodeAddress);
               if method.DispatchKind = dkVtable then
                  vmt[method.VirtualIndex] := method.CodeAddress;
            end;
         end;
end;

{ TRttiPackage.TNewClassMap }

procedure TRttiPackage.TNewClassMap.Add(const name: string; value: TClass);
var
   list: TList<TClass>;
begin
   if not self.TryGetValue(name, list) then
   begin
      list := TList<TClass>.Create;
      inherited Add(name, list);
   end;
   list.Add(value);
end;

end.
