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

unit rsImport;

interface
uses
   RTTI, TypInfo,
   rsDefs, rsDefsBackend;

type
   TTypeLookupFunc = reference to function(base: TRttiType): TTypeSymbol;
   TTypeRegisterProc = reference to procedure(value: TRttiType);
   TParseHeaderFunc = reference to function(const header: string;
     parent: TUnitSymbol; add: boolean): TProcSymbol;
   NoImportAttribute = rsDefsBackend.NoImportAttribute;

   IExternalType = interface
   ['{D1A64764-87B2-48EF-B2FC-641D31C3856A}']
      function GetInfo: TRttiType;
   end;

   TExternalType = class(TTypeSymbol, IExternalType)
   private
      FInfo: TRttiType;
      function GetInfo: TRttiType;
   protected
      function GetTypeInfo: PTypeInfo; override;
   public
      constructor Create(info: TRttiType);
   end;

   TExternalArrayType = class(TArrayTypeSymbol, IExternalType)
   private
      FInfo: TRttiType;
      function GetInfo: TRttiType;
   protected
      function GetTypeInfo: PTypeInfo; override;
   public
      constructor Create(info: TRttiType);
      function CanDerefArray: boolean; override;
   end;

   TExternalClassType = class(TClassTypeSymbol, IExternalType)
   private
      FInfo: TRttiType;
      function GetInfo: TRttiType;
   protected
      function GetTypeInfo: PTypeInfo; override;
   public
      constructor Create(info: TRttiInstanceType); overload;
      constructor Create(info: TRttiInstanceType; LookupType: TTypeLookupFunc;
        ParseHeader: TParseHeaderFunc; AddType: TTypeRegisterProc; parent: TUnitSymbol;
        baseType: TClassTypeSymbol); overload;
      procedure AddArrayProp(const name, indexType, propType: string;
        canRead, canWrite: boolean; default: boolean = false);
   end;

implementation
uses
   SysUtils;

{ TExternalType }

constructor TExternalType.Create(info: TRttiType);
begin
   assert(assigned(info));
   inherited Create(info.Name);
   FInfo := info;
end;

function TExternalType.GetInfo: TRttiType;
begin
   result := FInfo;
end;

function TExternalType.GetTypeInfo: PTypeInfo;
begin
   result := FInfo.Handle;
end;

{ TExternalClassType }

constructor TExternalClassType.Create(info: TRttiInstanceType);
var
   base: TClassTypeSymbol;
begin
   assert(assigned(info));
   inherited Create(info.Name);
   self.metaclass := info.MetaclassType;
   FInfo := info;
   FFieldCount := (length(info.GetFields));
   if NativeTypeDefined(info.BaseType) then
   begin
      base := TypeOfRttiType(info.BaseType) as TClassTypeSymbol;
      FMethodCount := base.methodCount;
      FPropCount := base.propCount;
   end;
end;

procedure TExternalClassType.AddArrayProp(const name, indexType, propType: string;
   canRead, canWrite, default: boolean);
var
   list: IParamList;
   propTypeSym, indexTypeSym: TTypeSymbol;
   readSpec, writeSpec: TSymbol;
begin
   if not (canRead or canWrite) then
      raise EParseError.CreateFmt('Array property "%s.%s%" must be readable or writable.', [self.name, name]);
   if default and assigned(FDefaultProperty) then
      raise EParseError.CreateFmt('Class %s already has a default property.', [self.name]);
   propTypeSym := FindNativeType(propType);
   indexTypeSym := FindNativeType(indexType);
   if canRead then
   begin
      list := EmptyParamList;
      list.Add(TParamSymbol.Create('index', indexTypeSym, [pfConst]));
      AddMethod(format('Get*%s', [name]), propTypeSym, mvPrivate, false, list);
      readSpec := self.SymbolLookup(UpperCase(format('Get*%s', [name])));
   end
   else readSPec := nil;
   if canWrite then
   begin
      list := EmptyParamList;
      list.Add(TParamSymbol.Create('index', indexTypeSym, [pfConst]));
      list.Add(TParamSymbol.Create('value', propTypeSym, [pfConst]));
      AddMethod(format('Set*%s', [name]), nil, mvPrivate, false, list);
      writeSpec := self.SymbolLookup(UpperCase(format('Set*%s', [name])));
   end
   else writeSpec := nil;
   list := EmptyParamList;
   list.Add(TParamSymbol.Create('index', indexTypeSym, [pfConst]));
   propTypeSym := MakeArrayPropType(propTypeSym);
   AddProp(name, propTypeSym, mvPublic, false, list, readSpec, writeSpec);
   if default then
      FDefaultProperty := self.SymbolLookup(UpperCase(name)) as TPropSymbol;
end;

constructor TExternalClassType.Create(info: TRttiInstanceType; LookupType: TTypeLookupFunc;
  ParseHeader: TParseHeaderFunc; AddType: TTypeRegisterProc; parent: TUnitSymbol;
  baseType: TClassTypeSymbol);

   procedure EnsureType(val: TRttiType);
   begin
      if not NativeTypeDefined(val) then
         AddType(val);
   end;

   function NoImport(obj: TRttiObject): boolean;
   var
      attr: TCustomAttribute;
   begin
      for attr in obj.GetAttributes do
         if attr is NoImportAttribute then
            exit(true);
      result := false;
   end;

var
   method: TRttiMethod;
   prop: TRttiProperty;
   proc: TProcSymbol;
   retval: TTypeSymbol;
   paramList: IParamList;
   param: TRttiParameter;
begin
   Create(info);
   if assigned(baseType) then
   begin
      self.AddField('*PARENT', baseType, mvProtected);
      self.parent := baseType;
   end;
   for method in info.GetDeclaredMethods do
   begin
      if method.Visibility in [mvPrivate, mvProtected] then
         Continue;
      if method.IsDestructor then
         Continue;
      if NoImport(method) then
         Continue;
      if assigned(method.ReturnType) then
      begin
         EnsureType(method.ReturnType);
         retval := LookupType(method.ReturnType)
      end
      else retval := nil;
      for param in method.GetParameters do
         EnsureType(param.ParamType);
      proc := ParseHeader(method.ToString, parent, false);
      paramList := proc.paramList;
      proc.Free;
      self.AddMethod(UpperCase(method.Name), retval, mvPublic, method.IsClassMethod, paramList);
   end;
   for prop in info.GetDeclaredProperties do
   begin
      if NoImport(prop) then
         Continue;
      EnsureType(prop.PropertyType);
      self.AddProp(prop as TRttiInstanceProperty);
   end;
end;

function TExternalClassType.GetInfo: TRttiType;
begin
   result := FInfo;
end;

function TExternalClassType.GetTypeInfo: PTypeInfo;
begin
   result := FInfo.Handle;
end;

{ TExternalArrayType }

constructor TExternalArrayType.Create(info: TRttiType);
begin
   if info is TRttiArrayType then
      inherited Create(1, TRttiArrayType(info).TotalElementCount, TypeOfRttiType(TRttiArrayType(info).ElementType))
   else if info is TRttiDynamicArrayType then
      inherited Create(0, 0, TypeOfRttiType(TRttiDynamicArrayType(info).ElementType))
   else raise Exception.Create('TExternalArrayType can only represent array types');
   FInfo := info;
//   FTypeInfo := info.handle;
end;

function TExternalArrayType.CanDerefArray: boolean;
begin
   result := true;
end;

function TExternalArrayType.GetInfo: TRttiType;
begin
   result := FInfo;
end;

function TExternalArrayType.GetTypeInfo: PTypeInfo;
begin
   result := FInfo.Handle;
end;

end.
