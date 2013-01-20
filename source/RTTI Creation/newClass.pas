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

unit newClass;

{$IF CompilerVersion < 21}
   {$MESSAGE FATAL 'This unit requires Delphi 2010 or later.'}
{$IFEND}

interface

uses
  TypInfo, Classes, Math, Generics.Collections, PrivateHeap,
  vmtStructure, vmtBuilder;

type
  TNewMember = class
  protected
    FTypeDataSize: word;
  end;

  TNewField = class(TNewMember)
  private
    FData: PFieldExEntry;
  public
    constructor Create(data: PFieldExEntry; dataSize: word);
    destructor Destroy; override;
    function IsManaged: boolean;
  end;

  TNewMethod = class(TNewMember)
  private
    FData: PVmtMethodEntry;
    FFlags: word;
    FIndex: smallint;
  public
    constructor Create(data: PVmtMethodEntry; dataSize: word; flags: word;
      virtualIndex: smallint);
    destructor Destroy; override;
  end;

  TNewFieldList = TObjectList<TNewField>;
  TNewMethodList = TObjectList<TNewMethod>;

  TNewClass = class
  private
    FParent: TClass;
    FClassName: string;
    FVMT: array of pointer;
    FExtraInstanceSize: cardinal;
    FNewFields: TNewFieldList;
    FNewMethods: TNewMethodList;
    FManagedFields: TList;
    FClassPtr: TClass;

    function offset: Integer;
    procedure AddNewField(name: string; info: PTypeInfo; size: Integer);
    procedure AddManagedField(info: PTypeInfo);
    function PrepareManagedFields(const buffer: IBuffer): integer;
    function PrepareNewFields(const buffer: IBuffer): integer;
    function PrepareNewMethods(const buffer: IBuffer): integer;
    procedure FixMethodRtti(overlay: PVmt; offset: integer);
    function GetMaxVMIndex: integer;
  public
    constructor Create(parent: TClass; classname: string);
    destructor Destroy; override;

    procedure AddCharField(name: string);
    procedure AddWideCharField(name: string);
    procedure AddAnsiCharField(name: string);

    procedure AddStringField(name: string);
    procedure AddUnicodeStringField(name: string);
    procedure AddAnsiStringField(name: string);

    procedure AddIntegerField(name: string);
    procedure AddOrdField(name: string; kind: TOrdType);
    procedure AddEnumField(name: string; info: PTypeInfo);
    procedure AddFloatField(name: string; kind: TFloatType);
    procedure AddInt64Field(name: string);

    procedure AddVariantField(name: string);
    procedure AddDynArrayField(name: string; info: PTypeInfo);
    procedure AddInterfaceField(name: string; info: PTypeInfo);
    procedure AddRecordField(name: string; info: PTypeInfo);

    procedure AddObjectField(name: string; objectClass: TClass);
    procedure AddClassRefField(name: string; classType: TClass);
    procedure AddPointerField(name: string; info: PTypeInfo);

    procedure AddMethod(data: PVmtMethodEntry; flags: word; virtualIndex: smallint);

    function classPtr(ph: TPrivateHeap = nil): TClass;
  end;

const
  VMT_SIZE = sizeof(TVmt) div sizeof(pointer);

function GetVirtualMethodCount(AClass: TClass): Integer;

{ ****************************************************************************** }
implementation

function GetVirtualMethodCount(AClass: TClass): Integer;
var
  BeginVMT: NativeInt;
  EndVMT: NativeInt;
  TablePointer: NativeInt;
  I: Integer;
begin
  BeginVMT := NativeInt(AClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PNativeInt(NativeInt(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + sizeof(pointer);
  repeat
    TablePointer := PNativeInt(NativeInt(AClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
      (TablePointer < EndVMT) then
      EndVMT := NativeInt(TablePointer);
    Inc(I, sizeof(pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div sizeof(pointer);
end;

{ TNewClass }

procedure TNewClass.AddAnsiCharField(name: string);
begin
  AddNewField(name, TypeInfo(ansiChar), 1);
end;

procedure TNewClass.AddWideCharField(name: string);
begin
  AddNewField(name, TypeInfo(wideChar), 2);
end;

procedure TNewClass.AddCharField(name: string);
begin
{$IFNDEF UNICODE}
  AddAnsiCharField(name);
{$ELSE}
  AddWideCharField(name);
{$ENDIF}
end;

procedure TNewClass.AddDynArrayField(name: string; info: PTypeInfo);
begin
  assert(info.kind = tkDynArray);
  AddManagedField(info);
  AddNewField(name, info, sizeof(pointer));
end;

procedure TNewClass.AddRecordField(name: string; info: PTypeInfo);
begin
  assert(info.kind = tkRecord);
  if (GetTypeData(info).ManagedFldCount > 0) then
    AddManagedField(info);
  AddNewField(name, info, GetTypeData(info).RecSize);
end;

procedure TNewClass.AddAnsiStringField(name: string);
begin
  AddManagedField(TypeInfo(ansiString));
  AddNewField(name, TypeInfo(ansiString), sizeof(pointer));
end;

procedure TNewClass.AddUnicodeStringField(name: string);
begin
  AddManagedField(TypeInfo(unicodeString));
  AddNewField(name, TypeInfo(unicodeString), sizeof(pointer));
end;

procedure TNewClass.AddStringField(name: string);
begin
{$IFNDEF UNICODE}
  AddAnsiStringField(name);
{$ELSE}
  AddUnicodeStringField(name);
{$ENDIF}
end;

procedure TNewClass.AddVariantField(name: string);
begin
  AddManagedField(TypeInfo(variant));
  AddNewField(name, TypeInfo(variant), sizeof(variant));
end;

procedure TNewClass.AddInt64Field(name: string);
begin
  AddNewField(name, TypeInfo(int64), sizeof(int64));
end;

procedure TNewClass.AddInterfaceField(name: string; info: PTypeInfo);
begin
  assert(info.kind = tkInterface);
  AddManagedField(info);
  AddNewField(name, info, sizeof(IInterface));
end;

procedure TNewClass.AddObjectField(name: string; objectClass: TClass);
begin
  AddNewField(name, objectClass.ClassInfo, sizeof(TObject));
end;

procedure TNewClass.AddClassRefField(name: string; classType: TClass);
begin
  AddNewField(name, classType.ClassInfo, sizeof(TClass));
end;

procedure TNewClass.AddPointerField(name: string; info: PTypeInfo);
begin
  assert(info.kind = tkPointer);
  AddNewField(name, info, sizeof(pointer));
end;

const
  ORD_SIZES: array [TOrdType] of byte = (1, 1, 2, 2, 4, 4);

procedure TNewClass.AddEnumField(name: string; info: PTypeInfo);
var
  size: Integer;
  data: PTypeData;
begin
  assert(info.kind = tkEnumeration);
  data := GetTypeData(info);
  if data.BaseType^ = TypeInfo(boolean) then
    size := sizeof(boolean)
  else
    size := ORD_SIZES[GetTypeData(data.BaseType^).OrdType];
  AddNewField(name, info, size);
end;

procedure TNewClass.AddIntegerField(name: string);
begin
  AddOrdField(name, otSlong);
end;

{$WARN USE_BEFORE_DEF OFF}
procedure TNewClass.AddOrdField(name: string; kind: TOrdType);
var
  info: PTypeInfo;
begin
  case kind of
    otSByte:
      info := TypeInfo(shortInt);
    otUByte:
      info := TypeInfo(byte);
    otSWord:
      info := TypeInfo(smallInt);
    otUWord:
      info := TypeInfo(word);
    otSLong:
      info := TypeInfo(Integer);
    otULong:
      info := TypeInfo(cardinal);
  else
    assert(false);
  end;
  AddNewField(name, info, ORD_SIZES[kind]);
end;

const
  FLOAT_SIZES: array [TFloatType] of byte =
    (sizeof(single), sizeof(double), sizeof(extended), sizeof(comp), sizeof
      (currency));

procedure TNewClass.AddFloatField(name: string; kind: TFloatType);
var
  info: PTypeInfo;
begin
  case kind of
    ftSingle:
      info := TypeInfo(single);
    ftDouble:
      info := TypeInfo(double);
    ftExtended:
      info := TypeInfo(extended);
    ftComp:
      info := TypeInfo(comp);
    ftCurr:
      info := TypeInfo(currency);
  else
    assert(false);
  end;
  AddNewField(name, info, FLOAT_SIZES[kind]);
end;
{$WARN USE_BEFORE_DEF ON}

procedure TNewClass.AddManagedField(info: PTypeInfo);
var
  field: PFieldInfo;
begin
  new(field);
  field.TypeInfo := retrieveTypeInfo(info);
  field.offset := self.offset;
  FManagedFields.Add(field);
end;

procedure TNewClass.AddMethod(data: PVmtMethodEntry; flags: word; virtualIndex: smallint);
begin
   FNewMethods.Add(TNewMethod.Create(data, data.Len + sizeof(data), flags, VirtualIndex));
end;

procedure TNewClass.AddNewField(name: string; info: PTypeInfo; size: Integer);
var
  entry: PFieldExEntry;
  bufferSize: Integer;
begin
  assert(size > 0);
  entry := CreateFieldExInfo(name, info, self.offset, bufferSize);
  FNewFields.Add(TNewField.Create(entry, bufferSize));
  Inc(FExtraInstanceSize, size);
end;

function TNewClass.PrepareManagedFields(const buffer: IBuffer): integer;
var
  x: word;
  dummySize: cardinal;
  i: integer;
begin
  if FManagedFields.Count > 0 then
  begin
    result := buffer.size;
    // first two values are set up this way so you can overlay a TTypeInfo
    // on the record and get (tkRecord, 0) (See System._FinalizeRecord)
    x := ord(tkRecord);
    dummySize := 0;
    buffer.Add(x, sizeof(word));
    buffer.Add(dummySize, sizeof(cardinal));
    buffer.Add(FManagedFields.Count, sizeof(cardinal));
    for I := 0 to FManagedFields.Count - 1 do
      buffer.Add(PFieldInfo(FManagedFields[I])^, sizeof(TFieldInfo));
  end
  else result := 0;
end;

function TNewClass.PrepareNewFields(const buffer: IBuffer): integer;
var
  nullPtr: pointer;
  i: integer;
begin
  if FNewFields.Count > 0 then
  begin
    result := buffer.size;
    buffer.add(NULL_WORD, sizeof(word));
    nullPtr := nil;
    buffer.add(nullPtr, sizeof(pointer));
    buffer.add(FNewFields.Count, sizeof(word));
    for I := 0 to FNewFields.Count - 1 do
      buffer.add(FNewFields[i].FData^, FNewFields[i].FTypeDataSize);
  end
  else result := 0;
end;

function TNewClass.PrepareNewMethods(const buffer: IBuffer): integer;
var
  i: integer;
  offsets: array of integer;
  offset: nativeInt;
begin
  if FNewMethods.Count > 0 then
  begin
    setLength(offsets, FNewMethods.Count);
    for I := 0 to FNewMethods.Count - 1 do
    begin
      offsets[i] := buffer.size;
      buffer.add(FNewMethods[i].FData^, FNewMethods[i].FTypeDataSize);
    end;
    result := buffer.size;
    buffer.add(NULL_WORD, sizeof(word));
    buffer.add(FNewMethods.Count, sizeof(word));
    for I := 0 to FNewMethods.Count - 1 do
    begin
      offset := buffer.size - offsets[i];
      buffer.add(offset, sizeof(nativeInt));
      buffer.add(FNewMethods[i].FFlags, sizeof(word));
      buffer.add(FNewMethods[i].FIndex, sizeof(smallint));
    end;
  end
  else result := 0;
end;

{$R-}
procedure TNewClass.FixMethodRtti(overlay: PVmt; offset: integer);
var
   table: PVmtMethodTableEx;
   i: integer;
   entryOffset: nativeInt;
begin
   nativeInt(overlay.MethodTable) := nativeInt(pointer(overlay)) + offset;
   assert(overlay.MethodTable.Count = 0);
   nativeInt(table) := nativeInt(overlay.MethodTable) + sizeof(word);
   for i := 0 to table.Count - 1 do
   begin
      entryOffset := nativeInt(table.Entry[i].Entry);
      nativeInt(table.Entry[i].Entry) := nativeInt(@table.Entry[i].Entry) - entryOffset;
   end;
end;
{$R+}

function TNewClass.GetMaxVMIndex: integer;
var
   new: TNewMethod;
begin
   result := 0;
   for new in FNewMethods do
      result := max(result, new.FIndex)
end;

function TNewClass.classPtr(ph: TPrivateHeap): TClass;
var
  overlay, overlayParent: PVmt;
  reader, writer: PPointer;
  I, bufferSize: integer;
  mfOffset, ftOffset, nameOffset, typeOffset, methodOffset: nativeInt;
  parentVMs, extraVMs: integer;
  name: ShortString;
  buffer: IBuffer;
  newVMT: pointer;
begin
  if assigned(FClassPtr) then
    Exit(FClassPtr);

  parentVMs := GetVirtualMethodCount(FParent);
  extraVMs := max(GetMaxVMIndex - parentVMs, 0);
  setLength(FVMT, VMT_SIZE + parentVMs + extraVMs);

  // copy the FParent's built-in metadata
  overlayParent := vmtOfClass(FParent);
  overlay := @FVMT[0];
  overlay^ := overlayParent^;

  // set up user-defined virtuals
  reader := PPointer(FParent);
  writer := @overlay.Destroy;
  Inc(writer);
  for I := 1 to GetVirtualMethodCount(FParent) do
  begin
    writer^ := reader^;
    Inc(reader);
    Inc(writer);
  end;

  buffer := newBuffer;
  buffer.Add(FVMT[0], length(FVMT) * sizeof(pointer));
  mfOffset := PrepareManagedFields(buffer);
  ftOffset := PrepareNewFields(buffer);
  methodOffset := PrepareNewMethods(buffer);

  nameOffset := buffer.size;

  // place class name pointer at the end of the list
  name := UTF8EncodeToShortString(FClassName);
  buffer.add(name, length(name) + 1);

  typeOffset := vmtBuilder.CreateClassInfo(FClassname, FParent, 0, buffer);

  newVMT := buffer.GetBuffer(bufferSize, ph);
  overlay := pointer(newVMT);

  // set up the VMT with new information
  // overlay.SelfPtr := newClass;
  overlay.IntfTable := nil;
  overlay.AutoTable := nil;
  if mfOffset > 0 then
    nativeInt(overlay.InitTable) := nativeInt(newVMT) + mfOffset
  else overlay.InitTable := nil;
  if ftOffset > 0 then
    nativeInt(overlay.FieldTable) := nativeInt(newVMT) + ftOffset
  else overlay.FieldTable := nil;
  if methodOffset > 0 then
    fixMethodRtti(overlay, methodOffset)
  else overlay.MethodTable := nil;
  overlay.DynamicTable := nil;
  nativeInt(overlay.classname) := nativeInt(newVMT) + nameOffset;
  overlay.InstanceSize := overlayParent.InstanceSize + FExtraInstanceSize;
  overlay.TypeInfo := pointer(nativeInt(overlay) + typeOffset);
  overlay.parent := @overlayParent.SelfPtr;
  // keep FastMM from complaining about all this garbage
  RegisterExpectedMemoryLeak(newVMT);
  nativeInt(result) := nativeInt(overlay) + sizeof(TVmt);
  getTypeData(overlay.TypeInfo).ClassType := pointer(result);
  FClassPtr := result;
end;

constructor TNewClass.Create(parent: TClass; classname: string);
begin
  FParent := parent;
  FClassName := classname;
  FNewFields := TNewFieldList.Create;
  FManagedFields := TList.Create;
  FNewMethods := TNewMethodLIst.Create;
end;

destructor TNewClass.Destroy;
var
  field: pointer;
begin
  FNewMethods.Free;
  FNewFields.Free;
  for field in FManagedFields do
    FreeMem(field);
  FManagedFields.Free;
  inherited Destroy;
end;

function TNewClass.offset: Integer;
begin
  Result := FParent.InstanceSize + integer(FExtraInstanceSize);
  dec(Result, sizeof(pointer)); //compensate for TMonitor pointer
end;

{ TNewField }

constructor TNewField.Create(data: PFieldExEntry; dataSize: word);
begin
  FData := data;
  FTypeDataSize := dataSize;
end;

destructor TNewField.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

function TNewField.IsManaged: boolean;
const
  MANAGED_TYPES = [tkString, tkLString, tkWString, tkVariant, tkInterface,
    tkDynArray, tkUString];
begin
  Result := FData.TypeRef^.kind in MANAGED_TYPES;
end;

{ TNewMethod }

constructor TNewMethod.Create(data: PVmtMethodEntry; dataSize: word; flags: word;
  virtualIndex: smallint);
begin
  FData := data;
  FTypeDataSize := dataSize;
  FFlags := flags;
  FIndex := virtualIndex;
end;

destructor TNewMethod.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

end.
