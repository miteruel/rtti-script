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

unit vmtBuilder;

interface
uses
  TypInfo, PrivateHeap,
  vmtStructure;

const
  NULL_WORD: word = 0;

type
  IBuffer = interface
    procedure add(const data; const size: integer);
    function GetBuffer(out size: integer; ph: TPrivateHeap = nil): pointer;
    function size: integer;
    procedure CopyTo(const Dest: IBuffer);
    function current: pointer;
  end;

function newBuffer: IBuffer;

function CreateClassInfo(const name: String; parent: TClass; newProperties: Word;
  const buffer: IBuffer): integer;

function CreateFieldExInfo(const name: string; info: PTypeInfo; offset: integer; out size: integer): PFieldExEntry;

function CreateMethodParam(flags: TParamFlags; &type: PTypeInfo; parOff: word;
  const name: string): IBuffer;

function CreateMethodInfo(address: pointer; const name: string; CC: TCallConv;
  retval: PTypeInfo; const params: array of IBuffer): PVmtMethodEntry;

function retrieveTypeInfo(const info: PTypeInfo): PPtypeInfo;

implementation
uses
  SysUtils, Classes, Math;

const
  MAX_BUFFER_SIZE = 2048 * 5;

type
  TBuffer = class(TInterfacedObject, IBuffer)
  private
    FBuffer: array[0..MAX_BUFFER_SIZE - 1] of byte;
    FIndex: integer;
  public
    procedure add(const data; const size: integer);
    function GetBuffer(out size: integer; ph: TPrivateHeap = nil): pointer;
    function size: integer;
    procedure CopyTo(const Dest: IBuffer);
    function current: pointer;
  end;

function newBuffer: IBuffer;
begin
  result := TBuffer.Create;
end;

{ Builder functions }

function CreateClassInfo(const name: String; parent: TClass; newProperties: Word;
  const buffer: IBuffer): integer;
var
  kind: TTypeKind;
  shortName: shortString;
  selfPtr: pointer;
  parentInfo: pointer;
  parentData: PClassData;
  propCount: smallint;
begin
  result := buffer.size;
  kind := tkClass;
  shortName := UTF8EncodeToShortString(name);
  selfPtr := nil; //this value isn't known yet and has to be changed later
  parentInfo := @vmtOfClass(parent).TypeInfo;
  parentData := GetClassData(parent);
  propCount := parentData.PropCount + newProperties;

  buffer.add(kind, sizeof(TTypeKind));
  buffer.add(shortName, length(name) + 1);
  buffer.add(selfPtr, sizeof(pointer));
  buffer.add(parentInfo, sizeof(pointer));
  buffer.add(propCount, sizeof(smallint));
  buffer.add(parentData.UnitName, length(parentData.UnitName) + 1);
end;

function CreateFieldExInfo(const name: string; info: PTypeInfo; offset: integer;
  out size: integer): PFieldExEntry;
var
  buffer: IBuffer;
  flags: byte;
  pInfo: PPTypeInfo;
  shortName: ShortString;
  attrLen: word;
begin
  buffer := newBuffer;
  flags := 0;
  PInfo := retrieveTypeInfo(info);
  shortName := UTF8Encode(name);
  attrLen := sizeof(word); //blank attribute section

  buffer.add(flags, sizeof(byte));
  buffer.add(pInfo, sizeof(pointer));
  buffer.add(offset, sizeof(integer));
  buffer.add(shortName, length(shortName) + 1);
  buffer.add(attrLen, sizeof(word));
  result := buffer.GetBuffer(size);
end;

function CreateMethodParam(flags: TParamFlags; &type: PTypeInfo; parOff: word;
  const name: string): IBuffer;
var
  typePtr: PPTypeInfo;
  shortName: ShortString;
  attrLen: word;
begin
  result := newBuffer;
  typePtr := retrieveTypeInfo(&type);
  attrLen := sizeof(word); //blank attribute section
  shortName := UTF8Encode(name);

  result.add(flags, sizeof(TParamFlags));
  result.add(typePtr, sizeof(pointer));
  result.add(parOff, sizeof(word));
  result.add(shortName, length(shortName) + 1);
  result.add(attrLen, sizeof(word));
end;

function CreateMethodInfo(address: pointer; const name: string; CC: TCallConv;
  retval: PTypeInfo; const params: array of IBuffer): PVmtMethodEntry;
var
  buffer: IBuffer;
  length: word;
  shortName: ShortString;
  tail: TVmtMethodEntryTail;
  param: IBuffer;
  nonStackParams, bufferLength: integer;
begin
  buffer := newBuffer;
  length := 0;
  shortName := UTF8Encode(name);
  if cc = ccReg then
    nonStackParams := 3
  else nonStackParams := 0;

  buffer.add(length, sizeof(word));
  buffer.add(address, sizeof(pointer));
  buffer.add(shortName, system.length(shortName) + 1);

  tail.Version := 3;
  tail.CC := cc;
  tail.ResultType := RetrieveTypeInfo(retval);
  //quick approximation, since apparently nothing uses tail.ParOff anyway
  tail.ParOff := (min(system.length(params) - NonStackParams, 0) + 2) * sizeof(pointer);
  tail.ParamCount := system.length(params);
  buffer.add(tail, sizeof(TVmtMethodEntryTail) - sizeof(TVmtMethodParam));

  for param in params do
    param.CopyTo(buffer);
  length := 2; //empty Attributes
  buffer.add(length, sizeof(word));

  result := buffer.GetBuffer(bufferLength);
  result.Len := buffer.size;
  RegisterExpectedMemoryLeak(result);
end;

{ TBuffer }

procedure TBuffer.add(const data; const size: integer);
begin
  if FIndex + size >= MAX_BUFFER_SIZE then
    raise ERangeError.Create('Buffer overflow');
  system.Move(data, FBuffer[FIndex], size);
  inc(FIndex, size);
end;

procedure TBuffer.CopyTo(const Dest: IBuffer);
begin
   if FIndex > 0 then
      dest.add(FBuffer[0], FIndex);
end;

function TBuffer.current: pointer;
begin
   result := @FBuffer[FIndex];
end;

function TBuffer.GetBuffer(out size: integer; ph: TPrivateHeap = nil): pointer;
begin
  if assigned(ph) then
    ph.GetMem(result, FIndex)
  else GetMem(Result, FIndex);
  system.Move(FBuffer[0], result^, FIndex);
  size := FIndex;
end;

function TBuffer.size: integer;
begin
  result := FIndex;
end;

{ Internals }
var
  infoList: TList;

function retrieveTypeInfo(const info: PTypeInfo): PPtypeInfo;
var
  index: integer;
begin
  if info = nil then
    Exit(nil);

  //known property of RTTI tables; should only be false if we're working across
  //package bounds
  result := pointer(NativeInt(info) - sizeof(pointer));

  if result^ <> info then
  begin
    index := infoList.IndexOf(info);
    if index = -1 then
      index := infoList.Add(info);
    result := @infoList.List[index];
  end;
end;

initialization
  infolist := TList.Create;
finalization
  infoList.Free;

end.
