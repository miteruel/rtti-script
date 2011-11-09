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

unit rsSystemUnit;

interface
uses
   TypInfo,
   rsDefs;

type
   TAddType = reference to procedure(info: PTypeInfo; parent: TUnitSymbol);

const
   SYSNAME = 'SYSTEM';

function BuildSysUnit(AddType: TAddType): TUnitSymbol;

implementation
uses
   SysUtils, RTTI,
   rsImport;

type
   TNameHackSymbol = class(TSymbol);

function BuildSysUnit(AddType: TAddType): TUnitSymbol;
var
   ctx: TRttiContext;
   rType: TRttiInstanceType;
   table: ISymbolTable;
   clsType: TExternalClassType;
   list: IParamList;
   stringType: TTypeSymbol;
   fieldSymbol: TSymbol;
   realType: TRttiType;
begin
   ctx := TRttiContext.Create;
   table := CreateSymbolTable(true);
   result := TUnitSymbol.Create(SYSNAME, table, table, true);
   AddType(typeInfo(boolean), result);
   AddType(typeInfo(integer), result);
   AddType(typeInfo(string), result);
   realType := ctx.GetType(TypeInfo(extended));
   if not NativeTypeDefined(realType) then
      AddNativeType(realType, TExternalType.Create(realType));
   TNameHackSymbol(TypeOfRttiType(realType)).FName := 'Real';
   result.publics.Add('REAL', TypeOfNativeType(typeInfo(extended)));

   rType := ctx.GetType(TObject) as TRttiInstanceType;
   if not NativeTypeDefined(rType) then
   begin
      clsType := TExternalClassType.Create(rType);
      clsType.AddMethod('CREATE', clsType, mvPublic, false, EmptyParamList);
      clsType.AddMethod('FREE', clsType, mvPublic, false, EmptyParamList);
      AddNativeType(rType, clsType);
   end
   else clsType := TypeOfRttiType(rType) as TExternalClassType;
   table.Add(UpperCase(rType.Name), clsType);

   stringType := TypeOfNativeType(TypeInfo(string));

   rType := ctx.GetType(Exception) as TRttiInstanceType;
   if not NativeTypeDefined(rType) then
   begin
      clsType := TExternalClassType.Create(rType);
      list := EmptyParamList;
      list.Add(TParamSymbol.Create('message', stringType, [pfConst]));
      clsType.AddMethod('Create', clsType, mvPublic, false, list);
      clsType.AddField('FMessage', stringType, mvPrivate);
      fieldSymbol := clsType.SymbolLookup('FMESSAGE');
      clsType.AddProp('Message', stringType, mvPublic, false, EmptyParamList, fieldSymbol, fieldSymbol);
      AddNativeType(rType, clsType);
   end
   else clsType := TypeOfRttiType(rType) as TExternalClassType;
   table.Add(UpperCase(rType.Name), clsType);

   table.Add('CRLF', TConstSymbol.Create('CRLF', #13#10));
   AddNativeType(nil, TTypeSymbol.Create('NULL*TYPE'));
end;

end.
