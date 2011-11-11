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

unit rsLinker;

interface
uses
   SysUtils, Classes, Generics.Collections,
   rsDefsBackend;

type
   TrsLinker = class
   private
      FStartLocations: TStringList;
      FExtRoutines: integer;
      procedure linkUnit(rsu: TrsScriptUnit; prog: TrsProgram);
      procedure ResolveVarRef(ref: TUnresolvedReference; startpos: integer;
        prog: TrsProgram);
      procedure ResolveCallRef(ref: TUnresolvedReference; startpos: integer;
        prog: TrsProgram);
      procedure IntegrateUnit(rsu: TrsScriptUnit; prog: TrsProgram);
   public
      constructor Create;
      destructor Destroy; override;
      function Link(const units: TUnitList; constants: TStringList; environment: TObject): TrsProgram;
   end;

   ELinkError = class(Exception);

implementation
uses
   Windows,
   NewClass;

{ TrsLinker }

procedure TrsLinker.ResolveVarRef(ref: TUnresolvedReference; startpos: integer; prog: TrsProgram);
var
   opcode: TrsAsmInstruction;
   index: integer;
begin
   opcode := prog.Text[startpos + ref.location];
   assert((opcode.left = -1) or (opcode.right = -1));
   index := prog.globals.IndexOf(ref.name);
   if index < 0 then
      raise ELinkError.CreateFmt('Missing variable reference: %s', [ref.name]);
   if opcode.right = - 1 then
      opcode.right := -index
   else opcode.left := -index;
   prog.Text[startpos + ref.location] := opcode;
end;

procedure TrsLinker.ResolveCallRef(ref: TUnresolvedReference; startpos: integer;
  prog: TrsProgram);
var
   opcode: TrsAsmInstruction;
   index: integer;
begin
   opcode := prog.Text[startpos + ref.location];
   assert(opcode.op = OP_CALL);
   assert(opcode.right = -1);
   index := prog.routines[ref.name].index;
   if index < 0 then
   begin
      opcode.op := OP_CALX;
      opcode.right := -index;
   end
   else opcode.right := (startpos + ref.location) - index;
   prog.Text[startpos + ref.location] := opcode;
end;

procedure TrsLinker.linkUnit(rsu: TrsScriptUnit; prog: TrsProgram);
var
   startpos: integer;
   ref: TUnresolvedReference;
begin
   startpos := nativeInt(FStartLocations.Objects[FStartLocations.IndexOf(rsu.name)]);
   for ref in rsu.Unresolved do
      case ref.refType of
         rtVar: ResolveVarRef(ref, startpos, prog);
         rtCall: ResolveCallRef(ref, startpos, prog);
         rtType: assert(false); //ResolveTypeRef(ref, startpos, prog);
      end;
   prog.units.add(rsu.&Unit);
end;

constructor TrsLinker.Create;
begin
   FStartLocations := TStringList.Create;
   FStartLocations.Sorted := true;
   FStartLocations.Duplicates := dupError;
end;

destructor TrsLinker.Destroy;
begin
   FStartLocations.Free;
   inherited Destroy;
end;

procedure TrsLinker.IntegrateUnit(rsu: TrsScriptUnit; prog: TrsProgram);
var
   pair: TPair<string, TrsProcInfo>;
   start: integer;
   info: TrsProcInfo;
   scriptClass: TNewClass;
begin
   if rsu.IsExternal and (length(prog.Text) > 0) then
      raise ELinkError.Create('Invalid unit order');
   start := length(prog.Text);
   FStartLocations.AddObject(rsu.name, TObject(start));
   prog.AddText(rsu.Text);
   for pair in rsu.Routines do
   begin
      info := pair.Value;
      if rsu.IsExternal then
      begin
         info.index := FExtRoutines;
         nativeInt(info.info.CodeAddress) := -FExtRoutines;
         dec(FExtRoutines);
      end
      else info.index := pair.Value.index + start;
      prog.Routines.Add(pair.Key, info);
   end;
   prog.Globals.AddStrings(rsu.Globals);
   for scriptClass in rsu.&Unit do
      prog.ScriptClasses.Add(scriptClass);

end;

function TrsLinker.Link(const units: TUnitList; constants: TStringList; environment: TObject): TrsProgram;
var
   rsu: TrsScriptUnit;
begin
//OutputDebugString('SAMPLING ON');
   FExtRoutines := -1;
   result := TrsProgram.Create;
   if assigned(environment) then
      result.Globals.AddObject('ENVIRONMENT*SELF', environment);
   for rsu in units do
      IntegrateUnit(rsu, result);

   //first pass needs to be finished before beginning second pass
   for rsu in units do
      linkUnit(rsu, result);
   result.constants := constants;
//OutputDebugString('SAMPLING OFF');
end;

end.
