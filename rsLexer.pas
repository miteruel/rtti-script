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

unit rsLexer;

interface
uses
   Classes, SysUtils,
   rsDefs;

type
   TRsLexer = class
   private
      FText: string;
      FStart, FEnd: PChar;
      FLastRet: PChar;
      FLineNum: integer;

      function IsAlpha(C: Char): Boolean; inline;
      function IsAlphaNumeric(C: Char): Boolean; inline;
      function SubtractChars(last, first: PChar): integer; inline;
      function GetText: string; inline;
      function CreateToken(kind: TTokenKind): TToken;
      function GetNextToken: TToken;
      function IsNewline: boolean; inline;
      procedure Advance; inline;
      procedure DoNewline;
      procedure SkipWhitespace;
      function SkipComment: boolean;
      function ReadIdentifier: TToken;
      function ReadColon: TToken;
      function ReadLT: TToken;
      function ReadGT: TToken;
      function ReadDot: TToken;
      function ReadHex: TToken;
      function ReadNum: TToken;
      procedure ScanInt;
      function ReadChar: TToken;
      procedure ScanHex;
      function ReadString: TToken;
   public
      function Lex(const text: string): TTokenQueue; overload;
      procedure Lex(var token: TToken); overload;
      procedure Load(const text: string);
   end;

   EEndOfFile = class(Exception);

implementation
uses
   Generics.Collections, Windows,
   Character;

{ TRsLexer }

procedure TRsLexer.Load(const text: string);
begin
   assert(length(text) > 0);
   FText := text;
   FStart := addr(FText[1]);
   FEnd := FStart;
   FLastRet := FStart;
end;

function TRsLexer.GetText: string;
var
   len: integer;
begin
   len := SubtractChars(FEnd, FStart);
   setLength(result, len);
   if len > 0 then
      System.Move(FStart^, result[1], len * sizeof(char));
end;

function TRsLexer.SubtractChars(last, first: PChar): integer;
begin
   result := (nativeUInt(last) - nativeUInt(first)) div sizeof(char);
end;

function TRsLexer.CreateToken(kind: TTokenKind): TToken;
begin
   result := TToken.Create(kind, FLineNum + 1, (SubtractChars(FStart, FlastRet) + 1),
     SubtractChars(FEnd, FStart), FStart);
end;

procedure TRsLexer.DoNewline;

   procedure NewLine;
   begin
      inc(FLineNum);
      FLastRet := FEnd;
   end;

begin
   if FEnd^ = #13 then
   begin
     inc(FEnd);
     if FEnd^ = #10 then
       inc(FEnd);
     newLine;
   end
   else if FEnd^ = #10 then
   begin
      inc(FEnd);
      NewLine;
   end
end;

function TRsLexer.IsNewline: boolean;
begin
   result := CharInSet(FEnd^, [#13, #10]);
end;

procedure TRsLexer.Advance;
begin
   if isNewline then
      DoNewline
   else if FEnd^ = #0 then
      raise EEndOfFile.Create('')
   else inc(FEnd);
end;

function TRsLexer.SkipComment: boolean;
var
   line: integer;
//   next: PChar;
begin
   result := false;
//   next := FEnd + 1;
   if FEnd^ = '{' then
   begin
      result := true;
      while not (FEnd^ = '}') do
         Advance;
      Advance; //see if this works
   end
   else if (FEnd^ = '(') and (FEnd[1] = '*') then
   begin
   (* test *)
      result := true;
      inc(FEnd, 2);
      while not ((FEnd^ = '*') and (FEnd[1] = ')')) do
         Advance;
   end
   else if (FEnd^ = '/') and (FEnd[1] = '/' ) then
   begin
      result := true;
      line := FLineNum;
      while line = FLineNum do
         Advance;
   end;
end;

procedure TRsLexer.SkipWhitespace;
begin
   repeat
      while CharInSet(FEnd^, [#32, #9, #13, #10]) do
         Advance;
   until not SkipComment;
   FStart := FEnd;
   inc(FEnd);
end;

function TRsLexer.IsAlpha(C: Char): Boolean;
begin
   Result := TCharacter.IsLetter(C) or (C = '_');
end;

function TRsLexer.IsAlphaNumeric(C: Char): Boolean;
begin
   Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
end;

function TRsLexer.ReadIdentifier: TToken;
begin
   while IsAlphaNumeric(FEnd^) do
      inc(FEnd);
   result := CreateToken(tkIdentifier);
   result.kind := rsDefs.GetKeyword(UpperCase(result.origText));
end;

procedure TRsLexer.ScanHex;
begin
   while CharInSet(FEnd^, ['0'..'9', 'a'..'f', 'A'..'F']) do
      inc(FEnd);
end;

procedure TRsLexer.ScanInt;
begin
   while CharInSet(FEnd^, ['0'..'9']) do
      inc(FEnd);
end;

function TRsLexer.ReadHex: TToken;
begin
   ScanHex;
   result := CreateToken(tkInt);
end;

function TRsLexer.ReadNum: TToken;
begin
   ScanInt;
   if FEnd^ = '.' then
   begin
      inc(FEnd);
      if FEnd^ = '.' then // to handle ranges such as "array [1..4]"
      begin
         dec(FEnd);
         result := CreateToken(tkInt);
      end
      else begin
         ScanInt;
         result := CreateToken(tkFloat);
      end;
   end
   else result := CreateToken(tkInt);
end;

function TRsLexer.ReadChar: TToken;
var
   letter: char;
begin
   if FEnd^ = '$' then
   begin
      inc(FEnd);
      ScanHex;
   end
   else ScanInt;
   inc(FStart);
   if FStart < FEnd then
   begin
      word(letter) := StrToInt(GetText);
      dec(FStart);
      result := TToken.Create(tkChar, FLineNum + 1, SubtractChars(FStart, FLastRet) + 1, 1, FStart);
   end
   else begin
      dec(FStart);
      result := CreateToken(tkError);
   end;
end;

function TRsLexer.ReadString: TToken;
var
   next: PChar;
begin
   while true do
   begin
      while FEnd^ <> '''' do
         inc(FEnd);
      next := FEnd + 1;
      if next^ <> '''' then
      begin
         inc(FEnd);
         break;
      end
      else inc(FEnd, 2);
   end;
   result := CreateToken(tkString);
end;

function TRsLexer.ReadColon: TToken;
begin
   if FEnd^ = '=' then
   begin
      inc(FEnd);
      result := CreateToken(tkAssign)
   end
   else result := CreateToken(tkColon);
end;

function TRsLexer.ReadLT: TToken;
var
   kind: TTokenKind;
begin
   case FEnd^ of
      '>': kind := tkNotEqual;
      '=': kind := tkLessEqual;
      else kind := tkLessThan;
   end;
   if kind <> tkLessThan then
      inc(FEnd);
   result := CreateToken(kind);
end;

function TRsLexer.ReadGT: TToken;
begin
   if FEnd^ = '=' then
   begin
      inc(FEnd);
      result := CreateToken(tkGreaterEqual);
   end
   else result := CreateToken(tkGreaterThan);
end;

function TRsLexer.ReadDot: TToken;
begin
   if FEnd^ = '.' then
   begin
      inc(FEnd);
      result := CreateToken(tkDotDot);
   end
   else result := CreateToken(tkDot);
end;

function TRsLexer.GetNextToken: TToken;
var
   kind: TTokenKind;
begin
   try
      SkipWhitespace;
   except
      on E: EEndOfFile do
      begin
         FStart := FEnd;
         Exit(CreateToken(tkEof));
      end;
   end;
   kind := tkNone;
   case FStart^ of
      ';': kind := tkSem;
      '=': kind := tkEquals;
      '+': kind := tkPlus;
      '-': kind := tkMinus;
      '*': kind := tkTimes;
      '/': kind := tkDivide;
      ',': kind := tkComma;
      '(': kind := tkOpenParen;
      ')': kind := tkCloseParen;
      '[': kind := tkOpenBracket;
      ']': kind := tkCloseBracket;
      #0:  kind := tkEof;
      ':': result := ReadColon;
      '<': result := ReadLT;
      '>': result := ReadGT;
      '.': result := ReadDot;
      '$': result := ReadHex;
      '0'..'9': result := ReadNum;
      '#': result := ReadChar;
      '''': result := ReadString;
      else if IsAlpha(FStart^) then
         result := ReadIdentifier
      else begin
         FEnd := FStart + 1;
         result := CreateToken(tkError);
      end;
   end;
   if kind <> tkNone then
      result := CreateToken(kind);
   FStart := FEnd;
end;

function TRsLexer.Lex(const text: string): TTokenQueue;
var
   token: TToken;
begin
//OutputDebugString('SAMPLING ON');
   self.Load(text);
   result := TQueue<TToken>.Create;
//   result.OwnsObjects := true;
   result.Capacity := length(text) div 4; //heuristic to avoid reallocations
   repeat
      token := GetNextToken;
      result.Enqueue(token);
   until token.Kind in [tkEOF, tkError];
//OutputDebugString('SAMPLING OFF');
end;

procedure TRsLexer.Lex(var token: TToken);
begin
   token := GetNextToken;
end;

end.
