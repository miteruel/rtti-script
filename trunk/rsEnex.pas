unit rsEnex;

interface
uses
   SysUtils, Generics.Collections;

type
   TEnex = record
      class function Select<T, U: class>(input: TEnumerable<T>): TList<U>; static;
      class function Where<T>(input: TEnumerable<T>; filter: TPredicate<T>): TList<T>; static;
      class function KeysWhereValue<T, U>(input: TEnumerable<TPair<T, U>>; filter: TPredicate<U>): TList<T>; static;
      class function FirstWhereOrDefault<T>(input: TEnumerable<T>; filter: TPredicate<T>; default: T): T; static;
   end;

implementation

{ TEnex }

class function TEnex.Select<T, U>(input: TEnumerable<T>): TList<U>;
var
   enumerator: T;
begin
   result := TList<U>.Create;
   try
      for enumerator in input do
         if enumerator.inheritsFrom(U) then
            result.Add(TObject(enumerator));
   except
      result.Free;
      raise;
   end;
end;

class function TEnex.Where<T>(input: TEnumerable<T>; filter: TPredicate<T>): TList<T>;
var
   enumerator: T;
begin
   result := TList<T>.Create;
   try
      for enumerator in input do
         if filter(enumerator) then
            result.Add(enumerator);
   except
      result.Free;
      raise;
   end;
end;

class function TEnex.FirstWhereOrDefault<T>(input: TEnumerable<T>;
  filter: TPredicate<T>; default: T): T;
var
   enumerator: T;
begin
   for enumerator in input do
      if filter(enumerator) then
         Exit(Enumerator);
   result := default;
end;

class function TEnex.KeysWhereValue<T, U>(input: TEnumerable<TPair<T, U>>;
  filter: TPredicate<U>): TList<T>;
var
   pair: TPair<T, U>;
begin
   result := TList<T>.Create;
   try
      for pair in input do
         if filter(pair.Value) then
            result.Add(pair.Key);
   except
      result.Free;
      raise;
   end;
end;

end.
