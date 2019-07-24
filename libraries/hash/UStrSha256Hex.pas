unit UStrSha256Hex;

{$mode objfpc}{$H+}

interface

Uses Classes, SysUtils, USha256;

Function StrSha256Hex(Str: ansistring): ansistring;

implementation

function SHA256Str(Hash: TSHA256HASH): ansistring;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to 6 do
    Result:= Result + IntToHex(Hash[i],8);
  Result:= Result + IntToHex(Hash[7],8);
end;

Function StrSha256Hex(Str: ansistring): ansistring;
begin
  Result := SHA256Str(CalcSHA256(Str));
end;

end.

