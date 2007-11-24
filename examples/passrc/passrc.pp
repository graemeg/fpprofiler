program passrc;

{$mode objfpc}

uses
  Classes, fpputils;

var
  PasTokenList: TFPList;
  
begin
  PasTokenList := TFPList.Create;
  
  ParseSource(ParamStr(1), PasTokenList);
  SaveTokenList(ParamStr(1) + '.out', PasTokenList);
  writeln('info: done.');
  PasTokenList.Free;
  readln;
end.
