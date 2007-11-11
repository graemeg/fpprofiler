program passrcex;

{$mode objfpc}

uses fpputils;

var
  PasTokenList: TPasTokenList;
  
begin
  ParseSource(ParamStr(1), PasTokenList);
  SaveTokenList(ParamStr(1) + '.out', PasTokenList);
  writeln('info: done.');
  readln;
end.
