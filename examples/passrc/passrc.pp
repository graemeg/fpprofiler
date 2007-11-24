program passrc;

{$mode objfpc}

uses
  Classes, fpputils;

var
  PasTokenList: TPasTokenList;
  
begin
  PasTokenList := TPasTokenList.Create;
  
  PasTokenList.ParseSource(ParamStr(1));
  PasTokenList.SaveToFile(ParamStr(1) + '.out');
  writeln('info: done.');
  PasTokenList.Free;
end.
