program fppview;

{$mode objfpc}{$H+}

uses
  strings, contnrs, FPPLogReader, FPPStats;

var
  FPPReader: TFPPLogReader;
  ProfStats: TCustomProfStats;
begin
  FPPReader := TFPPLogReader.Create(ParamStr(1));

  ProfStats := TCallingListProfStats.Create(FPPReader);

  readln;
  FPPReader.Free;
  ProfStats.Free;
end.
