program fppview;

{$mode objfpc}{$H+}

uses
  strings, contnrs, FPPLogReader, FPPStats, FPPReport;

var
  FPPReader: TFPPLogReader;
  ProfStats: TCustomProfStats;
begin
  FPPReader := TFPPLogReader.Create(ParamStr(1));

  //ProfStats := TCallingListProfStats.Create(FPPReader, rtPlain);
  //ProfStats.Run;
  //ProfStats.Free;

  ProfStats := TCallGraphStats.Create(FPPReader, rtGraphViz);
  ProfStats.Run;
  ProfStats.Free;


  FPPReader.Free;
  readln;
end.
