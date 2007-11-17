program fppview;

{$mode objfpc}{$H+}

uses
  strings, contnrs, FPPLogReader, FPPStats, FPPReport;

var
  FPPReader: TFPPLogReader;
  ProfStats: TCustomProfStats;
begin
  FPPReader := TFPPLogReader.Create(ParamStr(1));

  ProfStats := TCallingListProfStats.Create(FPPReader, rtPlain);
  ProfStats.Run;

  FPPReader.Free;
  ProfStats.Free;
end.
