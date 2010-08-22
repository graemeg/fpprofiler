{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profiler statistics class

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FPPStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPReader, FPPReport, FPCallGraph;

type

  { TCustomProfStats }

  TCustomProfStats = class(TObject)
    FReport: TFPPReportType;
    FReader: TFPPReader;
    procedure SetReport(const AValue: TFPPReportType);
  private

  protected
    FPPReport: TCustomFPPReport;

  public
    constructor Create(AReader: TFPPReader; const AValue: TFPPReportType); virtual;
    destructor Destroy; override;
    
    property Report: TFPPReportType read FReport write SetReport;
    procedure Run; virtual;
  end;

  { TFlatProfStats }

  TFlatProfStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPReader; const AValue: TFPPReportType); override;
    destructor Destroy; override;

    procedure Run; override;
  end;

  { TCallGraphStats }

  TCallGraphStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPReader; const AValue: TFPPReportType); override;
    destructor Destroy; override;

    procedure Run; override;
  end;

implementation

{ TCustomProfStats }

procedure TCustomProfStats.SetReport(const AValue: TFPPReportType);
begin
  if (FReport=AValue) and Assigned(FPPReport) then exit;
  FReport:=AValue;
  case FReport of
    rtPlain: FPPReport := TPlainReport.Create;
    rtGraphViz: FPPReport := TGraphVizReport.Create;
  else
    FPPReport := TPlainReport.Create;
  end;
end;

constructor TCustomProfStats.Create(AReader: TFPPReader;
  const AValue: TFPPReportType);
begin
  FReader := AReader;
  SetReport(AValue);
end;

destructor TCustomProfStats.Destroy;
begin
  FPPReport.Free;
  inherited Destroy;
end;

procedure TCustomProfStats.Run;
begin
  if not Assigned(FPPReport) then
    raise Exception.Create('No report object created.');
end;

{ TFlatProfStats }

constructor TFlatProfStats.Create(AReader: TFPPReader; const AValue: TFPPReportType);
begin
  inherited Create(AReader, AValue);
end;

destructor TFlatProfStats.Destroy;
begin
  inherited Destroy;
end;

procedure TFlatProfStats.Run;
var
  i: integer;
begin
  inherited Run;
    
  FPPReport.Clear;
  FPPReport.Cells[0,0] := '#';
  FPPReport.Cells[0,1] := 'Position';
  FPPReport.Cells[0,2] := 'Elapsed msec';
  FPPReport.Cells[0,3] := 'Function';
  FPPReport.Cells[0,4] := 'Source';
  FPPReport.Cells[0,5] := 'Line';

  for i := 0 to Pred(FReader.Count) do
  begin
    FPPReport.Cells[i + 1, 0] :=  IntToStr(i + 1);
    FPPReport.Cells[i + 1, 1] :=  FReader[i].position;
    FPPReport.Cells[i + 1, 2] :=  IntToStr(FReader[i].elapsed);
    FPPReport.Cells[i + 1, 3] :=  FReader[i].func;
    FPPReport.Cells[i + 1, 4] :=  FReader[i].source;
    FPPReport.Cells[i + 1, 5] :=  IntToStr(FReader[i].line);
  end;

  FPPReport.WriteTable;
end;

{ TCallGraphStats }

constructor TCallGraphStats.Create(AReader: TFPPReader; const AValue: TFPPReportType);
begin
  inherited Create(AReader, AValue);
end;

destructor TCallGraphStats.Destroy;
begin
  inherited Destroy;
end;

procedure TCallGraphStats.Run;
var
  FPCallGraph: TFPCallGraph;
  i: integer;
  Caller: TStrings;
begin
  inherited Run;

  FPCallGraph := TFPCallGraph.Create;
  
  Caller := TStringList.Create;
  //first entry is mother of all calls so put it on the stack
  Caller.Add(FReader[0].func);
  
  for i := 1 to FReader.Count - 1 do
    if FReader[i].position = 'entry' then
    begin
      //writeln('  peeking: ',Caller[0]);
      FPCallGraph.AddCall(Caller[0], FReader[i].func);
      //writeln('  pushing: ',FReader[i].func);
      Caller.Insert(0, FReader[i].func);
    end
    else
      begin
        //writeln('  popping: ',Caller[0]);
        Caller.Delete(0);
      end;

  Caller.Free;
  
  FPPReport.CallGraph(FPCallGraph);

  FPCallGraph.Free;
end;

end.

