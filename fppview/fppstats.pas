unit FPPStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPLogReader, FPPReport, FPCallGraph;

type

  { TCustomProfStats }

  TCustomProfStats = class(TObject)
    FReport: TFPPReportType;
    FReader: TFPPLogReader;
    procedure SetReport(const AValue: TFPPReportType);
  private

  protected
    FPPReport: TCustomFPPReport;

  public
    constructor Create(AReader: TFPPLogReader; const AValue: TFPPReportType); virtual;
    destructor Destroy; override;
    
    property Report: TFPPReportType read FReport write SetReport;
    procedure Run; virtual;
  end;

  { TFlatProfStats }

  TFlatProfStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPLogReader; const AValue: TFPPReportType); override;
    destructor Destroy; override;

    procedure Run; override;
  end;

  { TCallGraphStats }

  TCallGraphStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPLogReader; const AValue: TFPPReportType); override;
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

constructor TCustomProfStats.Create(AReader: TFPPLogReader;
  const AValue: TFPPReportType);
begin
  FReader := AReader;
  SetReport(AValue);
end;

destructor TCustomProfStats.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomProfStats.Run;
begin
  if not Assigned(FPPReport) then
    raise Exception.Create('No report object created.');
end;

{ TFlatProfStats }

constructor TFlatProfStats.Create(AReader: TFPPLogReader; const AValue: TFPPReportType);
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
  FPPReport.Cells[0,1] := 'Elapsed msec';
  FPPReport.Cells[0,2] := 'Function';
  FPPReport.Cells[0,3] := 'Source';
  FPPReport.Cells[0,4] := 'Line';

  for i := 0 to Pred(FReader.Count) do
  begin
    FPPReport.Cells[i + 1, 0] :=  IntToStr(i + 1);
    FPPReport.Cells[i + 1, 1] :=  IntToStr(FReader[i].elapsed);
    FPPReport.Cells[i + 1, 2] :=  FReader[i].func;
    FPPReport.Cells[i + 1, 3] :=  FReader[i].source;
    FPPReport.Cells[i + 1, 4] :=  IntToStr(FReader[i].line);
  end;

  FPPReport.WriteTable;
end;

{ TCallGraphStats }

constructor TCallGraphStats.Create(AReader: TFPPLogReader;
  const AValue: TFPPReportType);
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
begin
  inherited Run;

  FPCallGraph := TFPCallGraph.Create;
  
  //this needs to be fixed, until now not the right
  //caller and callee are passed to CallGraph, but
  //we need to start somewhere don't we?
  for i := 0 to FReader.Count - 2 do
    FPCallGraph.AddCall(FReader[i].func, FReader[i+1].func);

  FPPReport.CallGraph(FPCallGraph);

  FPCallGraph.Free;
end;

end.

