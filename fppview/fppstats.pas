unit FPPStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPLogReader, FPPReport;

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
    constructor Create(AReader: TFPPLogReader; const AValue: TFPPReportType); virtual; abstract;
    destructor Destroy; virtual; abstract;
    
    property Report: TFPPReportType read FReport write SetReport;
    procedure Run; virtual; abstract;
  end;

  { TCallingListProfStats }

  TCallingListProfStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPLogReader; const AValue: TFPPReportType);
    destructor Destroy;

    procedure Run; override;
  end;

  { TFlatProfStats }

  TFlatProfStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPLogReader; const AValue: TFPPReportType);
    destructor Destroy;

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
  else
    FPPReport := TPlainReport.Create;
  end;
end;

{ TCallingListProfStats }

constructor TCallingListProfStats.Create(AReader: TFPPLogReader; const AValue: TFPPReportType);
begin
  FReader := AReader;
  SetReport(AValue);
end;

destructor TCallingListProfStats.Destroy;
begin

end;

procedure TCallingListProfStats.Run;
var
  i: integer;
begin
  if not Assigned(FPPReport) then
    raise Exception.Create('No report object created.');
    
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

{ TFlatProfStats }

constructor TFlatProfStats.Create(AReader: TFPPLogReader; const AValue: TFPPReportType);
begin

end;

destructor TFlatProfStats.Destroy;
begin

end;

procedure TFlatProfStats.Run;
begin

end;

end.

