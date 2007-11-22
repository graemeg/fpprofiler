program fppview;

{$mode objfpc}{$H+}

uses
  CustApp, Classes, FPPReader, FPPStats, FPPReport, SysUtils;

type

  { TFPPViewApp }

  TFPPViewApp = class(TCustomApplication)
  private

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run;
    procedure Usage;
  end;
  
var
  Application: TFPPViewApp;

{ TFPPViewApp }

constructor TFPPViewApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TFPPViewApp.Destroy;
begin
  inherited Destroy;
end;

procedure TFPPViewApp.Run;
var
  FPPReader: TFPPLogReader;
  ProfStats: TCustomProfStats;
  ReportType: TFPPReportType;
  sTmp: string;
begin
  if HasOption('h', 'help') then
    Usage;

  if HasOption('l', 'log') then
    FPPReader := TFPPLogReader.Create(GetOptionValue('l', 'log'))
  else
    Usage;

  //default
  ReportType := rtPlain;
  if HasOption('f','format') then
  begin
    sTmp := GetOptionValue('f', 'format');
    //if sTmp = 'latex' then ReportType := rtLatex;
    if sTmp = 'graphviz' then ReportType := rtGraphViz;
  end;

  if HasOption('s','stat') then
  begin
    sTmp := GetOptionValue('s', 'stat');
    if sTmp = 'callgraph' then
      ProfStats := TCallGraphStats.Create(FPPReader, ReportType)
    else
      if sTmp = 'flat' then
        ProfStats := TFlatProfStats.Create(FPPReader, ReportType)
      else
        //invalid stat
        ProfStats := TFlatProfStats.Create(FPPReader, ReportType)
  end
  else
    //default
    ProfStats := TFlatProfStats.Create(FPPReader, ReportType);

  ProfStats.Run;
  ProfStats.Free;

  FPPReader.Free;
end;

procedure TFPPViewApp.Usage;

  procedure ShowOption(const C,LC,Msg : String);
  begin
    writeln(Format(' -%s --%-20s %s',[C,LC,MSG]));
  end;

  procedure ShowArgOption(const C,LC,Msg : String); overload;
  begin
    writeln(Format(' -%s --%-20s %s',[C,LC+'='+'Value',MSG]));
  end;

  procedure ShowArgOption(const C,LC, Value, Msg : String); overload;
  begin
    writeln(Format(' -%s --%-20s %s',[C, LC+'='+Value, MSG]));
  end;

begin
  writeln(Format('Usage: %s filename [options]',[Paramstr(0)]));
  writeln;
  writeln('Where options is one or more of the following:');
  ShowOption('h','help','This screen.');
  ShowArgOption('l','log','Profile log file (mandatory).');
  writeln;
  writeln('Profiling statistics');
  ShowArgOption('s','stat','flat','Flat profile (default).');
  ShowArgOption('s','stat','callgraph','Call graph.');
  writeln;
  writeln('Output format');
  ShowArgOption('f','format','plain','Human readable text format (default).');
  ShowArgOption('f','format','latex','Latex format.');
  ShowArgOption('f','format','graphviz','GraphViz format (not all stats supported).');
  writeln;
  halt;
end;

begin
  Application := TFPPViewApp.Create(nil);
  Application.Run;
  Application.Free;
end.
