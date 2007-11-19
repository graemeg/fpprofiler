unit FPPReport;

interface

uses
  Classes, SysUtils, StrUtils, FPCallGraph;

type
  TFPPReportType = (rtPlain, rtGraphViz);
  { TCustomFPPReport }

  TCustomFPPReport = class(TObject)
  private
    function GetCell(ARow, AColumn: integer): string;
    procedure SetCell(ARow, AColumn: integer; const AValue: string);
  protected
    FColumnCount: integer;
    FRowCount: integer;
    FCells: array of array of string;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Clear;
    procedure WriteTable; virtual; abstract;
    procedure CallGraph(CallGraph: TFPCallGraph); virtual; abstract;

    property Cells[ARow, AColumn: integer]: string read GetCell write SetCell;
  end;

  { TPlainReport }

  TPlainReport = class(TCustomFPPReport)
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteTable; override;
  end;

  { TGraphVizReport }

  TGraphVizReport = class(TCustomFPPReport)
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteTable;
    procedure CallGraph(ACallGraph: TFPCallGraph); override;
  end;

implementation

{$i customreport.inc}
{$i plainreport.inc}
{$i graphvizreport.inc}

end.
