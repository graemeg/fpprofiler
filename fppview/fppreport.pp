unit FPPReport;

interface

uses
  Classes, SysUtils;

type
  TFPPReportType = (rtPlain);
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

implementation

{$i customreport.inc}
{$i plainreport.inc}

end.
