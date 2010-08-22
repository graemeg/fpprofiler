{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profiler report class

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FPPReport;

{$mode objfpc}{$H+}

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
    procedure CallGraph(ACallGraph: TFPCallGraph); override;
  end;

  { TGraphVizReport }

  TGraphVizReport = class(TCustomFPPReport)
  private

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteTable; override;
    procedure CallGraph(ACallGraph: TFPCallGraph); override;
  end;

implementation

{$i customreport.inc}
{$i plainreport.inc}
{$i graphvizreport.inc}

end.
