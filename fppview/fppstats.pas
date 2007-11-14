unit FPPStats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPLogReader;

type

  { TCustomProfStats }

  TCustomProfStats = class(TObject)
  private

  public
    constructor Create(AReader: TFPPLogReader); virtual; abstract;
    destructor Destroy; virtual; abstract;
  end;

  { TCallingListProfStats }

  TCallingListProfStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPLogReader);
    destructor Destroy;
  end;

  { TFlatProfStats }

  TFlatProfStats = class(TCustomProfStats)
  private

  public
    constructor Create(AReader: TFPPLogReader);
    destructor Destroy;
  end;


implementation

{ TCallingListProfStats }

constructor TCallingListProfStats.Create(AReader: TFPPLogReader);
var
  i: integer;
begin
  for i := 0 to Pred(AReader.Count) do
    writeln(i:5, ' ',
            IntToStr(AReader[i].elapsed) + 'msec':15, ' ',
            AReader[i].func:15, ' ',
            AReader[i].source:15, ' ',
            AReader[i].line:15);
end;

destructor TCallingListProfStats.Destroy;
begin

end;

{ TFlatProfStats }

constructor TFlatProfStats.Create(AReader: TFPPLogReader);
begin

end;

destructor TFlatProfStats.Destroy;
begin

end;

end.

