unit FPPLogReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type
  TFPPEntry = record
    elapsed: longint;    //msec since first frame was created
    func: string;        //function - procedure name that made the call
    source: string;      //sourcefile where procedure is located
    line: integer;       //line number in sourcefile
  end;

  TFPPEntryArray = array of TFPPEntry;

  { TFPPLogReader }

  TFPPLogReader = class(TObject)
  private
    FCount: integer;
    StartDateTime: TDateTime;
    
    FData: TFPPEntryArray;

    function GetData(Index: Integer): TFPPEntry;
    procedure ReadLine(ALine: string);
    procedure SetData(Index: Integer; const AValue: TFPPEntry);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    procedure AddData(elapsed: longint; func: string; source: string; line: integer);
    property Count: integer read FCount;
    property Data[Index: Integer]: TFPPEntry read GetData write SetData; default;
  end;

implementation

{ TFPPLogReader }

procedure TFPPLogReader.ReadLine(ALine: string);
begin
  AddData(StrToInt(ExtractDelimited(1, ALine, [' '])),
          ExtractDelimited(2, ALine, [' ']),
          ExtractDelimited(3, ALine, [' ']),
          StrToInt(ExtractDelimited(4, ALine, [' '])));
end;

function TFPPLogReader.GetData(Index: Integer): TFPPEntry;
begin
  Result := FData[Index];
end;

procedure TFPPLogReader.SetData(Index: Integer; const AValue: TFPPEntry);
begin
  FData[Index] := AValue;
end;

constructor TFPPLogReader.Create(const FileName: string);
var
  T: Text;
  s: string;
begin
  FCount := 0;
  SetLength(FData, FCount);

  assign(T, FileName);
  reset(T);

  while not eof(T) do
  begin
    readln(T, s);
    ReadLine(s);
  end;
end;

destructor TFPPLogReader.Destroy;
begin
  inherited Destroy;
end;

procedure TFPPLogReader.AddData(elapsed: longint; func: string; source: string; line: integer);
begin
  Inc(FCount);
  SetLength(FData, FCount);

  FData[Pred(FCount)].elapsed := elapsed;
  FData[Pred(FCount)].func := func;
  FData[Pred(FCount)].source := source;
  FData[Pred(FCount)].line := line;
end;

end.

