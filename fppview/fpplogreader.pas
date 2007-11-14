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
var
  DateTime: TDateTime;
  year: word;
  month: word;
  day: word;
  hour: word;
  minute: word;
  second: word;
  msec: word;
begin
  year := StrToInt(ExtractDelimited(1, ALine, [' ']));
  month := StrToInt(ExtractDelimited(2, ALine, [' ']));
  day := StrToInt(ExtractDelimited(3, ALine, [' ']));

  hour := StrToInt(ExtractDelimited(4, ALine, [' ']));
  minute := StrToInt(ExtractDelimited(5, ALine, [' ']));
  second := StrToInt(ExtractDelimited(6, ALine, [' ']));
  msec := StrToInt(ExtractDelimited(7, ALine, [' ']));

  DateTime := EncodeDate(year, month, day) + EncodeTime(hour, minute, second, msec);
  
  if FCount = 0 then
    StartDateTime := DateTime;
    
  AddData(Round((DateTime - StartDateTime) * 24 * 3600 * 1000),
                ExtractDelimited(8, ALine, [' ']),
                ExtractDelimited(9, ALine, [' ']),
                StrToInt(ExtractDelimited(10, ALine, [' '])));
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

