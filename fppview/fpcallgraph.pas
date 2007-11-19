unit FPCallGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TFPCall = record
    Count: integer;
    Name: string;
  end;
  TFCallArray = array of TFPCall;
  
  { TFPCallList }

  TFPCallList = class(TObject)
  private
    FList: TFCallArray;
    function GetList(index: integer): TFPCall;
    procedure SetList(index: integer; const AValue: TFPCall);
  public
    Count: integer;
    Name: string;

    constructor Create;
    destructor Destroy; override;
    
    property List[index: integer]: TFPCall read GetList write SetList; default;
    procedure AddCall(AName: string);
    function FindFunction(AName: string): integer;
  end;
  
  TFPCallListArray = array of TFPCallList;
  
  { TFPCallGraph }

  TFPCallGraph = class(TObject)
  private
    FList: TFPCallListArray;
    function GetList(AIndex: integer): TFPCallList;
    procedure SetList(AIndex: integer; const AValue: TFPCallList);
  public
    Count: integer;

    constructor Create;
    destructor Destroy; override;

    property List[AIndex: integer]: TFPCallList read GetList write SetList; default;
    
    procedure AddCall(Caller, Callee: string);
    function FindFunction(AName: string): integer;
  end;

implementation

{ TFPCallList }

function TFPCallList.GetList(index: integer): TFPCall;
begin
  Result := FList[index];
end;

procedure TFPCallList.SetList(index: integer; const AValue: TFPCall);
begin
  FList[index] := AValue;
end;

constructor TFPCallList.Create;
begin
  Name := '';
  Count := 0;
end;

destructor TFPCallList.Destroy;
begin
  inherited Destroy;
end;

procedure TFPCallList.AddCall(AName: string);
var
  index: integer;
begin
  index := FindFunction(AName);

  FList[index].Name := AName;
  FList[index].Count := FList[index].Count + 1;
end;

function TFPCallList.FindFunction(AName: string): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if List[i].Name = Name then
    begin
      Result := i;
      exit;
    end;

  Inc(Count);
  SetLength(FList, Count);
  Result := Count - 1;
end;

{ TFPCallGraph }

function TFPCallGraph.GetList(AIndex: integer): TFPCallList;
begin
  Result := FList[AIndex];
end;

procedure TFPCallGraph.SetList(AIndex: integer; const AValue: TFPCallList);
begin
  FList[AIndex] := AValue;
end;

constructor TFPCallGraph.Create;
begin

end;

destructor TFPCallGraph.Destroy;
begin
  inherited Destroy;
end;

procedure TFPCallGraph.AddCall(Caller, Callee: string);
var
  index: integer;
begin
  index := FindFunction(Caller);
  
  List[index].AddCall(Callee);
end;

function TFPCallGraph.FindFunction(AName: string): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if List[i].Name = AName then
    begin
      Result := i;
      exit;
    end;
    
  Inc(Count);
  SetLength(FList, Count);
  Result := Count - 1;
  FList[Result] := TFPCallList.Create;
  FList[Result].Name := AName;
end;

end.

