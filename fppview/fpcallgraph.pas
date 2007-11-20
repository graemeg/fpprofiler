unit FPCallGraph;

interface

uses
  Classes, SysUtils; 

type
  { TFPCallGraph }

  TFPCallGraph = class(TObject)
  private

  public
    Caller: TStrings;
    Called: array of array of integer;

    constructor Create;
    destructor Destroy; override;

    procedure AddCall(ACaller, ACalled: string);
    function FindCall(AName: string): integer;
  end;

implementation

{ TFPCallGraph }

constructor TFPCallGraph.Create;
begin
  Caller := TStringList.Create;
end;

destructor TFPCallGraph.Destroy;
begin
  Caller.Free;

  inherited Destroy;
end;

procedure TFPCallGraph.AddCall(ACaller, ACalled: string);
var
  r: integer;
  c: integer;
begin
  r := FindCall(ACaller);
  c := FindCall(ACalled);
  
  Called[r,c] := Called[r,c] + 1;
end;

function TFPCallGraph.FindCall(AName: string): integer;
begin
  Result := Caller.IndexOf(AName);
  
  if Result = -1 then
  begin
    Caller.Add(AName);
    SetLength(Called, Caller.Count, Caller.Count);
    Result := Caller.Count - 1;
  end;
end;

end.

