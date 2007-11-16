{ TPlainReport }

constructor TPlainReport.Create;
begin

end;

destructor TPlainReport.Destroy;
begin
  inherited Destroy;
end;

procedure TPlainReport.WriteTable;
var
  r: integer;
  c: integer;
  ColWidth: array of integer;
begin
  //determine columnwidth needed
  SetLength(ColWidth, FColumnCount);

  for r := 1 to FRowCount - 1 do
    for c := 0 to FColumnCount - 1 do
      if ColWidth[c] < Length(Cells[r,c]) then
        ColWidth[c] := Length(Cells[r,c]);
  
  
  //print header
  for c := 0 to FColumnCount - 1 do
    write(Cells[1,c]);

  //print separator line
        
  //print data
  for r := 1 to FRowCount - 1 do
  begin
    for c := 1 to FColumnCount - 1 do
      write(Cells[r,c]);
    writeln;
  end;
end;

