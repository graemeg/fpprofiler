{ TCustomFPPReport }

function TCustomFPPReport.GetCell(ARow, AColumn: integer): string;
begin
  if ARow >= FRowCount then
    raise Exception.Create('Row index out of range');
  if AColumn >= FColumnCount then
    raise Exception.Create('Column index out of range');

  Result := FCells[ARow, AColumn];
end;

procedure TCustomFPPReport.SetCell(ARow, AColumn: integer; const AValue: string
  );
begin
  if ARow >= FRowCount then
  begin
    FRowCount := ARow + 1;
    SetLength(FCells, FRowCount, FColumnCount);
  end;

  if AColumn >= FColumnCount then
  begin
    FColumnCount := AColumn + 1;
    SetLength(FCells, FRowCount, FColumnCount);
  end;

  FCells[ARow, AColumn] := AValue;
end;

constructor TCustomFPPReport.Create;
begin
  Clear;
end;

destructor TCustomFPPReport.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCustomFPPReport.Clear;
begin
  FRowCount := 0;
  FColumnCount := 0;
  SetLength(FCells, FRowCount, FRowCount);
end;

