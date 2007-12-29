{ TGraphVizReport }

constructor TGraphVizReport.Create;
begin

end;

destructor TGraphVizReport.Destroy;
begin
  inherited Destroy;
end;

procedure TGraphVizReport.WriteTable;
begin

end;

procedure TGraphVizReport.CallGraph(ACallGraph: TFPCallGraph);
var
  i: integer;
  j: integer;
begin
  writeln('# example compilation of dot script');
  writeln('# dot -Tpng test.dot -o test.png');

  writeln('digraph G {');

  //definition of nodes
  for i := 0 to ACallGraph.Caller.Count - 1 do
    writeln('  ', ACallGraph.Caller[i], ';');

  writeln;

  //definition of connections
  for i := 0 to ACallGraph.Caller.Count - 1 do
    for j := 0 to ACallGraph.Caller.Count - 1 do
      if ACallGraph.Called[i,j] <> 0 then
        writeln('  ', ACallGraph.Caller[i], ' -> ', ACallGraph.Caller[j], ' [label="' , ACallGraph.Called[i,j], ' calls"];');

  writeln('}');
end;

end.
