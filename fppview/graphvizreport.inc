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
  writeln('# dot -Tjpg test.dot -o test.jpg');

  writeln('digraph G {');

  //definition of nodes
  for i := 0 to ACallGraph.Count - 1 do
    writeln('  ', ACallGraph[i].Name, ';');

  writeln;

  //definition of connections
  for i := 0 to ACallGraph.Count - 1 do
    for j := 0 to ACallGraph[i].Count - 1 do
      writeln('  ', ACallGraph[i].Name, ' -> ', ACallGraph[i][j].Name, ' [label="' , ACallGraph[i][j].Count, ' calls"];');

  writeln('}');
end;

end.
