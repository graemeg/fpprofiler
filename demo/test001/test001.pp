program test001;

{$mode objfpc}{$H+}

uses test_001;

var
  i, j: integer;

procedure GetTrace;
  procedure test;
  begin
    writeln('test');

    for i := 0 to 10000000 do
    begin
      inc(j);
    end;
  end;
begin
  writeln('gettrace');

  for i := 0 to 10000000 do
    inc(j);

  test;
end;
    
begin
  writeln('main');

  for i := 0 to 10000000 do
  begin
    inc(j);
  end;

  GetTrace;
  
  for i := 0 to 100 do
  begin
    TestOtherUnit;
  end;
  
  writeln(j);
end.

