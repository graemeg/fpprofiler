program test;

{$mode objfpc}{$H+}

uses test2, fpprof;

var
  i, j: integer;

procedure GetTrace;
  procedure test;
  begin
    fpprof_profile;
    writeln('test');

    for i := 0 to 10000000 do
      inc(j);

    fpprof_profile;
  end;
begin
  fpprof_profile;
  writeln('gettrace');

  for i := 0 to 10000000 do
    inc(j);

  test;
  fpprof_profile;
end;
    
begin
  fpprof_profile;
  writeln('main');

  for i := 0 to 10000000 do
    inc(j);

  GetTrace;
  
  TestOtherUnit;
  
  writeln(j);
  
  fpprof_profile;
end.

