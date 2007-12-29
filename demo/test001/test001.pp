{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profiling test file

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program test001;

{$mode objfpc}{$H+}

uses test_001;

var
  i, j: integer;

procedure GetTrace;
  procedure test;
  begin
    writeln('test ''s'' ');

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
    case i of
      1: writeln('test');
      2: writeln('TEST');
    end;
    inc(j);
  end;

  GetTrace;
  
  for i := 0 to 100 do
  begin
    TestOtherUnit;
  end;
  
  writeln(j);
end.

