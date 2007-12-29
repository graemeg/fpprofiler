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

unit test_001;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure TestOtherUnit;

implementation

procedure TestOtherUnit;
var
  i: integer;
begin
  for i := 1 to 1000 do 
    writeln('new unit');
end;

end.

