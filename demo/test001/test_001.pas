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

