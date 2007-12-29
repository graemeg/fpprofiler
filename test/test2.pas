unit test2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpprof;

procedure TestOtherUnit;

implementation

procedure TestOtherUnit;
begin
  fpprof_profile;

  writeln('new unit');

  fpprof_profile;
end;

end.

