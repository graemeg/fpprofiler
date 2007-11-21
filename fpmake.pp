program fpmake;

{$DEFINE ALLPACKAGES}
 
uses
  fpmkunit;

var
  T : TTarget;

begin
  with Installer(TBasicInstaller) do
  begin
    {$i fpp\fpmake.inc}
    {$i fpprof\fpmake.inc}
    {$i fppview\fpmake.inc}
    Run;
  end;
end.
