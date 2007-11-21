program fpmake;
 
uses
  fpmkunit;

var
  T : TTarget;

begin
  with Installer(TBasicInstaller) do
  begin
    {$i fpmake.inc}
    Run;
  end;
end.
