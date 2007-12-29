program fpmake;

{$DEFINE ALLPACKAGES}
{$DEFINE HEAPTRC}

uses
  fpmkunit;

var
  T : TTarget;

begin
  with Installer(TBasicInstaller) do
  begin
    {$i examples\insert\fpmake.inc}
    {$i examples\revert\fpmake.inc}
    {$i examples\passrc\fpmake.inc}
    {$i fpp\fpmake.inc}
    {$i fpprof\fpmake.inc}
    {$i fppview\fpmake.inc}
    {$i test\fpmake.inc}
    Run;
  end;
end.
