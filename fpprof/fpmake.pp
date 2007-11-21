program fpmake;
 
uses
  fpmkunit;

var
  T : TTarget;

begin
  with Installer(TBasicInstaller) do
  begin
    StartPackage('fpprof');
    BaseInstallDir := '..\bin';
    Author := 'Darius Blaszyk';
    License := 'GPL';
    ExternalURL := 'www.freepascal.org';
    Email := 'dhkblaszyk@zeelandnet.nl';
    Description := 'this package is part of the fpprofiler project';
    Version := '0.1.0';

    T:=Targets.AddUnit('fpprof.pp');
    T:=Targets.AddUnit('fpputils.pas');
    T.Mode := cmObjFPC;
    T.UnitPath.Add('..\fcl-passrc\');
    EndPackage;
    Run;
  end;
end.
