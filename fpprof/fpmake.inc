    StartPackage('fpprof');

    {$IF defined(ALLPACKAGES)}
      Directory:='fpprof';
      BaseInstallDir := '.';
    {$ELSE}
      BaseInstallDir := '..';
    {$ENDIF}

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
