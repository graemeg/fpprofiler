    StartPackage('fppview');

    {$IF defined(ALLPACKAGES)}
      Directory:='fppview';
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

    T:=Targets.AddUnit('fppview');
    T.Mode := cmObjFPC;
    T.TargetType := ttProgram;
    T.UnitPath.Add('..\fcl-passrc\');
    T.UnitPath.Add('..\fpprof\');
    EndPackage;
