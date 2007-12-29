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

    T := Targets.AddProgram('fppview');
    {$IF defined(HEAPTRC)}
      T.Options := '-gh -gl';
    {$ENDIF}
    T.Mode := cmObjFPC;
    T.UnitPath.Add('..\fcl-passrc\');
    T.UnitPath.Add('..\fpprof\');

    Targets.AddImplicitUnit('fpcallgraph', False);
    Targets.AddImplicitUnit('fpcallgraph', False);
    Targets.AddImplicitUnit('fpplogreader', False);
    Targets.AddImplicitUnit('fppreport', False);
    Targets.AddImplicitUnit('fppstats', False);

    EndPackage;
