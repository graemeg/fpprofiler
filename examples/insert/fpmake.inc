    StartPackage('insert');

    {$IF defined(ALLPACKAGES)}
      Directory:='examples\insert';
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

    T := Targets.AddProgram('insert');
    {$IF defined(HEAPTRC)}
      T.Options := '-gh -gl';
    {$ENDIF}
    T.Mode := cmObjFPC;
    T.UnitPath.Add('..\..\fcl-passrc\');
    T.UnitPath.Add('..\..\fpprof\');
    
    EndPackage;
