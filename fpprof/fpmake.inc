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

    T := Targets.AddImplicitUnit('pscanner.pp', False);
    {$IF defined(ALLPACKAGES)}
      T.Directory := '..\..\fcl-passrc\';
    {$ELSE}
      T.Directory := '..\fcl-passrc\';
    {$ENDIF}
    T.ResourceStrings := True;

    Targets.AddUnit('fpprof.pp');
    Targets.AddUnit('fpputils.pas');

    EndPackage;
