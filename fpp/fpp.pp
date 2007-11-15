program fpp;

{$mode objfpc}

uses
  Classes, Process, CustApp, SysUtils, fpputils, PScanner;

type

  { TEnvironment }

  TEnvironment = class(TObject)
  private
    FCommandLine: string;
    FPathList:    TStrings;
    procedure AddSearchPath(path: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    property CommandLine: string read FCommandLine write FCommandLine;
    property PathList: TStrings read FPathList write FPathList;
    function FileList(ExtensionMask: string): TStrings;
  end;

  { TFPPApplication }

  TFPPApplication = class(TCustomApplication)
  private
    Environment: TEnvironment;
    Verbose: boolean;

    procedure ShowHelp;
    procedure Show(msg: string);
    procedure Compile;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run;
  end;


var
  Application: TFPPApplication;

  procedure InsertToken(ATokenList: TFPList; APos: integer; AToken: TToken; AValue: string);
  var
    pt: ^TPasToken;
  begin
    New(pt);
    pt^.token := AToken;
    pt^.Value := AValue;
    ATokenList.Insert(APos, pt);
  end;

  procedure ModifyCode(tokenlist: TFPList);
  var
    i: integer;

    procedure InsertFPProfUnit;
    var
      i: integer;
    begin
      for i := 0 to tokenlist.Count - 1 do
        if TPasToken(tokenlist[i]^).token = tkUses then
        begin
          InsertToken(tokenlist, i + 1, tkIdentifier, ' fpprof,');
          Exit;
        end;

      for i := 0 to tokenlist.Count - 1 do
        if (TPasToken(tokenlist[i]^).token = tkProgram) or
          (TPasToken(tokenlist[i]^).token = tkUnit) then
        begin
          InsertToken(tokenlist, i + 1, tkIdentifier, 'uses fpprof;');
          Exit;
        end;

      InsertToken(tokenlist, 1, tkIdentifier, 'uses fpprof;');
    end;

  begin
    InsertFPProfUnit;

    i := 0;
    while i < tokenlist.Count do
    begin
      case TPasToken(tokenlist[i]^).token of
        tkBegin:
        begin
          InsertToken(tokenlist, i + 1, tkIdentifier, ' fpprof_profile; ');
          Inc(i);
        end;
        tkEnd:
        begin
          InsertToken(tokenlist, i - 1, tkIdentifier, ' fpprof_profile; ');
          Inc(i);
        end;
      end;
      Inc(i);
    end;

    SaveTokenList('test.debug.pp', tokenlist);
  end;

  procedure TEnvironment.AddSearchPath(path: string);
  begin
    if DirectoryExists(path) then
      PathList.Add(path);
  end;

  constructor TEnvironment.Create;
  var
    i:     integer;
    param: string;
  begin
    inherited Create;
    PathList := TStringList.Create;

    CommandLine := '';

    for i := 1 to ParamCount do
    begin
      CommandLine := CommandLine + ' ' + ParamStr(i);
      param := ParamStr(i);
      case param[1] of
        '-': if pos('-FU', ParamStr(i)) <> 0 then
            AddSearchPath(copy(ParamStr(i), 4, Length(ParamStr(i)) - 3));
        else

          AddSearchPath(ExtractFilePath(param));
      end;
    end;
  end;

  destructor TEnvironment.Destroy;
  begin
    PathList.Free;
    inherited Destroy;
  end;

  function TEnvironment.FileList(ExtensionMask: string): TStrings;
  var
    i: integer;
  begin
    Result := TStringList.Create;
    for i := 0 to PathList.Count - 1 do
      RecursiveFileSearch(PathList[i], ExtensionMask, Result);
  end;

  procedure TFPPApplication.ShowHelp;
  begin
    writeln('GNU FreePascal profiler 0.1');
    writeln('Copyright 2007 Darius Blaszyk.');
    writeln('FPP is free software, covered by the GNU General Public License, and you are');
    writeln('welcome to change it and/or distribute copies of it under certain conditions.');
    writeln('There is absolutely no warranty for FPP.');
    writeln;
  end;

  procedure TFPPApplication.Show(msg: string);
  begin
    if Verbose then
      Writeln(msg);
  end;

  procedure TFPPApplication.Compile;
  var
    FPCProcess: TProcess;
  begin
    FPCProcess := TProcess.Create(nil);

    FPCProcess.CommandLine := 'fpc ' + Environment.CommandLine;

    writeln('executing: ', FPCProcess.CommandLine);
    writeln;
    FPCProcess.Options := FPCProcess.Options + [poWaitOnExit];
    FPCProcess.Execute;

    FPCProcess.Free;
  end;

  constructor TFPPApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);

    Environment := TEnvironment.Create;
  end;

  destructor TFPPApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TFPPApplication.Run;
  begin
    ShowHelp;

    InsertProfilingCode(Environment.FileList('.pp;.pas;.inc;.lpr'), @ModifyCode);

    RemoveProfilingCode(Environment.FileList('.fpprof'));
  end;

begin
  Application := TFPPApplication.Create(nil);
  Application.Run;
  Application.Free;
  readln;
end.
