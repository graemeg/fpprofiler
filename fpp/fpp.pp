program fpp;

{$mode objfpc}

uses
  Classes, Process, CustApp, SysUtils, fpputils, PScanner, Dos;

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
    begin_count: integer;

    procedure InsertFPProfUnit;
    var
      i: integer;
    begin
    //find uses clause and insert unit
      for i := 0 to tokenlist.Count - 1 do
        if TPasToken(tokenlist[i]^).token = tkUses then
        begin
        //insert fpprof unit (with whitespace and comma)
          InsertToken(tokenlist, i + 1, tkIdentifier, ' fpprof,');
          Exit;
        end;

    //unit not found, find program / unit keyword
      for i := 0 to tokenlist.Count - 1 do
        if (TPasToken(tokenlist[i]^).token = tkProgram) or
          (TPasToken(tokenlist[i]^).token = tkUnit) then
        begin
        //insert fpprof unit (with uses keyword)
          InsertToken(tokenlist, i + 1, tkIdentifier, 'uses fpprof;');
          Exit;
        end;

    //just try and insert it at the beginning
      InsertToken(tokenlist, 1, tkIdentifier, 'uses fpprof;');
    end;

  begin
  //insert fpprof unit
    InsertFPProfUnit;

  //insert function fpprof_info after each tkBegin and before each tkEnd
    i := 0;
    begin_count := 0;
    while i < tokenlist.Count do
    begin
      case TPasToken(tokenlist[i]^).token of
        tkBegin:
        begin
          InsertToken(tokenlist, i + 1, tkIdentifier, ' fpprof_profile; ');
          Inc(i);
          Inc(begin_count);
        end;
        tkEnd:
        begin
          if begin_count > 0 then
          begin
            InsertToken(tokenlist, i - 1, tkIdentifier, ' fpprof_profile; ');
            Inc(i);
            Dec(begin_count);
          end;
        end;
      end;
      Inc(i);
    end;

    //save result for debuging
    //SaveTokenList('test.debug.pp', tokenlist);
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

    //add debugging info and fpprof unit path
    CommandLine := '-g -Fu' + getenv('fpprof');;

    for i := 1 to ParamCount do
    begin
      CommandLine := CommandLine + ' ' + ParamStr(i);
      param := ParamStr(i);
      case param[1] of
        '-': if pos('-Fu', ParamStr(i)) <> 0 then
            AddSearchPath(copy(ParamStr(i), 4, Length(ParamStr(i)) - 3));
        else
      //filename
          AddSearchPath(ExtractFilePath(ExpandFileName(param)));
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
      FileSearch(PathList[i], ExtensionMask, Result);
  end;

{ TFPPApplication }

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

    //insert profiling code
    InsertProfilingCode(Environment.FileList('.pp;.pas;.inc;.lpr'), @ModifyCode);

    //compile the sources
    Compile;

    //remove the profiling code
    RemoveProfilingCode(Environment.FileList('.fpprof'));
  end;

begin
  Application := TFPPApplication.Create(nil);
  Application.Run;
  Application.Free;
  //readln;
end.
