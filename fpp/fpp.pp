{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Free Pascal Profile application

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program fpp;

{$mode objfpc}{$H+}

uses
  Classes, Process, CustApp, SysUtils, PScanner, FPPWriter, fpputils;

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
    procedure ShowProductInfo;
    procedure Show(msg: string);
    procedure Compile;
    procedure Usage;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
  end;

var
  Application: TFPPApplication;

  procedure ModifyCode(AFileName: string; tokenlist: TPasTokenList);
  var
    i: integer;
    begin_count: integer;

    procedure InsertFPProfUnit;
    var
      i: integer;
    begin
      //find uses clause and insert unit
      for i := tokenlist.Count-1 downto 0 do
      begin
        if (tokenlist[i].token = tkUses) then
        begin
          //insert fpprof unit (with whitespace and comma)
          tokenlist.Insert(i - 1, tkIdentifier, ' fpprof, ');
          Exit;
        end;
      end;

      //unit not found, find program / unit keyword
      for i := tokenlist.Count-1 downto 0 do
        if (tokenlist[i].token = tkProgram) or
          (tokenlist[i].token = tkUnit) then
        begin
          //insert fpprof unit (with uses keyword)
          tokenlist.Insert(i - 6, tkIdentifier, 'uses fpprof;');
          Exit;
        end;

      //just try and insert it at the beginning
      tokenlist.Insert(Tokenlist.Count-2, tkIdentifier, 'uses fpprof;');
    end;

  begin
    //insert fpprof unit
    if ExtractFileExt(AFileName) <> '.inc' then
      InsertFPProfUnit;

    //insert function fpprof_info after each tkBegin and before each tkEnd
    begin_count := 0;
    for i := tokenlist.Count-1 downto 0 do
    begin
      case tokenlist[i].token of
        tkCase:     inc(begin_count);
        tkBegin:
          begin
            Inc(begin_count);

            if begin_count = 1 then
            begin
              tokenlist.Insert(i-1, tkIdentifier, ' fpprof_entry_profile; '+LineEnding);
            end;
          end;
        tkEnd:
          begin
            if begin_count = 1 then
            begin
              tokenlist.Insert(i+1 , tkIdentifier, ' fpprof_exit_profile; '+LineEnding);
            end;
            if begin_count > 0 then
              Dec(begin_count);
          end;
      end; { case }
    end; { while }

    //save result for debuging
    //tokenlist.SaveToFile('test.debug.pp');
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
    CommandLine := '-gl -Fu' + GetEnvironmentVariable('fpprof');

    for i := 1 to ParamCount do
    begin
      CommandLine := CommandLine + ' ' + ParamStr(i);
      param := ParamStr(i);
      case param[1] of
        '-':
            if pos('-Fu', ParamStr(i)) <> 0 then
                AddSearchPath(copy(ParamStr(i), 4, Length(ParamStr(i)) - 3));
            else
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

  procedure TFPPApplication.ShowProductInfo;
  begin
    writeln('GNU FreePascal profiler 0.2');
    writeln('Copyright 2007 Darius Blaszyk. Contributions by Graeme Geldenhuys 2010.');
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

  procedure TFPPApplication.Usage;

    procedure ShowOption(const C,LC,Msg : String);
    begin
      writeln(Format(' -%s --%-20s %s',[C,LC,MSG]));
    end;

    procedure ShowArgOption(const C,LC,Msg : String); overload;
    begin
      writeln(Format(' -%s --%-20s %s',[C,LC+'='+'Value',MSG]));
    end;

    procedure ShowArgOption(const C,LC, Value, Msg : String); overload;
    begin
      writeln(Format(' -%s --%-20s %s',[C, LC+'='+Value, MSG]));
    end;

  begin
    writeln(Format('Usage: %s filename [options]',[Paramstr(0)]));
    writeln;
    writeln('Where options is one or more of the following:');
    ShowOption('h','help','This screen.');
    ShowArgOption('i','no-insert','Do not insert profiling code.');
    ShowArgOption('r','no-remove','Do not remove profiling code.');
    writeln;
    writeln('Environment variable used:');
    writeln('  fpprof      points to the directory containing the fpprof.pas unit.');
    writeln('              current value is: ' + GetEnvironmentVariable('fpprof'));
    writeln('  fpc         points to the compiler binary to be used.');
    writeln('              current value is: ' + GetEnvironmentVariable('fpc'));
    writeln;
  end;

  procedure TFPPApplication.Compile;
  var
    FPCProcess: TProcess;
    lFPC: string;
  begin
    FPCProcess := TProcess.Create(nil);
    try
      lFPC := GetEnvironmentVariable('fpc');
      if lFPC = '' then
        lFPC := 'fpc';
      FPCProcess.CommandLine := lFPC + ' ' + Environment.CommandLine;

      writeln('executing: ', FPCProcess.CommandLine);
      writeln;

      FPCProcess.Options := FPCProcess.Options + [poWaitOnExit];
      FPCProcess.Execute;
    finally
      FPCProcess.Free;
    end;
  end;

  constructor TFPPApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    Environment := TEnvironment.Create;
  end;

  destructor TFPPApplication.Destroy;
  begin
    Environment.Free;
    inherited Destroy;
  end;

  procedure TFPPApplication.Run;
  begin
    // TODO: Add a Silent option with no output
    ShowProductInfo;

    if HasOption('h','help') then
    begin
      Usage;
      exit;
    end;
    //insert profiling code
    if not HasOption('i','no-insert') then
      InsertProfilingCode(Environment.FileList('.pp;.pas;.inc;.lpr'), @ModifyCode);

    //compile the sources
    Compile;

    //remove the profiling code
    if not HasOption('r','no-remove') then
      RemoveProfilingCode(Environment.FileList('.fpprof'));
  end;

begin
  Application := TFPPApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
