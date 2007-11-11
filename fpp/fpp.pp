program fpp;

{$mode objfpc}

uses
  Classes, Process, CustApp, SysUtils, fpputils;

type

  { TEnvironment }

  TEnvironment = class(TObject)
  private
    FCommandLine: string;
    FFileList: TStrings;
    procedure AddSearchPath(path: string);
  public
    constructor Create;
    destructor Destroy; override;

    property CommandLine: string read FCommandLine write FCommandLine;
    property FileList: TStrings read FFileList write FFileList;
  end;

  { TFPPApplication }

  TFPPApplication = class(TCustomApplication)
  private
    Environment: TEnvironment;
    Verbose: boolean;

    procedure ShowHelp;
    procedure Show(msg: string);
    procedure InsertProfilingCode;
    procedure RemoveProfilingCode;
    procedure Compile;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run;
  end;

procedure TEnvironment.AddSearchPath(path: string);
var
  Info : TSearchRec;
  Ext: string;
  SearchDir: string;
begin
  //search all source files in this path
  SearchDir := path;
  if FindFirst(SearchDir+'*', faAnyFile, Info)=0 then
    repeat
      Ext := ExtractFileExt(Info.Name);

      if (Ext = '.pas') or (Ext = '.pp') or (Ext = '.inc') then
        FileList.Add(SearchDir + Info.Name);

    until FindNext(info)<>0;

  FindClose(Info);
end;

constructor TEnvironment.Create;
var
  i: integer;
  param: string;
begin
  inherited Create;
  
  FileList := TStringList.Create;

  CommandLine := ' -g ';

  for i := 1 to ParamCount do
  begin
    CommandLine := CommandLine + ' ' + ParamStr(i);
    
    param := ParamStr(i);
    
    case param[1] of
      '-' : begin
              if pos('-FU', ParamStr(i)) <> 0 then
                AddSearchPath(copy(ParamStr(i), 4, Length(ParamStr(i)) - 3));
             end;
    else
      //filename
      FileList.Add(param);
    end;
  end;
end;

destructor TEnvironment.Destroy;
begin
  FileList.Free;
  
  inherited Destroy;
end;

var
  Application: TFPPApplication;

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

procedure TFPPApplication.InsertProfilingCode;
var
  i: integer;
  source: Text;
  target: Text;
  s: string;
  index: integer;
begin
  for i := 0 to Environment.FileList.Count - 1 do
  begin
    RenameFile(Environment.FileList[i],Environment.FileList[i] + '.fpprof');

    assignfile(source, Environment.FileList[i] + '.fpprof');
    reset(source);

    assignfile(target, Environment.FileList[i]);
    rewrite(target);

    while not eof(source) do
    begin
      Readln(source, s);
      
      //replace by fcl-passrc??
      
      //note: add uses clause
      //note: skip comments
      //note: skip literal strings
      
      //add function calls
      index := pos('begin', s);
      if index <> 0 then
        insert(' fpprof_profile; ', s, index + 5);

      index := pos('end;', s);
      if index <> 0 then
        insert(' fpprof_profile; ', s, index);

      index := pos('end.', s);
      if index <> 0 then
        insert(' fpprof_profile; ', s, index);

      Writeln(target, s);
    end;

    closefile(target);
    closefile(source);
  end;
end;

procedure TFPPApplication.RemoveProfilingCode;
var
  i: integer;
begin
  for i := 0 to Environment.FileList.Count - 1 do
  begin
    DeleteFile(Environment.FileList[i]);
    RenameFile(Environment.FileList[i] + '.fpprof', Environment.FileList[i]);
  end;
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
  InsertProfilingCode;

  //compile the sources
  Compile;

  //remove the profiling code
  RemoveProfilingCode;
end;

begin
  Application := TFPPApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
