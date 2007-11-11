program passrctest;

{
ignorefiles:
  (REPORTED)
  packages\extra\gtk\conv\fixgdk.pp
  packages\extra\gtk\conv\fixgdkcdecl.pp
  packages\extra\gtk\conv\fixgtk.pp
  packages\extra\gtk\conv\fixgtkcdecl.pp
  
  (SKIP BECAUSE OF THE SHEAR SIZE)
  packages\extra\univint\FPCMacOSAll.pas
}

{$mode objfpc}
{ $define debug}

uses
  fpputils, Classes, dos, Process, SysUtils;

var
  FileList: TStrings;
  fpcdir: string;
  FPCProcess: TProcess;

begin
  //retrieve the fpc source location
  fpcdir := getenv('fpcdir');
  if fpcdir = '' then
  begin
    writeln('error: no fpcdir environment variable defined');
    halt;
  end;

  writeln('info: fpcsrc environment variable found -> ', fpcdir);

  //retrieve all source files in fpc source directory
  writeln('info: recursively searching files');
  FileList := TStringList.Create;
  RecursiveFileSearch(fpcdir, '.pp;.pas;.inc;.lpr', FileList);

  writeln('info: processing files');
  InsertProfilingCode(FileList, nil);
  
  //compile fpc
  FPCProcess := TProcess.Create(nil);
  FPCProcess.CurrentDirectory := fpcdir;
  FPCProcess.CommandLine := 'make clean all';
  FPCProcess.Options := [poWaitOnExit];
  FPCProcess.Execute;
  
  FileList.Clear;
  RecursiveFileSearch(fpcdir, '.fpprof', FileList);

  writeln('info: reverting all files');
  RemoveProfilingCode(FileList);

  //check the exitcode
  if FPCProcess.ExitStatus <> 0 then
    writeln('error: test failed')
  else
    writeln('info: test succeeded');

  FPCProcess.Free;
end.
