{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profiler testcase application (compile FPC)

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program passrctest;

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
  //don't insert any code, but just touch them by the source scanner
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

  FileList.Free;
  FPCProcess.Free;
end.
