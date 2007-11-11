program revert;

uses
  fpputils, Classes, dos, Process, SysUtils;

var
  FileList: TStrings;
  fpcdir: string;

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
  RecursiveFileSearch(fpcdir, '.fpprof', FileList);

  writeln('info: reverting all files');
  RemoveProfilingCode(FileList);

  writeln('info: done.');
end.
