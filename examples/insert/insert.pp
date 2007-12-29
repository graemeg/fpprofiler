program insert;

uses
  fpputils, Classes, dos, SysUtils;

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
  fpputils.FileSearch(fpcdir, '.pp;.pas;.inc;.lpr', FileList, True);

  writeln('info: processing files');
  InsertProfilingCode(FileList, nil);

  FileList.Free;
  writeln('info: done.');
end.
