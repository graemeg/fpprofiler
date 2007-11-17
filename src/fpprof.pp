{

    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Code profiler functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpprof;

interface

uses
  SysUtils, lineinfo;

procedure fpprof_profile;

implementation

var
  fpprof_text: ^Text = nil;
  fpprof_allocated: boolean;
  
function fpprof_info(frame_pointer: pointer): string;
var
  caller_addr : Pointer;
  SystemTime: TSystemTime;
  sYear : string;
  sMonth : string;
  sDay : string;
  sHour : string;
  sMinute : string;
  sSecond : string;
  sMilliseconds : string;
  func: string;
  source: string;
  line: longint;
  sline: string;
begin
  GetLocalTime(SystemTime);
  str(SystemTime.Year, sYear);
  str(SystemTime.Month, sMonth);
  str(SystemTime.Day, sDay);
  str(SystemTime.Hour, sHour);
  str(SystemTime.Minute, sMinute);
  str(SystemTime.Second, sSecond);
  str(SystemTime.Millisecond, sMilliseconds);

  caller_addr := get_caller_addr(frame_pointer);
  GetLineInfo(ptruint(caller_addr), func, source, line);
  str(line, sline);

  fpprof_info := sYear + ' ' + sMonth + ' ' + sDay + ' ' + sHour + ' ' +
                 sMinute + ' ' + sSecond + ' ' + sMilliseconds + ' ' +
                 func + ' ' + source + ' ' + sline;
end;

procedure fpprof_profile;
begin
  if not Assigned(fpprof_text) then exit;
  writeln(fpprof_text^, fpprof_info(get_frame));
end;

procedure fpprof_initializeoutput;
var
  fpprof_filename: string;
begin
  fpprof_filename := ChangeFileExt(ExpandFileName(Paramstr(0)), '.fpprof');

  New(fpprof_text);

  Assign(fpprof_text^, fpprof_filename);
  {$I-}
  Rewrite(fpprof_text^);
  {$I+}

  if ioresult<>0 then
  begin
    Freemem(fpprof_text);
    fpprof_text := nil;
    writeln('cannot open file: ', fpprof_filename);
  end;

  if fpprof_text=nil then
  begin
    if TextRec(Output).Mode=fmClosed then
      fpprof_text := nil
    else
      fpprof_text := @Output;
    fpprof_allocated := false;
  end else
    fpprof_allocated := true;
end;

procedure fpprof_finalizeoutput;
begin
  if fpprof_allocated then
  begin
    Close(fpprof_text^);
    Dispose(fpprof_text);
    fpprof_allocated := false;
  end;
end;

initialization
  fpprof_initializeoutput;

finalization
  fpprof_finalizeoutput;
  
end.

