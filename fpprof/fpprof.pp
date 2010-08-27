{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Code profiler functions

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpprof;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, lineinfo, FPPWriter;

procedure fpprof_entry_profile;
procedure fpprof_exit_profile;

implementation

var
  fpprof_log: TFPPWriter;
  fpprof_starttime: QWord;
  
{ The following includes define platform/architecture specific
implementations to get accurate system times. The function is defined as:

  function fpprof_getsystemtime: QWord;
}

{$IFDEF WIN32}
  {$i win32systemtime.inc}
{$ELSE}
  {$i systemtime.inc}
{$ENDIF}

procedure fpprof_info(position: string; frame_pointer: pointer);
var
  caller_addr: Pointer;
  func, source: shortstring;
  line: longint;
  sline: string;
  systemtime : string;
begin
  str(fpprof_getsystemtime - fpprof_starttime, systemtime);

  caller_addr := get_caller_addr(frame_pointer);
  GetLineInfo(ptruint(caller_addr), func, source, line);
  str(line, sline);

  fpprof_log.AddTrace(position, systemtime, func, source, sline);
end;

procedure fpprof_entry_profile;
begin
  fpprof_info('entry', get_frame);
end;

procedure fpprof_exit_profile;
begin
  fpprof_info('exit', get_frame);
end;

procedure fpprof_initialize;
begin
  fpprof_log := TFPPWriter.Create(True);
  fpprof_log.CreateTraceLog;

  fpprof_starttime := fpprof_getsystemtime;
end;

procedure fpprof_finalize;
begin
  fpprof_log.Save;
  fpprof_log.Free;
end;

initialization
  fpprof_initialize;

finalization
  fpprof_finalize;
  
end.

