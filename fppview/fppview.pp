program fpprofile;

uses
  strings, contnrs;

type
  TFrame = record
             elapsed: longint;
             func: string;
             source: string;
             line: integer;
           end;
  TFrames = array of TFrame;
  
var
  T: Text;
  s: string;
  FrameList: TFrames;

procedure processline(s: string);
begin
end;

begin
  assign(T, ParamStr(1));
  reset(T);
  
  while not eof(T) do
  begin
    readln(T, s);
    processline(s);
  end;
  readln;
end.
