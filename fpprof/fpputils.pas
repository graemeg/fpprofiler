unit fpputils;

{$mode objfpc}
{ $define debug}

interface

uses
  pscanner, PasTree, dos, SysUtils, Classes, FPPWriter;

const
  FPPROF_EXT = '.fpprof';

type
  TPasToken = record
    token: TToken;
    value: string;
  end;
  PPasToken = ^TPasToken;

  TModTokenProc = procedure(AFileName: string; tokenlist: TFPList);

procedure FileSearch(SearchDir: string; ExtensionMask: string; var FileList: TStrings);
procedure RecursiveFileSearch(SearchDir: string; ExtensionMask: string; var FileList: TStrings);
procedure InsertProfilingCode(FileList: TStrings; ModTokenProc: TModTokenProc);
procedure RemoveProfilingCodeFromFile(const FileName: string);
procedure RemoveProfilingCode(FileList: TStrings);
function ParseSource(FileName: string; var PasTokenList: TFPList): boolean;
procedure SaveTokenList(FileName: string; PasTokenList: TFPList);

implementation

function ParseSource(FileName: string; var PasTokenList: TFPList): boolean;
var
  pas: TPascalScanner;
  fr: TFileResolver;
  index: integer;
  token: TToken;
  pt: PPasToken;
begin
  Result := False;
  
  fr := TFileResolver.Create;
  pas := TPascalScanner.Create(fr);

  try
    pas.Options := [poSkipIncludeFiles, poDontEatDefines, poMonoLithicASMBlocks];
    pas.OpenFile(FileName);

    index := 0;
    repeat
      inc(index);

      token := pas.FetchToken;

      New(pt);
      pt^.token:= token;
      pt^.value:= pas.CurTokenString;
      PasTokenList.Add(pt);

      {$ifdef debug}
      //writeln(token, '>', pas.CurTokenString);
      {$endif}
    until pas.CurToken = tkEOF;
    
    Result := True;
  finally
    pas.Free;
    fr.Free;
  end;
end;

procedure SaveTokenList(FileName: string; PasTokenList: TFPList);
var
  t: text;
  i: integer;
begin
  assign(t, FileName);
  rewrite(t);

  for i:=0 to PasTokenList.Count - 1 do
  begin
    case TPasToken(PasTokenList[i]^).token of
      tkWhitespace: write(t, ' ');
      tkString: write(t, chr(39) + TPasToken(PasTokenList[i]^).value + chr(39));
      tkBraceOpen: write(t, '(');
      tkBraceClose: write(t, ')');
      tkMul: write(t, '*');
      tkPlus: write(t, '+');
      tkComma: write(t, ',');
      tkMinus: write(t, '-');
      tkDot: write(t, '.');
      tkDivision: write(t, '/');
      tkColon: write(t, ':');
      tkSemicolon: write(t, ';');
      tkLessThan: write(t, '<');
      tkEqual: write(t, '=');
      tkGreaterThan: write(t, '>');
      tkAt: write(t, '@');
      tkSquaredBraceOpen: write(t, '[');
      tkSquaredBraceClose: write(t, ']');
      tkCaret: write(t, '^');
      tkDotDot: write(t, '..');
      tkAssign: write(t, ':=');
      tkNotEqual: write(t, '<>');
      tkLessEqualThan: write(t, '<=');
      tkGreaterEqualThan: write(t, '>=');
      tkPower: write(t, '**');
      tkSymmetricalDifference: write(t, '><');
      tkLineEnding: writeln(t, LineEnding);
      tkTab: writeln(t, #9);
      tkDirective, tkDefine, tkInclude: write(t, '{', TPasToken(PasTokenList[i]^).value, '}')
    else
      //remove comments from source
      if TPasToken(PasTokenList[i]^).token <> tkComment then
        write(t, TPasToken(PasTokenList[i]^).value);
    end;
  end;

  close(t);
end;

procedure FileSearch(SearchDir: string; ExtensionMask: string;
  var FileList: TStrings);
var
  Info : TSearchRec;
  ExtensionList: TStrings;
begin
  SearchDir := IncludeTrailingPathDelimiter(SearchDir);

  ExtensionList := TStringList.Create;
  ExtensionList.Delimiter := ';';
  ExtensionList.DelimitedText := ExtensionMask;

  if FindFirst(SearchDir+'*',faAnyFile and faDirectory,Info)=0 then
    repeat
      if ExtensionList.IndexOf(ExtractFileExt(Info.Name)) <> -1 then
        FileList.Add(SearchDir + Info.Name);
    until FindNext(Info)<>0;

  FindClose(Info);

  ExtensionList.Free;
end;

procedure RecursiveFileSearch(SearchDir: string; ExtensionMask: string; var FileList: TStrings);
var
  Info : TSearchRec;
  ExtensionList: TStrings;
begin
  SearchDir := IncludeTrailingPathDelimiter(SearchDir);
  
  ExtensionList := TStringList.Create;
  ExtensionList.Delimiter := ';';
  ExtensionList.DelimitedText := ExtensionMask;
  
  if FindFirst(SearchDir+'*',faAnyFile and faDirectory,Info)=0 then
  begin
    repeat
      if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') and (Info.Name <> '..')then
        RecursiveFileSearch(SearchDir + Info.Name, ExtensionMask, FileList);

      if ExtensionList.IndexOf(ExtractFileExt(Info.Name)) <> -1 then
        FileList.Add(SearchDir + Info.Name);
    until FindNext(Info)<>0;
  end;
  FindClose(Info);
  
  ExtensionList.Free;
end;

procedure InsertProfilingCode(FileList: TStrings; ModTokenProc: TModTokenProc);
var
  i: integer;
  j: integer;
  PasTokenList: TFPList;
  Success: boolean;
  writer: TFPPWriter;
begin
  PasTokenList := TFPList.Create;
  
  writer := TFPPWriter.Create;
  writer.CreateIgnored;

  //make a copy of the original files and process them
  for i := 0 to FileList.Count - 1 do
  begin
    write('insert: ', FileList[i]);
    
    try
      Success := ParseSource(FileList[i], PasTokenList);

      Success := Success and RenameFile(FileList[i], FileList[i] + FPPROF_EXT);

      //perform the code modification
      if Assigned(ModTokenProc) then
        ModTokenProc(FileList[i], PasTokenList);

      SaveTokenList(FileList[i], PasTokenList);
      
      if Success then
        writeln(' .......... OK')
    except
      writeln(' .......... FAIL');
      writer.AddIgnoredFile(FileList[i]);
    end;
    
    //clear list and dispose members
    for j := 0 to PasTokenList.Count - 1 do
      Dispose(PPasToken(PasTokenList[j]));

    PasTokenList.Clear;
  end;

  writer.Save;
  writer.Free;

  PasTokenList.Free;
end;

procedure RemoveProfilingCodeFromFile(const FileName: string);
var
  NewFileName: string;
begin
  NewFileName := Copy(FileName, 1, Length(FileName) - Length(FPPROF_EXT));

  write('revert: ', NewFileName);

  DeleteFile(NewFileName);
  if RenameFile(FileName, NewFileName) then
    writeln(' .......... OK')
  else
    writeln(' .......... FAIL');
end;

procedure RemoveProfilingCode(FileList: TStrings);
var
  i: integer;
begin
  //restore the original files
  for i := 0 to FileList.Count - 1 do
    RemoveProfilingCodeFromFile(FileList[i]);
end;

end.
