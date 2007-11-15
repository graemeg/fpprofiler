unit fpputils;

{ $define debug}

interface

uses
  pscanner, PasTree, dos, SysUtils, Classes;

const
  FPPROF_EXT = '.fpprof';

type
  TPasToken = record
    token: TToken;
    value: string;
  end;

  TModTokenProc = procedure(tokenlist: TFPList);

procedure RecursiveFileSearch(SearchDir: string; ExtensionMask: string; var FileList: TStrings);
procedure InsertProfilingCode(FileList: TStrings; ModTokenProc: TModTokenProc);
procedure RemoveProfilingCodeFromFile(const FileName: string);
procedure RemoveProfilingCode(FileList: TStrings);
procedure ParseSource(FileName: string; var PasTokenList: TFPList);
procedure SaveTokenList(FileName: string; PasTokenList: TFPList);

implementation

procedure ParseSource(FileName: string; var PasTokenList: TFPList);
var
  pas: TPascalScanner;
  fr: TFileResolver;
  index: integer;
  token: TToken;
  pt: ^TPasToken;
begin
  fr := TFileResolver.Create;
  pas := TPascalScanner.Create(fr);
  pas.Options := [poSkipIncludeFiles, poDontEatDefines, poMonoLithicASMBlocks];
  pas.OpenFile(FileName);

  index := 0;
  repeat
    inc(index);

    token := pas.FetchToken;

    New(pt);
    pt^.token:= token;
    pt^.value:= pas.CurTokenString;
    PasTokenList.Add(@pt);

    {$ifdef debug}
    writeln(token, '>', pas.CurTokenString);
    {$endif}
  until pas.CurToken = tkEOF;

  pas.Free;
  fr.Free;
end;

procedure SaveTokenList(FileName: string; PasTokenList: TFPList);
  //following function is a bit of a hack as
  //passrc removes double literals (should be fixed there)
  function InsertLiteral(AString: string): string;
  var
    i: Integer;
  begin
    i := 1;
    while i < length(AString) do
    begin
      if AString[i] = chr(39) then
      begin
        Insert(chr(39), AString, i);
        Inc(i);
      end;
      Inc(i);
    end;
    
    Result := AString;
  end;
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
      tkString: if TPasToken(PasTokenList[i]^).value = chr(39) then
                  write(t, chr(39) + chr(39) + chr(39) + chr(39) + chr(39))
                else
                  write(t, chr(39) + InsertLiteral(TPasToken(PasTokenList[i]^).value) + chr(39));
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
end;

procedure InsertProfilingCode(FileList: TStrings; ModTokenProc: TModTokenProc);
var
  i: integer;
  PasTokenList: TFPList;
  IgnoreFile: Text;
  Success: boolean;
begin
  PasTokenList := TFPList.Create;
  
  assign(IgnoreFile, 'ignorelist.dat');
  rewrite(IgnoreFile);
  //make a copy of the original files and process them
  for i := 0 to FileList.Count - 1 do
  begin
    write('inserting code to -> ', FileList[i]);
    
    try
      ParseSource(FileList[i], PasTokenList);

      Success :=  RenameFile(FileList[i], FileList[i] + FPPROF_EXT);

      //perform the code modification
      if Assigned(ModTokenProc) then
        ModTokenProc(PasTokenList);

      SaveTokenList(FileList[i], PasTokenList);
      
      if Success then
        writeln(' .......... OK')
    except
      writeln(' .......... FAIL');
      writeln(IgnoreFile, FileList[i]);
    end;
    
    PasTokenList.Clear;
  end;
  close(IgnoreFile);
  PasTokenList.Free;
end;

procedure RemoveProfilingCodeFromFile(const FileName: string);
var
  NewFileName: string;
begin
  NewFileName := Copy(FileName, 1, Length(FileName) - Length(FPPROF_EXT));

  write('reverting -> ', NewFileName);

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
