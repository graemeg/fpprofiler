{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Profiling helper functions and classes

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpputils;

{$mode objfpc}{$H+}
{ $define debug}

interface

uses
  pscanner, PasTree, SysUtils, Classes, FPPWriter;

const
  FPPROF_EXT = '.fpprof';

type
  TPasToken = record
    token: TToken;
    value: string;
  end;
  PPasToken = ^TPasToken;

  { TPasTokenList }

  TPasTokenList = class(TObject)
    FFileName: string;
  private
    FList: TFPList;
  
    function GetList(index: integer): TPasToken;
    procedure SetFileName(const AValue: string);
    procedure SetList(index: integer; const AValue: TPasToken);

  public
    constructor Create;
    destructor Destroy; override;
    
    function Count: integer;
    function ParseSource(AFileName: string): boolean;
    function Add(AToken: TToken; AValue: string): Integer;
    procedure Clear;
    procedure Insert(APos: integer; AToken: TToken; AValue: string);
    procedure SaveToFile(const AFileName: string);
    property FileName: string read FFileName write SetFileName;
    property List[index: integer]: TPasToken read GetList write SetList; default;
  end;

  TModTokenProc = procedure(AFileName: string; tokenlist: TPasTokenList);

procedure FileSearch(SearchDir: string; ExtensionMask: string; var FileList: TStrings; Recursive: boolean = false);
procedure InsertProfilingCode(FileList: TStrings; ModTokenProc: TModTokenProc);
procedure RemoveProfilingCodeFromFile(const FileName: string);
procedure RemoveProfilingCode(FileList: TStrings);

implementation

procedure FileSearch(SearchDir: string; ExtensionMask: string;
  var FileList: TStrings; Recursive: boolean = false);
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
      if Recursive then
        if ((Info.Attr and faDirectory) = faDirectory) and (Info.Name <> '.') and (Info.Name <> '..')then
          FileSearch(SearchDir + Info.Name, ExtensionMask, FileList, Recursive);

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
  PasTokenList: TPasTokenList;
  Success: boolean;
  writer: TFPPWriter;
begin
  PasTokenList := TPasTokenList.Create;
  
  writer := TFPPWriter.Create;
  writer.CreateIgnored;

  //make a copy of the original files and process them
  for i := 0 to FileList.Count - 1 do
  begin
    write('insert: ', FileList[i]);

    try
      Success := PasTokenList.ParseSource(FileList[i]);

      Success := Success and RenameFile(FileList[i], FileList[i] + FPPROF_EXT);

      //perform the code modification
      if Assigned(ModTokenProc) then
        ModTokenProc(FileList[i], PasTokenList);

      PasTokenList.SaveToFile(FileList[i]);

      if Success then
        writeln(' .......... OK')
    except
      writeln(' .......... FAIL');
      writer.AddIgnoredFile(FileList[i]);
    end;

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

{ TPasTokenList }

function TPasTokenList.GetList(index: integer): TPasToken;
begin
  Result := TPasToken(FList[index]^);
end;

procedure TPasTokenList.SetFileName(const AValue: string);
begin
  if FFileName=AValue then exit;
  FFileName:=AValue;

end;

procedure TPasTokenList.SetList(index: integer; const AValue: TPasToken);
begin
  FList.Add(@AValue);
end;

constructor TPasTokenList.Create;
begin
  FList := TFPList.Create;
end;

destructor TPasTokenList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TPasTokenList.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PPasToken(FList[i]));

  FList.Clear;
end;

procedure TPasTokenList.Insert(APos: integer; AToken: TToken; AValue: string);
var
  pt: ^TPasToken;
begin
  New(pt);
  pt^.token := AToken;
  pt^.Value := AValue;
  FList.Insert(APos, pt);
end;

procedure TPasTokenList.SaveToFile(const AFileName: string);
var
  t: text;
  i: integer;
begin
  assign(t, AFileName);
  rewrite(t);

  for i:=0 to FList.Count - 1 do
  begin
    case TPasToken(FList[i]^).token of
      tkWhitespace: write(t, ' ');
      tkString: write(t, chr(39) + TPasToken(FList[i]^).value + chr(39));
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
      tkDirective, tkDefine, tkInclude: write(t, '{', TPasToken(FList[i]^).value, '}')
    else
      //remove comments from source
      if TPasToken(FList[i]^).token <> tkComment then
        write(t, TPasToken(FList[i]^).value);
    end;
  end;

  close(t);
end;

function TPasTokenList.ParseSource(AFileName: string): boolean;
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
    pas.OpenFile(AFileName);

    index := 0;
    repeat
      inc(index);

      token := pas.FetchToken;

      Add(token, pas.CurTokenString);

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

function TPasTokenList.Add(AToken: TToken; AValue: string): Integer;
begin
  Insert(0, AToken, AValue);
end;

function TPasTokenList.Count: integer;
begin
  Result := FList.Count;
end;

end.
