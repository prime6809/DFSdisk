program DFSDisk;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ConsoleUtils, DFSDiskUnit, DFSUtilUnit
  { you can add units after this };

type

  {Operations that we can perform, specified as command line commands }
  TOpCodes = (
    Nop = 0,
    DFSCreate,
    DFSRead,
    DFSWrite,
    DFSCat,
    DFSDump
  );

  { TDFSDisk }

  TDFSDisk = class(TCustomApplication)
  protected
    CmdParams       : TStringList;
    OpStr           : STRING;
    DFSImageName    : STRING;
    DFSFileName     : STRING;
    IOFileName      : STRING;
    DFSLoad         : LongWord;
    DFSExec         : LongWord;
    DFSQual         : CHAR;
    DFSTracks       : INTEGER;
    DFSLabel        : STRING;
    DFSOption       : BYTE;
    DFSCount        : BYTE;

    OpCode          : TOpCodes;

    Disk            : TDFSDiskImage;

    procedure DoRun; override;
    PROCEDURE DoHelp;
    FUNCTION DecodeCommand : BOOLEAN;
    FUNCTION GetNumRange(OptShort   : CHAR;
                         OptLong    : STRING;
                         DefValue   : INTEGER;
                         Range      : ARRAY OF INTEGER) : INTEGER; overload;

    FUNCTION GetNumRange(OptShort   : CHAR;
                         OptLong    : STRING;
                         DefValue   : INTEGER;
                         MinValue   : INTEGER;
                         MaxValue   : INTEGER) : INTEGER; overload;

    PROCEDURE DoProcess;
    PROCEDURE DoCreate;
    PROCEDURE DoRead;
    PROCEDURE DoWrite;
    PROCEDURE DoCat;
    PROCEDURE DoDump;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

CONST
    {Short form options}
    OptSFCount  = 'c';          // Over-ride catalog filecount
    OptSDFSName = 'd';          // Specify DFS filename to read / write
    OptSExec    = 'e';          // Specify Execution address when writing
    OptSFile    = 'f';          // Specify host file to read / write
    OptSHelp    = 'h';          // Get some help
    OptSLoad    = 'l';          // Specify load address when writing
    OptSLabel   = 'L';          // Set disk label when creating
    OptSOpt     = 'o';          // Set disk opt when creating
    OptSQual    = 'q';          // Specify qualifier, defaults to '$'
    OptSTracks  = 't';          // Specify max tracks when writing (40 or 80).

    {Long form options}
    OptLFCount  = 'count';
    OptLDFSName = 'dfs';
    OptLExec    = 'exec';
    OptLFile    = 'file';
    OptLHelp    = 'help';
    OptLLoad    = 'load';
    OptLLabel   = 'label';
    OptLOpt     = 'opt';
    OptLQual    = 'qual';
    OptLTracks  = 'tracks';

    {Short and long opts as strings for use in CheckOpts}
    ShortOpts   : string = OptSFCount+  ':'+
                           OptSDFSName+ ':'+
                           OptSExec+    ':'+
                           OptSFile+    ':'+
                           OptSHelp+
                           OptSLoad+    ':'+
                           OptSLabel+   ':'+
                           OptSOpt+     ':'+
                           OptSQual+    ':'+
                           OptSTracks+  ':';

    LongOpts    : string = OptLFCount+  ': '+
                           OptLDFSName+ ': '+
                           OptLExec+    ': '+
                           OptLFile+    ': '+
                           OptLHelp+    ' '+
                           OptLLoad+    ': '+
                           OptLLabel+   ': '+
                           OptLOpt+     ': '+
                           OptLQual+    ': '+
                           OptLTracks+  ': ';

    {Valid operations}
    CmdCreate   = 'create';                     // Create a new disk image
    CmdRead     = 'read';                       // Transfer DFS file to host
    CmdWrite    = 'write';                      // Transfer host file to DFS
    CmdCat      = 'cat';                        // Catalog the disk
    CmdDump     = 'dump';                       // Dump the file in hex

    NoCmds      = 5;

    {Note these must be in the same order as opcodes, defined above}
    Commands    : ARRAY [1..NoCmds] OF STRING = (CmdCreate, CmdRead, CmdWrite, CmdCat, CmdDump);

    {Default number of tracks when creating an image}
    DefTracks   = 40;
{ TDFSDisk }

PROCEDURE TDFSDisk.DoHelp;

BEGIN;
  WriteHelp;
  Terminate;
  Exit;
END;

PROCEDURE TDFSDisk.DoCreate;

BEGIN;
  IF (Disk.CreateImage(DFSTracks,DFSQual,DFSLabel,DFSOption)) THEN
    Disk.SaveToFile(DFSImageName);

  WriteLnFmt('Created image file %s, title %s, qual %s, opt %d',[DFSImageName,DFSLabel,DFSQual,DFSOption]);
END;

PROCEDURE TDFSDisk.DoRead;

VAR OutStream   : TFileStream;

BEGIN;
  {check filename given, error if not}
  IF (DFSFileName='') THEN
    Raise Exception.Create('Error: you must specify DFS filename when reading');

  {if output filename not given, default to DFS filename}
  IF (IOFileName='') THEN
    IOFileName:=DFSFileName;

  {Create output file, and read DFS disk file into it}
  OutStream:=TFileStream.Create(IOFileName, fmCreate + fmOpenWrite);
  TRY
    Disk.ReadToStream(DFSFileName,DFSQual,OutStream);
    WriteLnFmt('Read DFS:%s to %s from image %s',[DFSFileName,IOFileName,DFSImageName]);
  FINALLY
    OutStream.Free;
  END;
END;

PROCEDURE TDFSDisk.DoWrite;

VAR InStream    : TFileStream;
    FileNo      : BYTE;

BEGIN;
  {Check for valid input filename}
  IF (IOFileName='') THEN
    Raise Exception.Create('Error: you must specify input filename when writing');

  {If DFS filename not given default to input filename, mangled to be DFS valid}
  IF (DFSFileName='') THEN
    DFSFileName:=GetDFSName(ExtractFileName(IOFileName));

  {Check that file does not already exist, error if so}
  IF (Disk.FindFileNo(DFSFileName,DFSQual) < MaxFileNo) THEN
    Raise Exception.CreateFmt('Error: file %s.%s already exists on disk image %s',[DFSQual,DFSFileName,DFSImageName]);

  {Open input file}
  InStream:=TFileStream.Create(IOFileName, fmOpenRead);
  TRY
    {Input file too big for disk, abort}
    IF (InStream.Size > MaxDFSFileSize) THEN
      Raise Exception.Create('Error: file too big for DFS disk');

    {Not enough free space on disk for input file}
    IF (NOT Disk.HasSpace(InStream.Size)) THEN
      Raise Exception.Create('Error: not enough free space on disk');

    {Write the input file to the DFS disk }
    IF (Disk.WriteFromStream(DFSFileName,DFSQual,DFSLoad,DFSExec,InStream)) THEN
    BEGIN;
      WriteLnFmt('Saving to %s',[DFSImageName]);
      Disk.SaveToFile(DFSImageName);
    END;

    WriteLnFmt('Wrote file %s to DFS:%s on image %s',[IOFileName,DFSFileName,DFSImageName]);
  FINALLY
    InStream.Free;
  END;
END;      https://github.com/prime6809/hello-world.git

PROCEDURE TDFSDisk.DoCat;

VAR FileNo      : BYTE;
    FileName    : TDFSFileName;
    Info        : TDFSFileInfo;

BEGIN;
  {itterate through files on disk printing their details}
  FOR FileNo:=0 TO (Disk.NoFiles-1) DO
  BEGIN
    Disk.GetCatEntry(FileNo,FileName,Info);
    WriteLnFmt('%s.%s %4.4X %4.4X %5.5X %3.3X',[
                            FileName.DirPrefix,Copy(FileName.Name,0,7),
                            Info.LoadAddr,Info.ExecAddr,GetFileSize(Info),
                            Info.StartSec]);
  END;
END;

PROCEDURE TDFSDisk.DoDump;

VAR DumpFile    : TMemoryStream;

BEGIN;
  {If DFS filename not given default to input filename}
  IF (DFSFileName='') THEN
    DFSFileName:=GetDFSName(ExtractFileName(IOFileName));

  {open a memory stream to read the DFS file into}
  DumpFile:=TMemoryStream.Create;
  TRY
    {Read the file to memory}
    Disk.ReadToStream(DFSFileName,DFSQual,DumpFile);

    {If file contains data then dump it}
    IF (DumpFile.Size>0) THEN
    BEGIN;
      DumpFile.Seek(0,soFromBeginning);
      HexDumpMem(DumpFile.Memory,DumpFile.Size,TRUE);
    END;
  FINALLY
    DumpFile.Free;
  END;
END;

FUNCTION TDFSDisk.DecodeCommand : BOOLEAN;

VAR Idx : INTEGER;

BEGIN;
  FOR Idx:=1 TO NoCmds DO
    IF (LowerCase(OpStr)=Commands[Idx]) THEN
      OpCode:=TOpCodes(Idx);

  Result:=(OpCode <> Nop);
END;

PROCEDURE TDFSDisk.DoProcess;

BEGIN;
  IF (OpCode<>DFSCreate) THEN
    Disk.LoadFromFile(DFSImageName);

  CASE OpCode OF
    DFSCreate   : DoCreate;
    DFSRead     : DoRead;
    DFSWrite    : DoWrite;
    DFSCat      : DoCat;
    DFSDump     : DoDump;
  END;
END;

FUNCTION TDFSDisk.GetNumRange(OptShort   : CHAR;
                              OptLong    : STRING;
                              DefValue   : INTEGER;
                              Range      : ARRAY OF INTEGER) : INTEGER;

VAR AIdx    : INTEGER;
    Found   : BOOLEAN;
BEGIN;
  Result:=StrToIntDef(GetOptionValue(OptShort, OptLong),DefValue);
  Found:=FALSE;

  FOR AIdx:=Low(Range) TO High(Range) DO
    IF (Result=Range[AIdx]) THEN
      Found:=TRUE;

  IF (NOT Found) THEN
    Result:=DefValue;
END;

FUNCTION TDFSDisk.GetNumRange(OptShort   : CHAR;
                              OptLong    : STRING;
                              DefValue   : INTEGER;
                              MinValue   : INTEGER;
                              MaxValue   : INTEGER) : INTEGER; overload;

BEGIN;
  Result:=StrToIntDef(GetOptionValue(OptShort, OptLong),DefValue);
  IF (NOT (Result IN [MinValue..MaxValue])) THEN
    Result:=DefValue;
END;

procedure TDFSDisk.DoRun;
var
  ErrorMsg  : String;
  QualStr   : STRING;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions(ShortOpts, LongOpts);

  // parse parameters
  if HasOption(OptSHelp, OptLHelp) then
    DoHelp;

  { add your program here }
  { Get parameters that are not options}
  GetNonOptions(ShortOpts, LongOpts, CmdParams);

  {Get command and DFS disk name}
  IF (CmdParams.Count > 1) THEN
  BEGIN;
    OpStr:=CmdParams.Strings[0];
    DFSImageName:=CmdParams.Strings[1];
  END
  ELSE
    ErrorMsg:=ErrorMsg+'Error must specify at least one operation and DFSImageFile';

  {Conditioanlly get options and their parameters}
  {DFS Filename}
  IF (HasOption(OptSDFSName, OptLDFSName)) THEN
    DFSFileName:=GetOptionValue(OptSDFSName, OptLDFSName);

  {Input or output filename}
  IF (HasOption(OptSFile, OptLFile)) THEN
    IOFileName:=GetOptionValue(OptSFile, OptLFile);

  {DFS Load address}
  IF (HasOption(OptSLoad, OptLLoad)) THEN
    DFSLoad:=StrToIntDef(GetOptionValue(OptSLoad, OptLLoad),$0000);

  {DFS Exec address}
  IF (HasOption(OptSExec, OptLExec)) THEN
    DFSExec:=StrToIntDef(GetOptionValue(OptSExec, OptLExec),$0000);

  {DFS Qualifier}
  IF (HasOption(OptSQual, OptLQual)) THEN
  BEGIN;
    QualStr:=Trim(GetOptionValue(OptSQual, OptLQual));
    IF (Length(QualStr)=1) THEN
      DFSQual:=QualStr[1]
    ELSE IF (Length(QualStr)=3) THEN
      DFSQual:=SrtIntToCharDef(QualStr,'$');
  END;

  {Number of tracks, when creating new image}
  IF (HasOption(OptSTracks, OptLTracks)) THEN
  BEGIN
    DFSTracks:=GetNumRange(OptSTracks, OptLTracks, DefTracks,[40,80]);
    Disk.MaxTracks:=DFSTracks;
  END;

  {Disk label}
  IF (HasOption(OptSLabel, OptLLabel)) THEN
    DFSLabel:=GetOptionValue(OptSLabel, OptLLabel);

  {DFS OPT value}
  IF (HasOption(OptSOpt, OptLOpt)) THEN
    DFSOption:=StrToIntDef(GetOptionValue(OptSOpt, OptLOpt),0);

  {Filecount, when overriding value in catalog sector}
  IF (HasOption(OptSFCount, OptLFCount)) THEN
    Disk.NoFiles:=GetNumRange(OptSFCount, OptLFCount, MaxFileNo, 0,MaxFileNo);

  IF (NOT DecodeCommand) THEN
    ErrorMsg:=ErrorMsg+Format('%s : Invalid command!',[OpStr]);

//  WriteLnFmt('Load=%4.4X, Exec=%4.4X',[DFSLoad, DFSExec]);
//  WriteLnFmt('Qual=''%s'' : %2.2X',[DFSQual,ORD(DFSQual)]);
//  WriteLn(ErrorMsg);

  {If we have errors, display them and terminate}
  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  DoProcess;

  // stop program loop
  Terminate;
end;

{Setup defaults}
constructor TDFSDisk.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  OpStr:='';
  OpCode:=Nop;
  DFSImageName:='';
  DFSFileName:='';
  DFSLoad:=$0000;
  DFSExec:=$0000;
  DFSQual:='$';
  IOFileName:='';
  DFSTracks:=DefTracks;
  CmdParams:=TStringList.Create;
  Disk:=TDFSDiskImage.Create;
end;

destructor TDFSDisk.Destroy;
begin
  CmdParams.Free;
  Disk.Free;
  inherited Destroy;
end;

procedure TDFSDisk.WriteHelp;
begin
  { add your help code here }
  WriteLnFmt('%s <operation> <DFSImage> [<DFSFileName>] [<LoadAddr>] [<ExecAddr>]',[ExeName]);
end;

var
  Application: TDFSDisk;
begin
  Application:=TDFSDisk.Create(nil);
  Application.Title:='DFS Disk writer';
  Application.Run;
  Application.Free;
end.
