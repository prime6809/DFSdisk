unit DFSDiskUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConsoleUtils;

CONST
        { Masks/Shifts within TDFSFileInfo.ExtraBits }

        StartSectorBits89	= $03;	// Start sector bits 8 and 9
        LoadAddrBits1617	= $0C;	// Load address bits 16 and 17
        FileLengthBits1617	= $30;	// File length bits 16 and 17
        ExecAddrBits1617	= $C0;	// File exec address bits 16 and 17

        { Bit 7 of some of the file name characters are the upper bits of some }
        { of the quantities on WEDFS }

        FileLengthBit18		= 5;	// Top bit of file length
        StartSectorBit10	= 6;	// Top bit of start sector

        { Masks for TDFSDiskHeader.Flags }

        TotalSectorsBits8to10	= $07;	// Bits 8,9 & 10 of total sectors
        BootFlagOptions		    = $30;	// *OPT 4,x values

        MaxCatalogFiles		= 62;
        DFSBlockSize	    = 256;	// 256 bytes / sector
        DFSBlockPerTrack    = 10;   // 10 sectors / track
        DFSOneTrack         = DFSBlockPerTrack * DFSBlockSize;

        CATEntryLen         = 8;    // Length of each catalog entry (or filename + qual)

        InfFileExt		= '.inf';
        LockedStr		= 'Locked';

        MaxNameLength   = 7;        // Maximum length of DFS filename.
        InvalidFileNo   = 255;      // Functions return this if file cannot be found
        MaxFileNo       = 30;       // File numbers are 0 to this (31 files)
        MaxDFSFileSize  = $3FFFF;   // Maximum DFS file size (256K-1).

TYPE
    { Filename entry within the first sector of the catalog there}
    { are 31 of these }
    TDFSFileName	= PACKED RECORD
      Name	        : ARRAY[0..6]OF CHAR;
      DirPrefix	    : CHAR;
    END;

    {File details entry, within the second sector of catalog, one for each file}
    TDFSFileInfo	= PACKED RECORD
      LoadAddr	: WORD;		// Bits 0..15 of load address
      ExecAddr	: WORD;		// Bits 0..15 of execution address
      FileLen	: WORD;		// Bits 0..15 of file length
      ExtraBits	: BYTE;		// Extra bits see above for details
      StartSec	: BYTE;		// Start sector of file
    END;

    {Catalog sector record}
    TDFSCatalog     = PACKED RECORD
      DiskName      : ARRAY[0..7]OF CHAR;   // Disk name first 8 chars
      Entries       : ARRAY[0..MaxFileNo]OF TDFSFileName;   // File name entries
    END;

    {File info sector record}
    TDFSCatalogInfo = PACKED RECORD
      DiskName      : ARRAY[0..3]OF CHAR;   // Disk name last 4 chars
      CycleCount    : BYTE;                 // Disk cycle count (BBC DFS), extra title byte System / Atom
      NumEntries    : BYTE;                 // Num catalog entries * CATEntryLen (8)
      OptionBits    : BYTE;                 // Disk options + MS 2 bits of Total sectors.
      TotalSecLSB   : BYTE;                 // LSB of total sectors on disk
      FileInfo      : ARRAY[0..MaxFileNo]OF TDFSFileInfo;   // File information recrds
    END;

TYPE
  TDFSDiskImage = Class(TObject)
  PROTECTED
    DiskMem     : TMemoryStream;
    Catalog     : TDFSCatalog;
    CatInfo     : TDFSCatalogInfo;
    FMaxTracks  : BYTE;
    FWritePos   : LONGINT;
    FFileCount  : BYTE;

    FUNCTION GetNoFiles : BYTE;
    PROCEDURE SetNoFiles(NoFiles    : BYTE);
    PROCEDURE SeekToSector(SectorNo : LONGWORD);
    PROCEDURE InsertCat(FileName    : STRING;
                        Qual        : CHAR;
                        LoadAddr    : LONGWORD;
                        ExecAddr    : LONGWORD;
                        FileLen     : LONGWORD);
  PUBLIC
    PROPERTY NoFiles    : BYTE READ GetNoFiles WRITE SetNoFiles;
    PROPERTY MaxTracks  : BYTE READ FMaxTracks WRITE FMaxTracks;

    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; override;

    PROCEDURE LoadFromFile(FileName : STRING);
    PROCEDURE SaveToFile(FileName : STRING);
    PROCEDURE GetCatEntry(EntryNo   : BYTE;
                          VAR Name  : TDFSFileName;
                          VAR Info  : TDFSFileInfo);
    FUNCTION FindFileNo(FileName : STRING;
                        Qual     : CHAR) : BYTE;
    FUNCTION ReadToStream(FileName  : STRING;
                          Qual      : CHAR;
                          StreamOut : TStream) : BOOLEAN;
    FUNCTION HasSpace(FileLen   : LONGINT) : BOOLEAN;
    FUNCTION WriteFromStream(FileName   : STRING;
                             Qual       : CHAR;
                             LoadAddr   : LONGWORD;
                             ExecAddr   : LONGWORD;
                             StreamIn   : TStream) : BOOLEAN;
    FUNCTION CreateImage(DiskTracks     : WORD;
                         Qual           : CHAR;
                         DiskLabel      : STRING;
                         Option         : BYTE) : BOOLEAN;
    FUNCTION DeleteFile(FileName        : STRING;
                        Qual            : CHAR) : BOOLEAN;
  END;



implementation

uses DFSUtilUnit;

CONSTRUCTOR TDFSDiskImage.Create;

BEGIN;
  INHERITED Create;
  DiskMem:=TMemoryStream.Create;
  FMaxTracks:=40;
  FWritePos:=-1;
  FFileCount:=$FF;
END;

DESTRUCTOR TDFSDiskImage.Destroy;

BEGIN;
  DiskMem.Free;

  INHERITED Destroy;
END;

{Load disk image from disk file}
PROCEDURE TDFSDiskImage.LoadFromFile(FileName : STRING);

BEGIN;
  IF (FileExists(FileName)) THEN
  BEGIN;
    WITH DiskMem DO
    BEGIN;
      LoadFromFile(FileName);
      Seek(0,soFromBeginning);
      Read(Catalog,Sizeof(Catalog));
      Read(CatInfo,Sizeof(CatInfo));
    END;
  END;
END;

{Save disk image to disk file}
PROCEDURE TDFSDiskImage.SaveToFile(FileName : STRING);

VAR Tries   : BYTE;
    Success : BOOLEAN;

BEGIN;
  Tries:=0;
  Success:=FALSE;

  REPEAT;
    TRY
      DiskMem.SaveToFile(FileName);
      Success:=TRUE;
    EXCEPT
      ON E: EFCreateError DO
      BEGIN;
        Tries:=Tries+1;
        sleep(500);
      END;
    END;
  UNTIL (Success OR (Tries=3));

  IF (NOT Success) THEN
    WriteLnFmt('FAILED to write file after %d attempts',[Tries+1]);
END;

{Get a catalog entry for supplied file number}
PROCEDURE TDFSDiskImage.GetCatEntry(EntryNo   : BYTE;
                                    VAR Name  : TDFSFileName;
                                    VAR Info  : TDFSFileInfo);

BEGIN;
  IF (EntryNo IN [0..30]) THEN
  BEGIN
    Name:=Catalog.Entries[EntryNo];
    Info:=CatInfo.FileInfo[EntryNo];
  END;
END;

{Return number of valid catalog entries on disk}
FUNCTION TDFSDiskImage.GetNoFiles : BYTE;

BEGIN
  IF (FFileCount IN [0..MaxFileNo]) THEN
    Result:=FFileCount
  ELSE
    Result:=CatInfo.NumEntries DIV CATEntryLen;
END;

PROCEDURE TDFSDiskImage.SetNoFiles(NoFiles    : BYTE);

BEGIN;
  IF (NoFiles IN [0..MaxFileNo]) THEN
    FFileCount:=NoFiles;
END;

{Seek to an absolute sector on the disk image}
PROCEDURE TDFSDiskImage.SeekToSector(SectorNo : LONGWORD);

BEGIN
  DiskMem.Seek((SectorNo * DFSBlockSize), soFromBeginning);
END;

{search the catalog for the specified file and qualifier                  }
{returns either the file number, if the file is found or InvalidFileNo if }
{the file is not found                                                    }
FUNCTION TDFSDiskImage.FindFileNo(FileName : STRING;
                                  Qual     : CHAR) : BYTE;

VAR FileNo  : BYTE;
    DFSName : STRING;

BEGIN;
  Result:=InvalidFileNo;

  IF (GetNoFiles > 0) THEN
  BEGIN;
    FileNo:=0;
    DFSName:=GetDFSName(FileName);
    WHILE ((FileNo < NoFiles) AND
           ((DFSName <> Catalog.Entries[FileNo].Name) OR
            (Qual <> Catalog.Entries[FileNo].DirPrefix))) DO
      FileNo:=FileNo+1;

    IF (FileNo < GetNoFiles) THEN
      Result:=FileNo
    ELSE
      Result:=InvalidFileNo;
  END
END;

{Copy the data for a file from the disk image to another stream }
FUNCTION TDFSDiskImage.ReadToStream(FileName    : STRING;
                                    Qual        : CHAR;
                                    StreamOut   : TStream) : BOOLEAN;

VAR FileNo  : BYTE;
    FLength : LONGINT;

BEGIN
  Result:=FALSE;

  {Get fileno of required file}
  FileNo:=FindFileNo(FileName, Qual);

  {If fileno valid, seek to it's position and copy it to output stream}
  IF (FileNo <= MaxFileNo) THEN
  BEGIN;
    FLength:=GetFileSize(CatInfo.FileInfo[FileNo]);
    SeekToSector(GetStartSector(CatInfo.FileInfo[FileNo]));
    StreamOut.CopyFrom(DiskMem,FLength);
  END;
END;

{Check to see if the current image has sufficient space to write a file to}
FUNCTION TDFSDiskImage.HasSpace(FileLen   : LONGINT) : BOOLEAN;

VAR SpaceAvailable  : LONGINT;

BEGIN;
  { Calculate maximum space available, no of full tracks - 2 sectors for catalog }
  SpaceAvailable:=(FMaxTracks * DFSOneTrack) - (2 * DFSBlockSize);

  { If we have files on the disk then the first in the catalog is the last     }
  { physically on the disk, so we can work out the first free sector by taking }
  { the first file's start, sector and adding it's length                      }
  IF (GetNoFiles > 0) THEN
  BEGIN
    FWritePos:=(GetStartSector(CatInfo.FileInfo[0]) +
                GetNoSectors(CatInfo.FileInfo[0])) * DFSBlockSize;

    SpaceAvailable:=SpaceAvailable-FWritePos;
  END
  ELSE
    FWritePos:=2*DFSBlockSize;  // Blank disk, start writing after catalog.

  Result:=((FileLen <= SpaceAvailable) AND (GetNoFiles < MaxFileNo));
END;

{Insert a new catalog entry at the beginning of the catalog, moving all current}
{entries down to make room, and then fill in supplied filename etc.            }
PROCEDURE TDFSDiskImage.InsertCat(FileName  : STRING;
                                  Qual      : CHAR;
                                  LoadAddr  : LONGWORD;
                                  ExecAddr  : LONGWORD;
                                  FileLen   : LONGWORD);

VAR EntryNo     : INTEGER;
    StartSec    : WORD;

BEGIN;
  {Move all catalog entries down one}
  FOR EntryNo:=MaxFileNo DOWNTO 1 DO
  BEGIN
    Catalog.Entries[EntryNo]:=Catalog.Entries[EntryNo-1];
    CatInfo.FileInfo[EntryNo]:=CatInfo.FileInfo[EntryNo-1];
  END;

  {Fill in filename & qualifier}
  WITH Catalog.Entries[0] DO
  BEGIN;
    Name:=GetDFSName(FileName);
    DirPrefix:=Qual;
  END;

  {Fill in resto of file data}
  StartSec:=FWritePos DIV DFSBlockSize;

  WITH CatInfo DO
  BEGIN;
    FileInfo[0].LoadAddr := LoadAddr AND $FFFF;
    FileInfo[0].ExecAddr := ExecAddr AND $FFFF;
    FileInfo[0].FileLen  := FileLen  AND $FFFF;
    FileInfo[0].StartSec := StartSec AND $FF;
    FileInfo[0].ExtraBits := ((ExecAddr AND $30000) SHR 10) +
                             ((FileLen  AND $30000) SHR 12) +
                             ((LoadAddr AND $30000) SHR 14) +
                             ((StartSec AND $300)   SHR 8);
    NumEntries:=NumEntries+CATEntryLen;
    CycleCount:=AddBCD(CycleCount,1);
  END;
END;

{Write a file from a stream to the disk image }
FUNCTION TDFSDiskImage.WriteFromStream(FileName   : STRING;
                                       Qual       : CHAR;
                                       LoadAddr   : LONGWORD;
                                       ExecAddr   : LONGWORD;
                                       StreamIn   : TStream) : BOOLEAN;

VAR FileNo  : BYTE;

BEGIN
  Result:=FALSE;

  {Check to see if file already exists on disk image}
  FileNo:=FindFileNo(FileName,Qual);

  {If we have enough free space, and file does not exist write it to disk}
  IF ((HasSpace(StreamIn.Size)) AND (FileNo > MaxFileNo)) THEN
  BEGIN;
    {Insert a catalog entry for new file}
    InsertCat(FileName,Qual,LoadAddr,ExecAddr,StreamIn.Size);

    {Position input and write pointers}
    StreamIn.Seek(0,soFromBeginning);
    DiskMem.Seek(FWritePos,soFromBeginning);

    {Copy file data}
    DiskMem.CopyFrom(StreamIn,StreamIn.Size);

    {Rewrite catalog data}
    DiskMem.Seek(0,soFromBeginning);
    DiskMem.Write(Catalog,SizeOf(Catalog));
    DiskMem.Write(CatInfo,SizeOf(CatInfo));
    Result:=TRUE;
  END;
END;

{Create a new disk image, based on the supplied parameters}
FUNCTION TDFSDiskImage.CreateImage(DiskTracks     : WORD;
                                   Qual           : CHAR;
                                   DiskLabel      : STRING;
                                   Option         : BYTE) : BOOLEAN;

VAR SectorCount : WORD;
    FileNo      : BYTE;

BEGIN;
  Result:=FALSE;

  FMaxTracks:=DiskTracks;
  DiskLabel:=TruncateAndPad(DiskLabel,12,' ');

  SectorCount:=FMaxTracks * DFSBlockPerTrack;

  {Initialize first two sectors to 0}
  FillChar(Catalog,SizeOf(Catalog),0);
  FillChar(CatInfo,SizeOf(CatInfo),0);

  {Fill in disk label}
  Catalog.DiskName:=Copy(DiskLabel,1,8);

  {Fill in qualifier}
  FOR FileNo:=0 TO MaxFileNo DO
    Catalog.Entries[FileNo].DirPrefix:=Qual;

  {Fill in other disk info}
  WITH CatInfo DO
  BEGIN;
    DiskName:=Copy(DiskLabel,9,4);
    CycleCount:=$00;
    NumEntries:=0;
    TotalSecLSB:=SectorCount AND $FF;
    OptionBits:=((Option AND $03) SHL 6) + ((SectorCount AND $300) SHR 8);
  END;

  {Write catalog}
  DiskMem.Write(Catalog,SizeOf(Catalog));
  DiskMem.Write(CatInfo,SizeOf(CatInfo));
  DiskMem.SetSize(SectorCount * DFSBlockSize);

  Result:=TRUE;
END;

FUNCTION TDFSDiskImage.DeleteFile(FileName        : STRING;
                                  Qual            : CHAR) : BOOLEAN;

VAR FileNo  : BYTE;
    EntryNo : INTEGER;

BEGIN;
  Result:=FALSE;    {assume we will fail!}

  FileNo:=FindFileNo(FileName,Qual);

  { If file found then delete it! }
  IF (FileNo <> InvalidFileNo) THEN
  BEGIN;
    {Move all catalog entries after file to be deleted up one}
    FOR EntryNo:=FileNo TO MaxFileNo DO
    BEGIN
      Catalog.Entries[EntryNo]:=Catalog.Entries[EntryNo+1];
      CatInfo.FileInfo[EntryNo]:=CatInfo.FileInfo[EntryNo+1];
    END;
    { Always blank the last entry, just incase disk had max files }
    FillChar(Catalog.Entries[MaxFileNo],Sizeof(Catalog.Entries[MaxFileNo]),0);
    FillChar(CatInfo.FileInfo[MaxFileNo],Sizeof(CatInfo.FileInfo[MaxFileNo]),0);

    {Decrement filecount}
    CatInfo.NumEntries:=CatInfo.NumEntries - CATEntryLen;

    {Rewrite catalog data}
    DiskMem.Seek(0,soFromBeginning);
    DiskMem.Write(Catalog,SizeOf(Catalog));
    DiskMem.Write(CatInfo,SizeOf(CatInfo));

    {Flag file deleted}
    Result:=TRUE;
  END;
END;

end.

