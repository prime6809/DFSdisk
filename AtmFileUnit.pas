UNIT AtmFileUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils;

CONST   ATMFileNameLen  = 16;

TYPE
  TAtmHead  = Record
    FileName    : ARRAY[1..ATMFileNameLen] OF CHAR; // Zero filled filename
    LoadAddr    : WORD;
    ExecAddr    : WORD;
    Length      : WORD;
  END;

  TAtmFile   = class(TObject)
  protected
    Header      : TAtmHead;

    FUNCTION GetFileName : STRING;
    PROCEDURE SetFileName(NewName   : STRING);
  public
    Errors      : STRING;
    FileData    : TMemoryStream;

    PROPERTY FileName   : STRING READ GetFileName WRITE SetFileName;
    PROPERTY LoadAddr   : WORD READ Header.LoadAddr WRITE Header.LoadAddr;
    PROPERTY ExecAddr   : WORD READ Header.ExecAddr WRITE Header.ExecAddr;
    PROPERTY DataLength : WORD READ Header.Length WRITE Header.Length;

    Constructor Create;
    Destructor Destroy; override;
    PROCEDURE CopyFromFile(CopyFrom : STRING;
                           CopyTo   : STRING);
    PROCEDURE ReadFromStream(InStream  : TStream);
    PROCEDURE ReadFromFile(InFileName : STRING);
    PROCEDURE WriteToFile(OutFileName : STRING);
  END;

IMPLEMENTATION

Constructor TAtmFile.Create;

BEGIN;
  Errors:='';
  FileName:='';
  LoadAddr:=$2800;
  ExecAddr:=$2800;
  FileData:=TMemoryStream.Create;
  INHERITED Create;
END;

Destructor TAtmFile.Destroy;

BEGIN;
  FileData.Free;
  INHERITED Destroy;
END;

FUNCTION TAtmFile.GetFileName : STRING;

BEGIN;
  Result:=Header.FileName;
END;

PROCEDURE TAtmFile.SetFileName(NewName   : STRING);

VAR MaxChar : INTEGER;
    Idx     : INTEGER;

BEGIN;
  // Zero pad filaneme
  FillChar(Header.FileName,ATMFileNameLen,0);

  // Copy filename
  IF (Length(NewName)<ATMFileNameLen) THEN
    MaxChar:=Length(NewName)
  ELSE
    MaxChar:=ATMFileNameLen;

  FOR Idx:=1 TO MaxChar DO
    Header.FileName[Idx]:=NewName[Idx];
END;

PROCEDURE TAtmFile.CopyFromFile(CopyFrom    : STRING;
                                CopyTo      : STRING);

VAR AtmBuff : TMemoryStream;
    InFile  : TFileStream;

BEGIN;
  AtmBuff:=TMemoryStream.Create;

  TRY
    IF (FileExists(CopyFrom)) THEN
    BEGIN;
      // Open input file
      InFile:=TFileStream.Create(CopyFrom,fmOpenRead);

      // set ATM length
      IF (InFile.Size < $FFFF) THEN
        Header.Length:=InFile.Size;

      // set load addr and exec address
      Header.LoadAddr:=LoadAddr;
      Header.ExecAddr:=ExecAddr;

      AtmBuff.Write(Header, Sizeof(Header));
      AtmBuff.CopyFrom(InFile,Header.Length);
      AtmBuff.SaveToFile(CopyTo);
    END
    ELSE
      Errors:=Format('Error input file %s does not exist!',[CopyFrom]);
  FINALLY
    AtmBuff.Free;
    InFile.Free;
  END;
END;

{Assumes stream is positioned at the beginning of the header data}
PROCEDURE TAtmFile.ReadFromStream(InStream  : TStream);

BEGIN;
  InStream.Read(Header,SizeOf(Header));
  FileData.Seek(0,soFromBeginning);
  FileData.CopyFrom(InStream,Header.Length);
END;

PROCEDURE TAtmFile.ReadFromFile(InFileName : STRING);

VAR FileBuf : TMemoryStream;

BEGIN;
  FileBuf:=TMemoryStream.Create;
  TRY
    IF (FileExists(InFileName)) THEN
    BEGIN;
      FileBuf.LoadFromFile(InFileName);
      FileBuf.Seek(0,soFromBeginning);
      ReadFromStream(FileBuf);
{      FileBuf.Read(Header,Sizeof(Header));
      FileData.Seek(0,soFromBeginning);
      FileData.CopyFrom(FileBuf,Header.Length);
}    END;
  FINALLY
    FileBuf.Free;
  END;
END;

PROCEDURE TAtmFile.WriteToFile(OutFileName : STRING);

VAR FileBuf : TMemoryStream;

BEGIN;
  FileBuf:=TMemoryStream.Create;
  TRY
    FileBuf.Write(Header,SizeOf(Header));
    FileData.Seek(0,soFromBeginning);
    FileBuf.CopyFrom(FileData,Header.Length);
    FileBuf.SaveToFile(OutFileName);
  FINALLY
    FileBuf.Free;
  END;
END;

END.

