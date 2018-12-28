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
  private
    Header      : TAtmHead;
  public
    FileName    : STRING;
    LoadAddr    : WORD;
    ExecAddr    : WORD;
    Errors      : STRING;

    Constructor Create;
    Destructor Destroy; override;
    PROCEDURE CopyFromFile(CopyFrom : STRING;
                           CopyTo   : STRING);
  END;

IMPLEMENTATION

Constructor TAtmFile.Create;

BEGIN;
  Errors:='';
  FileName:='';
  LoadAddr:=$2800;
  ExecAddr:=$2800;
  INHERITED Create;
END;

Destructor TAtmFile.Destroy;

BEGIN;
  INHERITED Destroy;
END;

PROCEDURE TAtmFile.CopyFromFile(CopyFrom    : STRING;
                                CopyTo      : STRING);

VAR AtmBuff : TMemoryStream;
    InFile  : TFileStream;
    Idx     : INTEGER;
    MaxChar : INTEGER;

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

      // Zero pad filaneme
      FillChar(Header.FileName,ATMFileNameLen,0);

      // Copy filename
      IF (Length(FileName)<ATMFileNameLen) THEN
        MaxChar:=Length(FileName)
      ELSE
        MaxChar:=ATMFileNameLen;

      FOR Idx:=1 TO MaxChar DO
        Header.FileName[Idx]:=FileName[Idx];

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

END.

