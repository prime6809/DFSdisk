UNIT DFSUtilUnit;

{$mode objfpc}{$H+}

INTERFACE

uses
  Classes, SysUtils,DFSDiskUnit;

FUNCTION GetFileSize(Info   : TDFSFileInfo) : LONGWORD;
FUNCTION GetStartSector(Info   : TDFSFileInfo) : LONGWORD;
FUNCTION GetNoSectors(Info   : TDFSFileInfo) : LONGWORD;

FUNCTION TruncateAndPad(InStr   : STRING;
                        MaxLen  : INTEGER;
                        PadChar : CHAR) : STRING;

FUNCTION GetDFSName(Name    : STRING) : STRING;

FUNCTION AddBCD(BCDNum    : BYTE;
                ToAdd     : BYTE) : BYTE;

implementation

FUNCTION TruncateAndPad(InStr   : STRING;
                        MaxLen  : INTEGER;
                        PadChar : CHAR) : STRING;
BEGIN;
  IF (Length(InStr) > MaxLen) THEN
    Result:=Copy(InStr,1,MaxLen)
  ELSE
    Result:=InStr+StringOfChar(PadChar,MaxLen-Length(InStr));
END;

FUNCTION GetFileSize(Info   : TDFSFileInfo) : LONGWORD;
BEGIN;
  Result:=Info.FileLen+((Info.ExtraBits AND FileLengthBits1617) SHL 12);
END;

FUNCTION GetStartSector(Info   : TDFSFileInfo) : LONGWORD;

BEGIN
  Result:=Info.StartSec+((Info.ExtraBits AND StartSectorBits89) SHL 8);
END;

FUNCTION GetNoSectors(Info   : TDFSFileInfo) : LONGWORD;

VAR FSize   : LONGINT;

BEGIN
  FSize:=GetFileSize(Info);
  Result:=FSize DIV DFSBlockSize;

  IF ((FSize MOD DFSBlockSize)<>0) THEN
    Result:=Result+1;
END;

FUNCTION GetDFSName(Name    : STRING) : STRING;

BEGIN;
  Result:=TruncateAndPad(ChangeFileExt(Name,''),7,' ');
END;
{Add a binary constant to a BCD constant, and return as BCD, overflow ignored!}
FUNCTION AddBCD(BCDNum    : BYTE;
                ToAdd     : BYTE) : BYTE;

VAR BinNum  : BYTE;

BEGIN;
  BinNum:=(((BCDNum AND $F0) SHR 4)*10) + (BCDNum AND $0F);
  BinNum:=BinNum+ToAdd;
  Result:=((BinNum DIV 10) SHL 4) + (BinNum MOD 10);
END;

END.

