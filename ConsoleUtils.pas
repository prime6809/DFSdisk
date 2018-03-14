unit ConsoleUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

PROCEDURE WriteFmt(ToWrite : STRING;
                   Params  : ARRAY OF CONST);
PROCEDURE WriteLnFmt(ToWrite : STRING;
                     Params  : ARRAY OF CONST);

FUNCTION SrtIntToCharDef(IntStr     : STRING;
                         Default    : CHAR) : CHAR;

PROCEDURE HexDumpMem(Buff	: PChar;
    		         Size	: INTEGER;
                     Head   : BOOLEAN = TRUE);

implementation

PROCEDURE WriteFmt(ToWrite : STRING;
                   Params  : ARRAY OF CONST);
BEGIN;
  Write(Format(ToWrite,Params));
END;

PROCEDURE WriteLnFmt(ToWrite : STRING;
                     Params  : ARRAY OF CONST);
BEGIN;
  WriteLn(Format(ToWrite,Params));
END;

FUNCTION SrtIntToCharDef(IntStr     : STRING;
                         Default    : CHAR) : CHAR;

VAR TempInt : INTEGER;

BEGIN;
  TempInt:=StrToIntDef(IntStr,ORD(Default));
  IF ((TempInt > -1) AND (TempInt < 256)) THEN
    Result:=CHR(TempInt)
  ELSE
    Result:=Default;
END;

PROCEDURE HexDumpMem(Buff	: PChar;
    	             Size	: INTEGER;
                     Head   : BOOLEAN = TRUE);

CONST	LineLen	= 16;

VAR     Line	: STRING;
	    Chars	: STRING;
        IdxHex	: STRING;
	    Idx		: INTEGER;
      	Ch		: CHAR;
        Last	: INTEGER;
BEGIN;
  IF (Head) THEN
  BEGIN;
    WriteLn('Addr 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F ASCII');
	WriteLn('----------------------------------------------------------');
  END;

  IdxHex:='0000';

  Last:=Size DIV LineLen;
  IF ((Size MOD LineLen)<>0) 	THEN
    Last:=Last+1;
  Last:=Last*LineLen;

  FOR Idx:=0 TO Last DO
  BEGIN;
    IF (((Idx MOD LineLen)=0) AND (Idx<>0)) THEN
    BEGIN;
      WriteLnFmt('%s %s%s',[IdxHex,Line,Chars]);
      IdxHex:=IntToHex(Idx,4);
      Line:='';
      Chars:='';
    END;

    IF (Idx<(Size-1)) THEN
      Ch:=Buff[Idx]
    ELSE
      Ch:=#0;

    Line:=Line+IntToHex(ORD(Ch),2)+' ';
    IF (Ch>=' ') THEN Chars:=Chars+Ch ELSE Chars:=Chars+' ';
  END;
END;


end.

