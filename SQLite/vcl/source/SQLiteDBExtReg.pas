(******************************************************************************)
(*
(*  Context Database Extensions Suite (SQLite)
(*
(*  Package registration unit.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit SQLiteDBExtReg;

interface

procedure Register;

implementation

uses Classes, Dialogs, Controls, ASGSQLite3, SQLiteDBExt, SQLiteOpenDatabase,
{$IFDEF VER130}
  DsgnIntf;
{$ELSE}
  DesignEditors, DesignIntf;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Database Extensions', [TSQLiteDatabaseExt, TSQLiteOpenDatabaseDialog]); 
end;

end.
