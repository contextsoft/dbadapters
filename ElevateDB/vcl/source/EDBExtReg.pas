(******************************************************************************)
(*
(*  Context Database Extensions Suite (ElevateDB)
(*
(*  Package registration unit.
(*
(*  Copyright (c) 2004-2006 Michael Baytalsky
(*
(******************************************************************************)
unit EDBExtReg;

interface

procedure Register;

implementation

uses Classes, Dialogs, Controls, EDBComps, EDBExt, EDBOpenDatabase,
{$IFDEF VER130}
DsgnIntf;
{$ELSE}
DesignEditors, DesignIntf;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Database Extensions', [TEDBDatabaseExt]);
  RegisterComponents('Database Extensions', [TEDBOpenDatabaseDialog]);
end;


end.
