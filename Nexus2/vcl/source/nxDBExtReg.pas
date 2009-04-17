(******************************************************************************)
(*
(*  Context Database Extensions Suite (Nexus2)
(*
(*  Package registration unit.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit nxDBExtReg;

interface

procedure Register;

implementation

uses Classes, Dialogs, Controls, nxdb, nxDBExt, nxOpenDatabase,
{$IFDEF VER130}
  DsgnIntf;
{$ELSE}
  DesignEditors, DesignIntf;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Database Extensions', [
    TnxDatabaseExt, TnxOpenDatabaseDialog]); 
end;

end.
