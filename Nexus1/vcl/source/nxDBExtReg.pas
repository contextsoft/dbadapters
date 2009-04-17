(******************************************************************************)
(*
(*  Context Database Extensions Suite (Nexus)
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

uses Classes, dbManager, nxdb, nxDBExt, Dialogs, Controls, nxOpenDatabase,
nxreRemoteServerEngine, nxtwWinsockTransport,
nxllComponent, nxsrServerEngine, nxsqlEngine,
{$IFDEF VER130}
DsgnIntf;
{$ELSE}
DesignEditors, DesignIntf;
{$ENDIF}

{$R nxOpenDatabase.dcr}

{ TnxTableNameProperty }

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TnxTableNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure Register;
begin
  RegisterComponents('Database Extensions', [
    TnxDatabaseExt,
    TnxTableExt,
    TnxQueryExt]);

  RegisterComponents('Database Extensions', [TnxOpenDatabaseDialog]);

  RegisterPropertyEditor(TypeInfo(string),TnxDatabaseExt,'SystemTableName',
    TnxTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TnxDatabaseExt,'ObjectsTableName',
    TnxTableNameProperty);
end;

{ TDBStringProperty }

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TnxTableNameProperty }

procedure TnxTableNameProperty.GetValueList(List: TStrings);
var
  Database: TnxDatabaseExt;
begin
  Database := GetComponent(0) as TnxDatabaseExt;
  Database.Session.GetTableNames(Database.DatabaseName, List);
end;

end.
