(******************************************************************************)
(*
(*  Context Database Extensions Suite (DBISAM)
(*
(*  Package registration unit.
(*
(*  Copyright (c) 2004-2006 Michael Baytalsky
(*
(******************************************************************************)
unit DBISAMExtReg;

interface

procedure Register;

implementation

uses Classes, DBISAMTb, DBISAMExt, DBISAMEv, DBISAMOpenDatabase,
  dbManager, Dialogs, Controls, 
{$IFDEF VER130}
DsgnIntf;
{$ELSE}
DesignEditors, DesignIntf;
{$ENDIF}

{$R DBISAMOpenDatabase.dcr}

{ TDBISAMTableNameProperty }

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDBISAMTableNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure Register;
begin
  RegisterComponents('Database Extensions', [
    TDBISAMDatabaseExt,
    TDBISAMTableExt,
    TDBISAMQueryExt]);
  RegisterComponents('Database Extensions', [TDBISAMEvents]);
  RegisterComponents('Database Extensions', [TDBISAMOpenDatabaseDialog]);

  RegisterPropertyEditor(TypeInfo(string), TDBISAMDatabaseExt,'SystemTableName',
    TDBISAMTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBISAMDatabaseExt,'ObjectsTableName',
    TDBISAMTableNameProperty);
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

{ TDBISAMTableNameProperty }

procedure TDBISAMTableNameProperty.GetValueList(List: TStrings);
var
  Database: TDBISAMDatabaseExt;
begin
  Database := GetComponent(0) as TDBISAMDatabaseExt;
  Database.Session.GetTableNames(Database.DatabaseName, List);
end;


end.
