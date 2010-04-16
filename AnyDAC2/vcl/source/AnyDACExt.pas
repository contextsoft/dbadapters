(******************************************************************************)
(*
(*  Context Database Extensions Suite (AnyDAC)
(*
(*  Contains: TADConnectionExt component.
(*
(*  Copyright (c) 2008 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : AnyDACExt.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 2.15
(*  DELPHI      : Delphi 5,6,7,2005,2007
(*
(******************************************************************************)
{I uAD.inc}

unit AnyDACExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, uADCompClient, uADPhysIntf,
  CtxDataTypes, CtxDataSetCommand, 
  CtxDBIntf, dbSchema;

type
  TADConnectionExt = class;

  {:$ TADRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TADRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TADConnectionExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TADConnectionExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a AnyDAC connection. }
  TADConnectionExt = class(TADConnection, ISchemaDatabase)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: String;
    FVersionStr: String;

    function GetDatabaseName: String;
    procedure SetDatabaseName(const Value: String);
    function GetVersionLabel: String;
    procedure SetVersionLabel(const Value: String);

    { Replication support }
    function GetSchema: TDatabaseSchema;
    procedure SetSchema(Value: TDatabaseSchema);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDatabaseURL: String;
    procedure SetDatabaseURL(const Value: String);

    function GetDriverName: String;

    { Checks if the database is connected }
    procedure CheckActive;
    { Checks if schema is assigned }
    procedure CheckSchema;
    procedure DoConnect; override;
  public
    {:: Creates an instance of a TADConnectionExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TADConnectionExt component.}
    destructor Destroy; override;

    {:: Creates TADTable component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TADQuery component, corresponding to Statement. }
    function CreateQuery(const Statement: String): TDataSet;

    { Schema related methods. Schema must be assigned for them to work }
    {:$ Updates database schema from the physical database tables. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be updated. }
    procedure ReverseEngineer;

    {:$ Checks whether the database version is *equal* or Newer }
    {:$ then the current Schema's Version. }
    function IsVersionCurrent(AllowNewer: Boolean = False): Boolean;
    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);

    function GetInTransaction: Boolean;

    function FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;

    function GetRangeCursor(const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = ''): TDBRangeCursor; overload;
    function GetRangeCursor(Relation: TRelation; KeyValues: Variant): TDBRangeCursor; overload;

    { Returns assignable SQL property. }
    function GetQuerySQL(Query: TDataSet): String;
    procedure SetQuerySQL(Query: TDataSet; const Statement: String);
    { Returns assignable Params property. }
    procedure GetQueryParams(Query: TDataSet; Params: TParams);
    procedure SetQueryParams(Query: TDataSet; Params: TParams);
    { Executes query that does not return result set. }
    procedure ExecSQL(Query: TDataSet);
    function ExecuteStatement(SQL: String; ResultSet: Pointer): Integer;

    function CreateCommand: TCtxDataCommand;

    { Parent object is always a table or schema. }
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;
    function GetSystemTableName: String;
  published
    { Published properties }
    {:$ Reference to a TDatabaseSchema component. }
    {:: Schema may contain information about the database structure as weel as some }
    {:: additional information like referential integrity constraints, triggers and more. }
    property Schema: TDatabaseSchema read FSchema write SetSchema;
    {:$ Text presentation of the database version. }
    {:: This property is effectively read-only. Any text assigned to it will be ignored. }
    property VersionLabel: String read GetVersionLabel write SetVersionLabel stored False;
    {:$ The name of the System table. }
    {:: The default value of this property is 'system'. }
    property SystemTableName: String read GetSystemTableName write FSystemTableName;
    {:$ Specifies the uniform path to the database for both local and C/S types of access. }
    {:: This parameter is usefull for displaying or storing database and session parameters }
    {:: for the specific database.<br> }
    {:: Attention! Assigning DatabaseURL may change the parameters of the session component }
    {:: the database is attached to. }
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL stored false;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TADConnection component and the connected Session component. }
  function GetDatabaseURL(ADConnection: TADConnection): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(ADConnection: TADConnection; DatabaseURL: String);

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

implementation

{ General Helper Rountines }

procedure Register;
begin
  RegisterComponents('Database Extensions', [TADConnectionExt]);
end;

function GetDatabaseURL(ADConnection: TADConnection): String;
begin
  Result := ADConnection.ResultConnectionDef.BuildString();
end;

procedure SetDatabaseURL(ADConnection: TADConnection; DatabaseURL: String);
begin
  ADConnection.ResultConnectionDef.ParseString(DatabaseURL);
end;

{ TADRangeCursor }

constructor TADRangeCursor.Create(Database: TADConnectionExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TADRangeCursor.CreateExt(Database: TADConnectionExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TADTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TADTable;
  DataSet := Table; // Assign inherited field

  // Do not set any range if no KeyFields specified
  if KeyFields = '' then exit;

  with Table do
  begin
    RangeFilter := CreateRangeFilter(KeyFieldsList, KeyValues);
    if TableFilter <> '' then
      RangeFilter := '(' + TableFilter + ') and (' + RangeFilter + ')';
    Filter := RangeFilter;
    if CaseInsensitive then
      FilterOptions := [foCaseInsensitive]
    else FilterOptions := [];
    Filtered := True;
  end;
end;

destructor TADRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TADConnectionExt }

constructor TADConnectionExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'SysTable';
  FVersionStr := '';
end;

destructor TADConnectionExt.Destroy;
begin
  DBDatabases.Remove(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TADConnectionExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TADConnectionExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TADConnectionExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TADConnectionExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TADConnectionExt.GetVersionLabel: String;
begin
  if Connected then
  begin
    if FVersionStr = '' then
      FVersionStr := VersionToStr(GetVersion);
    Result := FVersionStr;
  end else
    Result := VersionToStr(Undefined);
end;

function TADConnectionExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TADConnectionExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TADConnectionExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TADConnectionExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

function TADConnectionExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TADConnectionExt.GetDriverName: String;
begin
  Result := 'AnyDAC';
end;

function TADConnectionExt.GetDatabaseURL: String;
begin
  Result := AnyDACExt.GetDatabaseURL(Self);
end;

procedure TADConnectionExt.SetDatabaseURL(const Value: String);
begin
  AnyDACExt.SetDatabaseURL(Self, Value);
end;

function TADConnectionExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TADRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TADConnectionExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TADRangeCursor.Create(Self, Relation, KeyValues);
end;

function TADConnectionExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TADConnectionExt.GetDatabaseName: String;
begin
  Result := inherited ConnectionName;
end;

procedure TADConnectionExt.SetDatabaseName(const Value: String);
begin
  inherited ConnectionName := Value;
  if DatabaseName <> '' then
  begin
    if DBDatabases.IndexOf(Self) < 0 then
      DBDatabases.Add(Self);
  end else begin
    if DBDatabases <> nil then
      DBDatabases.Remove(Self);
  end;
end;

function TADConnectionExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TADTable.Create(nil);
  try
    TADTable(Result).Connection := Self;
    TADTable(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TADConnectionExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TADQuery.Create(nil);
  with TADQuery(Result) do
  try
    Connection := Self;
    // Assign SQL statement
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TADConnectionExt.ExecSQL(Query: TDataSet);
begin
  TADQuery(Query).ExecSQL;
end;

function TADConnectionExt.ExecuteStatement(SQL: String; ResultSet: Pointer): Integer;
var
  Q: TADQuery;
begin
  Q := TADQuery(CreateQuery(SQL));
  if Q.OpenOrExecute then
    TDataSet(ResultSet^) := Q;
  Result := Q.RowsAffected;
end;

procedure TADConnectionExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TADQuery(Query).Params);
end;

procedure TADConnectionExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TADQuery(Query).Params.Assign(Params);
end;

function TADConnectionExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TADQuery(Query).SQL.Text;
end;

procedure TADConnectionExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TADQuery(Query).SQL.Text := Statement;
end;

function TADConnectionExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TADConnectionExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TADTable).IndexDefs;
end;

{ DDL functions: ReverseEngineering}

procedure TADConnectionExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TADTable;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TADTable;
  Tables := TStringList.Create;
  try
    GetTableNames(Tables, False);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ExtractObjectName(Tables[I]);

      if AnsiSameText(LogicalTableName, FSystemTableName) then
        continue;

      try
        Table.TableName := Tables[I];
        Table.FieldDefs.Update;
      except
        // invisible
        continue;
      end;
      
      Table.IndexDefs.Update;
      Idx := Schema.TableDefs.IndexOf(LogicalTableName);
      if Idx < 0 then begin
        // Create table defs
        with Schema.TableDefs.Add do
        begin
          TableName := LogicalTableName;
          FieldDefs.Assign(Table.FieldDefs);
          IndexDefs.Assign(Table.IndexDefs);
        end;
      end else begin
        with Schema.TableDefs[Idx] do
        begin
          // Processing Field Definitions
          // Pass #1: Removing fields defs, that has been deleted from table
          // and updating the ones that remain
          J := 0;
          while J < FieldDefs.Count do
          begin
            DefIdx := Table.FieldDefs.IndexOf(FieldDefs[J].Name);
            if DefIdx < 0 then
              FieldDefs[J].Free
            else begin
              FieldDefs[J].Assign(Table.FieldDefs[DefIdx]);
              Inc(J);
            end;
          end;
          // Pass #2: Adding fields defs, that has been added to a table
          for J := 0 to Table.FieldDefs.Count - 1 do
            if FieldDefs.IndexOf(Table.FieldDefs[J].Name) < 0 then
              FieldDefs.Add.Assign(Table.FieldDefs[J]);

          // Processing Index Definitions
          // Pass #1: Removing index defs, that has been deleted from table
          // and updating the ones that remain
          J := 0;
          while J < IndexDefs.Count do
          begin
            DefIdx := Table.IndexDefs.IndexOf(IndexDefs[J].Name);
            if DefIdx < 0 then
              IndexDefs[J].Free
            else begin
              IndexDefs[J].Assign(Table.IndexDefs[DefIdx]);
              Inc(J);
            end;
          end;
          // Pass #2: Adding fields defs, that has been added to a table
          for J := 0 to Table.IndexDefs.Count - 1 do
            if IndexDefs.IndexOf(Table.IndexDefs[J].Name) < 0 then
              IndexDefs.Add.Assign(Table.IndexDefs[J]);
        end;
      end;
    end;
    // Pass #2. Removing definitions for dropped tables
    with Schema do
    begin
      // Remove table file extensions
      for I := 0 to Tables.Count - 1 do
        Tables[I] := ExtractObjectName(Tables[I]);
      // Process definitions
      I := 0;
      while I < TableDefs.Count do
      begin
        if Tables.IndexOf(TableDefs[I].TableName) < 0 then
          TableDefs[I].Free
        else Inc(I);
      end;
    end;
  finally
    Tables.Free;
    Table.Free;
  end;
end;

function TADConnectionExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

procedure TADConnectionExt.DoConnect;
begin
  inherited;
  FVersionStr := '';
end;

procedure TADConnectionExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
var
  eScopes: TADPhysObjectScopes;
begin
  eScopes := [osMy, osOther];
  if SystemTables then
    Include(eScopes, osSystem);
  inherited GetTableNames('', '', '', List, eScopes, [tkTable]);
end;

function TADConnectionExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

end.




