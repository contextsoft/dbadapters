(******************************************************************************)
(*
(*  Context Database Extensions Suite (MySQL)
(*
(*  Contains: TMySQLDatabaseExt component.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit MySQLExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, MyAccess,
  CtxDataTypes, CtxDBIntf, dbSchema, CtxDataSetCommand;

type
  TMySQLDatabaseExt = class;

  {:$ TMySQLRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TMySQLRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TMySQLDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TMySQLDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a MySQL database connection. }
  TMySQLDatabaseExt = class(TMyConnection, ISchemaDatabase)
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
    {:: Creates an instance of a TMySQLDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TMySQLDatabaseExt component.}
    destructor Destroy; override;

    {:: Creates TMyTable component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TMyQuery component, corresponding to Statement. }
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

    { Parent object is always a table or schema. }
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;
    function GetSystemTableName: String;

    function CreateCommand: TCtxDataCommand;
    { Plan informaion }
    function GetQueryPlan(Query: TDataSet): String;
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

  {:$ Retrieves database URL from the TMySQLDatabase component and the connected Session component. }
  function GetDatabaseURL(MySQLDatabase: TMyConnection): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(MySQLDatabase: TMyConnection; DatabaseURL: String);

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

implementation

{ General Helper Rountines }

const
  DEFAULT_PORT = 3306;

procedure Register;
begin
  RegisterComponents('Database Extensions', [TMySQLDatabaseExt]);
end;

function GetDatabaseURL(MySQLDatabase: TMyConnection): String;
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  ConnectionType := 'MySQL';
  RemoteHost := MySQLDatabase.Server;
  if MySQLDatabase.Port <> DEFAULT_PORT then
    RemoteHost := RemoteHost + ':' + IntToStr(MySQLDatabase.Port);
  DatabaseName := MySQLDatabase.Database;
  Result := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
end;

procedure SetDatabaseURL(MySQLDatabase: TMyConnection; DatabaseURL: String);
var
  ConnectionType, RemoteHost, DatabaseName: String;
  P: Integer;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  P := AnsiPos(':', RemoteHost);
  if P > 0 then
  begin
    MySQLDatabase.Port := StrToIntDef(copy(RemoteHost, P+1, Length(RemoteHost)), DEFAULT_PORT);
    RemoteHost := copy(RemoteHost, 1, P-1);
  end;
  MySQLDatabase.Server := RemoteHost;
  MySQLDatabase.Database := DatabaseName;
end;

{ TMySQLRangeCursor }

constructor TMySQLRangeCursor.Create(Database: TMySQLDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TMySQLRangeCursor.CreateExt(Database: TMySQLDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TMyTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TMyTable;
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

destructor TMySQLRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TMySQLDatabaseExt }

constructor TMySQLDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'SysTable';
  FVersionStr := '';
end;

destructor TMySQLDatabaseExt.Destroy;
begin
  DBDatabases.Remove(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TMySQLDatabaseExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TMySQLDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TMySQLDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TMySQLDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TMySQLDatabaseExt.GetVersionLabel: String;
begin
  if Connected then
  begin
    if FVersionStr = '' then
      FVersionStr := VersionToStr(GetVersion);
    Result := FVersionStr;
  end else
    Result := VersionToStr(Undefined);
end;

function TMySQLDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TMySQLDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TMySQLDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TMySQLDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

function TMySQLDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TMySQLDatabaseExt.GetDriverName: String;
begin
  Result := 'MySQL';
end;

function TMySQLDatabaseExt.GetDatabaseURL: String;
begin
  Result := MySQLExt.GetDatabaseURL(Self);
end;

procedure TMySQLDatabaseExt.SetDatabaseURL(const Value: String);
begin
  MySQLExt.SetDatabaseURL(Self, Value);
end;

function TMySQLDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TMySQLRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TMySQLDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TMySQLRangeCursor.Create(Self, Relation, KeyValues);
end;

function TMySQLDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TMySQLDatabaseExt.GetDatabaseName: String;
begin
  Result := inherited Database;
end;

procedure TMySQLDatabaseExt.SetDatabaseName(const Value: String);
begin
  inherited Database := Value;
  if DatabaseName <> '' then
  begin
    if DBDatabases.IndexOf(Self) < 0 then
      DBDatabases.Add(Self);
  end else begin
    if DBDatabases <> nil then
      DBDatabases.Remove(Self);
  end;
end;

function TMySQLDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TMyTable.Create(nil);
  try
    TMyTable(Result).Connection := Self;
    TMyTable(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TMySQLDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TMyQuery.Create(nil);
  with TMyQuery(Result) do
  try
    Connection := Self;
    // Assign SQL statement
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TMySQLDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TMyQuery(Query).Execute;
end;

function TMySQLDatabaseExt.ExecuteStatement(SQL: String; ResultSet: Pointer): Integer;
var
  Q: TMyQuery;
  Temp: String;
  I: Integer;
  AParams: TParams;
begin
  Q := TMyQuery(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    I := 1;
    Temp := NextToken(SQL, ' ', I);
    if AnsiSameText(Temp, 'select') or AnsiSameText(Temp, 'show') or
      AnsiSameText(Temp, 'describe')
    then begin
      Q.Active := True;
      TDataSet(ResultSet^) := Q;
    end else Q.Execute;
    // Result := IProviderSupport(Q).PSExecuteStatement(SQL, AParams, ResultSet);
    Result := Q.RowsAffected;
  finally
    AParams.Free;
    if TDataSet(ResultSet^) <> Q then
      Q.Free;
  end;
end;

procedure TMySQLDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TMyQuery(Query).Params);
end;

procedure TMySQLDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TMyQuery(Query).Params.AssignValues(Params);
end;

function TMySQLDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TMyQuery(Query).SQL.Text;
end;

procedure TMySQLDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TMyQuery(Query).SQL.Text := Statement;
end;

function TMySQLDatabaseExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TMySQLDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TMyTable).IndexDefs;
end;

{ DDL functions: ReverseEngineering}

procedure TMySQLDatabaseExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TMyTable;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TMyTable;
  Tables := TStringList.Create;
  try
    GetTableNames(Tables, False);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ExtractObjectName(Tables[I]);

      if AnsiSameText(LogicalTableName, FSystemTableName) then continue;

      Table.TableName := Tables[I];
      Table.FieldDefs.Update;
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

function TMySQLDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

procedure TMySQLDatabaseExt.DoConnect;
begin
  inherited;
  FVersionStr := '';
end;

procedure TMySQLDatabaseExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin
  inherited GetTableNames(List);
end;

function TMySQLDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

function TMySQLDatabaseExt.GetQueryPlan(Query: TDataSet): String;
begin
  Result := ''; // not implemented
end;

end.




