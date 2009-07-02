(******************************************************************************)
(*
(*  Context Database Extensions Suite (Absolute DB)
(*
(*  Contains: TABSDatabaseExt components.
(*
(*  Copyright (c) 2004-2006 Michael Baytalsky
(*
(******************************************************************************)
unit ABSExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ABSMain, ABSTypes, CtxDataTypes, CtxDataSetCommand, CtxDBIntf, dbSchema;

type
  TABSDatabaseExt = class;

  {:$ TABSRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TABSRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TABSDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TABSDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;
  
  {:$ Represents a ABS database connection. }
  TABSDatabaseExt = class(TABSDatabase, ISchemaDatabase, ICtxDatabase, ICtxDataProvider)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: String;
    FVersionStr: String;

    function GetSystemTableName: String;
    function GetDatabaseName: String;
    procedure SetDatabaseName(const Value: String);
    function GetVersionLabel: String;
    procedure SetVersionLabel(const Value: String);

    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);

    { Replication support }
    function GetSchema: TDatabaseSchema;
    procedure SetSchema(Value: TDatabaseSchema);
    procedure SetSystemTableName(const Value: String);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDatabaseURL: String;
    procedure SetDatabaseURL(const Value: String);

    function GetEngineName: String;

    procedure CheckActive;
      { Checks if the database is connected }
    procedure CheckSchema;
  public
    {:: Creates an instance of a TABSDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TABSDatabaseExt component.}
    destructor Destroy; override;

    {:: Creates TABSTable component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TABSQuery component, corresponding to Statement. }
    function CreateQuery(const Statement: String): TDataSet;

    { Schema related methods. Schema must be assigned for them to work }
    {:$ Updates database schema from the physical database tables. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be updated. }
    procedure ReverseEngineer;

    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);

    {:$ Creates a new database based on the schema information. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be used to create database tables. }
    {:: If the database already exists in the specified location, OwerwriteTables }
    {:: parameter controls whether the tables will be overwriten. }
    {:: CreateSystemTable parameter controls whether the System table should be created. }
    {:! The name for the System table is stored in SystemTableName property. }
    procedure CreateNewDatabase(OverwriteTables: Boolean = True; CreateSystemTable: Boolean = True);

    {:$ Applies updates contained in the schema Updates collection. }
    {:: This method will only execute updates for the database's version and up }
    {:: until the most recent version is reached. }
    {:: Returns true if the database version is okey after the update. }
    function UpdateDatabase: Boolean;

    {:$ Checks whether the database version is *equal* or Newer }
    {:$ then the current Schema's Version. }
    function IsVersionCurrent(AllowNewer: Boolean = False): Boolean;

    procedure Commit;
    function GetInTransaction: Boolean;

    {:$ Refreshes all connected Tables, which are in dsBrowse state. }
    {:: This method is useful after Rollback. }
    procedure RefreshTables;

    function FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;

    function GetRangeCursor(const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = ''): TDBRangeCursor; overload;
    function GetRangeCursor(Relation: TRelation; KeyValues: Variant): TDBRangeCursor; overload;

    function GetDriverName: String;

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

    {:$ Version of the database. }
    {:: The information about the database version is stored in a System table. }
    property Version: TSchemaVersion read GetVersion write SetVersion;
    property SystemTableName: String read GetSystemTableName write SetSystemTableName;
  published
    { Published properties }
    {:$ Reference to a TDatabaseSchema component. }
    {:: Schema may contain information about the database structure as weel as some }
    {:: additional information like referential integrity constraints, triggers and more. }
    property Schema: TDatabaseSchema read FSchema write SetSchema;
    {:$ Text presentation of the database version. }
    {:: This property is effectively read-only. Any text assigned to it will be ignored. }
    property VersionLabel: String read GetVersionLabel write SetVersionLabel stored False;
    {:$ Specifies the uniform path to the database for both local and C/S types of access. }
    {:: This parameter is usefull for displaying or storing database and session parameters }
    {:: for the specific database. It may also be used with TABSOpenDatabase dialog.<br> }
    {:: Attention! Assigning DatabaseURL may change the parameters of the session component }
    {:: the database is attached to. }
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL stored false;

    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TABSDatabase component and the connected Session component. }
  function GetDatabaseURL(ABSDatabase: TABSDatabase): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(ABSDatabase: TABSDatabase; DatabaseURL: String);

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';
  SErrorCreatingTable = 'Error creating table %s. Error: %s';

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('Database Extensions', [TABSDatabaseExt]);
end;

{ General Helper Rountines }

function GetDatabaseURL(ABSDatabase: TABSDatabase): String;
begin
  Result := ABSDatabase.DatabaseFileName;
end;

procedure SetDatabaseURL(ABSDatabase: TABSDatabase; DatabaseURL: String);
begin
  ABSDatabase.DatabaseFileName := DatabaseURL;
end;

{ TABSRangeCursor }

constructor TABSRangeCursor.Create(Database: TABSDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TABSRangeCursor.CreateExt(Database: TABSDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  IdxDef: TIndexDef;
  RangeFilter: String;
  Table: TABSTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := TABSTable(Database.CreateTable(TableName));
  Table.Active := True;
  DataSet := Table; // Assign inherited field

  // Do not set any range if no KeyFields specified
  if KeyFields = '' then exit;

  Table.GetFieldList(KeyFieldsList, KeyFields);
  if ExtraKeyFields = '' then
    IdxDef := Table.IndexDefs.GetIndexForFields(KeyFields, CaseInsensitive)
  else IdxDef := Table.IndexDefs.GetIndexForFields(KeyFields + ';' + ExtraKeyFields, CaseInsensitive);

  with Table do
    if IdxDef = nil then
    begin
      // Create filter for fields
      RangeFilter := CreateRangeFilter(KeyFieldsList, KeyValues);
      if TableFilter <> '' then
        RangeFilter := '(' + TableFilter + ') and (' + RangeFilter + ')';
      Filter := RangeFilter;
      if CaseInsensitive then
        FilterOptions := [foCaseInsensitive]
      else FilterOptions := [];
      Filtered := True;
      CancelRange;
    end else begin
      // Setup range
      IndexName := IdxDef.Name;
      SetRangeStart;
      KeyFieldCount := KeyFieldsList.Count;
      AssignKeyFields(KeyFieldsList, KeyValues);
      SetRangeEnd;
      KeyFieldCount := KeyFieldsList.Count;
      AssignKeyFields(KeyFieldsList, KeyValues);
      ApplyRange;
      if TableFilter <> Filter then
        Filter := TableFilter;
      Filtered := Filter <> '';
    end;
end;

destructor TABSRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TABSDatabaseExt }

constructor TABSDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'SysTable';
  FVersionStr := '';
end;

destructor TABSDatabaseExt.Destroy;
begin
  UnRegisterCtxDataProvider(Self);
  inherited Destroy;
end;

procedure TABSDatabaseExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TABSDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TABSDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := dbSchema.GetDatabaseVersion(Self);
end;

function TABSDatabaseExt.GetVersionLabel: String;
begin
  if Connected then
  begin
    if FVersionStr = '' then
      FVersionStr := VersionToStr(GetVersion);
    Result := FVersionStr;
  end else begin
    FVersionStr := '';
    Result := VersionToStr(Undefined);
  end;
end;

function TABSDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, Self.Version, AllowNewer);
end;

procedure TABSDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TABSDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
  GetVersionLabel; // Force it to update version property if connected
end;

procedure TABSDatabaseExt.SetSystemTableName(const Value: String);
begin
  if FSystemTableName <> Value then
  begin
    FSystemTableName := Value;
    // Close system table if it's opened
    if Connected and Assigned(FSchema) then
      GetVersionLabel;
  end;
end;

procedure TABSDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  dbSchema.SetDatabaseVersion(Self, Value);
end;

procedure TABSDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

function TABSDatabaseExt.UpdateDatabase: Boolean;
begin
  Result := dbSchema.UpdateDatabase(Self);
end;

procedure TABSDatabaseExt.RefreshTables;
var
  I: Integer;
begin
  for I := 0 to DataSetCount - 1 do
    if DataSets[I].InheritsFrom(TABSDataSet) and
      (TABSDataSet(DataSets[I]).State = dsBrowse)
    then
      TABSDataSet(DataSets[I]).Refresh;
end;

function TABSDatabaseExt.GetEngineName: String;
begin
  Result := 'Absolute DB';
end;

function TABSDatabaseExt.GetDatabaseURL: String;
begin
  Result := ABSExt.GetDatabaseURL(Self);
end;

procedure TABSDatabaseExt.SetDatabaseURL(const Value: String);
begin
  ABSExt.SetDatabaseURL(Self, Value);
end;

function TABSDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TABSRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TABSDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TABSRangeCursor.Create(Self, Relation, KeyValues);
end;

function TABSDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TABSDatabaseExt.GetDatabaseName: String;
begin
  Result := inherited DatabaseName;
end;

procedure TABSDatabaseExt.SetDatabaseName(const Value: String);
begin
  inherited DatabaseName := Value;
  RegisterCtxDataProvider(Self);
end;

function TABSDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TABSTable.Create(nil);
  TABSTable(Result).DatabaseName := DatabaseName;
  TABSTable(Result).SessionName := SessionName;
  TABSTable(Result).TableName := TableName;
end;

function TABSDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TABSQuery.Create(nil);
  with TABSQuery(Result) do
  try
    DatabaseName := Self.DatabaseName;
    SessionName := Self.SessionName;
    // Assign SQL statement
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TABSDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TABSQuery(Query).ExecSQL;
end;

function TABSDatabaseExt.ExecuteStatement(SQL: String;
  ResultSet: Pointer): Integer;
var
  Q: TABSQuery;
  Temp: String;
  I: Integer;
  AParams: TParams;
begin
  Q := TABSQuery(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    I := 1;
    Temp := NextToken(SQL, ' ', I);
    if AnsiSameText(Temp, 'select') then
    begin
      Q.Active := True;
      if ResultSet <> nil then
        TDataSet(ResultSet^) := Q;
    end else
      Q.ExecSQL;
    Result := Q.RowsAffected;
  finally
    AParams.Free;
    if (ResultSet = nil) or (TDataSet(ResultSet^) <> Q) then
      Q.Free;
  end;
end;

procedure TABSDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TABSQuery(Query).Params);
end;

procedure TABSDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TABSQuery(Query).Params.AssignValues(Params);
end;

function TABSDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TABSQuery(Query).SQL.Text;
end;

procedure TABSDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TABSQuery(Query).SQL.Text := Statement;
end;

function TABSDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

function TABSDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

{ DDL functions: CreateDatabase, ReverseEngineering, RestructureFromSchema }

procedure TABSDatabaseExt.CreateNewDatabase(OverwriteTables: Boolean = True; CreateSystemTable: Boolean = True);
var
  I: Integer;
  Table: TABSTable;
begin
  CheckSchema;
  CheckActive;
  // Create a new database based on schema information
  Table := CreateTable('') as TABSTable;
  try
    for I := 0 to Schema.TableDefs.Count - 1 do
    begin
      with Schema.TableDefs[I] do
      try
        Table.TableName := TableName;
        if Table.Exists then
        begin
          // Overwrite table completely - i.e. delete and re-create
          if OverwriteTables then
            Table.DeleteTable
          else continue;
        end;

        Table.FieldDefs.Assign(FieldDefs);
        Table.IndexDefs.Assign(IndexDefs);

        Table.CreateTable;
      except
        on E: Exception do
          DatabaseErrorFmt(SErrorCreatingTable, [TableName, E.Message]);
      end;
    end;

    // Set version. This will automatically create system table and update it
    // to the right values
    if CreateSystemTable then
      Version := Schema.Version;
  finally
    Table.Free;
  end;
end;

procedure TABSDatabaseExt.ReverseEngineer;
const
  SBlobCompressionAlgorithms: array [TCompressionAlgorithm] of String = ('NONE','ZLIB','BZIP','PPM');
var
  I, J: Integer;
  Table: TABSTable;
  Tables: TStringList;
  LogicalTableName: String;
  FldDef: TFieldDefinition;
  IdxDef: TIndexDefinition;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TABSTable;
  Tables := TStringList.Create;
  try
    Schema.Clear;
    GetTableNames(Tables);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := Tables[I];
      if AnsiCompareText(LogicalTableName, FSystemTableName) = 0 then continue;

      Table.TableName := Tables[I];
      Table.FieldDefs.Update;
      Table.IndexDefs.Update;
      // Create table def
      with Schema.TableDefs.Add do
      begin
        TableName := LogicalTableName;

        for J := 0 to Table.FieldDefs.Count - 1 do
        begin
          FldDef := FieldDefs.Add;
          FldDef.Assign(Table.FieldDefs[J]);
          with Table.AdvFieldDefs[J] do
          begin
            if not MinValue.IsNull then
              FldDef.SetPropValue('Min', MinValue.AsAnsiString);
            if not MaxValue.IsNull then
              FldDef.SetPropValue('Max', MaxValue.AsAnsiString);
            if not DefaultValue.IsNull then
              FldDef.DefaultExpression := DefaultValue.AsAnsiString;

            case DataType of
              aftAutoIncShortint:
                FldDef.SetPropValue('AutoIncDataType', 'SHORTINT');
              aftAutoIncSmallint:
                FldDef.SetPropValue('AutoIncDataType', 'SMALLINT');
              aftAutoIncInteger:
                FldDef.SetPropValue('AutoIncDataType', 'INTEGER');
              aftAutoIncLargeint:
                FldDef.SetPropValue('AutoIncDataType', 'LARGEINT');
              aftAutoIncByte:
                FldDef.SetPropValue('AutoIncDataType', 'BYTE');
              aftAutoIncWord:
                FldDef.SetPropValue('AutoIncDataType', 'WORD');
              aftAutoIncCardinal:
                FldDef.SetPropValue('AutoIncDataType', 'CARDINAL');
            end;

            if FldDef.DataType = ftAutoInc then
            begin
              if AutoincIncrement <> 1 then
                FldDef.SetPropValue('Increment', IntToStr(AutoincIncrement));
              if AutoincInitialValue <> 0 then
                FldDef.SetPropValue('InitialValue', IntToStr(AutoincInitialValue));
              if AutoincMinValue <> 0 then
                FldDef.SetPropValue('AutoIncMin', IntToStr(AutoincMinValue));
              if AutoincMaxValue < High(Int64) then
                FldDef.SetPropValue('AutoIncMax', IntToStr(AutoincMaxValue));

              FldDef.SetPropValue('Cycled', BoolToStr(AutoincCycled, True));
            end;

            if FldDef.DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftNClob] then
            begin
              FldDef.SetPropValue('BlobBlockSize', IntToStr(BLOBBlockSize));
              FldDef.SetPropValue('BlobCompressionAlgorithm', SBlobCompressionAlgorithms[BlobCompressionAlgorithm]);
              FldDef.SetPropValue('BlobCompressionMode', IntToStr(BlobCompressionMode));
            end;
          end;
        end;

        for J := 0 to Table.IndexDefs.Count - 1 do
        begin
          IdxDef := IndexDefs.Add;
          IdxDef.Assign(Table.IndexDefs[J]);
        end;
      end;
    end;
  finally
    Tables.Free;
    Table.Free;
  end;
end;

function TABSDatabaseExt.GetConnected: Boolean;
begin
  Result := inherited Connected;
end;

procedure TABSDatabaseExt.SetConnected(Value: Boolean);
begin
  FVersionStr := '';
  inherited Connected := Value;
end;

function TABSDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TABSTable).IndexDefs;
end;

procedure TABSDatabaseExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin
  Session.GetTableNames(Self.DatabaseName, List);
end;

function TABSDatabaseExt.GetDriverName: String;
begin
  Result := 'Absolute DB';
end;

procedure TABSDatabaseExt.Commit;
begin
  inherited Commit;
end;

function TABSDatabaseExt.FindKey(Table: TDataSet;
  const KeyValues: array of const): Boolean;
begin
  Result := (Table as TABSTable).FindKey(KeyValues);
end;

function TABSDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

end.
