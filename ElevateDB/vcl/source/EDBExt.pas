(******************************************************************************)
(*
(*  Context Database Extensions Suite (ElevateDB)
(*
(*  Contains: TEDBDatabaseExt component.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit EDBExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, EDBComps, 
  CtxDataTypes, CtxDBIntf, dbSchema, CtxDataSetCommand;

type
  TEDBDatabaseExt = class;

  {:$ TEDBRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TEDBRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TEDBDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TEDBDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a ADO database connection. }
  TEDBDatabaseExt = class(TEDBDatabase, ISchemaDatabase)
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
    procedure SetConnected(Value: Boolean);
    function GetConnected: Boolean;

    { Checks if the database is connected }
    procedure CheckActive;
    { Checks if schema is assigned }
    procedure CheckSchema;
  public
    {:: Creates an instance of a TEDBDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TEDBDatabaseExt component.}
    destructor Destroy; override;

    {:: Creates TTableExt component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TQueryExt component, corresponding to Statement. }
    function CreateQuery(const Statement: String): TDataSet;

    { Schema related methods. Schema must be assigned for them to work }
    {:$ Updates database schema from the physical database tables. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be updated. }
    procedure ReverseEngineer;

    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);

    {:$ Checks whether the database version is *equal* or Newer }
    {:$ then the current Schema's Version. }
    function IsVersionCurrent(AllowNewer: Boolean = False): Boolean;
    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
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
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;
    function GetSystemTableName: String;

    function CreateCommand: TCtxDataCommand;
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
    {:$ Specifies the uniform path to the database. }
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL stored false;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TEDBDatabase component and the connected Session component. }
  function GetDatabaseURL(EDBDatabase: TEDBDatabase): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(EDBDatabase: TEDBDatabase; DatabaseURL: String);

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

const
  ctLocal = '';
  ctLAN = 'LAN';
  ctInternet = 'Internet';
  INTERNET_COMPRESSION = 6;
  DEFAULT_PORT = 12010;
  
implementation

uses
{$IFDEF VER130}
  FileCtrl,
{$ENDIF}
  DbConsts, ActiveX, TypInfo;


{ General Helper Rountines }

procedure Register;
begin
  RegisterComponents('Database Extensions', [TEDBDatabaseExt]);
end;

{ General Helper Rountines }

function GetDatabaseURL(EDBDatabase: TEDBDatabase): String;
var
  ConnectionType, RemoteHost, DatabaseName: String;
  Q: TEDBQuery;
begin
  if EDBDatabase.Session.SessionType = stLocal then
  begin
    ConnectionType := ctLocal;
    RemoteHost := '';
    DatabaseName := EDBDatabase.Database;
    if DatabaseName <> '' then
    begin
      Q := TEDBQuery.Create(nil);
      try
        EDBDatabase.Session.Execute('SELECT PATH FROM DATABASES WHERE NAME = '+AnsiQuotedStr(DatabaseName,''''), nil, Q);
        if Q.FieldCount > 0 then
          DatabaseName := Q.Fields[0].AsString;
      except
        // Unable to retrieve database path
      end;
      Q.Free;
    end;
  end else begin
    if EDBDatabase.Session.RemoteCompression = 0 then
      ConnectionType := ctLAN
    else ConnectionType := ctInternet;
    RemoteHost := EDBDatabase.Session.RemoteHost;
    if EDBDatabase.Session.RemotePort <> DEFAULT_PORT then
      RemoteHost := RemoteHost + ':' + IntToStr(EDBDatabase.Session.RemotePort);
    DatabaseName := EDBDatabase.Database;
  end;
  Result := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
end;

procedure SetDatabaseURL(EDBDatabase: TEDBDatabase; DatabaseURL: String);
var
  ConnectionType, RemoteHost, RemotePort, DatabaseName: String;
  TempName: String;
  P: Integer;
  Q: TEDBQuery;
  Guid: TGuid;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  if ConnectionType = ctLocal then
  begin
    EDBDatabase.Session.SessionType := stLocal;
    Q := TEDBQuery.Create(nil);
    try
      EDBDatabase.Session.Execute('SELECT NAME FROM DATABASES WHERE UPPER(PATH) = '+AnsiQuotedStr(AnsiUpperCase(DatabaseName),''''), nil, Q);
      if not Q.Eof then
        TempName := Q.Fields[0].AsString
      else begin
        // EDBDatabase.Session.Execute('DROP DATABASE '+AnsiQuotedStr(DatabaseName,'"') + 'KEEP CONTENTS';
        CreateGUID(Guid);
        TempName := NameToIdent('DB',GuidToString(Guid)); // random name, not present in config file
        EDBDatabase.Session.Execute('CREATE DATABASE '+AnsiQuotedStr(TempName,'"') + ' PATH ' + AnsiQuotedStr(DatabaseName,''''), nil, Q);
        // We could probably clean up this config later
      end;
    finally
      Q.Free;
    end;
    EDBDatabase.Database := TempName;
  end else begin
    EDBDatabase.Session.SessionType := stRemote;
    if AnsiCompareText(ConnectionType, ctLAN) = 0 then
      EDBDatabase.Session.RemoteCompression := 0
    else EDBDatabase.Session.RemoteCompression := INTERNET_COMPRESSION;

    P := Pos(':', RemoteHost);
    if P > 0 then
    begin
      RemotePort := copy(RemoteHost, P+1, MaxInt);
      RemoteHost := copy(RemoteHost, 1, P - 1);
      EDBDatabase.Session.RemotePort := StrToIntDef(RemotePort, DEFAULT_PORT);
    end;

    EDBDatabase.Session.RemoteHost := RemoteHost;
    EDBDatabase.Database := DatabaseName;
  end;
end;

{ TEDBRangeCursor }

constructor TEDBRangeCursor.Create(Database: TEDBDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TEDBRangeCursor.CreateExt(Database: TEDBDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TEDBTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TEDBTable;
  DataSet := Table; // Assign inherited field

  // Do not set any range if no KeyFields specified
  if KeyFields = '' then exit;

  Table.GetFieldList(KeyFieldsList, KeyFields);

  with Table do
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
  end;
end;

destructor TEDBRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TEDBDatabaseExt }

constructor TEDBDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'SysTable';
  FVersionStr := '';
end;

destructor TEDBDatabaseExt.Destroy;
begin
  DBDatabases.Remove(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TEDBDatabaseExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TEDBDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TEDBDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TEDBDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TEDBDatabaseExt.GetVersionLabel: String;
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

function TEDBDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TEDBDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TEDBDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TEDBDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

procedure TEDBDatabaseExt.StartTransaction;
begin
  inherited StartTransaction;
end;

procedure TEDBDatabaseExt.Commit;
begin
  inherited Commit;
end;

procedure TEDBDatabaseExt.Rollback;
begin
  inherited Rollback;
end;

function TEDBDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TEDBDatabaseExt.GetDriverName: String;
begin
  Result := 'ADO';
end;

function TEDBDatabaseExt.GetDatabaseURL: String;
begin
  Result := EDBExt.GetDatabaseURL(Self);
end;

procedure TEDBDatabaseExt.SetDatabaseURL(const Value: String);
begin
  EDBExt.SetDatabaseURL(Self, Value);
end;

function TEDBDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TEDBRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TEDBDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TEDBRangeCursor.Create(Self, Relation, KeyValues);
end;

function TEDBDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TEDBDatabaseExt.GetDatabaseName: String;
begin
  Result := inherited DatabaseName;
end;

procedure TEDBDatabaseExt.SetDatabaseName(const Value: String);
begin
  inherited DatabaseName := Value;
  if DatabaseName <> '' then
  begin
    if DBDatabases.IndexOf(Self) < 0 then
      DBDatabases.Add(Self);
  end else begin
    if DBDatabases <> nil then
      DBDatabases.Remove(Self);
  end;
end;

function TEDBDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TEDBTable.Create(nil);
  try
    TEDBTable(Result).DatabaseName := Self.DatabaseName;
    TEDBTable(Result).SessionName := Self.SessionName;
    TEDBTable(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TEDBDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TEDBQuery.Create(nil);
  with TEDBQuery(Result) do
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

procedure TEDBDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TEDBQuery(Query).ExecSQL;
end;

function TEDBDatabaseExt.ExecuteStatement(SQL: String;
  ResultSet: Pointer): Integer;
var
  Q: TEDBQuery;
  AParams: TParams;
begin
  Q := TEDBQuery(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    Result := IProviderSupport(Q).PSExecuteStatement(SQL, AParams, ResultSet);
  finally
    AParams.Free;
    Q.Free;
  end;
end;

procedure TEDBDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TEDBQuery(Query).Params);
end;

procedure TEDBDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TEDBQuery(Query).Params.AssignValues(Params);
end;

function TEDBDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TEDBQuery(Query).SQL.Text;
end;

procedure TEDBDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TEDBQuery(Query).SQL.Text := Statement;
end;

function TEDBDatabaseExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TEDBDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TEDBTable).IndexDefs;
end;

procedure TEDBDatabaseExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TEDBTable;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TEDBTable;
  Tables := TStringList.Create;
  try
    GetTableNames(Tables, False);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ChangeFileExt(Tables[I], '');

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
        Tables[I] := ChangeFileExt(Tables[I], '');
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

function TEDBDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

function TEDBDatabaseExt.GetConnected: Boolean;
begin
  Result := inherited Connected;
end;

procedure TEDBDatabaseExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin
  Session.GetTableNames(DatabaseName, List); // EDBComps
end;

procedure TEDBDatabaseExt.SetConnected(Value: Boolean);
begin
  FVersionStr := '';
  Connected := Value;
end;

function TEDBDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

end.




