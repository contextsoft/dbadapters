(******************************************************************************)
(*
(*  Context Database Extensions Suite (Nexus2)
(*
(*  Contains: TSQLiteDatabaseExt component.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit SQLiteDBExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, CtxDBIntf, dbSchema, CtxDataTypes, CtxDataSetCommand, ASGSQLite3;

type
  TSQLiteDatabaseExt = class;

  {:$ TSQLiteRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TSQLiteRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TSQLiteDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TSQLiteDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a SQLite database connection. }
  TSQLiteDatabaseExt = class(TASQLite3DB, ISchemaDatabase)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: String;
    FDatabaseName: String;
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
    {:: Creates an instance of a TnxDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TnxDatabaseExt component.}
    destructor Destroy; override;

    {:: Creates TASQLite3Table component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TASQLite3Query component, corresponding to Statement. }
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
    {:$ Specifies the uniform path to the database. }
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL stored false;
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TASQLite3DB component and the connected Session component. }
  function GetDatabaseURL(SQLiteDatabase: TASQLite3DB): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(SQLiteDatabase: TASQLite3DB; DatabaseURL: String);

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

implementation

uses
{$IFDEF VER130}
  FileCtrl,
{$ENDIF}
  DbConsts, ActiveX, TypInfo;
{ General Helper Rountines }

procedure Register;
begin
  RegisterComponents('Database Extensions', [TSQLiteDatabaseExt]);
end;

function GetDatabaseURL(SQLiteDatabase: TASQLite3DB): String;
var
  DatabaseName: string;
begin
  result := '';
  if SQLiteDatabase = nil then
    exit;
  DatabaseName := SQLiteDatabase.Database;
  Result := EncodeDatabaseURL('', '', DatabaseName);
end;

procedure SetDatabaseURL(SQLiteDatabase: TASQLite3DB; DatabaseURL: String);
var
  DatabaseName: string;
  RemoteHost: string;
  ConnectionType: string;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  SQLiteDatabase.Database := DatabaseName;
end;

{ TSQLiteRangeCursor }

constructor TSQLiteRangeCursor.Create(Database: TSQLiteDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TSQLiteRangeCursor.CreateExt(Database: TSQLiteDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TASQLite3Table;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TASQLite3Table;
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

destructor TSQLiteRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TSQLiteDatabaseExt }

constructor TSQLiteDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'SysTable';
  FVersionStr := '';
end;

destructor TSQLiteDatabaseExt.Destroy;
begin
  DBDatabases.Remove(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TSQLiteDatabaseExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TSQLiteDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TSQLiteDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TSQLiteDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TSQLiteDatabaseExt.GetVersionLabel: String;
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

function TSQLiteDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TSQLiteDatabaseExt.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TSQLiteDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TSQLiteDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

procedure TSQLiteDatabaseExt.StartTransaction;
begin
  inherited StartTransaction;
end;

procedure TSQLiteDatabaseExt.Commit;
begin
  inherited Commit;
end;

procedure TSQLiteDatabaseExt.Rollback;
begin
  inherited Rollback;
end;

function TSQLiteDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TSQLiteDatabaseExt.GetDriverName: String;
begin
  Result := 'SQLite';
end;

function TSQLiteDatabaseExt.GetDatabaseURL: String;
begin
  Result := SQLiteDBExt.GetDatabaseURL(Self);
end;

procedure TSQLiteDatabaseExt.SetDatabaseURL(const Value: String);
begin
  SQLiteDBExt.SetDatabaseURL(Self, Value);
end;

function TSQLiteDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TSQLiteRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TSQLiteDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TSQLiteRangeCursor.Create(Self, Relation, KeyValues);
end;

function TSQLiteDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TSQLiteDatabaseExt.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

procedure TSQLiteDatabaseExt.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
  if FDatabaseName <> '' then
  begin
    if DBDatabases.IndexOf(Self) < 0 then
      DBDatabases.Add(Self);
  end else begin
    if DBDatabases <> nil then
      DBDatabases.Remove(Self);
  end;
end;

function TSQLiteDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TASQLite3Table.Create(nil);
  try
    TASQLite3Table(Result).Connection := Self;
    TASQLite3Table(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLiteDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TASQLite3Query.Create(nil);
  with TASQLite3Query(Result) do
  try
    Connection := Self;
    // Assign SQL statement
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TSQLiteDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TASQLite3Query(Query).ExecSQL;
end;

function TSQLiteDatabaseExt.ExecuteStatement(SQL: String; ResultSet: Pointer): Integer;
var
  Q: TASQLite3Query;
  AParams: TParams;
begin
  Q := TASQLite3Query(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    Result := IProviderSupport(Q).PSExecuteStatement(SQL, AParams, ResultSet);
  finally
    AParams.Free;
    Q.Free;
  end;
end;

procedure TSQLiteDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TASQLite3Query(Query).Params);
end;

procedure TSQLiteDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TASQLite3Query(Query).Params.AssignValues(Params);
end;

function TSQLiteDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TASQLite3Query(Query).SQL.Text;
end;

procedure TSQLiteDatabaseExt.SetQuerySQL(Query: TDataSet; const Statement: String);
begin
  TASQLite3Query(Query).SQL.Text := Statement;
end;

function TSQLiteDatabaseExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TSQLiteDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := nil;
  //Result := (DataSet as TASQLite3Table).IndexDefs;
end;

procedure TSQLiteDatabaseExt.ReverseEngineer;
begin
end;


(*
{ DDL functions: ReverseEngineering}

procedure TSQLiteDatabaseExt.ReverseEngineer;
var
  I, J, K: Integer;
  Fields, Tables: TStringList;
  Dictionary: TnxDataDictionary;
  FldDesc: TnxFieldDescriptor;
  IdxDesc: TnxIndexDescriptor;
  TableDef: TTableDefinition;
  IdxFld: TIndexField;
  LastRel: TRelationship;
begin
  CheckActive;
  CheckSchema;
  Dictionary := TnxDataDictionary.Create;
  Tables := TStringList.Create;
  Fields := TStringList.Create;
  try
    Schema.Clear;
    GetTableNames(Tables, False);
    for I := 0 to Tables.Count - 1 do
    begin
      if AnsiSameText(Tables[I], SystemTableName) then continue;

      Dictionary.Clear;
      GetDataDictionary(Tables[I], '', Dictionary);
      TableDef := Schema.TableDefs.Add;
      TableDef.Name := Tables[I];

      Dictionary.FieldsDescriptor.GetFields(Fields);
      // Import fields
      for J := 0 to Fields.Count - 1 do
      with TableDef.FieldDefs.Add do
      begin
        FldDesc := Fields.Objects[J] as TnxFieldDescriptor;
        Name := FldDesc.Name;
        Description := FldDesc.fdDesc;
        DataType := NxFieldTypeToDbFieldType[FldDesc.fdType];
        Size := FldDesc.fdUnits;
        Required := FldDesc.fdRequired;
        // Precision = Units, Scale = FldDesc.fdDecPl
        if DataType in [ftFloat, ftCurrency, ftBCD, ftFMTBcd] then
        begin
          Precision := FldDesc.fdUnits;
          SetPropValue('Scale', IntToStr(FldDesc.fdDecPl));
        end;
        if Assigned(FldDesc.fdDefaultValue) then
        begin
          if FldDesc.fdDefaultValue is TnxAutoGuidDefaultValueDescriptor then
            DefaultExpression := 'NEWGUID'
          else if FldDesc.fdDefaultValue is TnxConstDefaultValueDescriptor then
            DefaultExpression := TnxConstDefaultValueDescriptor(FldDesc.fdDefaultValue).AsVariant
          else if FldDesc.fdDefaultValue is TnxCurrentDateTimeDefaultValueDescriptor then
            case DataType of
              ftTime: DefaultExpression := 'CURRENT_TIME';
              ftDate: DefaultExpression := 'CURRENT_DATE';
              else DefaultExpression := 'CURRENT_TIMESTAMP';
            end
          else if FldDesc.fdDefaultValue is TnxCurrentUserDefaultValueDescriptor then
            DefaultExpression := 'CURRENT_USER'
          else if FldDesc.fdDefaultValue is TnxEmptyDefaultValueDescriptor then
            DefaultExpression := 'EMPTY';
        end;
      end;
      // Import indexes
      for J := 0 to Dictionary.IndicesDescriptor.IndexCount - 1 do
      begin
        IdxDesc := Dictionary.IndicesDescriptor.IndexDescriptor[J];
        if not (IdxDesc.KeyDescriptor is TnxCompKeyDescriptor) then
          continue;
        if (AnsiPos('$SQL$', IdxDesc.Name) = 1) and (AnsiPos('$SQL$PRIMARYKEY$', IdxDesc.Name) <> 1) then
          continue;
        if TnxCompKeyDescriptor(IdxDesc.KeyDescriptor).KeyFieldCount = 0 then
          continue;

        with TableDef.IndexDefs.Add do
        begin
          if AnsiPos('$SQL$PRIMARYKEY$', IdxDesc.Name) = 1 then
          begin
            Name := GetNewIndexName(TableDef.IndexDefs, IdxDesc.ConstraintName);
            Options := [ixPrimary, ixUnique];
          end else
            Name := GetNewIndexName(TableDef.IndexDefs, IdxDesc.Name);

          Description := IdxDesc.Desc;
          if IdxDesc.Dups = idNone then
            Options := Options + [ixUnique];

          if (IdxDesc.Number = Dictionary.IndicesDescriptor.DefaultIndex) and (IdxDesc.Dups = idNone) then
            Options := Options + [ixPrimary];

          with TnxCompKeyDescriptor(IdxDesc.KeyDescriptor) do
          for K := 0 to KeyFieldCount - 1 do
          begin
            IdxFld := IndexFields.Add;
            IdxFld.Name := KeyFields[K].Field.Name;
            IdxFld.Description := KeyFields[K].Desc;
            if KeyFields[K].NullBehaviour = nbBottom then
              IdxFld.SetPropValue('Nulls', 'NULLS LAST');
            IdxFld.Descending := not KeyFields[K].Ascend;
            if KeyFields[K].InheritsFrom(TnxTextKeyFieldDescriptor) then
              IdxFld.CaseInsensitive := TnxTextKeyFieldDescriptor(KeyFields[K]).IgnoreCase;

            if Assigned(KeyFields[K].LocaleDescriptor) then
            with KeyFields[K].LocaleDescriptor do
            begin
              if IgnoreKanaType then
                IdxFld.SetPropValue('IgnoreKanaType', 'True');
              if IgnoreNonSpace then
                IdxFld.SetPropValue('IgnoreNonSpace', 'True');
              if IgnoreSymbols then
                IdxFld.SetPropValue('IgnoreSymbols', 'True');
              if IgnoreWidth then
                IdxFld.SetPropValue('IgnoreWidth', 'True');
              if UseStringSort then
                IdxFld.SetPropValue('UseStringSort', 'True');

              IdxFld.SetPropValue('Locale', IntToStr(Locale));
            end;
          end;
        end;
      end;
    end;
    // Foreign keys
    with CreateQuery(sqlSelectRelationships) do
    try
      Active := True;
      LastRel := nil;
      while not EOF do
      begin
        if (LastRel = nil) or not AnsiSameText(LastRel.Name, FieldByName('CTX_RELATIONSHIP').AsString) then
        begin
          LastRel := Schema.Relationships.Add;
          LastRel.Name := FieldByName('CTX_RELATIONSHIP').AsString;
          LastRel.DetailTableName := FieldByName('CTX_DETAIL_TABLE').AsString;
          LastRel.MasterTableName := FieldByName('CTX_MASTER_TABLE').AsString;
          LastRel.DetailRelationName := FieldByName('CTX_RELATIONSHIP').AsString;
          LastRel.MasterRelationName := LastRel.MasterTableDef.Relations.GetAutoName(LastRel, LastRel.DetailTableName);
          LastRel.UpdateAction := RelActionFromString(FieldByName('CTX_ON_UPDATE').AsString);
          LastRel.DeleteAction := RelActionFromString(FieldByName('CTX_ON_DELETE').AsString);
          LastRel.DetailKeyFields := FieldByName('CTX_DETAIL_FIELD_NAME').AsString;
          LastRel.MasterKeyFields := FieldByName('CTX_MASTER_FIELD_NAME').AsString;
          LastRel.UpdateRelations;
        end else begin
          LastRel.DetailRelation.AddKeyField := FieldByName('CTX_DETAIL_FIELD_NAME').AsString;
          LastRel.DetailRelation.AddForeignKeyField := FieldByName('CTX_MASTER_FIELD_NAME').AsString;
        end;
        Next;
      end;
    finally
      Free;
    end;
    // Checks
    with CreateQuery(sqlSelectConstraints) do
    try
      Active := True;
      while not EOF do
      begin
        TableDef := Schema.GetTableDef(FieldByName('CTX_TABLE_NAME').AsString);
        if TableDef <> nil then
        with TableDef.Constraints.Add do
        begin
          Name := FieldByName('CTX_CONSTRAINT_NAME').AsString;
          Check := FieldByName('CTX_CHECK').AsString;
        end;
        Next;
      end;
    finally
      Free;
    end;
    // Views
    with CreateQuery(sqlSelectViews) do
    try
      Active := True;
      while not EOF do
      begin
        with Schema.ViewDefs.Add do
        begin
          Name := FieldByName('CTX_VIEW_NAME').AsString;
          Definition.Text := FieldByName('CTX_DEFINITION').AsString;
          Description := FieldByName('CTX_DESCRIPTION').AsString;
        end;
        Next;
      end;
    finally
      Free;
    end;
    // Stored Procs
    with CreateQuery(sqlSelectProcedures) do
    try
      Active := True;
      while not EOF do
      begin
        with Schema.StoredProcs.Add do
        begin
          Name := FieldByName('CTX_PROCEDURE_NAME').AsString;
          Definition.Text := FieldByName('CTX_DEFINITION').AsString;
          Description := FieldByName('CTX_DESCRIPTION').AsString;
          IsFunction := FieldByName('CTX_IS_FUNCTION').AsInteger = 1;
        end;
        Next;
      end;
    finally
      Free;
    end;
    // Triggers
    with CreateQuery(sqlSelectTriggers) do
    try
      Active := True;
      while not EOF do
      begin
        TableDef := Schema.GetTableDef(FieldByName('CTX_TABLE_NAME').AsString);
        if TableDef <> nil then
        with TableDef.Triggers.Add do
        begin
          Name := FieldByName('CTX_TRIGGER_NAME').AsString;
          Definition := FieldByName('CTX_DEFINITION').AsString;
          Description := FieldByName('CTX_DESCRIPTION').AsString;
        end;
        Next;
      end;
    finally
      Free;
    end;

  finally
    Fields.Free;
    Tables.Free;
    Dictionary.Free;
  end;
end;
*)

function TSQLiteDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

function TSQLiteDatabaseExt.GetConnected: Boolean;
begin
  Result := inherited Connected;
end;

procedure TSQLiteDatabaseExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin
  inherited GetTableNames(List);
end;

procedure TSQLiteDatabaseExt.SetConnected(Value: Boolean);
begin
  FVersionStr := '';
  inherited Connected := Value;
end;

function TSQLiteDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

function TSQLiteDatabaseExt.GetQueryPlan(Query: TDataSet): String;
begin
  Result := ''; // not implemented
end;



end.




