(******************************************************************************)
(*
(*  Context Database Extensions Suite (Interbase\Firebird\Yaffil)
(*
(*  Contains: TFIBDatabaseExt components.
(*
(*  Copyright (c) 2006 Roman V. Babenko
(*
(*  ------------------------------------------------------------
(*  FILE        : FIBExt.pas
(*  AUTHOR(S)   : Roman V. Babenko (romb@devrace.com, romb@devrona.com),
(*                Michael Baytalsky (mike@contextsoft.com)
(*  DELPHI      : Delphi 5,6,7,2005,2006
(*
(******************************************************************************)
unit FIBExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, CtxDBIntf, dbSchema, CtxDataTypes,
  pFIBDatabase, pFIBDataSet, FIBQuery;

type
  TFIBDatabaseExt = class;

  {:$ TFIBRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TFIBRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TFIBDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TFIBDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a FIB database connection. }
  TFIBDatabaseExt = class(TpFIBDatabase, ISchemaDatabase, ICtxDataProvider, ICtxDatabase)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: String;
    FVersionStr: String;
    FDatabaseName: String;

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
//    procedure DoConnect; override; TODO: DoConnect
  public
    {:: Creates an instance of a TFIBDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TFIBDatabaseExt component.}
    destructor Destroy; override;

    {:: Creates TFIBTable component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TFIBQuery component, corresponding to Statement. }
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
    function ExecuteStatement(ASQL: String; ResultSet: Pointer): Integer;

    function CreateCommand: TCtxDataCommand; 

    { Parent object is always a table or schema. }
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
    {:$ Specifies the uniform path to the database. }
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL stored false;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
  end;

  TCtxFIBCommand = class (TCtxDataCommand)
  protected
    FQuery: TFIBQuery;
    FRowsAffected: Int64;
    FEOF: Boolean;

    function GetPrepared: Boolean; override;
    procedure SetPrepared(Value: Boolean); override;
    procedure InternalPrepare;
    procedure InternalUnPrepare;
    function GetRowsAffected: Int64; override;
  public
    {:: Executes command }
    procedure Execute; override;
    {:: Returns false if CurrentRow is nil. }
    function EOF: Boolean; override;
    {:: Read next row. Assign nil to CurrentRow if no more rows found. }
    procedure Next; override;
  end;

  {:$ Retrieves database URL from the TDatabase component and the connected Session component. }
  function GetDatabaseURL(Database: TpFIBDatabase): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(Database: TpFIBDatabase; DatabaseURL: string);

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

  procedure Register;

implementation

uses
  ibase, FIBDatabase, FIBDataSet, pFIBProps, pFIBCacheQueries,
  fib, IB_Externals, StdFuncs;

procedure Register;
begin
  RegisterComponents('Database Extensions', [TFIBDatabaseExt]);
end;

{ .$DEFINE _IBFBDEBUG}

procedure addLog(const AMsg: string; args: array of const);
{$IFDEF _IBFBDEBUG}
var
  vFileName: string;
  fileContent: TStrings;
begin
  fileContent := TStringList.Create;
  try
    vFileName := ExtractFilePath(ParamStr(0))+'ib.log.txt';
    if FileExists(vFileName) then
    begin
      fileContent.LoadFromFile(vFileName);
    end;
    fileContent.Add(Format(AMsg, args));
    fileContent.SaveToFile(vFileName);
  finally
    FreeAndNil(fileContent);
  end;
end;
{$ELSE}

begin
end;
{$ENDIF}


{ General Helper Rountines }

function GetDatabaseURL(Database: TpFIBDatabase): String;
begin
  Result := Database.DBName;
end;

procedure SetDatabaseURL(Database: TpFIBDatabase; DatabaseURL: string);
begin
  Database.DatabaseName := DatabaseURL;
end;

function GetCtxDataType(AType: Integer): TCtxDataType;
begin
  case AType of
    SQL_VARYING:
      Result := cdtString;
    SQL_LONG:
      Result := cdtInteger;
    SQL_BOOLEAN:
      Result := cdtBoolean;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
      Result := cdtFloat;
    SQL_TYPE_TIME, SQL_TYPE_DATE, SQL_DATE:
      Result := cdtDateTime;
    SQL_SHORT:
      Result := cdtSmallInt;
    SQL_INT64:
      Result := cdtFloat;
      // Result := cdtLargeInt;
    SQL_BLOB, SQL_QUAD:
      Result := cdtBlob;
    SQL_TEXT:
      Result := cdtMemo;
  else
    Result := cdtUnknown;
  end;
end;

{ TFIBRangeCursor }

constructor TFIBRangeCursor.Create(Database: TFIBDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
  begin
    DatabaseError(SInvalidRangeCursor);
  end;
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields,
    Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TFIBRangeCursor.CreateExt(Database: TFIBDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TpFIBDataSet;
begin
  if (TableName = '') then
  begin
    DatabaseError(SInvalidRangeCursor);
  end;

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TpFIBDataSet;
  DataSet := Table; // Assign inherited field

  // Do not set any range if no KeyFields specified
  if KeyFields = '' then
  begin
    exit;
  end;

  Table.GetFieldList(KeyFieldsList, KeyFields);
  with Table do
    begin
      // Create filter for fields
      RangeFilter := CreateRangeFilter(KeyFieldsList, KeyValues);
      if TableFilter <> '' then
      begin
        RangeFilter := '(' + TableFilter + ') and (' + RangeFilter + ')';
      end;
      Filter := RangeFilter;
      if CaseInsensitive then
      begin
        FilterOptions := [foCaseInsensitive]
      end else
      begin
        FilterOptions := [];
      end;
      Filtered := True;
    end;
end;

destructor TFIBRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TFIBDatabaseExt }

constructor TFIBDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'SysTable';
  FVersionStr := '';
  DefaultTransaction := TpFIBTransaction.Create(Self);
  DefaultUpdateTransaction := TpFIBTransaction.Create(Self);
  addLog('Create Database', []);
end;

destructor TFIBDatabaseExt.Destroy;
begin
  UnRegisterCtxDataProvider(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TFIBDatabaseExt.CheckActive;
begin
  if (Handle = nil) then
  begin
    DatabaseError(SDatabaseClosed);
  end;
end;

procedure TFIBDatabaseExt.CheckSchema;
begin
  if Schema = nil then
  begin
    DatabaseError(sDatabaseSchemaIsNotAssigned);
  end;
end;

function TFIBDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
  addLog('get version %d.%d', [Result.Major, Result.Minor]);
end;

procedure TFIBDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  addLog('set version %d.%d', [Value.Major, Value.Minor]);
  SetDatabaseVersion(Self, Value);
end;

function TFIBDatabaseExt.GetVersionLabel: String;
begin
  if Connected then
  begin
    if FVersionStr = '' then
    begin
      FVersionStr := VersionToStr(GetVersion);
    end;
    Result := FVersionStr;
  end else
  begin
    Result := VersionToStr(Undefined);
  end;
end;

function TFIBDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TFIBDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
  begin
    Schema := nil;
  end;
  inherited;
end;

procedure TFIBDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TFIBDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

procedure TFIBDatabaseExt.StartTransaction;
begin
  DefaultTransaction.StartTransaction;
end;

procedure TFIBDatabaseExt.Commit;
begin
  DefaultTransaction.CommitRetaining;
end;

procedure TFIBDatabaseExt.Rollback;
begin
  DefaultTransaction.RollbackRetaining;
end;

function TFIBDatabaseExt.GetInTransaction: Boolean;
begin
  Result := DefaultTransaction.InTransaction;
end;

function TFIBDatabaseExt.GetDriverName: String;
begin
  Result := 'IB\FB\YA'; //TODO: ???
end;

function TFIBDatabaseExt.GetDatabaseURL: String;
begin
  Result := FIBExt.GetDatabaseURL(Self);
end;

procedure TFIBDatabaseExt.SetDatabaseURL(const Value: String);
begin
  FIBExt.SetDatabaseURL(Self, Value);
end;

function TFIBDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TFIBRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TFIBDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TFIBRangeCursor.Create(Self, Relation, KeyValues);
end;

function TFIBDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TFIBDatabaseExt.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

procedure TFIBDatabaseExt.SetDatabaseName(const Value: String);
begin
  // inherited DatabaseName := Value;
  FDatabaseName := Value;
  RegisterCtxDataProvider(Self);
end;

function TFIBDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TpFIBDataSet.Create(nil);
  try
    TpFIBDataSet(Result).Database := Self;
    TpFIBDataSet(Result).Transaction := DefaultTransaction;
    TpFIBDataSet(Result).UpdateTransaction := DefaultUpdateTransaction;
    TpFIBDataSet(Result).SQLs.SelectSQL.Text := Format('SELECT T.* FROM "%s" T', [TableName]);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TFIBDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TpFIBDataSet.Create(nil);
  with TpFIBDataSet(Result) do
  try
    TpFIBDataSet(Result).Database := Self;
    TpFIBDataSet(Result).Transaction := Self.DefaultTransaction;
    TpFIBDataSet(Result).UpdateTransaction := Self.DefaultUpdateTransaction;
    TpFIBDataSet(Result).PrepareOptions := TpFIBDataSet(Result).PrepareOptions +
       [psUseBooleanField,psUseGuidField,psAskRecordCount,psUseLargeIntField];

    // Assign SQL statement
    SQLs.SelectSQL.Text := Statement;
//    QSelect.ParamCheck := False;
//    AutoUpdateOptions.CanChangeSQLs := True;
//    AutoUpdateOptions.AutoReWriteSqls := True;
//    Options := Options + [poFreeHandlesAfterClose];
    UniDirectional := True; //TODO: UniDirectional
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TFIBDatabaseExt.ExecSQL(Query: TDataSet);
var
  vQuery: TFIBQuery;

  procedure AssignParams;
  var
    i: integer;
  begin
    // vQuery.ParamCount := TpFIBDataSet(Query).ParamCount;
    for i := 0 to TpFIBDataSet(Query).ParamCount - 1 do
      vQuery.Params[i].Value := TpFIBDataSet(Query).Params[i].Value;
  end;

begin
  addLog('Exec SQL begin', []);
  vQuery := GetQueryForUse(TpFIBDataSet(Query).Transaction, TpFIBDataSet(Query).SelectSQL.Text);
  try
    AssignParams;
    // vQuery.ExecQuery;
    vQuery.ExecuteImmediate;
  finally
    FreeQueryForUse(vQuery);
  end;
  // TpFIBDataSet(Query).Open;
  if TpFIBDataSet(Query).Transaction.InTransaction then
    TpFIBDataSet(Query).Transaction.CommitRetaining;
  addLog('Exec SQL end', []);
end;

function TFIBDatabaseExt.ExecuteStatement(ASQL: String;
  ResultSet: Pointer): Integer;
var
  Q: TpFIBDataSet;
  AParams: TParams;
begin
  Q := TpFIBDataSet(CreateQuery(ASQL));
//
  AParams := TParams.Create;
  try
    Result := IProviderSupport(Q).PSExecuteStatement(ASQL, AParams, ResultSet);
    if Q.Transaction.InTransaction then
      Q.Transaction.CommitRetaining;
  finally
    AParams.Free;
  //  FreeAndNil(Q);
  end;
  addLog('Execute statement %d', [Result]);
end;

procedure TFIBDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
var
  I: Integer;
  FldType: TFieldType;
  CurParam: TParam;
begin
  Params.Clear;
  for I := 0 to TpFIBDataSet(Query).ParamCount - 1 do
  begin
    case TpFIBDataSet(Query).Params[I].SQLType of
      SQL_TEXT, SQL_VARYING: FldType := DB.ftString;
      SQL_DOUBLE, SQL_FLOAT: FldType := DB.ftFloat;
      SQL_SHORT: FldType := DB.ftSmallint;
      SQL_LONG: FldType := DB.ftInteger;
      SQL_INT64: FldType := DB.ftLargeint;
      // ftFloat; // may be BCD for D6+?
      SQL_TIMESTAMP: FldType := DB.ftDateTime;
      SQL_TYPE_TIME: FldType := DB.ftTime;
      SQL_TYPE_DATE: FldType := DB.ftDate;
      SQL_BLOB, SQL_ARRAY: FldType := DB.ftBlob;
    else
      FldType := DB.ftString;
    end;
      CurParam := Params.CreateParam(
        FldType, TpFIBDataSet(Query).Params[I].Name, ptInputOutput
      );
      CurParam.Value := TpFIBDataSet(Query).Params[I].Value
   end;
end;

procedure TFIBDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
var
  I: Integer;
begin
  for I := 0 to Params.Count - 1 do
    TpFIBDataSet(Query).ParamByName(Params[I].Name).Value := Params[I].Value;
end;

function TFIBDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TpFIBDataSet(Query).SQLs.SelectSQL.Text;
end;

procedure TFIBDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TpFIBDataSet(Query).SQLs.SelectSQL.Text := Statement;
end;

function TFIBDatabaseExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TFIBDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  // TODO: IndexDefs
  Result := TIndexDefs.Create(DataSet);
end;

{ DDL functions: ReverseEngineering}

procedure TFIBDatabaseExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TpFIBDataSet;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TpFIBDataSet;
  Tables := TStringList.Create;
  try
    GetTableNames(Tables, False);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ChangeFileExt(Tables[I], '');

      if AnsiSameText(LogicalTableName, FSystemTableName) then continue;

      Table.SQLs.SelectSQL.Text := Format('SELECT T.* FROM %s T', [Tables[I]]);
      Table.FieldDefs.Update;
      Idx := Schema.TableDefs.IndexOf(LogicalTableName);
      if Idx < 0 then begin
        // Create table defs
        with Schema.TableDefs.Add do
        begin
          TableName := LogicalTableName;
          FieldDefs.Assign(Table.FieldDefs);
          //IndexDefs.Assign(Table.IndexDefs); //TODO: IndexDefs
          //TODO: Reverce Eng
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
      {    J := 0;
          while J < IndexDefs.Count do
          begin
            DefIdx := Table.IndexDefs.IndexOf(IndexDefs[J].Name);
            if DefIdx < 0 then
              IndexDefs[J].Free
            else begin
              IndexDefs[J].Assign(Table.IndexDefs[DefIdx]);
              Inc(J);
            end;
          end;} //TODO: IndexDefs
          //Pass #2: Adding fields defs, that has been added to a table
{          for J := 0 to Table.IndexDefs.Count - 1 do
            if IndexDefs.IndexOf(Table.IndexDefs[J].Name) < 0 then
              IndexDefs.Add.Assign(Table.IndexDefs[J]);} //TODO: IndexDefs
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

function TFIBDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

function TFIBDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxFIBCommand.Create(Self);
end;

{ TCtxFIBCommand }

function TCtxFIBCommand.EOF: Boolean;
begin
  CheckPrepared;
  Result := FEOF;
end;

procedure TCtxFIBCommand.Execute;
var
  I: Integer;
  P: TCtxParameter;
begin
  CheckProviderConnected;
  Prepared := True;

  // Setup parameters
  for I := 0 to FQuery.ParamCount - 1 do
  begin
    P := Params.Find(FQuery.Params[I].Name);
    if P <> nil then
      FQuery.Params[I].Value := P.Value;
  end;

  if not DataProvider.InTransaction then
    DataProvider.StartTransaction;
  FQuery.ExecQuery;
  FEOF := False;
  if FCommandType = ctSQLUpdate then
  begin
    // Retrieve output parameters back from query
    for I := 0 to FQuery.FieldCount - 1 do
    begin
      P := Params.Find(FQuery.Fields[I].Name);
      if (P <> nil) and (P.ParamType in [cptOutput, cptInputOutput, cptResult]) then
        P.Value := FQuery.Fields[I].Value;
    end;
  end;
  Next;
end;

function TCtxFIBCommand.GetPrepared: Boolean;
begin
  Result := FQuery <> nil;
end;

function TCtxFIBCommand.GetRowsAffected: Int64;
begin
  Result := FRowsAffected;
end;

procedure TCtxFIBCommand.InternalPrepare;
var
  I: Integer;
  F: TFIBXSQLVAR;
begin
  CheckProviderConnected;

  FQuery := TFIBQuery.Create(nil);
  try
    FQuery.Database := Self.DataProviderObject as TFIBDatabase;
    FQuery.Transaction := (Self.DataProviderObject as TFIBDatabase).DefaultTransaction;
    case FCommandType of
      ctTableDirect: FQuery.SQL.Text := 'select * from ' + CommandText;
      ctSQLSelect, ctSQLUpdate: FQuery.SQL.Text := CommandText;
    end;

    if not DataProvider.InTransaction then
      DataProvider.StartTransaction;
    // Prepare result set
    FQuery.Prepare;
    FFields.Clear;
    for I := 0 to FQuery.FieldCount - 1 do
    begin
      F := FQuery.Fields[I];
      FFields.AddParameter(F.Name, cptOutput, GetCtxDataType(F.SQLType), F.Size);
    end;

    // Prepare params
    if FParams.Count = 0 then
      for I := 0 to FQuery.ParamCount - 1 do
        Params.AddParameter(FQuery.Params[I].Name, cptInput, GetCtxDataType(FQuery.Params[I].SQLType), FQuery.Params[I].Size);
        
  except
    FreeAndNil(FQuery);
    raise;
  end;
end;

procedure TCtxFIBCommand.InternalUnPrepare;
begin
  FreeAndNil(FQuery);
  FEOF := True;
  FRowsAffected := 0;
  FFields.Clear;
  // FParams.Clear;
end;

// type TFriendBlobStream=class(TFIBBlobStream);

function ValueToVariant(Value: TFIBXSQLVAR): Variant;
const
  IBBuffDateDelta=678576;
  FMSecsPerDay: Single = MSecsPerDay;
var
  LocalData: Pointer;
  Size: Word;
  D: Double;
begin
  // fi:=vFieldDescrList[j-1];
  with Value.Data^ do
  if (sqlind <> nil) and (sqlind^ = -1) then
    Result := varNull
  (* Check null, if so return a default string *)
  else
  begin
    LocalData := sqldata;

    case sqltype and (not 1) of
      SQL_ARRAY:
        Result := '(Array)';
      SQL_BLOB:
        Result := '(Blob)';// Value.AsString;
      SQL_VARYING:
      begin
        Size := PWord(LocalData)^;     // It is isc_vax_integer(LocalData, 2);
        if Size = 0 then
          Result := ''
        else
          Result := StrPas(@Value.Data^.sqldata[2]);
      end;
      SQL_TEXT:
        Result := StrPas(LocalData);
        // Move(LocalData^, Buffer[fi^.fdDataOfs-DiffSizesRecData],fi^.fdDataSize)
      SQL_TIMESTAMP:
        with PISC_QUAD(LocalData)^ do
        begin
          D := (gds_quad_high + IBBuffDateDelta)*FMSecsPerDay+(gds_quad_low div 10);
          Result := VarFromDateTime(TDateTime(D));
        end;
      SQL_TYPE_DATE:
        begin
          D := PISC_DATE(LocalData)^ + IBBuffDateDelta;
          Result := VarFromDateTime(TDateTime(D));
        end;
      SQL_TYPE_TIME:
        begin
          D := PISC_TIME(LocalData)^ div 10;
          Result := VarFromDateTime(TDateTime(D));
        end;
      SQL_INT64:
        if sqlscale < -4 then
          Result := Value.AsDouble
        else
          Result := PInt64(LocalData)^;
      SQL_SHORT:
        if sqlscale <> 0 then
          Result := PShort(sqldata)^*E10[sqlscale]
        else
          Result := Trunc(PShort(sqldata)^*E10[sqlscale]);
      SQL_LONG:
        if sqlscale <> 0 then
          Result := PLong(sqldata)^*E10[sqlscale]
        else
          Result := Trunc(PLong(sqldata)^*E10[sqlscale]);
      SQL_FLOAT:
        Result := PFloat(sqldata)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        Result := PDouble(sqldata)^;
      SQL_BOOLEAN:
        if PShort(sqldata)^ = ISC_TRUE then
          Result := True
        else
          Result := False;
    else
      raise Exception.Create('Invalid data conversion')
    end;
  end;
end;

procedure TCtxFIBCommand.Next;
var
  I: Integer;
begin
  if (FQuery <> nil) and FQuery.Open and not FQuery.Eof then
  begin
    // Read current row
    for I := 0 to FQuery.FieldCount - 1 do
      FFields[I].Value := FQuery.Fields[I].Value; //. ValueToVariant(FQuery.Fields[I]);
    // Move to next row
    FQuery.Next;
  end else
  begin
    FEOF := True;
    FFields.ClearValues;
  end;
end;

procedure TCtxFIBCommand.SetPrepared(Value: Boolean);
begin
  if Value <> Prepared then
  begin
    if Value then
      InternalPrepare
    else InternalUnPrepare;
  end;
end;

end.




