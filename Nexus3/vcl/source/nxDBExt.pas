(******************************************************************************)
(*
(*  Context Database Extensions Suite (Nexus2)
(*
(*  Contains: TnxDatabaseExt component.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit nxDBExt;

interface

{$DEFINE NX_REMOTESERVER}

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, nxdb, {nxsdTypes,} CtxDBIntf, dbSchema, CtxDataTypes, CtxDataSetCommand;

type
  TnxDatabaseExt = class;

  {:$ TnxRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TnxRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TnxDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TnxDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a Nexus 2 database connection. }
  TnxDatabaseExt = class(TnxDatabase, ISchemaDatabase)
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

    {:: Creates TnxTable component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TnxQuery component, corresponding to Statement. }
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
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TnxDatabase component and the connected Session component. }
  function GetDatabaseURL(nxDatabase: TnxDatabase): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(nxDatabase: TnxDatabase; DatabaseURL: String);
  procedure Register;
  procedure DecodeServerName(AServerName: string; var AHost: string; var APort: integer);

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

implementation

uses
{$IFDEF VER130}
  FileCtrl,
{$ENDIF}
  DbConsts, ActiveX, TypInfo, nxsdServerEngine,
{$IFDEF NX_REMOTESERVER}
  nxreRemoteServerEngine,
  nxllTransport, nxtwWinsockTransport, nxtnNamedPipeTransport, nxtcCOMTransport,
  nxptBasePooledTransport,
{$ENDIF}
  nxsdDataDictionary, nxsrServerEngine, nxsdTypes;

const
  DEFAULT_PORT = 16000;


{ General Helper Rountines }

procedure Register;
begin
  RegisterComponents('Database Extensions', [TnxDatabaseExt]);
end;

{$IFDEF NX_REMOTESERVER}
function GetProtocolName(Transport: TnxBaseTransport): string;
begin
  if Transport is TnxWinsockTransport then
    result := 'Tcp'
  else if Transport is TnxNamedPipeTransport then
    result := 'Pipe'
  else if Transport is TnxBaseCOMTransport then
    result := 'COM'
  else
    result := Transport.GetName;
end;

procedure DecodeServerName(AServerName: string; var AHost: string; var APort: integer);
var
  P: integer;
begin
  P := AnsiPos(':', AServerName);
  if P > 0 then
  begin
    APort := StrToIntDef(copy(AServerName, P+1, Length(AServerName)), DEFAULT_PORT);
    AHost := copy(AServerName, 1, P-1);
  end else
  begin
    APort := DEFAULT_PORT;
    AHost := AServerName;
  end;
end;

{$ENDIF}

function GetDatabaseURL(nxDatabase: TnxDatabase): String;
var
  DatabaseName: string;
  Engine: TnxBaseServerEngine;
{$IFDEF NX_REMOTESERVER}
  Transport: TnxBaseTransport;
  _ServerName: string;
{$ENDIF}
begin
  result := '';
  if nxDatabase.Session = nil then
    exit;
  Engine := nxDatabase.Session.ServerEngine;
  if Engine = nil then
    exit;
  DatabaseName := nxDatabase.AliasName;
  if DatabaseName = '' then
    DatabaseName := nxDatabase.AliasPath;
{$IFDEF NX_REMOTESERVER}
  if Engine is TnxRemoteServerEngine then
  begin
    Transport := TnxRemoteServerEngine(Engine).Transport;
    if Transport = nil then
      exit;
    _ServerName := Transport.ServerName;
    if (Transport is TnxBasePooledTransport)
      and (TnxBasePooledTransport(Transport).Port <> DEFAULT_PORT) then
      _ServerName := _ServerName + ':' + IntToStr(TnxBasePooledTransport(Transport).Port);
    Result := EncodeDatabaseURL(GetProtocolName(Transport),
      _ServerName, DatabaseName);
  end
  else
{$ENDIF}
    Result := EncodeDatabaseURL('', '', DatabaseName);
end;

function GetServerEngine(AOwner: TComponent): TnxServerEngine;
var
  i: integer;
begin
  for i := 0 to AOwner.ComponentCount - 1 do begin
    if AOwner.Components[i] is TnxServerEngine then begin
      result := TnxServerEngine(AOwner.Components[i]);
      exit;
    end;
  end;
  result := TnxServerEngine.Create(AOwner);
end;

{$IFDEF NX_REMOTESERVER}
function GetTransport(AOwner: TComponent;const ConnectionType: string): TnxBaseTransport;
begin
  if StrIComp(PChar(ConnectionType), 'Tcp') = 0 then
    result := TnxWinsockTransport.Create(AOwner)
  else if StrIComp(PChar(ConnectionType), 'Pipe') = 0 then
    result := TnxNamedPipeTransport.Create(AOwner)
  else if StrIComp(PChar(ConnectionType), 'COM') = 0 then
    result := TnxRegisteredCOMTransport.Create(AOwner)
  else
    result := nil;
end;

function GetRemoteServerEngine(AOwner: TComponent; const ConnectionType, RemoteHost: string): TnxRemoteServerEngine;
var
  _Host: string;
  _Port: integer;
begin
  result := TnxRemoteServerEngine.Create(AOwner);
  result.Transport := GetTransport(AOwner, ConnectionType);
  if result.Transport <> nil then
  begin
    DecodeServerName(RemoteHost, _Host, _Port);
    result.Transport.ServerName := _Host;
    if result.Transport is TnxBasePooledTransport then
      (result.Transport as TnxBasePooledTransport).Port := _Port;
  end;
end;

function CheckTransportType(AOwner: TComponent; Engine: TnxRemoteServerEngine; const ConnectionType: string): TnxBaseTransport;
begin
  if Engine.Transport = nil then
    result := GetTransport(AOwner, ConnectionType)
  else begin
    if StrIComp(PChar(ConnectionType), 'Tcp') = 0 then
      if Engine.Transport is TnxWinsockTransport then
        result := Engine.Transport
      else
        result := TnxWinsockTransport.Create(AOwner)
    else if StrIComp(PChar(ConnectionType), 'Pipe') = 0 then
      if Engine.Transport is TnxNamedPipeTransport then
        result := Engine.Transport
      else
        result := TnxNamedPipeTransport.Create(AOwner)
    else if StrIComp(PChar(ConnectionType), 'COM') = 0 then
      if Engine.Transport is TnxBaseCOMTransport then
        result := Engine.Transport
      else
        result := TnxRegisteredCOMTransport.Create(AOwner)
    else
      result := nil;
  end;
end;
{$ENDIF}

procedure SetDatabaseURL(nxDatabase: TnxDatabase; DatabaseURL: String);
var
  ConnectionType: string;
  RemoteHost: string;
  DatabaseName: string;
  Engine: TnxBaseServerEngine;
{$IFDEF NX_REMOTESERVER}
  Transport: TnxBaseTransport;
  _Port: integer;
{$ENDIF}
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  if (Pos(':', DatabaseName) = 0) and (Pos('\', DatabaseName) = 0) then
    nxDatabase.AliasName := DatabaseName
  else
    nxDatabase.AliasPath := DatabaseName;
  if (nxDatabase.Session = nil) or not(nxDatabase.Session is TnxSession) then
    exit;

  nxDatabase.Session.Active := False;

  Engine := nxDatabase.Session.ServerEngine;

  if ConnectionType = '' then
  begin
    if (Engine = nil) and (nxDatabase.Owner <> nil) then
      TnxSession(nxDatabase.Session).ServerEngine := GetServerEngine(nxDatabase.Owner);
  end else
  begin
{$IFDEF NX_REMOTESERVER}
    if (Engine <> nil) and (Engine is TnxRemoteServerEngine) and ((Engine as TnxRemoteServerEngine).Transport <> nil) then
    begin
      TnxRemoteServerEngine(Engine).Transport.Free;
      TnxRemoteServerEngine(Engine).Transport := nil;
    end;
    if (Engine = nil) or not (Engine is TnxRemoteServerEngine) then
    begin
      TnxSession(nxDatabase.Session).ServerEngine := GetRemoteServerEngine(nxDatabase, ConnectionType, RemoteHost);
    end else
    begin
      Transport := CheckTransportType(nxDatabase, TnxRemoteServerEngine(Engine), ConnectionType);
      DecodeServerName(RemoteHost, RemoteHost, _Port);
      Transport.ServerName := RemoteHost;
      if Transport is TnxBasePooledTransport then
        (Transport as TnxBasePooledTransport).Port := _Port;
    end;
{$ENDIF}
  end;
end;

{ TnxRangeCursor }

constructor TnxRangeCursor.Create(Database: TnxDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TnxRangeCursor.CreateExt(Database: TnxDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TnxTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TnxTable;
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

destructor TnxRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TnxDatabaseExt }

constructor TnxDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'SysTable';
  FVersionStr := '';
end;

destructor TnxDatabaseExt.Destroy;
begin
  DBDatabases.Remove(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TnxDatabaseExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TnxDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TnxDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TnxDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TnxDatabaseExt.GetVersionLabel: String;
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

function TnxDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TnxDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TnxDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TnxDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

procedure TnxDatabaseExt.StartTransaction;
begin
  inherited StartTransaction;
end;

procedure TnxDatabaseExt.Commit;
begin
  inherited Commit;
end;

procedure TnxDatabaseExt.Rollback;
begin
  inherited Rollback;
end;

function TnxDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TnxDatabaseExt.GetDriverName: String;
begin
  Result := 'Nexus 3';
end;

function TnxDatabaseExt.GetDatabaseURL: String;
begin
  Result := nxDBExt.GetDatabaseURL(Self);
end;

procedure TnxDatabaseExt.SetDatabaseURL(const Value: String);
begin
  nxDBExt.SetDatabaseURL(Self, Value);
end;

function TnxDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TnxRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TnxDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TnxRangeCursor.Create(Self, Relation, KeyValues);
end;

function TnxDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TnxDatabaseExt.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

procedure TnxDatabaseExt.SetDatabaseName(const Value: String);
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

function TnxDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TnxTable.Create(nil);
  try
    TnxTable(Result).Database := Self;
    TnxTable(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TnxDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TnxQuery.Create(nil);
  with TnxQuery(Result) do
  try
    Database := Self;
    // Assign SQL statement
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TnxDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TnxQuery(Query).ExecSQL;
end;

function TnxDatabaseExt.ExecuteStatement(SQL: String;
  ResultSet: Pointer): Integer;
var
  Q: TnxQuery;
  AParams: TParams;
begin
  Q := TnxQuery(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    Result := IProviderSupport(Q).PSExecuteStatement(SQL, AParams, ResultSet);
  finally
    AParams.Free;
    Q.Free;
  end;
end;

procedure TnxDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TnxQuery(Query).Params);
end;

procedure TnxDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TnxQuery(Query).Params.AssignValues(Params);
end;

function TnxDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TnxQuery(Query).SQL.Text;
end;

procedure TnxDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TnxQuery(Query).SQL.Text := Statement;
end;

function TnxDatabaseExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TnxDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TnxTable).IndexDefs;
end;

(*
  TFieldDataType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftBytes,
    ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, ftLargeint,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant,
    ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftNChar,
    ftNClob, ftRecRev);
*)

const
  NxFieldTypeToDbFieldType: array [TnxFieldType] of TFieldDataType = (
     ftBoolean,  ftFixedChar,  ftNChar,  ftSmallInt,  ftSmallInt,  ftWord,  ftSmallInt,  ftSmallInt,
     ftInteger, ftLargeint, ftAutoInc,  ftFloat, ftFloat, ftFloat, ftCurrency,  ftDate,  ftTime,
     ftDateTime,  ftUnknown,  ftBlob,  ftMemo,  ftGraphic,  ftBytes,  ftString,  ftString,
     ftWideString,  ftRecRev,  ftGuid,  ftBCD,  ftNClob, ftFmtBCD
  );

  sqlSelectRelationships =
    'select'#13#10+
    '  fk.FK_CONSTRAINT_NAME AS CTX_RELATIONSHIP,'#13#10+
    '  fk.FK_CONSTRAINT_TABLE_NAME AS CTX_DETAIL_TABLE,'#13#10+
    '  fk.FK_CONSTRAINT_NAME AS CTX_DETAIL_RELATION,'#13#10+
    '  fk.FK_CONSTRAINT_REFERENCES_TABLE_NAME AS CTX_MASTER_TABLE,'#13#10+
    '  fk.FK_CONSTRAINT_TABLE_NAME AS CTX_MASTER_RELATION,'#13#10+
    '  fk.FK_CONSTRAINT_UPDATE_RULE AS CTX_ON_UPDATE,'#13#10+
    '  fk.FK_CONSTRAINT_DELETE_RULE AS CTX_ON_DELETE,'#13#10+
    '  fkm.FK_CONSTRAINT_REFERENCED_COLUMNS_INDEX AS CTX_MASTER_FIELD_INDEX,'#13#10+
    '  fkm.FK_CONSTRAINT_REFERENCED_COLUMNS_NAME AS CTX_MASTER_FIELD_NAME,'#13#10+
    '  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX AS CTX_DETAIL_FIELD_INDEX,'#13#10+
    '  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_NAME AS CTX_DETAIL_FIELD_NAME'#13#10+
    'from'#13#10+
    '  #FOREIGNKEY_CONSTRAINTS fk,'#13#10+
    '  #FOREIGNKEY_CONSTRAINTS_REFERENCING_COLUMNS fkd,'#13#10+
    '  #FOREIGNKEY_CONSTRAINTS_REFERENCED_COLUMNS fkm'#13#10+
    'where'#13#10+
    '  fk.FK_CONSTRAINT_NAME = fkd.FK_CONSTRAINT_NAME and'#13#10+
    '  fk.FK_CONSTRAINT_TABLE_NAME = fkd.FK_CONSTRAINT_TABLE_NAME and'#13#10+
    '  fk.FK_CONSTRAINT_NAME = fkm.FK_CONSTRAINT_NAME and'#13#10+
    '  fk.FK_CONSTRAINT_TABLE_NAME = fkm.FK_CONSTRAINT_TABLE_NAME and'#13#10+
    '  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX ='#13#10+
    '  fkm.FK_CONSTRAINT_REFERENCED_COLUMNS_INDEX'#13#10+
    'order by'#13#10+
    '  fk.FK_CONSTRAINT_TABLE_NAME,'#13#10+
    '  fk.FK_CONSTRAINT_NAME,'#13#10+
    '  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX';

(*
    'SELECT'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_NAME AS CTX_RELATIONSHIP,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_TABLE_NAME AS CTX_DETAIL_TABLE,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_NAME AS CTX_DETAIL_RELATION,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_REFERENCES_TABLE_NAME AS CTX_MASTER_TABLE,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_REFERENCES_CONSTRAINT_NAME AS CTX_MASTER_RELATION,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_UPDATE_RULE AS CTX_ON_UPDATE,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_DELETE_RULE AS CTX_ON_DELETE,'#13#10+
    ' CTX_MASTER.SEGMENT_INDEX AS CTX_MASTER_FIELD_INDEX,'#13#10+
    ' CTX_MASTER.SEGMENT_FIELD AS CTX_MASTER_FIELD_NAME,'#13#10+
    ' CTX_DETAIL.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX AS CTX_DETAIL_FIELD_INDEX,'#13#10+
    ' CTX_DETAIL.FK_CONSTRAINT_REFERENCING_COLUMNS_NAME AS CTX_DETAIL_FIELD_NAME'#13#10+
    'FROM'#13#10+
    ' #INDEXFIELDS AS CTX_MASTER,'#13#10+
    ' #INDEXES,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS,'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS_REFERENCING_COLUMNS AS CTX_DETAIL'#13#10+
    'WHERE'#13#10+
    ' CTX_MASTER.INDEX_NAME = #INDEXES.INDEX_NAME AND'#13#10+
    ' CTX_MASTER.TABLE_NAME = #INDEXES.TABLE_NAME AND'#13#10+
    ' #INDEXES.CONSTRAINT_NAME = #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_REFERENCES_CONSTRAINT_NAME AND'#13#10+
    ' CTX_DETAIL.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX = CTX_MASTER.SEGMENT_INDEX AND'#13#10+
    ' CTX_DETAIL.FK_CONSTRAINT_NAME = #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_NAME'#13#10+
    'ORDER BY'#13#10+
    ' #FOREIGNKEY_CONSTRAINTS.FK_CONSTRAINT_NAME,'#13#10+
    ' CTX_MASTER.SEGMENT_INDEX';
*)
(*
select
  fk.FK_CONSTRAINT_NAME AS CTX_RELATIONSHIP,
  fk.FK_CONSTRAINT_TABLE_NAME AS CTX_DETAIL_TABLE,
  fk.FK_CONSTRAINT_NAME AS CTX_DETAIL_RELATION,
  fk.FK_CONSTRAINT_REFERENCES_TABLE_NAME AS CTX_MASTER_TABLE,
  fk.FK_CONSTRAINT_TABLE_NAME AS CTX_MASTER_RELATION,
  fk.FK_CONSTRAINT_UPDATE_RULE AS CTX_ON_UPDATE,
  fk.FK_CONSTRAINT_DELETE_RULE AS CTX_ON_DELETE,
  fkm.FK_CONSTRAINT_REFERENCED_COLUMNS_INDEX AS CTX_MASTER_FIELD_INDEX,
  fkm.FK_CONSTRAINT_REFERENCED_COLUMNS_NAME AS CTX_MASTER_FIELD_NAME,
  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX AS CTX_DETAIL_FIELD_INDEX,
  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_NAME AS CTX_DETAIL_FIELD_NAME
from
  #FOREIGNKEY_CONSTRAINTS fk,
  #FOREIGNKEY_CONSTRAINTS_REFERENCING_COLUMNS fkd,
  #FOREIGNKEY_CONSTRAINTS_REFERENCED_COLUMNS fkm
where
  fk.FK_CONSTRAINT_NAME = fkd.FK_CONSTRAINT_NAME and
  fk.FK_CONSTRAINT_TABLE_NAME = fkd.FK_CONSTRAINT_TABLE_NAME and
  fk.FK_CONSTRAINT_NAME = fkm.FK_CONSTRAINT_NAME and
  fk.FK_CONSTRAINT_TABLE_NAME = fkm.FK_CONSTRAINT_TABLE_NAME and
  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX =
  fkm.FK_CONSTRAINT_REFERENCED_COLUMNS_INDEX
order by
  fk.FK_CONSTRAINT_TABLE_NAME,
  fk.FK_CONSTRAINT_NAME,
  fkd.FK_CONSTRAINT_REFERENCING_COLUMNS_INDEX
*)


  sqlSelectConstraints =
    'SELECT'#13#10+
    '  CHECK_CONSTRAINT_NAME AS CTX_CONSTRAINT_NAME,'#13#10+
    '  CHECK_CONSTRAINT_TABLE_NAME AS CTX_TABLE_NAME,'#13#10+
    '  CHECK_CONSTRAINT_CHECK_CLAUSE AS CTX_CHECK'#13#10+
    'FROM #CHECK_CONSTRAINTS'#13#10;

  sqlSelectTriggers =
    'SELECT'#13#10+
    '  TRIGGER_NAME AS CTX_TRIGGER_NAME,'#13#10+
    '  TRIGGER_TARGET AS CTX_TABLE_NAME,'#13#10+
    '  TRIGGER_SOURCE AS CTX_DEFINITION,'#13#10+
    '  TRIGGER_DESCRIPTION AS CTX_DESCRIPTION'#13#10+
    'FROM #TRIGGERS'#13#10;

  sqlSelectViews =
    'SELECT'#13#10+
    '  VIEW_NAME AS CTX_VIEW_NAME,'#13#10+
    '  VIEW_DESCRIPTION AS CTX_DESCRIPTION,'#13#10+
    '  VIEW_SOURCE AS CTX_DEFINITION'#13#10+
    'FROM #VIEWS'#13#10;

  sqlSelectProcedures =
    'SELECT'#13#10+
    '  PROCEDURE_NAME AS CTX_PROCEDURE_NAME,'#13#10+
    '  PROCEDURE_DESCRIPTION AS CTX_DESCRIPTION,'#13#10+
    '  PROCEDURE_SOURCE AS CTX_DEFINITION,'#13#10+
    '  0 AS CTX_IS_FUNCTION'#13#10+
    'FROM #PROCEDURES'#13#10+
    'UNION ALL'#13#10+
    'SELECT'#13#10+
    '  FUNCTION_NAME AS CTX_PROCEDURE_NAME,'#13#10+
    '  FUNCTION_DESCRIPTION AS CTX_DESCRIPTION,'#13#10+
    '  FUNCTION_SOURCE AS CTX_DEFINITION,'#13#10+
    '  1 AS CTX_IS_FUNCTION'#13#10+
    'FROM #FUNCTIONS'#13#10;

function RelActionFromString(const Action: String): TRelationAction;
begin
  if AnsiSameText(Action, 'CASCADE') then
    Result := raCascade
  else if AnsiSameText(Action, 'SET NULL') then
    Result := raNullify
  else if AnsiSameText(Action, 'SET DEFAULT') then
    Result := raSetDefault
  else Result := raError;
end;

function GetNewIndexName(IdxDefs: TIndexDefinitions; const ProposedName: String): String;
var
  NameCounter: Integer;
begin
  Result := ProposedName;
  NameCounter := 1;
  while IdxDefs.Find(Result) <> nil do
  begin
    Result := ProposedName + IntToStr(NameCounter);
    Inc(NameCounter);
  end;
end;

{ DDL functions: ReverseEngineering}

procedure TnxDatabaseExt.ReverseEngineer;
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

(*
procedure TnxDatabaseExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TnxTable;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TnxTable;
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
*)

function TnxDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

function TnxDatabaseExt.GetConnected: Boolean;
begin
  Result := inherited Active;
end;

procedure TnxDatabaseExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin
  inherited GetTableNames(List);
end;

procedure TnxDatabaseExt.SetConnected(Value: Boolean);
begin
  FVersionStr := '';
  inherited Active := Value;
end;

function TnxDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;


end.




