(******************************************************************************)
(*
(*  Context Database Extensions Suite (DBISAM)
(*
(*  Contains: TDBISAMDatabaseExt, TDBISAMQueryExt, TDBISAMTableExt components.
(*
(*  Copyright (c) 2004-2006 Michael Baytalsky
(*
(******************************************************************************)
unit DBISAMExt;

{$I CtxVer.inc}

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBISAMTb, dbExtParser, CtxDataTypes, dbExtUtils, CtxDBIntf, dbSchema, CtxDataSetCommand;

{$I DBISAMVR.INC}

type
{$IFnDEF D2009_ORLATER}
  TRecordBuffer = PChar;
{$ENDIF}


  TDBISAMDatabaseExt = class;
  TDBISAMTableExt = class;
  TDBISAMQueryExt = class;

  {:$ TDatabaseOperationProgress type is used by the database's OnProgress event. }
  TDatabaseOperationProgress = procedure (Sender: TDBISAMDatabaseExt; const Operation: String; PercentDone: Byte; var Abort: Boolean) of object;

  {:$ TDBISAMRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TDBISAMRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TDBISAMDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TDBISAMDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;

    {$IFnDEF DBISAM_V4}
    procedure SetSuppressAutoIncValues(Value: Boolean); override;
    {$ENDIF}
  end;
  
  {:$ Represents a DBISAM database connection. }
  {:: TDBISAMDatabaseExt extends the functionality of TDBISAMDatabase with the }
  {:: following new features:<br> }
  {:: - Additional events, allowing more control over datasets being opened and }
  {:: closed, using this database connection;<br> }
  {:: - Database schema specifications (see TDatabaseSchema component);<br> }
  {:: - Creating/Updating database structure according to the schema version;<br> }
  {:: - Referentical integrity constraints;<br> }
  {:: - Cascade operations;<br> }
  {:: - Extended fields specifications;<br> }
  {:: - 2 types of triggers executed when a record or a compound object has changed;<br> }
  {:: - 2 types of replications, including ability to synchronize with a }
  {:: database of the similar structure or with the database of a generic }
  {:: structure;<br> }
  {:: - Exporting and importing changes to/from portable binary format (stream);<br> }
  {:: - Exporting and importing whole database to/from portable binary format (stream);<br> }
  {:! Unlike TDBISAMDatabase, TDBISAMDatabaseExt requires explicit declaration }
  {:! for every database connection that require one or more of the above }
  {:! described features. If you don't declare TDBISAMDatabaseExt explicitly }
  {:! the engine will automatically create TDBISAMDatabase component instead.<br> }
  {:! The extended features of TDBISAMDatabaseExt are only accessible if }
  {:! TDBISAMDatabaseExt used in combination with TDatabaseSchema, TDBISAMTableExt }
  {:! and TDBISAMQueryExt. }
  TDBISAMDatabaseExt = class(TDBISAMDatabase, ICtxDatabase, ISchemaDatabase, IDatabaseExt)
  protected
    { Protected declarations }
    FDataSetAfterOpen: TDataSetNotifyEvent;
    FDataSetBeforeClose: TDataSetNotifyEvent;
    FOnProgress: TDatabaseOperationProgress;

    FSchema: TDatabaseSchema;
    FSystemTableName: String;
    FObjectsTableName: String;

    FUpdateOptions: TUpdateOptions;
    FSaveUpdateOptions: TUpdateOptions;
    FAutoRefreshOnRollback: Boolean;

    FUserName: String;

    FSystemTable: TSystemTable;
    FObjectsTable: TObjectsTable;

    FActiveTransaction: TActiveTransaction;

    FDatabaseLockCounter: Integer;
    FInTransactionCount: Integer;
    FVirtualKeys: TStringList;
    FCachedTables: TList;
    FMaxCachedTables: Integer;
    FEnabledTriggers: TSelectTriggers;
    FReplicating: Boolean;

    function GetObjectsTableName: String;
    function GetSystemTableName: String;
    function GetDatabaseName: String;
    procedure SetDatabaseName(const Value: String);
    procedure SetReplicating(Value: Boolean);
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetVersionLabel: String;
    procedure SetVersionLabel(const Value: String);

    function GetUserName: String;
    procedure SetUserName(const Value: String);

    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);

    procedure DoDataSetAfterOpen(DataSet: TDataSet); virtual;
    procedure DoDataSetBeforeClose(DataSet: TDataSet); virtual;

    { Replication support }
    function GetActiveTransaction: TActiveTransaction;
    function GetSchema: TDatabaseSchema;
    procedure SetSchema(Value: TDatabaseSchema);
    procedure SetSystemTableName(const Value: String);
    procedure SetObjectsTableName(const Value: String);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ExecuteUpdate(DatabaseUpdate: TDatabaseUpdate; var Abort: Boolean); virtual;

    function GetSnapshotID: Integer;
    function GetReplicationID: Integer;
    procedure SetSnapshotID(Value: Integer);
    procedure SetReplicationID(Value: Integer);
    function GetDatabaseURL: String;
    procedure SetDatabaseURL(const Value: String);
    function GetUpdateOptions: TUpdateOptions;

    function GetEngineName: String;

    function GetObjectsTable: TObjectsTable;
    function GetSystemTable: TSystemTable;

    procedure DestroySystemTable;
    procedure DestroyObjectsTable;
    procedure CheckActive;
      { Checks if the database is connected }
    procedure CheckSchema;
      { Checks if schema is assigned }
    function FindTable(const TableName: String): TDBISAMTableExt;
      { Attempts to locate table in cache }
    procedure RemoveFromCache(DataSet: TDataSet);
      { Removes DataSet from Chache - called AfterClose}
    procedure DoQueryProgress(Sender: TObject; PercentDone: Word; var AbortQuery: Boolean);
      { Invokes OnProgress event if assigned with Operation = 'Executing Query...' }
    procedure FreeTableFromCache(const TableName: string);
  public
    {:: Creates an instance of a TDBISAMDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TDBISAMDatabaseExt component.}
    destructor Destroy; override;

    {:: Invokes OnProgress event handler for the TDBISAMDatabaseExt component. }
    procedure DoProgress(const Operation: String; PercentDone: Byte; var Abort: Boolean);

    {:: Creates TDBISAMTableExt component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TDBISAMQueryExt component, corresponding to Statement. }
    function CreateQuery(const Statement: String): TDataSet;

    {:$ Returns TDBISAMTableExt component for a specified table. }
    {:: This method utilizes internal cache, allocated within TDBISAMDatabaseExt }
    {:: component, to avoid frequently opening and closing tables.<br> }
    {:: If a table component for the requested tablename already exists in the }
    {:: database cache, this method will return an existing component. Otherwise }
    {:: it will create a new TDBISAMTableExt component, assign Database, Session }
    {:: and TableName properties and optionally open the table.<br> }
    {:: CancelRange parameter specifies whether ot not this method should Cancel }
    {:: any existing range that might be set on the table. }
    {:! The caller is responsible for destroying created component. Even if Open }
    {:! parameter is False, this method might still return an open table. However, }
    {:! if Open parameter is true the table is guaranteed to be open. }
    function OpenTable(const TableName: String; Open: Boolean = False; CancelRange: Boolean = True): TDBISAMTableExt; virtual;

    {:$ Places table in the database cache if it's opened. }
    {:: The database then will be responsible for destroying this Table component. }
    {:: It will also be able to return this Table later if requested through the }
    {:: CreateTable method }
    procedure ReleaseTable(Table: TDBISAMTableExt);

    {:$ Destroys all cached table components. }
    procedure ClearTableCache;

    {:$ Checks for the System table to be opened. }
    {:: If CreateIfNotExist parameter is True, the method will attempt to create a physical }
    {:: table of the propper structure and then create a corresponding component. }
    {:! The name for the System table is stored in SystemTableName property. }
    function CheckSystemTable(CreateIfNotExist: Boolean = False): Boolean;

    {:: Checks for the Objects table to be opened }
    {:: If CreateIfNotExist parameter is True, the method will attempt to create a physical }
    {:: table of the propper structure and then create a corresponding component. }
    {:! The name for the Objects table is stored in ObjectsTableName property. }
    function CheckObjectsTable(CreateIfNotExist: Boolean = False): Boolean;

    {:$ Locks Database by re-opening System table exclusively. }
    procedure LockDatabase;

    {:$ Unlocks Database by re-opening System table non-exclusively. }
    procedure UnLockDatabase;

    { Schema related methods. Schema must be assigned for them to work }
    {:$ Updates database schema from the physical database tables. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be updated. }
    procedure ReverseEngineer;

    {:$ Restructures the database according to the information contained in schema. }
    procedure RestructureFromSchema;

    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);
    
    {:$ Creates a new database based on the schema information. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be used to create database tables. }
    {:: If the database already exists in the specified location, OwerwriteTables }
    {:: parameter controls whether the tables will be overwriten. }
    {:: CreateSystemTable parameter controls whether the System table should be created. }
    {:: CreateObjectsTable parameter controls whether the Objects table should be created. }
    {:! The name for the System table is stored in SystemTableName property. }
    procedure CreateNewDatabase(OverwriteTables: Boolean = True; CreateSystemTable: Boolean = True; CreateObjectsTable: Boolean = True);

    {:$ Applies updates contained in the schema Updates collection. }
    {:: This method will only execute updates for the database's version and up }
    {:: until the most recent version is reached. }
    {:: Returns true if the database version is okey after the update. }
    function UpdateDatabase: Boolean;

    {:$ Checks whether the database version is *equal* or Newer }
    {:$ then the current Schema's Version. }
    function IsVersionCurrent(AllowNewer: Boolean = False): Boolean;

    {:$ Begins a new transaction against the database. }
    {:: Call StartTransaction to begin a new transaction against the database. }
    {:: Before calling StartTransaction, an application should check the status of }
    {:: the InTransaction property. If InTransaction is True, indicating that a }
    {:: transaction is already in progress, a subsequent call to StartTransaction without }
    {:: first calling Commit or Rollback to end the current transaction raises an exception. }
    {:: See StartTransactionCount method for more information on how to implement }
    {:: recursive transactions. }
    procedure StartTransaction; overload;
    procedure StartTransaction(Tables: TStrings = nil); overload; override; 

    {:$ Begins a new 'recursive' transaction against the database. }
    {:: This method does not start a physical transaction, instead it increment }
    {:: a counter, that will be used further by Commit or Rollback procedures. }
    {:: Once within a recursive transaction, use StartTransaction to start a }
    {:: physical transaction against the database. Check the status of InTransaction property }
    {:: before calling StartTransaction. <br> }
    {:: Ex. if not Database.InTransaction then Database.StartTransaction <br> }
    {:: If you're using StartTransactionCount you don't have to worry about }
    {:: the transaction being committed or rolled back within the recursive }
    {:: call. Commit or Rollback will only proceed with their function }
    {:: when the internal counter is rolled back to 0. See code example for more }
    {:: information. }
    procedure StartTransactionCount;
    {:$ Permanently stores updates, insertions, and deletions of data associated }
    {:$ with the current transaction, and ends the current transactions. }
    {:: Call Commit to permanently store to the database all updates, insertions, }
    {:: and deletions of data associated with the current transaction and then end }
    {:: the transaction. The current transaction is the last transaction started by }
    {:: calling StartTransaction or StartTransactionCount.<br> }
    {:: If you use StartTransactionCount, then Commit will decrement the }
    {:: internal counter before actually performing its function and only }
    {:: perform actual commit if the counter is zero, indicating, that the }
    {:: procedure has reached the top level of recursion. See code example for }
    {:: information on how to use recursive transactions.<br> }
    {:: The optional ForceFlush parameter allows you to specifically indicate }
    {:: whether the commit should also perform an operating system flush to disk. }
    {:: The default value is True. }
    procedure Commit;

    function GetInTransaction: Boolean;

    {:$ Cancels all updates, insertions, and deletions for the current transaction }
    {:$ and ends the transaction. }
    {:: Call Rollback to cancel all updates, insertions, and deletions for the }
    {:: current transaction and to end the transaction. The current transaction is }
    {:: the last transaction started by calling StartTransaction. }
    procedure Rollback;

    {:$ Refreshes all connected Tables, which are in dsBrowse state. }
    {:: This method is useful after Rollback. }
    procedure RefreshTables;

    { The rest of IDatabaseExt interface }
    procedure BeginReplicating;
    procedure EndReplicating;
    function GetReplicating: Boolean;
    function GetEnabledTriggers: TSelectTriggers;

    function FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;

    function AddVirtualKey(const TableName, KeyFields: String; KeyValues: Variant; CaseInsensitive: Boolean = False): Integer;
    function VirtualKeyExists(const VirtualKey: String): Boolean;
    procedure DeleteVirtualKey(VirtualKeyID: Integer);

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

    { Parent object is always a table or schema. }
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;

    function CreateCommand: TCtxDataCommand;

    {:: The ID of the snapshot. 0 is reserved for the main database. }
    {:: This ID can be used to ensure, that other ID's generated by this snapshots are unique. }
    property SnapshotID: Integer read GetSnapshotID write SetSnapshotID default 0;
    property ReplicationID: Integer read GetReplicationID write SetReplicationID default 0;
    {:$ Reference to the System table. }
    {:! This property is read-only. }
    property SystemTable: TSystemTable read FSystemTable;
    {:$ Reference to the Objects table. }
    {:! This property is read-only. }
    property ObjectsTable: TObjectsTable read FObjectsTable;
    {:$ Version of the database. }
    {:: The information about the database version is stored in a System table. }
    property Version: TSchemaVersion read GetVersion write SetVersion;
    {:$ Major version of the database. }
    {:: The information about the database version is stored in a System table. }
    {:: This property is read-only. In order to update version information use  }
    {:: Version property insted. }
    property MajorVersion: Integer read GetMajorVersion;
    {:$ Minor version of the database. }
    {:: The information about the database version is stored in a System table. }
    {:: This property is read-only. In order to update version information use  }
    {:: Version property insted. }
    property MinorVersion: Integer read GetMinorVersion;
    {:$ ActiveTransaction object contains information about objects being changed }
    {:$ within a current transaction. }
    property ActiveTransaction: TActiveTransaction read FActiveTransaction;
    {:$ True if the database is being replicated or imported. }
    {:: This property is read-only. }
    property Replicating: Boolean read FReplicating;
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
    property SystemTableName: String read GetSystemTableName write SetSystemTableName;
    {:$ The name of the Objects table. }
    {:: The default value of this property is 'objects'. }
    property ObjectsTableName: String read GetObjectsTableName write SetObjectsTableName;
    {:$ Specifies various options, controlling the behaviour of the database during update operations. }
    {:: Set UpdateOptions to enable/disable certain tasks, that can be performed when }
    {:: updating (editing, inserting or deleting) a record in one of the connected data sets.<br> }
    {:: TUpdateOption = (<br>}
    {::   uoEnableErrorConstraints - if set, the database will perform error constraint check, based on information stored in Schema.Relations collection. <br>}
    {::   uoEnableCascadeConstraints - if set, the database will execute cascade operations (delete or update), based on information stored in Schema.Relations collection. <br>}
    {::   uoEnableAggregates - if set, the calculated fields (sum & count) will be automatically updated whenever one of the references records changes. <br>}
    {::   uoEnableTriggers - if set, the database will execute triggers defined in Schema.Triggers for the modified dataset. <br>}
    {::   uoEnableChangeTracking - if set, the database will write information about every change made to the database objects into 'objects' table. This information is necessary for the database replications. <br>}
    {:: ); <br>}
    {:! All update operations require Schema component to be assigned to the database. }
    property UpdateOptions: TUpdateOptions read GetUpdateOptions write FUpdateOptions;
    {:$ Maximum number of open table components (TDBISAMTableExt) that can }
    {:$ be stored within an instance of the database component. }
    property MaxCachedTables: Integer read FMaxCachedTables write FMaxCachedTables;
    {:$ Specifies the uniform path to the database for both local and C/S types of access. }
    {:: This parameter is usefull for displaying or storing database and session parameters }
    {:: for the specific database. It may also be used with TDBISAMOpenDatabase dialog.<br> }
    {:: Attention! Assigning DatabaseURL may change the parameters of the session component }
    {:: the database is attached to. }
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL stored false;
    {:$ If set to True causes the database to refresh all active tables after Rolling back a transaction. }
    {:: Default value: False.}
    property AutoRefreshOnRollback: Boolean read FAutoRefreshOnRollback write FAutoRefreshOnRollback default False;

    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    { Events }
    {:$ Occurs after an application completes opening a dataset and before any data access occurs. }
    property DataSetAfterOpen: TDataSetNotifyEvent read FDataSetAfterOpen write FDataSetAfterOpen;
    {:$ Occurs before an application executes a request to close the dataset. }
    property DataSetBeforeClose: TDataSetNotifyEvent read FDataSetBeforeClose write FDataSetBeforeClose;
    {:$ Occurs when database performs some long lasting operation or executes a query using ExecuteSQL or ExecuteSQLParam methods. }
    property OnProgress: TDatabaseOperationProgress read FOnProgress write FOnProgress;
  end;

  {:$ Use TDBISAMTableExt to access data in a single database table using DBISAM. }
  {:: TDBISAMTableExt provides access to extended functionality like referential }
  {:: integrity constraints, replications ans such and should be used in conjunction }
  {:: with TDBISAMDatabaseExt. }
  TDBISAMTableExt = class(TDBISAMTable)
  protected
    { Protected declarations }
    FTemporary: Boolean;
    FClearFieldDefsOnClose: Boolean;
    FOptimizedLookups: Boolean;
    FAllowAutoOpen: Boolean;
    FAutoFieldsProperties: Boolean;
    FFieldsPropertiesUpdated: Boolean;
    FUpdateOptions: TUpdateOptions;

    procedure CheckActive; override;
    procedure CalculateFields(Buffer: TRecordBuffer); override;

    procedure DoAfterOpen; override;
    procedure DoBeforeClose; override;

    procedure CloseCursor; override;
    procedure OpenCursor(InfoQuery: Boolean); override;

    procedure DoOnNewRecord; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalDelete; override;

    procedure SetAutoFieldsProperties(const Value: Boolean);
    procedure DataEvent(Event: TDataEvent; Info: Integer); override;
  public
    { Public declarations }
    {:: Creates an instance of TDBISAMTableExt component. }
    constructor Create(AOwner: TComponent); override;
    {:: Initializes FieldDefs collection from persistent fields. }
    procedure AssignFieldDefsFromFields(Source: TDataSet = nil);
    {:$ Updates properties of defined persistent fields from the information contained in the Schema component. }
    {:: Schema component must be assigned to a database this table is connected to. }
    procedure UpdateFieldsProperties;
  published
    { Published declarations }
    {:: If this property is set to True the table will be automatically opened }
    {:: before performing any read/write operations, like searching or accessing field values.<br> }
    {:: Default value is False.}
    property AllowAutoOpen: Boolean read FAllowAutoOpen write FAllowAutoOpen default False;
    {:: If this property is set to True the table will use optimized method of  }
    {:: calculating lookup fields. This might considerably improve access speed }
    {:: when many lookup fields are defined based on the same foreighn key and information }
    {:: is accessed via C/S connection.<br> }
    {:: Default value is False.}
    property OptimizedLookups: Boolean read FOptimizedLookups write FOptimizedLookups default False;
    {:: If this property is set to True, then table will be automatically created upon opening }
    {:: and deleted after it has been closed.<br> }
    {:: If FieldDefs are not assigned at that time they will be temporarily assgned }
    {:: from persistent fields and cleared after the table is deleted.<br> }
    {:: Default value is False.}
    property Temporary: Boolean read FTemporary write FTemporary default False;
    {:: If this property is set to true the field properties will be automatically }
    {:: set from the connected Schema component. Origin property for each field is }
    {:: used to locate field definition within a database schema. }
    {:: Default value is False.}
    property AutoFieldsProperties: Boolean read FAutoFieldsProperties write SetAutoFieldsProperties default False;
    {:$ Specifies various options, controlling the behaviour of the table during update operations. }
    {:: Set UpdateOptions to enable/disable certain tasks, that can be performed when }
    {:: updating (editing, inserting or deleting) a record in the table.<br> }
    {:: TUpdateOption = (<br>}
    {::   uoEnableErrorConstraints - if set, the database will perform error constraint check, based on information stored in Schema.Relations collection for this table.}
    {::   uoEnableCascadeConstraints - if set, the database will execute cascade operations (delete or update), based on information stored in Schema.Relations collection for this table.}
    {::   uoEnableTriggers - if set, the database will execute triggers defined in Schema.Triggers for this table. }
    {::   uoEnableChangeTracking - if set, the database will write information about every change made to the database objects into 'objects' table. This information is necessary for the database replications. }
    {:: ); }
    {:! All update operations require Schema component to be assigned for the database this table is connected to. }
    property UpdateOptions: TUpdateOptions read FUpdateOptions write FUpdateOptions;
  end;

  {:$ Use TDBISAMQueryExt to access one or more tables in a database using SQL statements. }
  {:: TDBISAMQueryExt provides access to extended functionality like referential }
  {:: integrity constraints, replications ans such and should be used in conjunction }
  {:: with TDBISAMDatabaseExt. }
  TDBISAMQueryExt = class(TDBISAMQuery)
  protected
    { Protected declarations }
    FOptimizedLookups: Boolean;
    FAllowAutoOpen: Boolean;
    FAutoFieldsProperties: Boolean;
    FFieldsPropertiesUpdated: Boolean;
    FUpdateOptions: TUpdateOptions;
    FSQLPattern: TStrings;
    FMacros: TMacros;
    FMacroBegin: String;
    FMacroEnd: String;
    FAllowMacros: Boolean;

    procedure SetAllowMacros(const Value: Boolean);
    procedure CheckActive; override;
    procedure CalculateFields(Buffer: TRecordBuffer); override;
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
    procedure DoBeforeClose; override;

    procedure DoOnNewRecord; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalDelete; override;

    procedure SetAutoFieldsProperties(const Value: Boolean);
    procedure DataEvent(Event: TDataEvent; Info: Integer); override;
    procedure SetSQL(const Value: TStrings);
    function GetMacros: TMacros;
    procedure SetMacros(Value: TMacros);
    procedure PatternChanged(Sender: TObject);
    procedure SetMacroBegin(const Value: String);
    procedure SetMacroEnd(const Value: String);
    procedure RecreateMacros;
  public
    { Public declarations }
    {:: Creates an instance of TDBISAMQueryExt component. }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {:: Initializes FieldDefs collection from persistent fields. }
    procedure UpdateFieldsProperties;
    {:: Returns macro object (TMacro) with the given name. }
    {:! If macro with the specified name doesn't exist, the exception will be raised. }
    function MacroByName(const Value: string): TMacro;
    {:: Expands SQL statement using Macro definitions and returns result in SQL parameter. }
    procedure ExpandSQL(SQL: TStrings);
    {:: Executes SQL statement provided in SQL property. }
    procedure ExecSQL;
  published
    { Published declarations }
    {:: If this property is set to True the query will be automatically opened (executed) }
    {:: before performing any read/write operations, like searching or accessing field values.<br> }
    {:: Default value is False.}
    property AllowAutoOpen: Boolean read FAllowAutoOpen write FAllowAutoOpen default False;
    {:: If this property is set to True the table will use optimized method of  }
    {:: calculating lookup fields. This might considerably improve access speed }
    {:: when many lookup fields are defined based on the same foreighn key and information }
    {:: is accessed via C/S connection.<br> }
    {:: Default value is False.}
    property OptimizedLookups: Boolean read FOptimizedLookups write FOptimizedLookups default False;
    {:: If this property is set to true the field properties will be automatically }
    {:: set from the connected Schema component. Origin property for each field is }
    {:: used to locate field definition within a database schema. }
    {:: Default value is False.}
    property AutoFieldsProperties: Boolean read FAutoFieldsProperties write SetAutoFieldsProperties default False;
    {:$ Specifies various options, controlling the behaviour of the table during update operations.
     :: This property only apply if the result set is live and editable.<br>
      Set UpdateOptions to enable/disable certain tasks, that can be performed when
      updating (editing, inserting or deleting) a record in the data set.<br>
      TUpdateOption = (<br>
        uoEnableErrorConstraints - if set, the database will perform error constraint check, based on information stored in Schema.Relations collection for this dataset.<br>
        uoEnableCascadeConstraints - if set, the database will execute cascade operations (delete or update), based on information stored in Schema.Relations collection for this dataset.<br>
        uoEnableTriggers - if set, the database will execute triggers defined in Schema.Triggers for this dataset.<br>
        uoEnableChangeTracking - if set, the database will write information about every change made to the database objects into 'objects' table. This information is necessary for the database replications.<br>
      ); <br>
     :! All update operations require Schema component to be assigned for the database this table is connected to. }
    property UpdateOptions: TUpdateOptions read FUpdateOptions write FUpdateOptions;

    {:: Contains the text of the SQL statement to execute for the query. Use SQL }
    {:: to provide the SQL statement that a query component executes when its Open }
    {:: or ExecSQL methods are called. At design time the SQL property can be edited }
    {:: by invoking the String List editor in the Object Inspector.}
    {:: The SQL property may contain multiple SQL statements as long as they are }
    {:: separated by semicolons (;).}
    {:: The SQL statement provided to the SQL property may contain replaceable }
    {:: parameters, following standard SQL-92 syntax conventions. Parameters are }
    {:: created and stored in the Params property.}
    {:: The SQL statement may also contain replaceable macros, defined by MacroBegin }
    {:: and MacroEnd properties. }
    property SQL: TStrings read FSQLPattern write SetSQL;

    {:: Collection of Macros defined for this query component. }
    {:: Macros can be used sometimes to populate query templates, where the functionality provided by
    {:: parameters is not sufficient.<br>}
    {:: Example of macro definition: <%filter_expression%> <br>}
    {:: select * from orders join <%table%> on orders.<%idfield%> = <%table%>.<%idfield%> <br>}
    {:: where DateEntered = '<%YEAR%>-01-01' <%additional_filter%><br>}
    {:: In the above query macro table could be substituted using MacroByName method }
    {:: for, say, Vendors or Customers, and idfield for VendorID or CustomerID accordingly }
    {:: YEAR could be substituted for the year portion of a date and additional_filter
    {:: could be specified for example as MacroByName('additional_filter').Value = 'and (Status = 0)'.}
    property Macros: TMacros read GetMacros write SetMacros;
    {:: The character used to dermine the beginning of a macro statement.<br>}
    {:: Default value is '<%'.<br>}
    property MacroBegin: String read FMacroBegin write SetMacroBegin;
    {:: The character used to dermine the end of a macro statement.<br>}
    {:: Default value is '%>'.<br>}
    property MacroEnd: String read FMacroEnd write SetMacroEnd;
    {:: If set to SQL statement provided in SQl peoperty will be pre-processed for macros according }
    {:: MacroBegin and MacroEnd properties and Macros defined in Macros collection will be replaced by their values. }
    property AllowMacros: Boolean read FAllowMacros write SetAllowMacros default False;
  end;

  {:$ Retrieves database URL from the TDBISAMDatabase component and the connected Session component. }
  function GetDatabaseURL(DBISAMDatabase: TDBISAMDatabase): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(DBISAMDatabase: TDBISAMDatabase; DatabaseURL: String);

implementation

uses
{$IFDEF VER130}
  FileCtrl,
{$ENDIF}
  ActiveX, TypInfo;

const
  ctLocal = '';
  ctLAN = 'LAN';
  ctInternet = 'Internet';
  INTERNET_COMPRESSION = 6;
  DEFAULT_PORT = 12005;
  defSysTableName = 'System';

const
  idxByReplicationID = 'byReplicationID';

{ General Helper Rountines }

function GetDatabaseURL(DBISAMDatabase: TDBISAMDatabase): String;
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  if DBISAMDatabase.Session.SessionType = stLocal then
  begin
    ConnectionType := ctLocal;
    RemoteHost := '';
    DatabaseName := DBISAMDatabase.Directory;
  end else begin
    {$IFDEF DBISAM_V4}
    if DBISAMDatabase.Session.RemoteCompression = 0 then
      ConnectionType := ctLAN
    else ConnectionType := ctInternet;
    {$ELSE}
    if DBISAMDatabase.Session.RemoteType = rtLAN then
      ConnectionType := ctLAN
    else ConnectionType := ctInternet;
    {$ENDIF}
    RemoteHost := DBISAMDatabase.Session.RemoteHost;
    if DBISAMDatabase.Session.RemotePort <> DEFAULT_PORT then
      RemoteHost := RemoteHost + ':' + IntToStr(DBISAMDatabase.Session.RemotePort);
    DatabaseName := DBISAMDatabase.RemoteDatabase;
  end;
  Result := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
end;

procedure SetDatabaseURL(DBISAMDatabase: TDBISAMDatabase; DatabaseURL: String);
var
  ConnectionType, RemoteHost, RemotePort, DatabaseName: String;
  P: Integer;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  if ConnectionType = ctLocal then
  begin
    DBISAMDatabase.Session.SessionType := stLocal;
    DBISAMDatabase.Directory := DatabaseName;
  end else begin
    DBISAMDatabase.Session.SessionType := stRemote;
    {$IFDEF DBISAM_V4}
    if AnsiCompareText(ConnectionType, ctLAN) = 0 then
      DBISAMDatabase.Session.RemoteCompression := 0
    else DBISAMDatabase.Session.RemoteCompression := INTERNET_COMPRESSION;
    {$ELSE}
    if AnsiCompareText(ConnectionType, ctLAN) = 0 then
      DBISAMDatabase.Session.RemoteType := rtLAN
    else DBISAMDatabase.Session.RemoteType := rtInternet;
    {$ENDIF}

    P := Pos(':', RemoteHost);
    if P > 0 then
    begin
      RemotePort := copy(RemoteHost, P+1, MaxInt);
      RemoteHost := copy(RemoteHost, 1, P - 1);
      DBISAMDatabase.Session.RemotePort := StrToIntDef(RemotePort, DEFAULT_PORT);
    end;

    DBISAMDatabase.Session.RemoteHost := RemoteHost;
    DBISAMDatabase.RemoteDatabase := DatabaseName;
  end;
end;

{ TDBISAMRangeCursor }

constructor TDBISAMRangeCursor.Create(Database: TDBISAMDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TDBISAMRangeCursor.CreateExt(Database: TDBISAMDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  {$IFDEF DBISAM_V4}
  IdxDef: TDBISAMIndexDef;
  {$ELSE}
  IdxDef: TIndexDef;
  {$ENDIF}
  RangeFilter: String;
  Table: TDBISAMTableExt;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.OpenTable(TableName, True, False);
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

destructor TDBISAMRangeCursor.Destroy;
var
  Database: TDBISAMDatabase;
begin
  KeyFieldsList.Free;
  if DataSet <> nil then
  begin
    Database := TDBISAMDatabase(TDBISAMTableExt(DataSet).Database);
    if (Database <> nil) and (Database.InheritsFrom(TDBISAMDatabaseExt)) then
      TDBISAMDatabaseExt(Database).ReleaseTable(TDBISAMTableExt(DataSet));
  end;
  inherited Destroy;
end;

{$IFnDEF DBISAM_V4}

procedure TDBISAMRangeCursor.SetSuppressAutoIncValues(Value: Boolean); 
begin
  TDBISAMTableExt(DataSet).SuppressAutoIncValues := Value;
end;

{$ENDIF}

{ TDBISAMDatabaseExt }

constructor TDBISAMDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := defSysTableName;
  FObjectsTableName := 'Objects';
  FSystemTable := nil;
  FObjectsTable := nil;
  FUserName := '';
  FDatabaseLockCounter := 0;
  FInTransactionCount := 0;
  FVirtualKeys := TStringList.Create;
  FActiveTransaction := nil;
  FUpdateOptions := [uoEnableErrorConstraints,
    uoEnableCascadeConstraints,
    uoEnableAggregates, uoEnableTriggers];
  FCachedTables := nil;
  SetReplicating(False);
  FMaxCachedTables := 10;
  FAutoRefreshOnRollback := False;
end;

destructor TDBISAMDatabaseExt.Destroy;
begin
  ClearTableCache;  // MB: 09.26.2007 Fixing memory leak
  UnRegisterCtxDataProvider(Self);
  SystemTableName := '';
  ObjectsTableName := '';
  FVirtualKeys.Free;
  if FCachedTables <> nil then
    FreeAndNil(FCachedTables);
  inherited Destroy;
end;

procedure TDBISAMDatabaseExt.CheckActive;
begin
  if (Handle = nil) then
    DatabaseError(SDatabaseClosed);
end;

procedure TDBISAMDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TDBISAMDatabaseExt.OpenTable(const TableName: String;
  Open: Boolean = False; CancelRange: Boolean = True): TDBISAMTableExt;
begin
  Result := FindTable(TableName);
  if Result = nil then
  begin
    Result := TDBISAMTableExt.Create(nil);
    Result.DatabaseName := DatabaseName;
    Result.SessionName := SessionName;
    Result.TableName := TableName;
    Result.UpdateOptions := Self.UpdateOptions;
  end else begin
    if CancelRange {and Result.Ranged} then
      Result.CancelRange
  end;
  // Make sure it's open, but don't close it
  if Open then
    Result.Active := True;
  // If it has already been opened, then refresh it
  if Result.Active then
    Result.Refresh;
end;

function TDBISAMDatabaseExt.FindTable(const TableName: String): TDBISAMTableExt;
var
  I: Integer;
begin
  Result := nil;
  if FCachedTables = nil then exit;
  for I := FCachedTables.Count - 1 downto 0 do
  begin
    Result := TDBISAMTableExt(FCachedTables[I]);
    if CompareText(Result.TableName, TableName) = 0 then
    begin
      FCachedTables.Delete(I); // remove from list
      exit;
    end;
  end;
  Result := nil;
end;

procedure TDBISAMDatabaseExt.ReleaseTable(Table: TDBISAMTableExt);
begin
  if (Table <> nil) then
  begin
    if Table.Active then
    begin
      // Return to browse mode
      if Table.State in dsEditModes then
        Table.Cancel;
      // Return to browse mode
      if FCachedTables = nil then
        FCachedTables := TList.Create;

      FCachedTables.Add(Table);
      FreeNotification(Table);
      Table.AfterClose := RemoveFromCache;

      if FCachedTables.Count > FMaxCachedTables then
        TDBISAMTableExt(FCachedTables[0]).Free;
    end else
      Table.Free;
  end;
end;

procedure TDBISAMDatabaseExt.ClearTableCache;
begin
  if FCachedTables <> nil then
  begin
    FreeObjects(FCachedTables);
    FCachedTables.Count := 0;
  end;
end;

procedure TDBISAMDatabaseExt.RemoveFromCache(DataSet: TDataSet);
begin
  if FCachedTables <> nil then
    FCachedTables.Remove(DataSet);
end;

procedure TDBISAMDatabaseExt.FreeTableFromCache(const TableName: string);
var
  Table: TDBISAMTable;
begin
  Table := FindTable(TableName);
  RemoveFromCache(Table);
  Table.Free;
end;

procedure TDBISAMDatabaseExt.DoDataSetAfterOpen(DataSet: TDataSet);
begin
  if Assigned(FDataSetAfterOpen) then
    FDataSetAfterOpen(DataSet);
end;

procedure TDBISAMDatabaseExt.DoDataSetBeforeClose(DataSet: TDataSet);
begin
  if Assigned(FDataSetBeforeClose) then
    FDataSetBeforeClose(DataSet);
end;

procedure TDBISAMDatabaseExt.DoProgress(const Operation: String;
  PercentDone: Byte; var Abort: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Operation, PercentDone, Abort);
end;

procedure TDBISAMDatabaseExt.DoQueryProgress(Sender: TObject;
  PercentDone: Word; var AbortQuery: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, sExecutingQuery, PercentDone, AbortQuery);
end;

procedure TDBISAMDatabaseExt.ExecuteUpdate(
  DatabaseUpdate: TDatabaseUpdate; var Abort: Boolean);
var
  ResultSet: TDataSet;
  SQLStatement: String;
begin
  Abort := False;
  with DatabaseUpdate do
  begin
    ResultSet := nil;
    try
      SQLStatement := Trim(SQLScript);
      if SQLStatement <> '' then
        ResultSet := ExecuteSQL(Self, SQLScript, Iterate, False);
      FreeAndNil(ResultSet);
    except
      if not IgnoreSQLError then raise;
    end;
  end;
end;

function TDBISAMDatabaseExt.GetMajorVersion: Integer;
begin
  Result := -1;
  if not Connected then exit;
  Result := Version.Major; // This will force it to re-read the version
end;

function TDBISAMDatabaseExt.GetMinorVersion: Integer;
begin
  Result := -1;
  if not Connected then exit;
  Result := Version.Minor; // This will force it to re-read the version
end;

function TDBISAMDatabaseExt.GetReplicationID: Integer;
begin
  Result := 0;
  if CheckSystemTable then begin
    SystemTable.DataSet.Refresh;
    Result := SystemTable.ReplicationID.AsInteger
  end;
end;

function TDBISAMDatabaseExt.GetSnapshotID: Integer;
begin
  Result := 0;
  if CheckSystemTable then begin
    SystemTable.Refresh;
    Result := SystemTable.SnapshotID.AsInteger;
  end;
end;

function TDBISAMDatabaseExt.GetVersion: TSchemaVersion;
begin
  Result := SchemaVersion(-1,-1);
  if not Connected then exit;
  if CheckSystemTable then
    Result := SchemaVersion(SystemTable.MajorVersion.AsInteger, SystemTable.MinorVersion.AsInteger);
end;

function TDBISAMDatabaseExt.GetVersionLabel: String;
begin
  Result := VersionToStr(GetVersion);
end;

function TDBISAMDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, Self.Version, AllowNewer);
end;

procedure TDBISAMDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FSchema then
      Schema := nil
    else if AComponent.InheritsFrom(TDBISAMTableExt) then
      RemoveFromCache(TDBISAMTableExt(AComponent));
  end;
  inherited;
end;


procedure TDBISAMDatabaseExt.SetObjectsTableName(const Value: String);
begin
  if FObjectsTableName <> Value then
  begin
    FObjectsTableName := Value;
    if FObjectsTable <> nil then
      DestroyObjectsTable;
  end;
end;

procedure TDBISAMDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
  GetVersionLabel; // Force it to update version property if connected
end;

procedure TDBISAMDatabaseExt.SetSnapshotID(Value: Integer);
begin
  if not CheckSystemTable(True) then exit;
  // Write version and other stuff to the 'system' table
  with SystemTable do
  begin
    DataSet.Refresh;
    DataSet.Edit;
    try
      SnapshotID.AsInteger := Value;
      DataSet.Post;
    except
      DataSet.Cancel;
      raise;
    end;
  end;
end;

procedure TDBISAMDatabaseExt.SetReplicationID(Value: Integer);
begin
  if not CheckSystemTable(True) then exit;
  // Write version and other stuff to the 'system' table
  with SystemTable do
  begin
    DataSet.Refresh;
    DataSet.Edit;
    try
      ReplicationID.AsInteger := Value;
      DataSet.Post;
    except
      DataSet.Cancel;
      raise;
    end;
  end;
end;

procedure TDBISAMDatabaseExt.SetSystemTableName(const Value: String);
begin
  if FSystemTableName <> Value then
  begin
    FSystemTableName := Value;
    // Close system table if it's opened
    if FSystemTable <> nil then
      DestroySystemTable;
  end;
end;

procedure TDBISAMDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  dbExtUtils.SetVersion(Self, Value);
end;

procedure TDBISAMDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

function TDBISAMDatabaseExt.UpdateDatabase: Boolean;
begin
  Result := dbSchema.UpdateDatabase(Self);
end;

function TDBISAMDatabaseExt.CheckObjectsTable(CreateIfNotExist: Boolean = False): Boolean;
begin
  CheckActive;
  // Make sure, that objects table exists. Open or create it and map fields
  if FObjectsTable = nil then
  begin
    FObjectsTable := TObjectsTable.Create(CreateTable(FObjectsTableName), True);
    try
      with TDBISAMTableExt(FObjectsTable.DataSet) do
      begin
        UpdateOptions := uoNone;
        IndexDefs.Add('', 'ObjectType;ObjectKey', [ixPrimary]);
        IndexDefs.Add(idxByReplicationID, 'ReplicationID;ObjectType;ObjectKey', [ixUnique]);
      end;
      FObjectsTable.CreatePersistentFields;
      FObjectsTable.ObjectType.Size := 32;
      FObjectsTable.ObjectKey.Size := 64;
      FObjectsTable.UserName.Size := 32;
    except
      DestroyObjectsTable;
    end;
  end;
  if (FObjectsTable <> nil) and (not FObjectsTable.DataSet.Active) then
  with TDBISAMTableExt(FObjectsTable.DataSet) do
  try
    if (not Exists) and CreateIfNotExist then
    begin
      AssignFieldDefsFromFields;
      CreateTable;
    end;
    if Exists then
      Active := True
    else DestroyObjectsTable;
  except
    DestroyObjectsTable;
  end;
  Result := (FObjectsTable <> nil) and FObjectsTable.DataSet.Active;
end;

function TDBISAMDatabaseExt.CheckSystemTable(CreateIfNotExist: Boolean = False): Boolean;
var
  STable: string;
begin
  CheckActive;
  if Self.GetSchema <> nil then
    STable := Self.GetSchema.SystemTableName else
    STable := FSystemTableName;
  if Trim(STable) = '' then
    STable := defSysTableName;  
  if not AnsiSameText(FSystemTableName, STable) and (STable <> '') then
  begin
    if SystemTable <> nil then
      DestroySystemTable;
    FSystemTableName := STable;
  end;

  // Make sure, that system table exists. Open or create it and map fields
  if SystemTable = nil then
  begin
    FSystemTable := TSystemTable.Create(CreateTable(FSystemTableName), True);
    try
      TDBISAMTableExt(FSystemTable.DataSet).UpdateOptions := uoNone;
      FSystemTable.CreatePersistentFields;
      FSystemTable.SchemaName.Size := 127;
    except
      DestroySystemTable;
    end;
  end;
  if (FSystemTable <> nil) and (not FSystemTable.DataSet.Active) then
  with TDBISAMTableExt(FSystemTable.DataSet) do
  try
    if (not Exists) and CreateIfNotExist then
    begin
      AssignFieldDefsFromFields;
      CreateTable;
    end;
    if Exists then
    begin
      Active := True;
      if EOF then
      begin
        // Insert an empty record into the system table
        Insert;
        try
          Post;
        except
          Cancel;
          raise;
        end;
        First;
      end;
    end else
      DestroySystemTable;
  except
    DestroySystemTable;
  end;
  Result := (FSystemTable <> nil) and (FSystemTable.DataSet.Active);
end;

procedure TDBISAMDatabaseExt.DestroyObjectsTable;
begin
  FreeAndNil(FObjectsTable);
end;

procedure TDBISAMDatabaseExt.DestroySystemTable;
begin
  FreeAndNil(FSystemTable);
end;

procedure TDBISAMDatabaseExt.LockDatabase;
begin
  if FDatabaseLockCounter = 0 then
  begin
    if not CheckSystemTable(True) then
      DatabaseError(sUnableToLockDatabase);
    SystemTable.DataSet.Active := False;
    TDBISAMTableExt(SystemTable.DataSet).Exclusive := True;
    try
      SystemTable.DataSet.Active := True;
    except
      DatabaseError(sUnableToLockDatabase);
    end;
  end;
  Inc(FDatabaseLockCounter);
end;

procedure TDBISAMDatabaseExt.UnLockDatabase;
begin
  Dec(FDatabaseLockCounter);
  if (FDatabaseLockCounter = 0) and (SystemTable <> nil) then
  begin
    SystemTable.DataSet.Active := False;
    TDBISAMTableExt(SystemTable.DataSet).Exclusive := False;
  end;
end;

procedure TDBISAMDatabaseExt.StartTransactionCount;
begin
  if (FInTransactionCount = 0) and InTransaction then
    FInTransactionCount := 1; // Account for a transaction that has already started
  Inc(FInTransactionCount);
end;


procedure TDBISAMDatabaseExt.StartTransaction;
begin
  StartTransaction(nil);
end;

procedure TDBISAMDatabaseExt.StartTransaction(Tables: TStrings = nil);
begin
  // Check system tables
  if Replicating or (uoEnableChangeTracking in UpdateOptions) then
  begin
    if not CheckSystemTable(True) then
      DatabaseError(SSystemTableNotFound);
    if not CheckObjectsTable(True) then
      DatabaseError(SObjectsTableNotFound);
  end;
  inherited StartTransaction(Tables);
  // This is just a precaution. FActiveTransaction must always be nil here.
  if FActiveTransaction <> nil then
    FreeAndNil(FActiveTransaction);

  // Create transaction object
  FActiveTransaction := TActiveTransaction.Create;

  // Make sure system table exists
  if Replicating or (uoEnableChangeTracking in UpdateOptions) then
  begin
    SystemTable.Refresh;
    FActiveTransaction.ReplicationID := SystemTable.ReplicationID.AsInteger;
    FActiveTransaction.SnapshotID := SystemTable.SnapshotID.AsInteger;
  end;
end;

procedure TDBISAMDatabaseExt.Commit;
begin
  // FInTransactionCount can only be Zero if StartTransactionCount has never been invoked
  if (FInTransactionCount <= 1) and InTransaction then
  begin
    if FActiveTransaction <> nil then
    begin
      ExecuteScheduledAggregates(Self);
      if uoEnableChangeTracking in UpdateOptions then
        WriteObjectChanges(Self);
      FreeAndNil(FActiveTransaction);
    end;
    inherited Commit;
    FInTransactionCount := 0; // Set FInTransactionCount back to Zero
  end else Dec(FInTransactionCount);
end;

procedure TDBISAMDatabaseExt.Rollback;
begin
  // FInTransactionCount can only be Zero if StartTransactionCount has never been invoked
  if (FInTransactionCount <= 1) and InTransaction then
  begin
    if FActiveTransaction <> nil then
      FreeAndNil(FActiveTransaction);
    inherited Rollback;
    FInTransactionCount := 0; // Set FInTransactionCount back to Zero
    if FAutoRefreshOnRollback then
      RefreshTables;
  end else Dec(FInTransactionCount);
end;

procedure TDBISAMDatabaseExt.RefreshTables;
var
  I: Integer;
begin
  for I := 0 to DataSetCount - 1 do
    if DataSets[I].InheritsFrom(TDBISAMDataSet) and
      (TDBISAMDataSet(DataSets[I]).State = dsBrowse)
    then
      TDBISAMDataSet(DataSets[I]).Refresh;
end;

function TDBISAMDatabaseExt.GetEngineName: String;
begin
  Result := 'DBISAM';
end;

function TDBISAMDatabaseExt.GetDatabaseURL: String;
begin
  Result := DBISAMExt.GetDatabaseURL(Self);
end;

procedure TDBISAMDatabaseExt.SetDatabaseURL(const Value: String);
begin
  DBISAMExt.SetDatabaseURL(Self, Value);
end;

procedure TDBISAMDatabaseExt.SetReplicating(Value: Boolean);
begin
  FReplicating := Value;
  if FReplicating then
    FEnabledTriggers := [taAlways, taOnlyWhenReplicating]
  else FEnabledTriggers := [taAlways, taExceptWhenReplicating];
end;

procedure TDBISAMDatabaseExt.BeginReplicating;
begin
  SetReplicating(True);
  FSaveUpdateOptions := UpdateOptions;
  UpdateOptions := [uoEnableTriggers, uoEnableAggregates]; // uoNone;
end;

procedure TDBISAMDatabaseExt.EndReplicating;
begin
  SetReplicating(False);
  UpdateOptions := FSaveUpdateOptions;
end;

function TDBISAMDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TDBISAMRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TDBISAMDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TDBISAMRangeCursor.Create(Self, Relation, KeyValues);
end;

function TDBISAMDatabaseExt.GetActiveTransaction: TActiveTransaction;
begin
  Result := FActiveTransaction;
end;

function TDBISAMDatabaseExt.GetObjectsTable: TObjectsTable;
begin
  Result := FObjectsTable;
end;

function TDBISAMDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TDBISAMDatabaseExt.GetSystemTable: TSystemTable;
begin
  Result := FSystemTable;
end;

function TDBISAMDatabaseExt.GetDatabaseName: String;
begin
  Result := inherited DatabaseName;
end;

procedure TDBISAMDatabaseExt.SetDatabaseName(const Value: String);
begin
  inherited DatabaseName := Value;
  RegisterCtxDataProvider(Self);
end;

function TDBISAMDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := OpenTable(TableName);
end;

function TDBISAMDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TDBISAMQueryExt.Create(nil);
  with TDBISAMQueryExt(Result) do
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

procedure TDBISAMDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TDBISAMQuery(Query).ExecSQL;
end;

function TDBISAMDatabaseExt.ExecuteStatement(SQL: String;
  ResultSet: Pointer): Integer;
var
  Q: TDBISAMQuery;
  AParams: TParams;
begin
  Q := TDBISAMQuery(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    Result := IProviderSupport(Q).PSExecuteStatement(SQL, AParams, ResultSet);
  finally
    AParams.Free;
    Q.Free;
  end;
end;

procedure TDBISAMDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TDBISAMQuery(Query).Params);
end;

procedure TDBISAMDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TDBISAMQuery(Query).Params.AssignValues(Params);
end;

function TDBISAMDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TDBISAMQuery(Query).SQL.Text;
end;

procedure TDBISAMDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TDBISAMQuery(Query).SQL.Text := Statement;
end;

function TDBISAMDatabaseExt.GetUserName: String;
begin
  Result := FUserName;
end;

procedure TDBISAMDatabaseExt.SetUserName(const Value: String);
begin
  FUserName := Value;
end;

function TDBISAMDatabaseExt.GetObjectsTableName: String;
begin
  Result := FObjectsTableName;
end;

function TDBISAMDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

function TDBISAMDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

{ DDL functions: CreateDatabase, ReverseEngineering, RestructureFromSchema }

procedure TDBISAMDatabaseExt.CreateNewDatabase(OverwriteTables: Boolean = True; CreateSystemTable: Boolean = True; CreateObjectsTable: Boolean = True);
var
  I: Integer;
  Counter: TProgressCounter;
  Table: TDBISAMTableExt;
begin
  CheckSchema;
  CheckActive;
  // Create a new database based on schema information
  InitMinMax(Counter, 0, Schema.TableDefs.Count - 1);
  Table := OpenTable('');
  try
    DoProgress(sCreatingDatabase, 0, Counter.Abort);
    if Counter.Abort then exit;

    for I := 0 to Schema.TableDefs.Count - 1 do
    begin
      if Progress(Counter) then
        DoProgress(sCreatingDatabase, Counter.PercentDone, Counter.Abort);
      if Counter.Abort then exit;

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

    // Check System & Objects tables
    Table.TableName := SystemTableName;
    if Table.Exists and OverwriteTables then
    begin
      Table.DeleteTable;
      CheckSystemTable(True);
    end;

    Table.TableName := ObjectsTableName;
    if Table.Exists and OverwriteTables then
    begin
      Table.DeleteTable;
      CheckObjectsTable(True);
    end;

    // Set version. This will automatically create system table and update it
    // to the right values
    if CreateSystemTable then
      Version := Schema.Version;

    // Create objects table if necessary
    if CreateObjectsTable then
      CheckObjectsTable(True);
  finally
    Table.Free;
    DoProgress(sCreatingDatabase, 100, Counter.Abort);
  end;
end;

procedure TDBISAMDatabaseExt.ReverseEngineer;
var
  I, J: Integer;
  Table: TDBISAMTableExt;
  Tables: TStringList;
  LogicalTableName: String;
  FldDef: TFieldDefinition;
  IdxDef: TIndexDefinition;
  STable: string;
begin
  CheckActive;
  CheckSchema;

  if Self.GetSchema <> nil then
    STable := Self.GetSchema.SystemTableName else
    STable := FSystemTableName;

  if Trim(STable) = '' then
    STable := defSysTableName;  


  // Update schema from the physical tables
  Table := OpenTable('');
  Tables := TStringList.Create;
  try
    Schema.Clear;
    GetTableNames(Tables);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ChangeFileExt(Tables[I], '');
      if (AnsiCompareText(LogicalTableName, STable) = 0)
        or (AnsiCompareText(LogicalTableName, FObjectsTableName) = 0)
      then continue;

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
          FldDef.Description := Table.FieldDefs[J].Description;
          FldDef.DefaultExpression := Table.FieldDefs[J].DefaultValue;
          if Table.FieldDefs[J].MinValue <> '' then
            FldDef.SetPropValue('Min', Table.FieldDefs[J].MinValue);
          if Table.FieldDefs[J].MaxValue <> '' then
            FldDef.SetPropValue('Max', Table.FieldDefs[J].MaxValue);
          case Table.FieldDefs[J].CharCase of
            fcUpperCase: FldDef.SetPropValue('CharCase', 'UPPER');
            fcLowerCase: FldDef.SetPropValue('CharCase', 'LOWER');
          end;
          if Table.FieldDefs[J].Compression > 0 then
            FldDef.SetPropValue('Compress', IntToStr(Table.FieldDefs[J].Compression));
        end;

        for J := 0 to Table.IndexDefs.Count - 1 do
        if not ((Table.IndexDefs[J].Name = '') and AnsiSameText(Table.IndexDefs[J].Fields, 'RecordID')) then
        begin
          IdxDef := IndexDefs.Add;
          IdxDef.Assign(Table.IndexDefs[J]);
          case Table.IndexDefs[J].Compression of
            icDuplicateByte: IdxDef.SetPropValue('Compress', 'DUPBYTE');
            icTrailingByte: IdxDef.SetPropValue('Compress', 'TRAILBYTE');
            icFull: IdxDef.SetPropValue('Compress', 'FULL');
          end;
        end;
      end;
    end;
  finally
    Tables.Free;
    Table.Free;
  end;
end;

(*
procedure TDBISAMDatabaseExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TDBISAMTableExt;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := OpenTable('');
  Tables := TStringList.Create;
  try
    GetTableNames(Tables);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ChangeFileExt(Tables[I], '');

      if (AnsiCompareText(LogicalTableName, FSystemTableName) = 0)
        or (AnsiCompareText(LogicalTableName, FObjectsTableName) = 0)
      then continue;

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

procedure TDBISAMDatabaseExt.RestructureFromSchema;
var
  I, J, Idx, DefIdx: Integer;
  Table: TDBISAMTableExt;
  Tables: TStringList;
  NeedRestructure: Boolean;
  {$IFDEF DBISAM_V4}
  RF: TDBISAMFieldDef;
  RI: TDBISAMIndexDef;
  RestructureFieldDefs: TDBISAMFieldDefs;
  RestructureIndexDefs: TDBISAMIndexDefs;
  {$ELSE}
  RF: TRestructureFieldDef;
  RI: TRestructureIndexDef;
  RestructureFieldDefs: TRestructureFieldDefs;
  RestructureIndexDefs: TRestructureIndexDefs;
  {$ENDIF}

  NewFieldNo: Integer;
begin
  CheckActive;
  CheckSchema;

  // Update schema from the physical tables
  Table := OpenTable('');
  Tables := TStringList.Create;
  try
    GetTableNames(Tables);
    // Processing tables
    // Pass #1. Deleting removed tables
    for I := 0 to Tables.Count - 1 do
    begin
      if (AnsiCompareText(Tables[I], FSystemTableName) = 0)
        or (AnsiCompareText(Tables[I], FObjectsTableName) = 0)
      then continue;

      Idx := Schema.TableDefs.IndexOf(Tables[I]);
      if Idx < 0 then begin
        Table.TableName := Tables[I];
        if Table.Exists then
          Table.DeleteTable;
      end;
    end;

    // Pass #2. Creating new and updating existing tables
    for I := 0 to Schema.TableDefs.Count - 1 do
    begin
      Table.TableName := Schema.TableDefs[I].TableName;
      if Table.Exists then
      begin
        NeedRestructure := False;

        // Update table structure...
        {$IFDEF DBISAM_V4}
        RestructureFieldDefs := Table.FieldDefs;
        RestructureIndexDefs := Table.IndexDefs;
        {$ELSE}
        RestructureFieldDefs := Table.RestructureFieldDefs;
        RestructureIndexDefs := Table.RestructureIndexDefs;
        {$ENDIF}
        RestructureFieldDefs.Update;
        RestructureIndexDefs.Update;
        // Calculate maximum FieldNo - must be unique for new fields
        NewFieldNo := 1;
        for J := 0 to RestructureFieldDefs.Count - 1 do
          if NewFieldNo < RestructureFieldDefs[J].FieldNo then
            NewFieldNo := RestructureFieldDefs[J].FieldNo;
        Inc(NewFieldNo);
        // Process field defs
        // Pass #2.1: Removing fields and index defs, that has been deleted
        with Schema.TableDefs[I] do
        begin
          J := 0;
          while J < RestructureFieldDefs.Count do
            if FieldDefs.IndexOf(RestructureFieldDefs[J].Name) < 0 then
            begin
              (* +++ Deprecated. Done via checkpoints in schema editor
              Idx := FieldDefs.IndexOfRenamed(RestructureFieldDefs[J].Name);
              if Idx >= 0 then begin
                // Rename it here, set type later
                RestructureFieldDefs[J].Name := FieldDefs[Idx].Name;
                Inc(J);
              end else begin
                RestructureFieldDefs.Delete(J);
              end;
              *)
              RestructureFieldDefs.Delete(J);
              NeedRestructure := True;
            end else
              Inc(J);
          J := 0;
          while J < RestructureIndexDefs.Count do
            if IndexDefs.IndexOf(RestructureIndexDefs[J].Name) < 0 then
            begin
              RestructureIndexDefs.Delete(J);
              NeedRestructure := True;
            end else Inc(J);
        end;

        // Pass #2.2: Adding new fields & index defs
        // and updating the ones that remain
        with Schema.TableDefs[I] do
        begin
          // Update fields
          for J := 0 to FieldDefs.Count - 1 do
          begin
            DefIdx := RestructureFieldDefs.IndexOf(FieldDefs[J].Name);
            with FieldDefs[J] do
            if DefIdx < 0 then begin
              RestructureFieldDefs.Insert(J, {J+1 }NewFieldNo, Name, FieldDataTypeToVCL[DataType], Size, Required);
              Inc(NewFieldNo);
              NeedRestructure := True;
            end else begin
              if DefIdx <> J then begin
                {$IFDEF DBISAM_V4}
                RestructureFieldDefs[DefIdx].Index := J;
                {$ELSE}
                RestructureFieldDefs.Move(DefIdx, J);
                {$ENDIF}
                NeedRestructure := True;
              end;
              RF := RestructureFieldDefs[J];
              if (RF.Name <> Name) or (RF.DataType <> FieldDataTypeToVCL[DataType])
                or (RF.Size <> Size) or (RF.Required <> Required) then
              begin
                // RF.FieldNo := J + 1;
                RF.Name := Name;
                RF.DataType := FieldDataTypeToVCL[DataType];
                RF.Size := Size;
                RF.Required := Required;
                NeedRestructure := True;
              end;
            end;
          end;

          // Update indexes
          for J := 0 to IndexDefs.Count - 1 do
          begin
            DefIdx := RestructureIndexDefs.IndexOf(IndexDefs[J].Name);
            with IndexDefs[J] do
            if DefIdx < 0 then begin
              RestructureIndexDefs.Add(Name, Fields, Options);
              NeedRestructure := True;
            end else begin
              RI := RestructureIndexDefs[DefIdx];
              if (RI.Name <> Name) or (RI.Fields <> Fields)
                or (RI.Options <> Options) then
              begin
                RI.Name := Name;
                RI.Fields := Fields;
                RI.Options := Options;
                NeedRestructure := True;
              end;
            end;
          end;
        end; { Schema.TableDefs[I] }

        if NeedRestructure then
          {$IFDEF DBISAM_V4}
          Table.AlterTable;
          {$ELSE}
          Table.RestructureTable;
          {$ENDIF}
      end else begin
        // Create new table (dbSchema)
        Table.FieldDefs.Assign(Schema.TableDefs[I].FieldDefs); // +++
        Table.IndexDefs.Assign(Schema.TableDefs[I].IndexDefs);
        Table.CreateTable;
      end;
    end;
  finally
    Tables.Free;
    Table.Free;
  end;
end;

function TDBISAMDatabaseExt.AddVirtualKey(const TableName, KeyFields: String;
  KeyValues: Variant; CaseInsensitive: Boolean): Integer;
begin
  Result := FVirtualKeys.Add(GetVirtualKey(TableName, KeyFields, KeyValues, CaseInsensitive));
end;

procedure TDBISAMDatabaseExt.DeleteVirtualKey(VirtualKeyID: Integer);
begin
  FVirtualKeys.Delete(VirtualKeyID);
end;

function TDBISAMDatabaseExt.VirtualKeyExists(const VirtualKey: String): Boolean;
begin
  Result := dbSchema.VirtualKeyExists(FVirtualKeys, VirtualKey);
end;

function TDBISAMDatabaseExt.GetReplicating: Boolean;
begin
  Result := FReplicating;
end;

function TDBISAMDatabaseExt.FindKey(Table: TDataSet;
  const KeyValues: array of const): Boolean;
begin
  Result := (Table as TDBISAMTable).FindKey(KeyValues);
end;

function TDBISAMDatabaseExt.GetEnabledTriggers: TSelectTriggers;
begin
  Result := FEnabledTriggers;
end;

function TDBISAMDatabaseExt.GetUpdateOptions: TUpdateOptions;
begin
  Result := FUpdateOptions;
end;

function TDBISAMDatabaseExt.GetConnected: Boolean;
begin
  Result := Connected;
end;

procedure TDBISAMDatabaseExt.SetConnected(Value: Boolean);
begin
  Connected := Value;
end;

function TDBISAMDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  DatabaseError(SCapabilityNotSupported); Result := nil;
  // +++ Result := (DataSet as TDBISAMTable).IndexDefs;
end;

procedure TDBISAMDatabaseExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin
  Session.GetTableNames(Self.DatabaseName, List);
end;

function TDBISAMDatabaseExt.GetDriverName: String;
begin
  Result := 'DBISAM';
end;

function TDBISAMDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

{ TDBISAMTableExt }

procedure TDBISAMTableExt.AssignFieldDefsFromFields(Source: TDataSet = nil);
var
  I: Integer;
begin
  // Create FieldDefs from persistent fields
  if Source = nil then
    Source := Self;
  FieldDefs.Clear;
  for I := 0 to Source.FieldCount - 1 do
  begin
    with Source.Fields[I] do
      if (FieldKind = fkData) then
        FieldDefs.Add(FieldName, DataType, Size, Required);
  end;
end;

procedure TDBISAMTableExt.CalculateFields(Buffer: TRecordBuffer);
begin
  if OptimizedLookups and (State <> dsInternalCalc) then
  begin
    SetPrivateCalcBuffer(Self, Buffer);
    ClearCalcFields(CalcBuffer);
    CalculateFieldsExt(Buffer, Self);
    DoOnCalcFields;
  end else
    inherited CalculateFields(Buffer);
end;

procedure TDBISAMTableExt.CheckActive;
begin
  if FAllowAutoOpen and (State = dsInactive) then
    Active := True;
  inherited CheckActive;
end;

procedure TDBISAMTableExt.CloseCursor;
begin
  inherited CloseCursor;
  if FTemporary and Exists then
  begin
    try
      DeleteTable;
    except
      // Ignore all exceptions here...
    end;
    if FClearFieldDefsOnClose then
    begin
      FieldDefs.Clear;
      FClearFieldDefsOnClose := False;
    end;
  end;
end;

constructor TDBISAMTableExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowAutoOpen := False;
  FOptimizedLookups := False;
  FTemporary := False;
  FClearFieldDefsOnClose := False;
  FAutoFieldsProperties := False;
  FFieldsPropertiesUpdated := False;
  FUpdateOptions := uoAll;
end;

procedure TDBISAMTableExt.DataEvent(Event: TDataEvent; Info: Integer);
begin
  if Event = dePropertyChange then
    FFieldsPropertiesUpdated := False;
  inherited;
end;

procedure TDBISAMTableExt.DoAfterOpen;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt) then
    TDBISAMDatabaseExt(Database).DoDataSetAfterOpen(Self);
  inherited;
end;

procedure TDBISAMTableExt.DoBeforeClose;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt) then
    TDBISAMDatabaseExt(Database).DoDataSetBeforeClose(Self);
  inherited;
end;

procedure TDBISAMTableExt.DoOnNewRecord;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FieldCount - 1 do
    with Fields[I] do
     if (DefaultExpression <> '') and IsNull then
       AsString := DefaultExpression;
end;

procedure TDBISAMTableExt.InternalOpen;
begin
  inherited;
  if DefaultFields or (AutoFieldsProperties and (not FFieldsPropertiesUpdated)) then
  begin
    UpdateFieldsProperties;
    FFieldsPropertiesUpdated := True;
  end;
end;

procedure TDBISAMTableExt.InternalDelete;
var
  CommitFailed: Boolean;
  TableDef: TTableDefinition;
  ObjectKey: Variant;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt)
    and (UpdateOptions <> uoNone) then
  begin
    CommitFailed := False;
    with TDBISAMDatabaseExt(Database) do
    try
      StartTransactionCount;

      TableDef := ExecuteTriggers(TDBISAMDatabaseExt(Database), Self, ctDeleted, Self.UpdateOptions);
      if Assigned(TableDef)
        and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions)
        and (TableDef.ObjectKeyFields <> '')
      then
        ObjectKey := FieldValues[TableDef.ObjectKeyFields]
      else ObjectKey := NULL;

      inherited InternalDelete;

      if Assigned(TableDef) and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions) then
        ExecuteTableTriggers(TDBISAMDatabaseExt(Database), ObjectKey, ctDeleted, ttAfter, TableDef);

      CommitFailed := True;
      Commit;
    except
      Rollback;
      if CommitFailed then Refresh;
      raise;
    end
  end else
    inherited InternalDelete;
end;

procedure TDBISAMTableExt.InternalPost;
var
  CommitFailed: Boolean;
  ChangeType: TChangeType;
  TableDef: TTableDefinition;
  UpdOpt: TUpdateOptions;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt) then
  begin
    CommitFailed := False;
    with TDBISAMDatabaseExt(Database) do
    try
      StartTransactionCount;

      if State = dsInsert then
        ChangeType := ctInserted
      else ChangeType := ctModified;

      TableDef := ExecuteTriggers(TDBISAMDatabaseExt(Database), Self, ChangeType, UpdateOptions);
      inherited InternalPost;
      UpdOpt := Self.UpdateOptions * UpdateOptions;
      if uoEnableTriggers in UpdOpt then
        ExecuteTableTriggers(TDBISAMDatabaseExt(Database), Self, ChangeType, ttAfter, TableDef);
      if (ChangeType = ctInserted) and
        ((uoEnableChangeTracking in UpdOpt)
        or (uoEnableTriggers in UpdOpt))
      then
        ObjectChanged(TDBISAMDatabaseExt(Database), Self, ctInserted, TableDef);

      CommitFailed := True;
      Commit;
    except
      Rollback;
      if CommitFailed then begin
        SetState(dsBrowse);
        Edit;
      end;
      raise;
    end
  end else
    inherited InternalPost;
end;

procedure TDBISAMTableExt.OpenCursor(InfoQuery: Boolean);
begin
  if FTemporary and (not Exists) then
  begin
    if FieldDefs.Count = 0 then
    begin
      AssignFieldDefsFromFields;
      FClearFieldDefsOnClose := True;
    end else
      FClearFieldDefsOnClose := False;
    CreateTable;
  end;
  inherited OpenCursor(InfoQuery);
end;

procedure TDBISAMTableExt.SetAutoFieldsProperties(const Value: Boolean);
begin
  if FAutoFieldsProperties <> Value then
  begin
    FAutoFieldsProperties := Value;
    FFieldsPropertiesUpdated := False;
    if {(Active or (FieldCount > 0)) and } FAutoFieldsProperties then
      UpdateFieldsProperties;
  end;
end;

procedure TDBISAMTableExt.UpdateFieldsProperties;
begin
  dbExtUtils.UpdateFieldsProperties(Self, Database);
end;

{ TDBISAMQueryExt }

constructor TDBISAMQueryExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowAutoOpen := False;
  FOptimizedLookups := False;
  FAutoFieldsProperties := False;
  FFieldsPropertiesUpdated := False;
  FUpdateOptions := uoAll;
  FSQLPattern := TStringList.Create;
  TStringList(FSQLPattern).OnChange := PatternChanged;
  FMacros := TMacros.Create(Self, TMacro);
  FMacroBegin := '<%';
  FMacroEnd := '%>';
  FAllowMacros := False;
end;

destructor TDBISAMQueryExt.Destroy;
begin
  inherited Destroy;
  FMacros.Free;
  FSQLPattern.Free;
end;

procedure TDBISAMQueryExt.CalculateFields(Buffer: TRecordBuffer);
begin
  if OptimizedLookups and (State <> dsInternalCalc) then
  begin
    SetPrivateCalcBuffer(Self, Buffer);
    ClearCalcFields(CalcBuffer);
    CalculateFieldsExt(Buffer, Self);
    DoOnCalcFields;
  end else
    inherited CalculateFields(Buffer);
end;

procedure TDBISAMQueryExt.CheckActive;
begin
  if FAllowAutoOpen and (State = dsInactive) then
    Active := True;
  inherited CheckActive;
end;

procedure TDBISAMQueryExt.DataEvent(Event: TDataEvent; Info: Integer);
begin
  if Event = dePropertyChange then
    FFieldsPropertiesUpdated := False;
  inherited;
end;

procedure TDBISAMQueryExt.DoAfterOpen;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt) then
    TDBISAMDatabaseExt(Database).DoDataSetAfterOpen(Self);
  inherited;
end;

procedure TDBISAMQueryExt.DoBeforeClose;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt) then
    TDBISAMDatabaseExt(Database).DoDataSetBeforeClose(Self);
  inherited;
end;

procedure TDBISAMQueryExt.DoOnNewRecord;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FieldCount - 1 do
    with Fields[I] do
     if (DefaultExpression <> '') and IsNull then
       AsString := DefaultExpression;
end;

procedure TDBISAMQueryExt.DoBeforeOpen;
begin
  if FAllowMacros then
    ExpandSQL(inherited SQL);
  inherited;
end;

procedure TDBISAMQueryExt.InternalOpen;
begin
  inherited;
  if DefaultFields or (AutoFieldsProperties and (not FFieldsPropertiesUpdated)) then
  begin
    UpdateFieldsProperties;
    FFieldsPropertiesUpdated := True;
  end;
end;

procedure TDBISAMQueryExt.InternalDelete;
var
  CommitFailed: Boolean;
  TableDef: TTableDefinition;
  ObjectKey: Variant;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt) then
  begin
    CommitFailed := False;
    with TDBISAMDatabaseExt(Database) do
    try
      StartTransactionCount;

      TableDef := ExecuteTriggers(TDBISAMDatabaseExt(Database), Self, ctDeleted, Self.UpdateOptions);
      if Assigned(TableDef)
        and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions)
        and (TableDef.ObjectKeyFields <> '')
      then
        ObjectKey := FieldValues[TableDef.ObjectKeyFields]
      else ObjectKey := NULL;

      inherited InternalDelete;

      if Assigned(TableDef) and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions) then
        ExecuteTableTriggers(TDBISAMDatabaseExt(Database), ObjectKey, ctDeleted, ttAfter, TableDef);

      CommitFailed := True;
      Commit;
    except
      Rollback;
      if CommitFailed then Refresh;
      raise;
    end
  end else
    inherited InternalDelete;
end;

procedure TDBISAMQueryExt.InternalPost;
var
  ChangeType: TChangeType;
  CommitFailed: Boolean;
  TableDef: TTableDefinition;
  UpdOpt: TUpdateOptions;
begin
  if (Database <> nil) and Database.InheritsFrom(TDBISAMDatabaseExt) then
  begin
    CommitFailed := False;
    with TDBISAMDatabaseExt(Database) do
    try
      StartTransactionCount;

      if State = dsInsert then
        ChangeType := ctInserted
      else ChangeType := ctModified;
      TableDef := ExecuteTriggers(TDBISAMDatabaseExt(Database), Self, ChangeType, UpdateOptions);
      inherited InternalPost;
      UpdOpt := Self.UpdateOptions * UpdateOptions;
      if uoEnableTriggers in UpdOpt then
        ExecuteTableTriggers(TDBISAMDatabaseExt(Database), Self, ChangeType, ttAfter, TableDef);
      // Track object changes *after* post for insert operations to support auto-calculated fields
      if (ChangeType = ctInserted) and
        ((uoEnableChangeTracking in UpdOpt)
        or (uoEnableTriggers in UpdOpt))
      then
        ObjectChanged(TDBISAMDatabaseExt(Database), Self, ctInserted, TableDef);

      CommitFailed := True;
      Commit;
    except
      Rollback;
      if CommitFailed then begin
        SetState(dsBrowse);
        Edit;
      end;
      raise;
    end
  end else
    inherited InternalPost;
end;

procedure TDBISAMQueryExt.SetAutoFieldsProperties(const Value: Boolean);
begin
  if FAutoFieldsProperties <> Value then
  begin
    FAutoFieldsProperties := Value;
    FFieldsPropertiesUpdated := False;
    if {(Active or (FieldCount > 0)) and } FAutoFieldsProperties then
      UpdateFieldsProperties;
  end;
end;

procedure TDBISAMQueryExt.UpdateFieldsProperties;
begin
  dbExtUtils.UpdateFieldsProperties(Self, Database);
end;

procedure TDBISAMQueryExt.SetSQL(const Value: TStrings);
begin
  FSQLPattern.BeginUpdate;
  try
    FSQLPattern.Assign(Value);
  finally
    FSQLPattern.EndUpdate;
  end;
end;

function TDBISAMQueryExt.GetMacros: TMacros;
begin
  Result := FMacros;
end;

procedure TDBISAMQueryExt.SetMacros(Value: TMacros);
begin
  FMacros.Assign(Value);
end;

function TDBISAMQueryExt.MacroByName(const Value: string): TMacro;
begin
  Result := FMacros.Find(Value);
  if Result = nil then
    DatabaseErrorFmt(SMacroNotFound, [Value]);
end;

procedure TDBISAMQueryExt.PatternChanged(Sender: TObject);
begin
  if Active then Disconnect;
  RecreateMacros;
  ExpandSQL(inherited SQL);
end;

procedure TDBISAMQueryExt.SetMacroBegin(const Value: String);
begin
  if FMacroBegin <> Value then
  begin
    FMacroBegin := Value;
    RecreateMacros;
  end;
end;

procedure TDBISAMQueryExt.SetMacroEnd(const Value: String);
begin
  if FMacroEnd <> Value then
  begin
    FMacroEnd := Value;
    RecreateMacros;
  end;
end;

procedure TDBISAMQueryExt.ExpandSQL(SQL: TStrings);
begin
  SQL.BeginUpdate;
  try
    if (not FAllowMacros) or (FMacros.Count = 0) then
      SQL.Assign(FSQLPattern)
    else SQL.Text := FMacros.ExpandMacros(FSQLPattern.Text, MacroBegin, MacroEnd);
  finally
    SQL.EndUpdate;
  end;
end;

procedure TDBISAMQueryExt.RecreateMacros;
begin
  if not FAllowMacros then exit;
  FMacros.UpdateMacros(FSQLPattern.Text, MacroBegin, MacroEnd);
end;

procedure TDBISAMQueryExt.ExecSQL;
begin
  if FAllowMacros then
    ExpandSQL(inherited SQL);
  inherited ExecSQL;
end;

procedure TDBISAMQueryExt.SetAllowMacros(const Value: Boolean);
begin
  if FAllowMacros <> Value then
  begin
    FAllowMacros := Value;
    RecreateMacros;
  end;
end;


end.
