(******************************************************************************)
(*
(*  Context Database Extensions Suite (nx)
(*
(*  Contains: TnxDatabaseExt, TnxQueryExt, TnxTableExt components.
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit nxDBExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, nxdb, nxsdTypes, 
  dbExtParser, dbExtUtils, CtxDBIntf, dbSchema,
  CtxDataTypes, CtxDataSetCommand;

type
  TnxDatabaseExt = class;
  TnxTableExt = class;
  TnxQueryExt = class;

  {:$ TDatabaseOperationProgress type is used by the database's OnProgress event. }
  TDatabaseOperationProgress = procedure (Sender: TnxDatabaseExt; const Operation: String; PercentDone: Byte; var Abort: Boolean) of object;

  {:$ TnxRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TnxRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TnxDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TnxDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;
  
  {:$ Represents a nx database connection. }
  {:: TnxDatabaseExt extends the functionality of TnxDatabase with the }
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
  {:! Unlike TnxDatabase, TnxDatabaseExt requires explicit declaration }
  {:! for every database connection that require one or more of the above }
  {:! described features. If you don't declare TnxDatabaseExt explicitly }
  {:! the engine will automatically create TnxDatabase component instead.<br> }
  {:! The extended features of TnxDatabaseExt are only accessible if }
  {:! TnxDatabaseExt used in combination with TDatabaseSchema, TnxTableExt }
  {:! and TnxQueryExt. }
  TnxDatabaseExt = class(TnxDatabase, IDatabaseExt, ISchemaDatabase)
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
    function FindTable(const TableName: String): TnxTableExt;
      { Attempts to locate table in cache }
    procedure RemoveFromCache(DataSet: TDataSet);
      { Removes DataSet from Chache - called AfterClose}
    procedure DoQueryProgress(Sender: TObject; PercentDone: Word; var AbortQuery: Boolean);
      { Invokes OnProgress event if assigned with Operation = 'Executing Query...' }
    procedure FreeTableFromCache(const TableName: string);
  public
    {:: Creates an instance of a TnxDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TnxDatabaseExt component.}
    destructor Destroy; override;

    {:: Invokes OnProgress event handler for the TnxDatabaseExt component. }
    procedure DoProgress(const Operation: String; PercentDone: Byte; var Abort: Boolean);

    {:: Creates TnxTableExt component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TnxQueryExt component, corresponding to Statement. }
    function CreateQuery(const Statement: String): TDataSet;

    {:$ Returns TnxTableExt component for a specified table. }
    {:: This method utilizes internal cache, allocated within TnxDatabaseExt }
    {:: component, to avoid frequently opening and closing tables.<br> }
    {:: If a table component for the requested tablename already exists in the }
    {:: database cache, this method will return an existing component. Otherwise }
    {:: it will create a new TnxTableExt component, assign Database, Session }
    {:: and TableName properties and optionally open the table.<br> }
    {:: CancelRange parameter specifies whether ot not this method should Cancel }
    {:: any existing range that might be set on the table. }
    {:! The caller is responsible for destroying created component. Even if Open }
    {:! parameter is False, this method might still return an open table. However, }
    {:! if Open parameter is true the table is guaranteed to be open. }
    function OpenTable(const TableName: String; Open: Boolean = False; CancelRange: Boolean = True): TnxTableExt; virtual;

    {:$ Places table in the database cache if it's opened. }
    {:: The database then will be responsible for destroying this Table component. }
    {:: It will also be able to return this Table later if requested through the }
    {:: CreateTable method }
    procedure ReleaseTable(Table: TnxTableExt);

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
    procedure StartTransaction;

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


    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);
    
    function FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;

    function AddVirtualKey(const TableName, KeyFields: String; KeyValues: Variant; CaseInsensitive: Boolean = False): Integer;
    function VirtualKeyExists(const VirtualKey: String): Boolean;
    procedure DeleteVirtualKey(VirtualKeyID: Integer);

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
    function GetDriverName: String;

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
    {:$ Maximum number of open table components (TnxTableExt) that can }
    {:$ be stored within an instance of the database component. }
    property MaxCachedTables: Integer read FMaxCachedTables write FMaxCachedTables;
    {:$ Specifies the uniform path to the database for both local and C/S types of access. }
    {:: This parameter is usefull for displaying or storing database and session parameters }
    {:: for the specific database. It may also be used with TnxOpenDatabase dialog.<br> }
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

  {:$ Use TnxTableExt to access data in a single database table using nx. }
  {:: TnxTableExt provides access to extended functionality like referential }
  {:: integrity constraints, replications ans such and should be used in conjunction }
  {:: with TnxDatabaseExt. }
  TnxTableExt = class(TnxTable)
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
    procedure CalculateFields(Buffer: PChar); override;

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
    {:: Creates an instance of TnxTableExt component. }
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

  {:$ Use TnxQueryExt to access one or more tables in a database using SQL statements. }
  {:: TnxQueryExt provides access to extended functionality like referential }
  {:: integrity constraints, replications ans such and should be used in conjunction }
  {:: with TnxDatabaseExt. }
  TnxQueryExt = class(TnxQuery)
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
    procedure CalculateFields(Buffer: PChar); override;

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
    function ExpandMacro(const MacroName: String): String;
    function AddMacro(const MacroName: String): String;
  public
    { Public declarations }
    {:: Creates an instance of TnxQueryExt component. }
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
    { IProviderSupport }
    (*
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer = nil): Integer; override;
      *)
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

  {:$ Retrieves database URL from the TnxDatabase component and the connected Session component. }
  function GetDatabaseURL(nxDatabase: TnxDatabase): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(nxDatabase: TnxDatabase; DatabaseURL: String);

implementation

uses
{$IFDEF VER130}
  FileCtrl,
{$ENDIF}
  DbConsts, ActiveX, TypInfo, nxsdServerEngine, nxreRemoteServerEngine,
  nxllTransport, nxtwWinsockTransport, nxtnNamedPipeTransport, nxtcCOMTransport,
  nxsdDataDictionary, nxsrServerEngine;

const
  idxByReplicationID = 'byReplicationID';

{ General Helper Rountines }


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

function GetDatabaseURL(nxDatabase: TnxDatabase): String;
var
  DatabaseName: string;
  Engine: TnxBaseServerEngine;
  Transport: TnxBaseTransport;
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
  if Engine is TnxRemoteServerEngine then begin
    Transport := TnxRemoteServerEngine(Engine).Transport;
    if Transport = nil then
      exit;
    Result := EncodeDatabaseURL(GetProtocolName(Transport),
      Transport.ServerName, DatabaseName);
  end
  else
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
begin
  result := TnxRemoteServerEngine.Create(AOwner);
  result.Transport := GetTransport(AOwner, ConnectionType);
  if result.Transport <> nil then
    result.Transport.ServerName := RemoteHost;
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

procedure SetDatabaseURL(nxDatabase: TnxDatabase; DatabaseURL: String);
var
  ConnectionType: string;
  RemoteHost: string;
  DatabaseName: string;
  Engine: TnxBaseServerEngine;
  Transport: TnxBaseTransport;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  if (Pos(':', DatabaseName) = 0) and (Pos('\', DatabaseName) = 0) then
    nxDatabase.AliasName := DatabaseName
  else
    nxDatabase.AliasPath := DatabaseName;
  if (nxDatabase.Session = nil) or not(nxDatabase.Session is TnxSession) then
    exit;
  Engine := nxDatabase.Session.ServerEngine;
  if ConnectionType = '' then begin
    TnxSession(nxDatabase.Session).ServerEngine := GetServerEngine(nxDatabase.Owner);
  end
  else begin
    if (Engine = nil) or not (Engine is TnxRemoteServerEngine) then begin
      TnxSession(nxDatabase.Session).ServerEngine := GetRemoteServerEngine(nxDatabase, ConnectionType, RemoteHost);
      exit;
    end;
    Transport := CheckTransportType(nxDatabase, TnxRemoteServerEngine(Engine), ConnectionType);
    Transport.ServerName := RemoteHost;
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
  IdxDef: TIndexDef;
  RangeFilter: String;
  Table: TnxTableExt;
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

destructor TnxRangeCursor.Destroy;
var
  Database: TnxDatabase;
begin
  KeyFieldsList.Free;
  if DataSet <> nil then
  begin
    Database := TnxDatabase(TnxTableExt(DataSet).Database);
    if (Database <> nil) and (Database.InheritsFrom(TnxDatabaseExt)) then
      TnxDatabaseExt(Database).ReleaseTable(TnxTableExt(DataSet));
  end;
  inherited Destroy;
end;

{ TnxDatabaseExt }

constructor TnxDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := 'System';
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

destructor TnxDatabaseExt.Destroy;
begin
  DBDatabases.Remove(Self);
  SystemTableName := '';
  ObjectsTableName := '';
  FVirtualKeys.Free;
  if FCachedTables <> nil then
    FreeAndNil(FCachedTables);
  inherited Destroy;
end;

procedure TnxDatabaseExt.CheckActive;
begin
  if not Active then
    DatabaseError(SDatabaseClosed);
end;

procedure TnxDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TnxDatabaseExt.OpenTable(const TableName: String;
  Open: Boolean = False; CancelRange: Boolean = True): TnxTableExt;
begin
  Result := FindTable(TableName);
  if Result = nil then
  begin
    Result := TnxTableExt.Create(nil);
    Result.Database := Self;
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

function TnxDatabaseExt.FindTable(const TableName: String): TnxTableExt;
var
  I: Integer;
begin
  Result := nil;
  if FCachedTables = nil then exit;
  for I := FCachedTables.Count - 1 downto 0 do
  begin
    Result := TnxTableExt(FCachedTables[I]);
    if CompareText(Result.TableName, TableName) = 0 then
    begin
      FCachedTables.Delete(I); // remove from list
      exit;
    end;
  end;
  Result := nil;
end;

procedure TnxDatabaseExt.ReleaseTable(Table: TnxTableExt);
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
        TnxTableExt(FCachedTables[0]).Free;
    end else
      Table.Free;
  end;
end;

procedure TnxDatabaseExt.ClearTableCache;
begin
  FreeObjects(FCachedTables);
  FCachedTables.Count := 0;
end;

procedure TnxDatabaseExt.RemoveFromCache(DataSet: TDataSet);
begin
  if FCachedTables <> nil then
    FCachedTables.Remove(DataSet);
end;

procedure TnxDatabaseExt.FreeTableFromCache(const TableName: string);
var
  Table: TnxTable;
begin
  Table := FindTable(TableName);
  RemoveFromCache(Table);
  Table.Free;
end;

procedure TnxDatabaseExt.DoDataSetAfterOpen(DataSet: TDataSet);
begin
  if Assigned(FDataSetAfterOpen) then
    FDataSetAfterOpen(DataSet);
end;

procedure TnxDatabaseExt.DoDataSetBeforeClose(DataSet: TDataSet);
begin
  if Assigned(FDataSetBeforeClose) then
    FDataSetBeforeClose(DataSet);
end;

procedure TnxDatabaseExt.DoProgress(const Operation: String;
  PercentDone: Byte; var Abort: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Operation, PercentDone, Abort);
end;

procedure TnxDatabaseExt.DoQueryProgress(Sender: TObject;
  PercentDone: Word; var AbortQuery: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, sExecutingQuery, PercentDone, AbortQuery);
end;

procedure TnxDatabaseExt.ExecuteUpdate(
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

function TnxDatabaseExt.GetMajorVersion: Integer;
begin
  Result := -1;
  if not Connected then exit;
  Result := Version.Major; // This will force it to re-read the version
end;

function TnxDatabaseExt.GetMinorVersion: Integer;
begin
  Result := -1;
  if not Connected then exit;
  Result := Version.Minor; // This will force it to re-read the version
end;

function TnxDatabaseExt.GetReplicationID: Integer;
begin
  Result := 0;
  if CheckSystemTable then begin
    SystemTable.DataSet.Refresh;
    Result := SystemTable.ReplicationID.AsInteger
  end;
end;

function TnxDatabaseExt.GetSnapshotID: Integer;
begin
  Result := 0;
  if CheckSystemTable then begin
    SystemTable.Refresh;
    Result := SystemTable.SnapshotID.AsInteger;
  end;
end;

function TnxDatabaseExt.GetVersion: TSchemaVersion;
begin
  Result := SchemaVersion(-1,-1);
  if not Connected then exit;
  if CheckSystemTable then
    Result := SchemaVersion(SystemTable.MajorVersion.AsInteger, SystemTable.MinorVersion.AsInteger);
end;

function TnxDatabaseExt.GetVersionLabel: String;
begin
  Result := VersionToStr(GetVersion);
end;

function TnxDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, Self.Version, AllowNewer);
end;

procedure TnxDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FSchema then
      Schema := nil
    else if AComponent.InheritsFrom(TnxTableExt) then
      RemoveFromCache(TnxTableExt(AComponent));
  end;
  inherited;
end;


procedure TnxDatabaseExt.SetObjectsTableName(const Value: String);
begin
  if FObjectsTableName <> Value then
  begin
    FObjectsTableName := Value;
    if FObjectsTable <> nil then
      DestroyObjectsTable;
  end;
end;

procedure TnxDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
  GetVersionLabel; // Force it to update version property if connected
end;

procedure TnxDatabaseExt.SetSnapshotID(Value: Integer);
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

procedure TnxDatabaseExt.SetReplicationID(Value: Integer);
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

procedure TnxDatabaseExt.SetSystemTableName(const Value: String);
begin
  if FSystemTableName <> Value then
  begin
    FSystemTableName := Value;
    // Close system table if it's opened
    if FSystemTable <> nil then
      DestroySystemTable;
  end;
end;

procedure TnxDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  dbExtUtils.SetVersion(Self, Value);
end;

procedure TnxDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

function TnxDatabaseExt.UpdateDatabase: Boolean;
begin
  Result := dbSchema.UpdateDatabase(Self);
end;

function TnxDatabaseExt.CheckObjectsTable(CreateIfNotExist: Boolean = False): Boolean;
begin
  CheckActive;
  // Make sure, that objects table exists. Open or create it and map fields
  if FObjectsTable = nil then
  begin
    FObjectsTable := TObjectsTable.Create(CreateTable(FObjectsTableName), True);
    try
      with TnxTableExt(FObjectsTable.DataSet) do
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
  with TnxTableExt(FObjectsTable.DataSet) do
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

function TnxDatabaseExt.CheckSystemTable(CreateIfNotExist: Boolean = False): Boolean;
begin
  CheckActive;
  // Make sure, that system table exists. Open or create it and map fields
  if SystemTable = nil then
  begin
    FSystemTable := TSystemTable.Create(CreateTable(FSystemTableName), True);
    try
      TnxTableExt(FSystemTable.DataSet).UpdateOptions := uoNone;
      FSystemTable.CreatePersistentFields;
      FSystemTable.SchemaName.Size := 127;
    except
      DestroySystemTable;
    end;
  end;
  if (FSystemTable <> nil) and (not FSystemTable.DataSet.Active) then
  with TnxTableExt(FSystemTable.DataSet) do
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

procedure TnxDatabaseExt.DestroyObjectsTable;
begin
  FreeAndNil(FObjectsTable);
end;

procedure TnxDatabaseExt.DestroySystemTable;
begin
  FreeAndNil(FSystemTable);
end;

procedure TnxDatabaseExt.LockDatabase;
begin
  if FDatabaseLockCounter = 0 then
  begin
    if not CheckSystemTable(True) then
      DatabaseError(sUnableToLockDatabase);
    SystemTable.DataSet.Active := False;
    TnxTableExt(SystemTable.DataSet).Exclusive := True;
    try
      SystemTable.DataSet.Active := True;
    except
      DatabaseError(sUnableToLockDatabase);
    end;
  end;
  Inc(FDatabaseLockCounter);
end;

procedure TnxDatabaseExt.UnLockDatabase;
begin
  Dec(FDatabaseLockCounter);
  if (FDatabaseLockCounter = 0) and (SystemTable <> nil) then
  begin
    SystemTable.DataSet.Active := False;
    TnxTableExt(SystemTable.DataSet).Exclusive := False;
  end;
end;

procedure TnxDatabaseExt.StartTransactionCount;
begin
  if (FInTransactionCount = 0) and InTransaction then
    FInTransactionCount := 1; // Account for a transaction that has already started
  Inc(FInTransactionCount);
end;

procedure TnxDatabaseExt.StartTransaction;
begin
  // Check system tables
  if Replicating or (uoEnableChangeTracking in UpdateOptions) then
  begin
    if not CheckSystemTable(True) then
      DatabaseError(SSystemTableNotFound);
    if not CheckObjectsTable(True) then
      DatabaseError(SObjectsTableNotFound);
  end;
  inherited StartTransaction;
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

procedure TnxDatabaseExt.Commit;
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

procedure TnxDatabaseExt.Rollback;
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

procedure TnxDatabaseExt.RefreshTables;
var
  I: Integer;
begin
  for I := 0 to DataSetCount - 1 do
    if DataSets[I].State = dsBrowse then
      DataSets[I].Refresh;
end;

function TnxDatabaseExt.GetEngineName: String;
begin
  Result := 'nx';
end;

function TnxDatabaseExt.GetDatabaseURL: String;
begin
  Result := nxDBExt.GetDatabaseURL(Self);
end;

procedure TnxDatabaseExt.SetDatabaseURL(const Value: String);
begin
  nxDBExt.SetDatabaseURL(Self, Value);
end;

procedure TnxDatabaseExt.SetReplicating(Value: Boolean);
begin
  FReplicating := Value;
  if FReplicating then
    FEnabledTriggers := [taAlways, taOnlyWhenReplicating]
  else FEnabledTriggers := [taAlways, taExceptWhenReplicating];
end;

procedure TnxDatabaseExt.BeginReplicating;
begin
  SetReplicating(True);
  FSaveUpdateOptions := UpdateOptions;
  UpdateOptions := [uoEnableTriggers, uoEnableAggregates]; // uoNone;
end;

procedure TnxDatabaseExt.EndReplicating;
begin
  SetReplicating(False);
  UpdateOptions := FSaveUpdateOptions;
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

function TnxDatabaseExt.GetActiveTransaction: TActiveTransaction;
begin
  Result := FActiveTransaction;
end;

function TnxDatabaseExt.GetObjectsTable: TObjectsTable;
begin
  Result := FObjectsTable;
end;

function TnxDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TnxDatabaseExt.GetSystemTable: TSystemTable;
begin
  Result := FSystemTable;
end;

function TnxDatabaseExt.GetDatabaseName: String;
begin
  Result := inherited DisplayName;
end;

procedure TnxDatabaseExt.SetDatabaseName(const Value: String);
begin
  inherited DisplayName := Value;
  if DatabaseName <> '' then
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
  Result := OpenTable(TableName);
end;

function TnxDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TnxQueryExt.Create(nil);
  with TnxQueryExt(Result) do
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

function TnxDatabaseExt.GetUserName: String;
begin
  Result := FUserName;
end;

procedure TnxDatabaseExt.SetUserName(const Value: String);
begin
  FUserName := Value;
end;

function TnxDatabaseExt.GetObjectsTableName: String;
begin
  Result := FObjectsTableName;
end;

function TnxDatabaseExt.GetSystemTableName: String;
begin
  Result := FSystemTableName;
end;

function TnxDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

{ DDL functions: CreateDatabase, ReverseEngineering, RestructureFromSchema }

procedure TnxDatabaseExt.CreateNewDatabase(OverwriteTables: Boolean = True; CreateSystemTable: Boolean = True; CreateObjectsTable: Boolean = True);
var
  I, J: Integer;
  Counter: TProgressCounter;
  Table: TnxTableExt;
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

        for J := 0 to Table.IndexDefs.Count - 1 do
          if Table.IndexDefs[J].Name = '' then
            Table.IndexDefs[J].Name := 'PrimaryIndex';

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

procedure TnxDatabaseExt.ReverseEngineer;
var
  I, J: Integer;
  Table: TnxTableExt;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;

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

      if (AnsiCompareText(LogicalTableName, FSystemTableName) = 0)
        or (AnsiCompareText(LogicalTableName, FObjectsTableName) = 0)
      then continue;

      Table.TableName := Tables[I];
      Table.FieldDefs.Update;
      Table.IndexDefs.Update;

      // Create table defs
      with Schema.TableDefs.Add do
      begin
        TableName := LogicalTableName;
        FieldDefs.Assign(Table.FieldDefs);
        IndexDefs.Assign(Table.IndexDefs);

        // Drop invalid indexes (including sequential access index)
        J := 0;
        while J < IndexDefs.Count do
          if IndexDefs[J].Fields = '' then
            IndexDefs[J].Free
          else Inc(J);
      end;
    end;
  finally
    Tables.Free;
    Table.Free;
  end;
end;

function TnxDatabaseExt.AddVirtualKey(const TableName, KeyFields: String;
  KeyValues: Variant; CaseInsensitive: Boolean): Integer;
begin
  Result := FVirtualKeys.Add(GetVirtualKey(TableName, KeyFields, KeyValues, CaseInsensitive));
end;

procedure TnxDatabaseExt.DeleteVirtualKey(VirtualKeyID: Integer);
begin
  FVirtualKeys.Delete(VirtualKeyID);
end;

function TnxDatabaseExt.VirtualKeyExists(const VirtualKey: String): Boolean;
begin
  Result := dbSchema.VirtualKeyExists(FVirtualKeys, VirtualKey);
end;

function TnxDatabaseExt.GetReplicating: Boolean;
begin
  Result := FReplicating;
end;

function TnxDatabaseExt.FindKey(Table: TDataSet;
  const KeyValues: array of const): Boolean;
begin
  Result := (Table as TnxTable).FindKey(KeyValues);
end;

function TnxDatabaseExt.GetEnabledTriggers: TSelectTriggers;
begin
  Result := FEnabledTriggers;
end;

function TnxDatabaseExt.GetUpdateOptions: TUpdateOptions;
begin
  Result := FUpdateOptions;
end;

function TnxDatabaseExt.GetConnected: Boolean;
begin
  Result := Connected;
end;

procedure TnxDatabaseExt.SetConnected(Value: Boolean);
begin
  Connected := Value;
end;

function TnxDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TnxTable).IndexDefs;
end;

procedure TnxDatabaseExt.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin
  inherited GetTableNames(List);
end;

function TnxDatabaseExt.GetDriverName: String;
begin
  Result := 'Nexus';
end;

function TnxDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

{ TnxTableExt }

procedure TnxTableExt.AssignFieldDefsFromFields(Source: TDataSet = nil);
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

procedure TnxTableExt.CalculateFields(Buffer: PChar);
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

procedure TnxTableExt.CheckActive;
begin
  if FAllowAutoOpen and (State = dsInactive) then
    Active := True;
  inherited CheckActive;
end;

procedure TnxTableExt.CloseCursor;
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

constructor TnxTableExt.Create(AOwner: TComponent);
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

procedure TnxTableExt.DataEvent(Event: TDataEvent; Info: Integer);
begin
  if Event = dePropertyChange then
    FFieldsPropertiesUpdated := False;
  inherited;
end;

procedure TnxTableExt.DoAfterOpen;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt) then
    TnxDatabaseExt(Database).DoDataSetAfterOpen(Self);
  inherited;
end;

procedure TnxTableExt.DoBeforeClose;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt) then
    TnxDatabaseExt(Database).DoDataSetBeforeClose(Self);
  inherited;
end;

procedure TnxTableExt.DoOnNewRecord;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FieldCount - 1 do
    with Fields[I] do
     if (DefaultExpression <> '') and IsNull then
       AsString := DefaultExpression;
end;

procedure TnxTableExt.InternalOpen;
begin
  inherited;
  if DefaultFields or (AutoFieldsProperties and (not FFieldsPropertiesUpdated)) then
  begin
    UpdateFieldsProperties;
    FFieldsPropertiesUpdated := True;
  end;
end;

procedure TnxTableExt.InternalDelete;
var
  CommitFailed: Boolean;
  TableDef: TTableDefinition;
  ObjectKey: Variant;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt)
    and (UpdateOptions <> uoNone) then
  begin
    CommitFailed := False;
    with TnxDatabaseExt(Database) do
    try
      StartTransactionCount;

      TableDef := ExecuteTriggers(TnxDatabaseExt(Database), Self, ctDeleted, Self.UpdateOptions);
      if Assigned(TableDef)
        and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions)
        and (TableDef.ObjectKeyFields <> '')
      then
        ObjectKey := FieldValues[TableDef.ObjectKeyFields]
      else ObjectKey := NULL;

      inherited InternalDelete;

      if Assigned(TableDef) and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions) then
        ExecuteTableTriggers(TnxDatabaseExt(Database), ObjectKey, ctDeleted, ttAfter, TableDef);

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

procedure TnxTableExt.InternalPost;
var
  CommitFailed: Boolean;
  ChangeType: TChangeType;
  TableDef: TTableDefinition;
  UpdOpt: TUpdateOptions;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt) then
  begin
    CommitFailed := False;
    with TnxDatabaseExt(Database) do
    try
      StartTransactionCount;

      if Self.State = dsInsert then
        ChangeType := ctInserted
      else ChangeType := ctModified;

      TableDef := ExecuteTriggers(TnxDatabaseExt(Database), Self, ChangeType, UpdateOptions);
      inherited InternalPost;
      UpdOpt := Self.UpdateOptions * UpdateOptions;
      if uoEnableTriggers in UpdOpt then
        ExecuteTableTriggers(TnxDatabaseExt(Database), Self, ChangeType, ttAfter, TableDef);
      if (ChangeType = ctInserted) and
        ((uoEnableChangeTracking in UpdOpt)
        or (uoEnableTriggers in UpdOpt))
      then
        ObjectChanged(TnxDatabaseExt(Database), Self, ctInserted, TableDef);

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

procedure TnxTableExt.OpenCursor(InfoQuery: Boolean);
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

procedure TnxTableExt.SetAutoFieldsProperties(const Value: Boolean);
begin
  if FAutoFieldsProperties <> Value then
  begin
    FAutoFieldsProperties := Value;
    FFieldsPropertiesUpdated := False;
    if {(Active or (FieldCount > 0)) and } FAutoFieldsProperties then
      UpdateFieldsProperties;
  end;
end;

procedure TnxTableExt.UpdateFieldsProperties;
begin
  dbExtUtils.UpdateFieldsProperties(Self, Database);
end;

{ TnxQueryExt }

constructor TnxQueryExt.Create(AOwner: TComponent);
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

destructor TnxQueryExt.Destroy;
begin
  inherited Destroy;
  FMacros.Free;
  FSQLPattern.Free;
end;

procedure TnxQueryExt.CalculateFields(Buffer: PChar);
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

procedure TnxQueryExt.CheckActive;
begin
  if FAllowAutoOpen and (State = dsInactive) then
    Active := True;
  inherited CheckActive;
end;

procedure TnxQueryExt.DataEvent(Event: TDataEvent; Info: Integer);
begin
  if Event = dePropertyChange then
    FFieldsPropertiesUpdated := False;
  inherited;
end;

procedure TnxQueryExt.DoAfterOpen;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt) then
    TnxDatabaseExt(Database).DoDataSetAfterOpen(Self);
  inherited;
end;

procedure TnxQueryExt.DoBeforeClose;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt) then
    TnxDatabaseExt(Database).DoDataSetBeforeClose(Self);
  inherited;
end;

procedure TnxQueryExt.DoOnNewRecord;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FieldCount - 1 do
    with Fields[I] do
     if (DefaultExpression <> '') and IsNull then
       AsString := DefaultExpression;
end;

procedure TnxQueryExt.DoBeforeOpen;
begin
  if FAllowMacros then
    ExpandSQL(inherited SQL);
  inherited;
end;

procedure TnxQueryExt.InternalOpen;
begin
  inherited;
  if DefaultFields or (AutoFieldsProperties and (not FFieldsPropertiesUpdated)) then
  begin
    UpdateFieldsProperties;
    FFieldsPropertiesUpdated := True;
  end;
end;

procedure TnxQueryExt.InternalDelete;
var
  CommitFailed: Boolean;
  TableDef: TTableDefinition;
  ObjectKey: Variant;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt) then
  begin
    CommitFailed := False;
    with TnxDatabaseExt(Database) do
    try
      StartTransactionCount;

      TableDef := ExecuteTriggers(TnxDatabaseExt(Database), Self, ctDeleted, Self.UpdateOptions);
      if Assigned(TableDef)
        and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions)
        and (TableDef.ObjectKeyFields <> '')
      then
        ObjectKey := FieldValues[TableDef.ObjectKeyFields]
      else ObjectKey := NULL;

      inherited InternalDelete;

      if Assigned(TableDef) and (uoEnableTriggers in Self.UpdateOptions * UpdateOptions) then
        ExecuteTableTriggers(TnxDatabaseExt(Database), ObjectKey, ctDeleted, ttAfter, TableDef);

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

procedure TnxQueryExt.InternalPost;
var
  ChangeType: TChangeType;
  CommitFailed: Boolean;
  TableDef: TTableDefinition;
  UpdOpt: TUpdateOptions;
begin
  if (Database <> nil) and Database.InheritsFrom(TnxDatabaseExt) then
  begin
    CommitFailed := False;
    with TnxDatabaseExt(Database) do
    try
      StartTransactionCount;

      if Self.State = dsInsert then
        ChangeType := ctInserted
      else ChangeType := ctModified;
      TableDef := ExecuteTriggers(TnxDatabaseExt(Database), Self, ChangeType, UpdateOptions);
      inherited InternalPost;
      UpdOpt := Self.UpdateOptions * UpdateOptions;
      if uoEnableTriggers in UpdOpt then
        ExecuteTableTriggers(TnxDatabaseExt(Database), Self, ChangeType, ttAfter, TableDef);
      // Track object changes *after* post for insert operations to support auto-calculated fields
      if (ChangeType = ctInserted) and
        ((uoEnableChangeTracking in UpdOpt)
        or (uoEnableTriggers in UpdOpt))
      then
        ObjectChanged(TnxDatabaseExt(Database), Self, ctInserted, TableDef);

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

procedure TnxQueryExt.SetAutoFieldsProperties(const Value: Boolean);
begin
  if FAutoFieldsProperties <> Value then
  begin
    FAutoFieldsProperties := Value;
    FFieldsPropertiesUpdated := False;
    if {(Active or (FieldCount > 0)) and } FAutoFieldsProperties then
      UpdateFieldsProperties;
  end;
end;

procedure TnxQueryExt.UpdateFieldsProperties;
begin
  dbExtUtils.UpdateFieldsProperties(Self, Database);
end;

procedure TnxQueryExt.SetSQL(const Value: TStrings);
begin
  FSQLPattern.BeginUpdate;
  try
    FSQLPattern.Assign(Value);
  finally
    FSQLPattern.EndUpdate;
  end;
end;

function TnxQueryExt.GetMacros: TMacros;
begin
  Result := FMacros;
end;

procedure TnxQueryExt.SetMacros(Value: TMacros);
begin
  FMacros.Assign(Value);
end;

function TnxQueryExt.MacroByName(const Value: string): TMacro;
begin
  Result := FMacros.Find(Value);
  if Result = nil then
    DatabaseErrorFmt(SMacroNotFound, [Value]);
end;

procedure TnxQueryExt.PatternChanged(Sender: TObject);
begin
  if Active then
  begin
    Close;
    UnPrepare;
  end;
  RecreateMacros;
  ExpandSQL(inherited SQL);
end;

procedure TnxQueryExt.SetMacroBegin(const Value: String);
begin
  if FMacroBegin <> Value then
  begin
    FMacroBegin := Value;
    RecreateMacros;
  end;
end;

procedure TnxQueryExt.SetMacroEnd(const Value: String);
begin
  if FMacroEnd <> Value then
  begin
    FMacroEnd := Value;
    RecreateMacros;
  end;
end;

function TnxQueryExt.AddMacro(const MacroName: String): String;
begin
  Result := '';
  if FMacros.Find(MacroName) = nil then
    TMacro(FMacros.Add).Name := MacroName;
end;

function TnxQueryExt.ExpandMacro(const MacroName: String): String;
var
  M: TMacro;
begin
  M := FMacros.Find(MacroName);
  if M <> nil then
    Result := M.Value
  else Result := '';
end;

procedure TnxQueryExt.ExpandSQL(SQL: TStrings);
begin
  SQL.BeginUpdate;
  try
    if (not FAllowMacros) or (FMacros.Count = 0) then
      SQL.Assign(FSQLPattern)
    else begin
      SQL.Text := FMacros.ExpandMacros(FSQLPattern.Text, MacroBegin, MacroEnd);
    end;
  finally
    SQL.EndUpdate;
  end;
end;

procedure TnxQueryExt.RecreateMacros;
begin
  if not FAllowMacros then exit;
  FMacros.UpdateMacros(FSQLPattern.Text, MacroBegin, MacroEnd);
end;

procedure TnxQueryExt.ExecSQL;
begin
  if FAllowMacros then
    ExpandSQL(inherited SQL);
  inherited ExecSQL;
end;

procedure TnxQueryExt.SetAllowMacros(const Value: Boolean);
begin
  if FAllowMacros <> Value then
  begin
    FAllowMacros := Value;
    RecreateMacros;
  end;
end;


end.
