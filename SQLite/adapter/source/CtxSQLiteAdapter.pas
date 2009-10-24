unit CtxSQLiteAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, dbSchema, CtxDBDesignerAdapter;

type
  TCtxSQLiteAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
  protected
    FDatabases: TObjectList;
    FSessions: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDriverName: OleVariant; stdcall;
    function SelectDatabase(var ConnectionStr: OleVariant): Boolean; stdcall;
    function OpenDatabase(const DatabaseName, DriverName, ConnectionStr, Params: OleVariant): Integer; stdcall;
    procedure CloseDatabase(DatabaseID: Integer); stdcall;
    function ExecuteVerb(DatabaseID, Verb: Integer; var Data: OleVariant): Integer; stdcall;
    function GetParamNames(var Params: OleVariant): Integer; stdcall;
  end;

  procedure GetCtxDBAdapters(var Adapters: OleVariant);

implementation

uses SQLiteOpenDatabase, dbExtUtils, SQLiteDBExt;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxSQLiteAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxSQLiteAdapter }

constructor TCtxSQLiteAdapter.Create;
begin
  FDatabases := TObjectList.Create(False);
end;

destructor TCtxSQLiteAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxSQLiteAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TSQLiteDatabaseExt(DatabaseID).Connected := False;
    TSQLiteDatabaseExt(DatabaseID).Free;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxSQLiteAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxSQLiteAdapter.GetDriverName: OleVariant;
begin
  Result := 'SQLite';
end;

function TCtxSQLiteAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TSQLiteDatabaseExt;
  ParamList: TStringList;
begin
  Database := TSQLiteDatabaseExt.Create(nil);
  try
    Database.Schema := TDatabaseSchema.Create(Database);
    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
    finally
      ParamList.Free;
    end;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;
    Database.Connected := True;
    FDatabases.Add(Database);
    Result := Integer(Database);
  except
    Database.Free;
    raise;
  end;
end;

function TCtxSQLiteAdapter.SelectDatabase(var ConnectionStr: OleVariant): Boolean;
begin
  with TSQLiteOpenDatabaseDialog.Create(nil) do
  try
    DatabaseURL := ConnectionStr;
    Result := Execute;
    if Result then
    begin
      //Connected := False;
      ConnectionStr := DatabaseURL;
    end;
  finally
    Free;
  end;
end;

function TCtxSQLiteAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"DriverDLL="';
end;

end.
