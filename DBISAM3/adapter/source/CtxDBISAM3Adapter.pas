unit CtxDBISAM3Adapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, DBISAMExt, dbSchema,
  DBISAMTb, CtxDBDesignerAdapter;

type
  TCtxDBISAMAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
  protected
    FDatabases: TObjectList;
    FSessions: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDriverName: OleVariant; stdcall;
    function SelectDatabase(var ConnectionStr: OleVariant): Boolean; stdcall;
    function OpenDatabase(const DatabaseName, DriverName,
      ConnectionStr, Params: OleVariant): Integer; stdcall;
    procedure CloseDatabase(DatabaseID: Integer); stdcall;
    function ExecuteVerb(DatabaseID, Verb: Integer; var Data: OleVariant): Integer; stdcall;
    function GetParamNames(var Params: OleVariant): Integer; stdcall;
  end;

  procedure GetCtxDBAdapters(var Adapters: OleVariant);

implementation

uses Dbisamcn, DBISAMOpenDatabase, dbExtUtils;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';
  SInvalidDatabaseHandle = 'The database is not found or inactive';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxDBISAMAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxDBISAMAdapter }

constructor TCtxDBISAMAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
  FSessions := TObjectList.Create(True);
end;

destructor TCtxDBISAMAdapter.Destroy;
begin
  FDatabases.Free;
  FSessions.Free;
  inherited;
end;

procedure TCtxDBISAMAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TDBISAMDatabaseExt(DatabaseID).Connected := False;
    TDBISAMDatabaseExt(DatabaseID).Session.Active := False;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxDBISAMAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;


function TCtxDBISAMAdapter.GetDriverName: OleVariant;
begin
  Result := 'DBISAM 3'; // Session.EngineVersion
end;

function TCtxDBISAMAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TDBISAMDatabaseExt;
  Session: TDBISAMSession;
  ParamList: TStringList;
begin
  Session := TDBISAMSession.Create(nil);
  Database := TDBISAMDatabaseExt.Create(nil);
  try
    Database.Schema := TDatabaseSchema.Create(Database);
    Session.AutoSessionName := True;
    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      Session.RemoteUser := ParamList.Values['User Name'];
      Session.RemotePassword := ParamList.Values['Password'];
    finally
      ParamList.Free;
    end;
    Database.SessionName := Session.SessionName;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;
    Database.Connected := True;
    FDatabases.Add(Database);
    FSessions.Add(Session);
    Result := Integer(Database);
  except
    Session.Free;
    Database.Free;
    raise;
  end;
end;

function TCtxDBISAMAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
begin
  with TDBISAMOpenDatabaseDialog.Create(nil) do
  try
    DBISAMSession := TDBISAMSession.Create(nil);
    try
      DatabaseURL := ConnectionStr;
      Result := Execute;
      if Result then
        ConnectionStr := DatabaseURL;
    finally
      DBISAMSession.Free;
    end;
  finally
    Free;
  end;
end;

function TCtxDBISAMAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User Name=","Password="';
end;

end.
