unit CtxNX3Adapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, nxDBExt, dbSchema, nxsrSqlEngineBase,
  nxDB, nxsrServerEngine, nxsqlEngine, CtxDBDesignerAdapter, dmMain;

type
  TCtxNX3Adapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
  protected
    FDatabases: TObjectList;
    FSessions: TObjectList;
    FNxServer: TNxServer;
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

uses nxOpenDatabase, dbExtUtils;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxNX3Adapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxNX3Adapter }

constructor TCtxNX3Adapter.Create;
begin
  FDatabases := TObjectList.Create(False);
  // FSessions := TObjectList.Create(False);
  FNxServer := TNxServer.Create(nil);
end;

destructor TCtxNX3Adapter.Destroy;
begin
  FNxServer.Free;
  FDatabases.Free;
  // FSessions.Free;
  inherited;
end;

procedure TCtxNX3Adapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TnxDatabaseExt(DatabaseID).Connected := False;
    TnxDatabaseExt(DatabaseID).Session.Active := False;
    TnxDatabaseExt(DatabaseID).Session.Free;
    TnxDatabaseExt(DatabaseID).Free;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxNX3Adapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxNX3Adapter.GetDriverName: OleVariant;
begin
  Result := 'Nexus 3';
end;

function TCtxNX3Adapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TnxDatabaseExt;
  Session: TnxSession;
  ParamList: TStringList;
begin
  Session := TnxSession.Create(FNxServer);
  Database := TnxDatabaseExt.Create(FNxServer);
  try
    Database.Schema := TDatabaseSchema.Create(Database);
    // Session.ServerEngine := FNxServer.nxServerEngine;
    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      Session.UserName := ParamList.Values['User Name'];
      Session.Password := ParamList.Values['Password'];
    finally
      ParamList.Free;
    end;
    // Database.LoginPrompt := False;
    Database.Session := Session;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;
    Database.Connected := True;
    FDatabases.Add(Database);
    // FSessions.Add(Session);
    Result := Integer(Database);
  except
    Session.Free;
    Database.Free;
    raise;
  end;
end;

function TCtxNX3Adapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
begin
  with TnxOpenDatabaseDialog.Create(nil) do
  try
    nxSession := TnxSession.Create(FNxServer);
    try
      DatabaseURL := ConnectionStr;
      Result := Execute;
      if Result then
        ConnectionStr := DatabaseURL;
    finally
      nxSession.Free;
    end;
  finally
    Free;
  end;
end;

function TCtxNX3Adapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User Name=","Password="';
end;

end.
