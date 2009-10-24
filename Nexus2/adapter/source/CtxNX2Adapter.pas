unit CtxNX2Adapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, nxDBExt, dbSchema, nxsdServerEngine, nxsrSqlEngineBase,
  nxDB, nxsrServerEngine, nxsqlEngine, CtxDBDesignerAdapter, dmMain;

type
  TCtxNX2Adapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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

uses nxOpenDatabase, dbExtUtils, forms;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';
  SErrorOpenDatabase = 'Unable to open connection to database: %s '+#13#10+ 'Error: %s';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxNX2Adapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxNX2Adapter }

constructor TCtxNX2Adapter.Create;
begin
  FDatabases := TObjectList.Create(False);
  // FSessions := TObjectList.Create(False);
  FNxServer := TNxServer.Create(nil);
end;

destructor TCtxNX2Adapter.Destroy;
begin
  FNxServer.Free;
  FDatabases.Free;
  // FSessions.Free;
  inherited;
end;

procedure TCtxNX2Adapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
  Eng: TnxBaseServerEngine;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    if TnxDatabaseExt(DatabaseID).InTransaction then
      TnxDatabaseExt(DatabaseID).Rollback;
    TnxDatabaseExt(DatabaseID).Connected := False;
    TnxDatabaseExt(DatabaseID).Session.Active := False;
    Eng := TnxDatabaseExt(DatabaseID).Session.ServerEngine;
    if (Eng <> nil) and (Eng.SessionCount = 0) then
      Eng.Active := False;
    TnxDatabaseExt(DatabaseID).Session.Free;
    TnxDatabaseExt(DatabaseID).Free;
    FDatabases.Delete(Idx);
  end;
end;
{
    TnxDatabaseExt(DatabaseID).Connected := False;
    TnxDatabaseExt(DatabaseID).Session.Active := False;
    TnxDatabaseExt(DatabaseID).Session.Free;
    TnxDatabaseExt(DatabaseID).Free;
    FDatabases.Delete(Idx);
}

function TCtxNX2Adapter.ExecuteVerb(DatabaseID, Verb: Integer; var Data: OleVariant): Integer;
var
  Idx: Integer;
  ResultSet: TDataSet;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  Result := 0;
  if (Idx < 0) or (DatabaseID = 0) then
    Result := -1
  else with TnxDatabaseExt(DatabaseID) do
  begin
    case Verb of
      // dvCreateDatabase:; not used currently
      dvReverseEngineer: begin
        ReverseEngineer;
        Data := StrToVarArray(Schema.SaveToStr);
      end;
      dvGetVersion: Data := VersionToStr(GetVersion);
      dvSetVersion: SetVersion(StrToVersion(Data));
      dvExecuteSQL: begin
        ResultSet := nil;
        try
          ExecuteStatement(VarToStr(Data), @ResultSet);
          if (ResultSet <> nil) and (ResultSet.FieldCount > 0) then
            Data := DataSetToVariant(ResultSet);
        finally
          FreeAndNil(ResultSet);
        end;
      end;
      else begin
        Result := -1;
      end;
    end;
  end;
  if Result <> 0 then
    Data := Format(SOperationNotSupported, [GetDriverName]);
end;

{
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;
}

function TCtxNX2Adapter.GetDriverName: OleVariant;
begin
  Result := 'Nexus 2';
end;

function TCtxNX2Adapter.OpenDatabase(const DatabaseName, DriverName,
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
    on E: Exception do
    begin
      Result := 0;
      Session.Free;
      Database.Free;
      E.Message := Format(SErrorOpenDatabase, [DatabaseName, E.Message]);
      Application.HandleException(E);
    end;
  end;
end;

function TCtxNX2Adapter.SelectDatabase(
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

function TCtxNX2Adapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User Name=","Password="';
end;

end.
