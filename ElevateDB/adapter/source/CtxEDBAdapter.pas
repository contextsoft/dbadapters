unit CtxEDBAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, EDBExt, dbSchema,
  EDBComps, CtxDBDesignerAdapter;

type
  TCtxEDBAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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

uses EDBOpenDatabase, dbExtUtils;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxEDBAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxEDBAdapter }

constructor TCtxEDBAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
  FSessions := TObjectList.Create(True);
end;

destructor TCtxEDBAdapter.Destroy;
begin
  FDatabases.Free;
  FSessions.Free;
  inherited;
end;

procedure TCtxEDBAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TEDBDatabaseExt(DatabaseID).Connected := False;
    TEDBDatabaseExt(DatabaseID).Session.Connected := False;
    FDatabases.Delete(Idx);
  end;
end;

{$IFDEF VER150}
function TCtxEDBAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;
{$ELSE}
function TCtxEDBAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
var
  Idx: Integer;
  ResultSet: TDataSet;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  Result := 0;
  if (Idx < 0) or (DatabaseID = 0) then
    Result := -1
  else with TEDBDatabaseExt(DatabaseID) do
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
          // IProviderSupport(Query).PSExecuteStatement(VarToStr(Data), AParams, @ResultSet);
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
{$ENDIF}

function TCtxEDBAdapter.GetDriverName: OleVariant;
begin
  Result := 'ElevateDB';
end;

function TCtxEDBAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TEDBDatabaseExt;
  Session: TEDBSession;
  ParamList: TStringList;
  ConfigFile: String;
begin
  Session := TEDBSession.Create(nil);
  Database := TEDBDatabaseExt.Create(nil);
  try
    Database.Schema := TDatabaseSchema.Create(Database);
    Session.AutoSessionName := True;
    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      Session.LoginUser := ParamList.Values['User Name'];
      Session.LoginPassword := ParamList.Values['Password'];
      ConfigFile := ParamList.Values['Config File'];     
      if ConfigFile <> '' then
      begin
        Session.LocalConfigPath :=  ExtractFilePath(ConfigFile);
        Session.LocalConfigName :=  ExtractFileName(ConfigFile);
      end;
    finally
      ParamList.Free;
    end;
    Database.SessionName := Session.SessionName;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;

    Database.LoginPrompt := False;
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

function TCtxEDBAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
begin
  with TEDBOpenDatabaseDialog.Create(nil) do
  try
    EDBSession := TEDBSession.Create(nil);
    try
      DatabaseURL := ConnectionStr;
      Result := Execute;
      if Result then
        ConnectionStr := DatabaseURL;
    finally
      EDBSession.Free;
    end;
  finally
    Free;
  end;
end;

function TCtxEDBAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User Name=","Password=","Config File="';
end;


initialization
  Engine.ConfigPath := ExtractFilePath(ParamStr(0));
end.
