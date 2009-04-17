unit CtxMySQLAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, dbSchema, 
  MySQLExt, CtxDBDesignerAdapter;

type
  TCtxMySQLAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
  protected
    FDatabases: TObjectList;
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

uses dbExtUtils, DBAccess, MyDacVcl;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxMySQLAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxMySQLAdapter }

constructor TCtxMySQLAdapter.Create;
begin
  inherited Create;
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxMySQLAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxMySQLAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TMySQLDatabaseExt(DatabaseID).Connected := False;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxMySQLAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxMySQLAdapter.GetDriverName: OleVariant;
begin
  Result := 'MySQLDAC';
end;

function TCtxMySQLAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TMySQLDatabaseExt;
  ParamList: TStringList;
  TempPort: String;
begin
  Database := TMySQLDatabaseExt.Create(nil);
  try
    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      Database.UserName := ParamList.Values['User_Name'];
      Database.Password := ParamList.Values['Password'];
      TempPort := ParamList.Values['Port'];
      if TempPort <> '' then
        Database.Port := StrToIntDef(TempPort, 3306);
    finally
      ParamList.Free;
    end;
    Database.LoginPrompt := False;
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

function TCtxMySQLAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
begin
  with TMySQLDatabaseExt.Create(nil) do
  try
    ConnectDialog := TMyConnectDialog.Create(nil);
    try
      DatabaseURL := ConnectionStr;
      Result := ConnectDialog.Execute;
      if Result then
        ConnectionStr := DatabaseURL;
    finally
      ConnectDialog.Free;
    end;
  finally
    Free;
  end;
end;

function TCtxMySQLAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User_Name=","Password=","Port="';
end;

end.
