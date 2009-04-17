unit CtxIBAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, IBDatabase, IBExt, dbSchema, Dialogs,
  CtxDBDesignerAdapter;

type
  TCtxIBAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
  protected
    FDatabases: TObjectList;
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

uses dbExtUtils, DbLogDlg, fIBOpenDatabase;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxIBAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxIBAdapter }

constructor TCtxIBAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxIBAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxIBAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TIBDatabaseExt(DatabaseID).Connected := False;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxIBAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxIBAdapter.GetDriverName: OleVariant;
begin
  Result := 'IB\FB';
end;

function TCtxIBAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TIBDatabaseExt;
begin
  Database := TIBDatabaseExt.Create(nil);
  try
    Database.LoginPrompt := False;
    Database.Params.CommaText := Params;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;
    Database.SQLDialect := 3;
    Database.InternalTransaction.AutoStopAction := saCommitRetaining;
    try
      Database.Connected := True;
    except
      if not Database.Connected then
      begin
        Database.LoginPrompt := True;
        Database.Connected := True;
      end;
    end;
    FDatabases.Add(Database);
    Result := Integer(Database);
  except
    Database.Free;
    raise;
  end;
end;

function TCtxIBAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
var
  Temp: String;
begin
  Temp := ConnectionStr;
  Result := EditConnectionString(Temp);
  if Result then
    ConnectionStr := Temp;
end;

function TCtxIBAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User_Name=","Password="';
end;

end.
