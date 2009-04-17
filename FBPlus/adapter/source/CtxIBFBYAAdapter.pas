unit CtxIBFBYAAdapter;

interface

uses
  Classes, SysUtils, Variants, Contnrs, DB, FIBExt, dbSchema, Dialogs,
  CtxDBDesignerAdapter;

type
  TCtxIBFBYAAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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
  Adapters := TCtxIBFBYAAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxIBFBYAAdapter }

constructor TCtxIBFBYAAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxIBFBYAAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxIBFBYAAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TFIBDatabaseExt(DatabaseID).Connected := False;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxIBFBYAAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxIBFBYAAdapter.GetDriverName: OleVariant;
begin
  Result := 'IB\FB\YA'; //TODO: ???
end;

function TCtxIBFBYAAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  vDatabase: TFIBDatabaseExt;
  LibName: String;
begin
  vDatabase := TFIBDatabaseExt.Create(nil);
  try
    vDatabase.DBParams.CommaText := Params;
    vDatabase.UseLoginPrompt :=
      (Length(Trim(vDatabase.ConnectParams.UserName))=0) and
      (Length(Trim(vDatabase.ConnectParams.Password))=0);
    vDatabase.DatabaseName := DatabaseName;
    vDatabase.DatabaseURL := ConnectionStr;

    LibName := Trim(vDatabase.DBParams.Values['LIBRARY_NAME']);
    if LibName = '' then
      LibName := 'fbclient.dll'; 
    vDatabase.LibraryName := LibName;   
    vDatabase.DBParams.Values['LIBRARY_NAME'] := '';

    vDatabase.SQLDialect := 3;
    vDatabase.Connected := True;
    FDatabases.Add(vDatabase);
    Result := Integer(vDatabase);
  except
    on E:Exception do
    begin
      FreeAndNil(vDatabase);
      raise;
    end;
  end;
end;

function TCtxIBFBYAAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
var
  vTemp: String;
begin
  vTemp := ConnectionStr;
  Result := EditConnectionString(vTemp);
  if Result then
  begin
    ConnectionStr := vTemp;
  end;
end;

function TCtxIBFBYAAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  // Params := '"USER_NAME=","PASSWORD=","CHARSET=","LIBRARY_NAME="';
  Params := '"USER_NAME=","PASSWORD=","LIBRARY_NAME="';
end;

end.
