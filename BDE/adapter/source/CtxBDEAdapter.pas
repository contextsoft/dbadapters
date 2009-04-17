unit CtxBDEAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, BDE, BDEExt, dbSchema,
  DBTables, CtxDBDesignerAdapter;

type
  TCtxBDEAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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

uses dbExtUtils, DbLogDlg, fBDEOpenDatabase;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxBDEAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxBDEAdapter }

constructor TCtxBDEAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxBDEAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxBDEAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TDatabaseExt(DatabaseID).Connected := False;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxBDEAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxBDEAdapter.GetDriverName: OleVariant;
begin
  Result := 'BDE\ODBC';
end;

function TCtxBDEAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TDatabaseExt;
begin
  Database := TDatabaseExt.Create(nil);
  try
    Database.LoginPrompt := False;
    Database.Params.CommaText := Params;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;
    Database.KeepConnection := False;
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

function TCtxBDEAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
begin
  Result := fBDEOpenDatabase.SelectDatabase(ConnectionStr);
end;

function TCtxBDEAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User Name=","Password="';
end;

end.
 