unit CtxABSAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, ABSExt, dbSchema,
  ABSMain, CtxDBDesignerAdapter;

type
  TCtxABSAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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

uses Dialogs, dbExtUtils;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxABSAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxABSAdapter }

constructor TCtxABSAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxABSAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxABSAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TABSDatabaseExt(DatabaseID).Connected := False;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxABSAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxABSAdapter.GetDriverName: OleVariant;
begin
  Result := 'Absolute DB';
end;

function TCtxABSAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TABSDatabaseExt;
  ParamList: TStringList;
begin
  Database := TABSDatabaseExt.Create(nil);
  try
    Database.Schema := TDatabaseSchema.Create(Database);
    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      Database.Password := ParamList.Values['Password'];
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

function TCtxABSAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Absolue Database (*.abs)|*.abs|Any File (*.*)|*.*';
    FileName := ConnectionStr;
    Result := Execute;
    if Result then
      ConnectionStr := FileName;
  finally
    Free;
  end;
end;

function TCtxABSAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"Password="';
end;

end.
