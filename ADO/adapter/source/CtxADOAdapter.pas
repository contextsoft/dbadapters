unit CtxADOAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, dbSchema,
  ADODB, ADOExt, CtxDBDesignerAdapter;

type
  TCtxADOAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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

uses dbExtUtils;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxADOAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxADOAdapter }

constructor TCtxADOAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxADOAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxADOAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
    FDatabases.Delete(Idx);
end;

function TCtxADOAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
end;

function TCtxADOAdapter.GetDriverName: OleVariant;
begin
  Result := 'ADO';
end;

function TCtxADOAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TADOConnectionExt;
  ParamList: TStringList;
begin
  Database := TADOConnectionExt.Create(nil);
  try
    Database.LoginPrompt := False;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;

    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      Database.ConnectionTimeout := StrToIntDef(ParamList.Values['Connection Timeout'], 60);
      Database.CommandTimeout := StrToIntDef(ParamList.Values['Command Timeout'], 60);
      Database.Open(ParamList.Values['User Name'], ParamList.Values['Password']);
    finally
      ParamList.Free;
    end;
    // Database.Connected := True;
    FDatabases.Add(Database);
    Result := Integer(Database);
  except
    Database.Free;
    raise;
  end;
end;

function TCtxADOAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
var
  NewConnStr,
  InitialConnStr: WideString;
begin
  Result := False;
  InitialConnStr := ConnectionStr;
  NewConnStr := PromptDataSource(0, InitialConnStr);
  if NewConnStr <> InitialConnStr then
  begin
    ConnectionStr := NewConnStr;
    Result := True;
  end;
end;

function TCtxADOAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User Name=","Password=","Connection Timeout=","Command Timeout="';
end;

end.
