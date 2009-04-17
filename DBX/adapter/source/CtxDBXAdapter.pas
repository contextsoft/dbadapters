unit CtxDBXAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, dbSchema,
  DBCommon, DBXpress, SQLExpr, SQLExt, CtxDBDesignerAdapter;

type
  TCtxDBXAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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

uses fOpenDBXDatabase;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxDBXAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxDBXAdapter }

constructor TCtxDBXAdapter.Create;
begin
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxDBXAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxDBXAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
    FDatabases.Delete(Idx);
end;

function TCtxDBXAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
var
  Idx: Integer;
  Query: TDataSet;
begin
  try
    Idx := FDatabases.IndexOf(TObject(DatabaseID));
    Result := 0;
    if (Idx < 0) or (DatabaseID = 0) then
      Result := -1
    else with TSQLConnectionExt(DatabaseID) do
    begin
      case Verb of
        // dvCreateDatabase:; not used currently
        dvReverseEngineer: begin
          Schema := TDatabaseSchema.Create(nil);
          try
            ReverseEngineer;
            Data := StrToVarArray(Schema.SaveToStr);
          finally
            Schema.Free;
            Schema := nil;
          end;
        end;
        dvGetVersion: Data := StrToVarArray(VersionToStr(GetVersion));
        dvSetVersion: SetVersion(StrToVersion(VarArrayToStr(Data)));
        dvExecuteSQL: begin
          Query := CreateQuery(VarArrayToStr(Data));
          try
            ExecSQL(Query);
          finally
            Query.Free;
          end;
        end;
        else
          Result := -1;
      end;
    end;
    if Result <> 0 then
      Data := Format(SOperationNotSupported, [GetDriverName]);
  except
    on E: Exception do
    begin
      Result := -1;
      Data := E.Message;
    end;
  end;
end;

function TCtxDBXAdapter.GetDriverName: OleVariant;
begin
  Result := 'DB Express';
end;

function TCtxDBXAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TSQLConnectionExt;
  ParamList: TStringList;
begin
  Database := TSQLConnectionExt.Create(nil);
  try
    Database.LoginPrompt := False;
    Database.DatabaseName := DatabaseName;
    Database.LoadParamsOnConnect := False;
    Database.DatabaseURL := ConnectionStr;
    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      Database.Params := ParamList;
    finally
      ParamList.Free;
    end;
    Database.LoadParamsOnConnect := False;
    Database.Connected := True;
    FDatabases.Add(Database);
    Result := Integer(Database);
  except
    Database.Free;
    Result := 0;
  end;
end;

function TCtxDBXAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
var
  ConnStr: String;
begin
  ConnStr := ConnectionStr;
  Result := SelectDBXDatabase(ConnStr);
  if Result then
    ConnectionStr := ConnStr;
end;

function TCtxDBXAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := 'User_Name=,Password=,SQLDialect=3';
end;

end.
