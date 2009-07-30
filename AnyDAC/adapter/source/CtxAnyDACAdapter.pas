{I uAD.inc}
unit CtxAnyDACAdapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, dbSchema, AnyDACExt, 
  CtxDBDesignerAdapter;

type
  TCtxADAdapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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

uses
{$IFDEF AnyDAC_MONITOR}
  uADMoniFlatFile,
  uADMoniRemoteClient,
{$ENDIF}  
  uADPhysManager,
{$IFDEF AnyDAC_D6}
  {$IFDEF AnyDAC_D11}
  uADPhysTDBX,
  {$ELSE}
  uADPhysDbExp,
  {$ENDIF}
{$ENDIF}
  uADPhysODBC,
  uADPhysOracl,
  uADPhysMySQL,
  uADPhysMSSQL,
  uADPhysMSAcc,
  uADPhysDB2,
  uADPhysASA,
  uADPhysIB,
  uADPhysPG,
  uADPhysADS,
  uADPhysSQLite,
  uADGUIxFormsWait, uADGUIxFormsfConnEdit, uADGUIxFormsfLogin, dbExtUtils;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';
  SEditConnection = 'Edit Connection';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxADAdapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxADAdapter }

constructor TCtxADAdapter.Create;
begin
  inherited Create;
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxADAdapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxADAdapter.CloseDatabase(DatabaseID: Integer);
var
  Idx: Integer;
begin
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  if Idx >= 0 then
  begin
    TADConnectionExt(DatabaseID).Connected := False;
    FDatabases.Delete(Idx);
  end;
end;

function TCtxADAdapter.ExecuteVerb(DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
(*
var
  Idx: Integer;
  Query: TDataSet;
*)  
begin
  Result := ExecuteVerbExt(FDatabases, DatabaseID, Verb, Data);
(*
  Idx := FDatabases.IndexOf(TObject(DatabaseID));
  Result := 0;
  if (Idx < 0) or (DatabaseID = 0) then
    Result := -1
  else with TADConnectionExt(DatabaseID) do
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
      else begin
        Result := -1;
      end;
    end;
  end;
  if Result <> 0 then
    Data := Format(SOperationNotSupported, [GetDriverName]);
*)
end;

function TCtxADAdapter.GetDriverName: OleVariant;
begin
  Result := 'AnyDAC';
end;

function TCtxADAdapter.OpenDatabase(const DatabaseName, DriverName,
  ConnectionStr, Params: OleVariant): Integer;
var
  Database: TADConnectionExt;
  I: Integer;
  ParamList: TStringList;
begin
  Database := TADConnectionExt.Create(nil);
  try
    // Database.LoginPrompt := False;
    Database.DatabaseName := DatabaseName;
    Database.DatabaseURL := ConnectionStr;

    ParamList := TStringList.Create;
    try
      ParamList.CommaText := Params;
      for I := 0 to ParamList.Count - 1 do
      if ParamList.Names[I] <> '' then
        Database.Params.Values[ParamList.Names[I]] := ParamList.ValueFromIndex[I];
    finally
      ParamList.Free;
    end;

    Database.Connected := True;
    FDatabases.Add(Database);
    Result := Integer(Database);
  except
    Database.Free;
    raise;
  end;
end;

function TCtxADAdapter.SelectDatabase(
  var ConnectionStr: OleVariant): Boolean;
var
  Database: TADConnectionExt;
begin
  Database := TADConnectionExt.Create(nil);
  try
    Database.DatabaseURL := ConnectionStr;
    Result := TfrmADGUIxFormsConnEdit.Execute(Database, SEditConnection);
    if Result then
      ConnectionStr := Database.DatabaseURL;
  finally
    Database.Free;
  end;
end;

function TCtxADAdapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User_Name=","Password="';
end;

end.
