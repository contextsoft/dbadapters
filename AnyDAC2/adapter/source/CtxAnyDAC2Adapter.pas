{I uAD.inc}
unit CtxAnyDAC2Adapter;

interface

uses Classes, SysUtils, Variants, Contnrs, DB, dbSchema, AnyDACExt, 
  CtxDBDesignerAdapter;

type
  TCtxAD2Adapter = class (TInterfacedObject, ICtxDBDesignerAdapter)
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
  uADStanDef,
  uADStanAsync,
  uADDAptManager,
  uADGUIxFormsWait, uADGUIxFormsfConnEdit, uADGUIxFormsfLogin, dbExtUtils;

resourcestring
  SOperationNotSupported = 'Selected operation is not supported by %s adapter';
  SEditConnection = 'Edit Connection';

procedure GetCtxDBAdapters(var Adapters: OleVariant);
begin
  Adapters := TCtxAD2Adapter.Create as ICtxDBDesignerAdapter;
end;

{ TCtxAD2Adapter }

constructor TCtxAD2Adapter.Create;
begin
  inherited Create;
  FDatabases := TObjectList.Create(True);
end;

destructor TCtxAD2Adapter.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

procedure TCtxAD2Adapter.CloseDatabase(DatabaseID: Integer);
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

function TCtxAD2Adapter.ExecuteVerb(DatabaseID, Verb: Integer;
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

function TCtxAD2Adapter.GetDriverName: OleVariant;
begin
  Result := 'AnyDAC 2';
end;

function TCtxAD2Adapter.OpenDatabase(const DatabaseName, DriverName,
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

    Database.LoginPrompt := Database.Params.Values['User_Name'] = '';

    Database.Connected := True;
    FDatabases.Add(Database);
    Result := Integer(Database);
  except
    Database.Free;
    raise;
  end;
end;

function TCtxAD2Adapter.SelectDatabase(
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

function TCtxAD2Adapter.GetParamNames(var Params: OleVariant): Integer;
begin
  Result := 0;
  Params := '"User_Name=","Password="';
end;

end.
