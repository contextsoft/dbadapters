(******************************************************************************)
(*
(*  Context Database Extensions Suite (Nexus)
(*
(*  Open NexusDB Database Dialog component.
(*  Contains:
(*      TnxOpenDatabase
(*      TnxOpenDatabaseDialog
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit nxOpenDatabase;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Db, dbSchema, dbExtUtils, nxdb, nxDBExt,
{$IFDEF NX_REMOTESERVER}
  nxtcCOMTransport, nxtnNamedPipeTransport,
  nxllTransport, nxptBasePooledTransport, nxtwWinsockTransport, nxreRemoteServerEngine,
{$ENDIF}
  nxllComponent, nxsrServerEngine, nxsdServerEngine,
  nxsqlEngine;

type
  TnxOpenDatabase = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxConnectionType: TGroupBox;
    rbLocal: TRadioButton;
    rbRemote: TRadioButton;
    gbxAvailableDatabases: TGroupBox;
    cbxAvailableDatabases: TComboBox;
    btnSelect: TButton;
    lblServer: TLabel;
    cbxServer: TComboBox;
    cbxRemoteType: TComboBox;

    procedure btnSelectClick(Sender: TObject);
    procedure ConnectionTypeChange(Sender: TObject);
    procedure cbxServerChange(Sender: TObject);
    procedure cbxAvailableDatabasesChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    FLastServer: String;
    FConnectionType: String;
    FRemoteHost: String;
    FDatabaseName: String;
    FMRUServers: TStrings;
    FMRUDatabases: TStrings;
    nxSession: TnxSession;

{$IFDEF NX_REMOTESERVER}
    nxRemoteServerEngine: TnxRemoteServerEngine;
    nxWinsockTransport: TnxWinsockTransport;
    nxNamedPipeTransport: TnxNamedPipeTransport;
    nxRegisteredCOMTransport: TnxRegisteredCOMTransport;
{$ENDIF}

    procedure LoadDatabases; virtual;
    procedure DataToForm; virtual;
    procedure FormToData; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function Execute(AnxSession: TnxSession; var DatabaseURL: String): Boolean;

    property MRUServers: TStrings read FMRUServers;
    property MRUDatabases: TStrings read FMRUDatabases;
  end;

  {:$ TnxOpenDatabaseDialog displays database selection dialog. }
  {:: TnxOpenDatabaseDialog displays a modal dialog box for selecting and opening NexusDB Databases. }
  {:: The dialog does not appear at runtime until it is activated by a call to the Execute method. }
  {:: When the user clicks OK, the dialog closes and the selected database path is stored }
  {:: in the DatabaseURL property.}
  TnxOpenDatabaseDialog = class(TComponent)
  protected
    FDatabaseURL: String;
    FnxSession: TnxSession;
    FMRUServers: TStrings;
    FMRUDatabases: TStrings;
    FTitle: String;

    procedure SetMRUDatabases(const Value: TStrings);
    procedure SetMRUServers(const Value: TStrings);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Creates an instance of TnxOpenDatabaseDialog component. }
    constructor Create(Owner: TComponent); override;
    {:$ Destroys the instance of TnxOpenDatabaseDialog component. }
    destructor Destroy; override;
    {:$ Executes modal open database dialog. }
    {:: Returns True if the user pressed OK to select a database. The result }
    {:: of users input is stored in the DatabaseURL property. }
    function Execute: Boolean;
    {:$ Contains a list of most recently user (MRU) server names. }
    property MRUServers: TStrings read FMRUServers write SetMRUServers;
    {:$ Contains a list of most recently user (MRU) database names. }
    property MRUDatabases: TStrings read FMRUDatabases write SetMRUDatabases;
  published
    {:$ Returns the URL for the selected database. }
    property DatabaseURL: String read FDatabaseURL write FDatabaseURL;
    {:$ Specifies the database session that will be used for login information and other parameters. }
    property NxSession: TnxSession read FnxSession write FnxSession;
    {:$ Specifies the title for the open database dialog. }
    {:: Default title is 'Open Database'. }
    property Title: String read FTitle write FTitle;
  end;

resourcestring
  SServerNameEmpty = 'Server name can not be empty.';
  SDatabasePathEmpty = 'Database path can not be empty.';
  SDatabaseNameEmpty = 'Database name can not be empty.';

implementation

uses ShlObj;

{$R *.DFM}

{ TnxOpenDatabase }

constructor TnxOpenDatabase.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FMRUServers := TStringList.Create;
  TStringList(FMRUServers).Duplicates := dupIgnore;
  TStringList(FMRUServers).Sorted := True;
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
  nxSession := TnxSession.Create(Self);

{$IFDEF NX_REMOTESERVER}
  nxRemoteServerEngine := TnxRemoteServerEngine.Create(Self);
  nxWinsockTransport := TnxWinsockTransport.Create(Self);
  nxNamedPipeTransport := TnxNamedPipeTransport.Create(Self);
  nxRegisteredCOMTransport := TnxRegisteredCOMTransport.Create(Self);
  nxRemoteServerEngine.Transport := nxWinsockTransport;
{$ELSE}
  rbRemote.Enabled := False;
  cbxRemoteType.Enabled := False;
{$ENDIF}
end;

destructor TnxOpenDatabase.Destroy;
begin
  inherited Destroy;
  FMRUServers.Free;
  FMRUDatabases.Free;
end;

function TnxOpenDatabase.Execute(AnxSession: TnxSession;
  var DatabaseURL: String): Boolean;
begin
  if AnxSession <> nil then
  begin
    nxSession.UserName := AnxSession.UserName;
    nxSession.Password := AnxSession.Password;
    nxSession.ServerEngine := AnxSession.ServerEngine;
  end;
  dbSchema.DecodeDatabaseURL(DatabaseURL, FConnectionType, FRemoteHost, FDatabaseName);
  DataToForm;
  Result := ShowModal = mrOK;
  if Result then
    DatabaseURL := dbSchema.EncodeDatabaseURL(FConnectionType, FRemoteHost, FDatabaseName);
end;

procedure TnxOpenDatabase.DataToForm;
begin
  // rbLocal.Enabled := True;
  // rbRemote.Enabled := True;
  rbLocal.Checked := FConnectionType = '';
  rbRemote.Checked := not rbLocal.Checked;

  cbxRemoteType.Visible := rbRemote.Checked;
  cbxRemoteType.ItemIndex := 0;
  cbxRemoteType.Text := FConnectionType;

  FLastServer := '/////';

  ConnectionTypeChange(Self);
end;

procedure TnxOpenDatabase.FormToData;
begin
  FDatabaseName := cbxAvailableDatabases.Text;
  if rbRemote.Checked then begin
    FConnectionType := cbxRemoteType.Text;
    FRemoteHost := cbxServer.Text;
  end else begin
    FConnectionType := '';
    FRemoteHost := '';
  end;
end;

procedure TnxOpenDatabase.LoadDatabases;
{$IFDEF NX_REMOTESERVER}
var
  _Host: string;
  _Port: integer;
{$ENDIF}
begin
  btnSelect.Visible := rbLocal.Checked;
  cbxServer.Enabled := rbRemote.Checked;
  lblServer.Enabled := rbRemote.Checked;

  with cbxAvailableDatabases do
  begin
    if rbRemote.Checked then begin
{$IFDEF NX_REMOTESERVER}
      nxSession.Active := False;
      Items.Clear;
      Style := csDropDownList;
      if (cbxServer.Text = '') and (cbxServer.Items.Count > 0) then
        cbxServer.ItemIndex := 0;
      if cbxServer.Text <> '' then
      begin
        nxRemoteServerEngine.Close;
        nxSession.ServerEngine := nxRemoteServerEngine;
        case cbxRemoteType.ItemIndex of
          0: nxRemoteServerEngine.Transport := nxWinsockTransport;
          1: nxRemoteServerEngine.Transport := nxNamedPipeTransport;
          2: nxRemoteServerEngine.Transport := nxRegisteredCOMTransport;
        end;
        nxRemoteServerEngine.Transport.Close;
        DecodeServerName(cbxServer.Text, _Host, _Port);
        nxRemoteServerEngine.Transport.ServerName := _Host;
        if nxRemoteServerEngine.Transport is TnxBasePooledTransport then
          (nxRemoteServerEngine.Transport as TnxBasePooledTransport).Port := _Port;
        try
          nxSession.Open;
          nxSession.GetAliasNames(Items);
        except
          // We don't care about exceptions here
        end;
      end;
      FLastServer := cbxServer.Text;
      nxSession.Active := False;
{$ENDIF}
    end else begin
      if (FConnectionType = '') and (FDatabaseName <> '') then
        MRUDatabases.Add(FDatabaseName);
      Style := csDropDown;
      Items.Assign(MRUDatabases);
    end;
    ItemIndex := Items.IndexOf(FDatabaseName);
    if ItemIndex < 0 then ItemIndex := 0;
  end;
  cbxAvailableDatabasesChange(nil);
end;

function BrowseCallback(Wnd: HWND; uMsg: UINT; lParam,lpData: LPARAM): Integer; stdcall;
begin
  Result := 0;
  if (uMsg = BFFM_INITIALIZED) then begin
    if (StrLen(PChar(lpData)) > 0) then
      SendMessage(Wnd, BFFM_SETSELECTION, 1, lpData);
  end;
end;

function GetDirectory(var DirectoryStr: string): Boolean;
var
   WindowList: Pointer;
   Buffer: array[0..MAX_PATH] of Char;
   ItemIdList: PItemIDList;
   BrowseInfo: TBrowseInfo;
begin
  Result := False;
  with BrowseInfo do begin
    hwndOwner := Screen.ActiveForm.Handle;
    pidlRoot := nil;
    pszDisplayName := Buffer;
    lpszTitle := 'Select the directory';
    ulFlags := BIF_RETURNONLYFSDIRS;
    lpfn := BrowseCallback;
    lParam := Integer(PChar(DirectoryStr));
  end;
  WindowList := DisableTaskWindows(0);
  try
    ItemIdList := ShBrowseForFolder(BrowseInfo);
  finally
    EnableTaskWindows(WindowList);
  end;
  if (ItemIDList = nil) then
    Exit;
  if SHGetPathFromIDList(ItemIDList,@Buffer) then begin
    Result := True;
    DirectoryStr := StrPas({$IFDEF VER230}PWideChar(@Buffer){$ELSE}@Buffer{$ENDIF});
  end;
end;

procedure TnxOpenDatabase.btnSelectClick(Sender: TObject);
var
  DefaultDir: String;
  F: TSearchRec;
begin
  if FindFirst(cbxAvailableDatabases.Text, faDirectory, F) <> 0 then
    DefaultDir := FDatabaseName
  else DefaultDir := cbxAvailableDatabases.Text;

  if GetDirectory(DefaultDir) then
    with cbxAvailableDatabases do
    begin
      if DefaultDir <> '' then
        MRUDatabases.Add(DefaultDir);
      Items.Assign(MRUDatabases);
      ItemIndex := Items.IndexOf(DefaultDir);
    end;
end;

procedure TnxOpenDatabase.ConnectionTypeChange(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    cbxAvailableDatabases.Enabled := False;
    cbxRemoteType.Visible := rbRemote.Checked;

    with cbxServer do
    begin
      if rbRemote.Checked then
      begin
        if FRemoteHost <> '' then
          MRUServers.Add(FRemoteHost);
        MRUServers.Add('localhost');
        Items.Assign(MRUServers);
        ItemIndex := Items.IndexOf(FRemoteHost);
        if ItemIndex < 0 then ItemIndex := 0;
      end else
        Text := '';
    end;

    LoadDatabases;
    cbxAvailableDatabases.Enabled := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TnxOpenDatabase.cbxServerChange(Sender: TObject);
begin
  if rbRemote.Checked and (cbxServer.Text <> FLastServer) then
    LoadDatabases;
end;

procedure TnxOpenDatabase.cbxAvailableDatabasesChange(Sender: TObject);
begin
  // Database Changed
end;

procedure TnxOpenDatabase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    FormToData;
    if FConnectionType = '' then
    begin
      if FDatabaseName = '' then
        DatabaseError(SDatabasePathEmpty);
    end else begin
      if FRemoteHost = '' then
        DatabaseError(SServerNameEmpty);
      if FDatabaseName = '' then
        DatabaseError(SDatabaseNameEmpty);
    end;
  end;
end;

{ TnxOpenDatabaseDialog }

constructor TnxOpenDatabaseDialog.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FDatabaseURL := '';
  FnxSession := nil;
  FMRUServers := TStringList.Create;
  TStringList(FMRUServers).Duplicates := dupIgnore;
  TStringList(FMRUServers).Sorted := True;
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
  FTitle := '';
end;

destructor TnxOpenDatabaseDialog.Destroy;
begin
  inherited Destroy;
  FMRUServers.Free;
  FMRUDatabases.Free;
end;

function TnxOpenDatabaseDialog.Execute: Boolean;
begin
  with TnxOpenDatabase.Create(Application.MainForm) do
  try
    if FTitle <> '' then
      Caption := FTitle;
    MRUServers.Assign(Self.MRUServers);
    MRUDatabases.Assign(Self.MRUDatabases);
    Result := Execute(FnxSession, FDatabaseURL);
    if Result then begin
      Self.MRUServers.Assign(MRUServers);
      Self.MRUDatabases.Assign(MRUDatabases);
    end;
  finally
    Free;
  end;
end;

procedure TnxOpenDatabaseDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FnxSession) and (Operation = opRemove) then
    FnxSession := nil;
  inherited;
end;

procedure TnxOpenDatabaseDialog.SetMRUDatabases(const Value: TStrings);
begin
  FMRUDatabases.Assign(Value);
end;

procedure TnxOpenDatabaseDialog.SetMRUServers(const Value: TStrings);
begin
  FMRUServers.Assign(Value);
end;

end.
