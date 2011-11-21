(******************************************************************************)
(*
(*  Context Database Extensions Suite (DBISAM)
(*
(*  Open DBISAM Database Dialog component.
(*  Contains:
(*      TDBISAMOpenDatabase
(*      TDBISAMOpenDatabaseDialog
(*
(*  Copyright (c) 2004-2006 Michael Baytalsky
(*
(******************************************************************************)
unit DBISAMOpenDatabase;

interface

{$I dbisamvr.inc}

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, DBISAMTb, Db;

type
  TDBISAMOpenDatabase = class(TForm)
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
    DBISAMSession: TDBISAMSession;

    procedure LoadDatabases; virtual;
    procedure DataToForm; virtual;
    procedure FormToData; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function Execute(ADBISAMSession: TDBISAMSession; var DatabaseURL: String): Boolean;

    property MRUServers: TStrings read FMRUServers;
    property MRUDatabases: TStrings read FMRUDatabases;
  end;

  {:$ TDBISAMOpenDatabaseDialog displays database selection dialog. }
  {:: TDBISAMOpenDatabaseDialog displays a modal dialog box for selecting and opening DBISAM Databases. }
  {:: The dialog does not appear at runtime until it is activated by a call to the Execute method. }
  {:: When the user clicks OK, the dialog closes and the selected database path is stored }
  {:: in the DatabaseURL property.}
  TDBISAMOpenDatabaseDialog = class(TComponent)
  protected
    FDatabaseURL: String;
    FDBISAMSession: TDBISAMSession;
    FMRUServers: TStrings;
    FMRUDatabases: TStrings;
    FTitle: String;

    procedure SetMRUDatabases(const Value: TStrings);
    procedure SetMRUServers(const Value: TStrings);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Creates an instance of TDBISAMOpenDatabaseDialog component. }
    constructor Create(Owner: TComponent); override;
    {:$ Destroys the instance of TDBISAMOpenDatabaseDialog component. }
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
    property DBISAMSession: TDBISAMSession read FDBISAMSession write FDBISAMSession;
    {:$ Specifies the title for the open database dialog. }
    {:: Default title is 'Open Database'. }
    property Title: String read FTitle write FTitle;
  end;

  {:$ Splits database URL into parts. }
  procedure DecodeDatabaseURL(const DatabaseURL: String; var ConnectionType, RemoteHost, DatabaseName: String);
  {:$ Returns database URL for the specified ConnectionType, RemoteHost and DatabaseName. }
  function EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName: String): String;
  {:: Retrieves database URL from the TDBISAMDatabase component and the connected Session component. }
  function GetDatabaseURL(DBISAMDatabase: TDBISAMDatabase): String;
  {:: Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(DBISAMDatabase: TDBISAMDatabase; DatabaseURL: String);

const
  ctLocal = '';
  ctLAN = 'LAN';
  ctInternet = 'Internet';

  INTERNET_COMPRESSION = 6;

resourcestring
  SServerNameEmpty = 'Server name can not be empty.';
  SDatabasePathEmpty = 'Database path can not be empty.';
  SDatabaseNameEmpty = 'Database name can not be empty.';

implementation

uses ShellAPI, ShlObj, DBISAMCn;

{$R *.DFM}

{ General Helpers Routines }

function BrowseCallback(Wnd: HWND; uMsg: UINT; lParam,lpData: LPARAM): Integer; stdcall;
begin
   Result:=0;
   if (uMsg=BFFM_INITIALIZED) then
      begin
      if (StrLen(PChar(lpData)) > 0) then
         SendMessage(Wnd,BFFM_SETSELECTION,1,lpData);
      end;
end;

function GetDirectory(var DirectoryStr: string): Boolean;
var
   WindowList: Pointer;
   Buffer: array[0..MAX_PATH] of Char;
   ItemIdList: PItemIDList;
   BrowseInfo: TBrowseInfo;
begin
   Result:=False;
   with BrowseInfo do
      begin
      hwndOwner:=Screen.ActiveForm.Handle;
      pidlRoot:=nil;
      pszDisplayName:=Buffer;
      lpszTitle:='Select the directory';
      ulFlags:=(BIF_RETURNONLYFSDIRS+BIF_EDITBOX);
      lpfn:=BrowseCallback;
      lParam:=Integer(PChar(DirectoryStr));
      end;
   WindowList:=DisableTaskWindows(0);
   try
      ItemIdList:=ShBrowseForFolder(BrowseInfo);
   finally
      EnableTaskWindows(WindowList);
   end;
   if (ItemIDList=nil) then
      Exit;
   if SHGetPathFromIDList(ItemIDList,@Buffer) then
      begin
      Result:=True;
      DirectoryStr:=StrPas(@Buffer);
      end;
end;

function GetDatabaseURL(DBISAMDatabase: TDBISAMDatabase): String;
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  if DBISAMDatabase.Session.SessionType = stLocal then
  begin
    ConnectionType := ctLocal;
    RemoteHost := '';
    DatabaseName := DBISAMDatabase.Directory;
  end else begin
    {$IFDEF DBISAM_V4}
    if DBISAMDatabase.Session.RemoteCompression = 0 then
      ConnectionType := ctLAN
    else ConnectionType := ctInternet;
    {$ELSE}
    if DBISAMDatabase.Session.RemoteType = rtLAN then
      ConnectionType := ctLAN
    else ConnectionType := ctInternet;
    {$ENDIF}
    RemoteHost := DBISAMDatabase.Session.RemoteHost;
    DatabaseName := DBISAMDatabase.RemoteDatabase;
  end;
  Result := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
end;

procedure SetDatabaseURL(DBISAMDatabase: TDBISAMDatabase; DatabaseURL: String);
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  if ConnectionType = ctLocal then
  begin
    DBISAMDatabase.Session.SessionType := stLocal;
    DBISAMDatabase.Directory := DatabaseName;
  end else begin
    DBISAMDatabase.Session.SessionType := stRemote;
    {$IFDEF DBISAM_V4}
    if AnsiCompareText(ConnectionType, ctLAN) = 0 then
      DBISAMDatabase.Session.RemoteCompression := 0
    else DBISAMDatabase.Session.RemoteCompression := INTERNET_COMPRESSION;
    {$ELSE}
    if AnsiCompareText(ConnectionType, ctLAN) = 0 then
      DBISAMDatabase.Session.RemoteType := rtLAN
    else DBISAMDatabase.Session.RemoteType := rtInternet;
    {$ENDIF}
    DBISAMDatabase.Session.RemoteHost := RemoteHost;
    DBISAMDatabase.RemoteDatabase := DatabaseName;
  end;
end;

procedure DecodeDatabaseURL(const DatabaseURL: String; var ConnectionType, RemoteHost, DatabaseName: String);
var
  P: Integer;
begin
  P := Pos(':\\', DatabaseURL);
  if P < 1 then begin
    // local
    ConnectionType := '';
    RemoteHost := '';
    DatabaseName := DatabaseURL;
  end else begin
    // remote lan or wan
    ConnectionType := copy(DatabaseURL, 1, P-1);
    RemoteHost := copy(DatabaseURL, P+3, Length(DatabaseURL));
    P := Pos('\', RemoteHost);
    DatabaseName := copy(RemoteHost, P+1, Length(RemoteHost));
    RemoteHost := copy(RemoteHost, 1, P-1);
  end;
end;

function EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName: String): String;
begin
  Result := '';
  if ConnectionType <> '' then
    Result := Result + ConnectionType + ':\\' + RemoteHost + '\';
  Result := Result + DatabaseName;
end;

{ TDBISAMOpenDatabase }

constructor TDBISAMOpenDatabase.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FMRUServers := TStringList.Create;
  TStringList(FMRUServers).Duplicates := dupIgnore;
  TStringList(FMRUServers).Sorted := True;
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
  DBISAMSession := TDBISAMSession.Create(Self);
end;

destructor TDBISAMOpenDatabase.Destroy;
begin
  inherited Destroy;
  FMRUServers.Free;
  FMRUDatabases.Free;
end;

function TDBISAMOpenDatabase.Execute(ADBISAMSession: TDBISAMSession;
  var DatabaseURL: String): Boolean;
begin
  DBISAMSession.SessionName := 'OpenDatabase';
  if ADBISAMSession <> nil then
  begin
    DBISAMSession.RemoteUser := ADBISAMSession.RemoteUser;
    DBISAMSession.RemotePassword := ADBISAMSession.RemotePassword;
    DBISAMSession.RemotePort := ADBISAMSession.RemotePort;
    DBISAMSession.OnRemoteLogin := ADBISAMSession.OnRemoteLogin;
  end;
  DecodeDatabaseURL(DatabaseURL, FConnectionType, FRemoteHost, FDatabaseName);
  DataToForm;
  Result := ShowModal = mrOK;
  if Result then
    DatabaseURL := EncodeDatabaseURL(FConnectionType, FRemoteHost, FDatabaseName);
end;

procedure TDBISAMOpenDatabase.DataToForm;
begin
  rbLocal.Enabled := True;
  rbRemote.Enabled := True;
  rbLocal.Checked := FConnectionType = ctLocal;
  rbRemote.Checked := not rbLocal.Checked;

  cbxRemoteType.Visible := rbRemote.Checked;
  cbxRemoteType.ItemIndex := 0;
  if FConnectionType = ctInternet then
    cbxRemoteType.ItemIndex := 1;

  FLastServer := '/////';

  ConnectionTypeChange(Self);
end;

procedure TDBISAMOpenDatabase.FormToData;
begin
  FDatabaseName := cbxAvailableDatabases.Text;
  if rbRemote.Checked then begin
    if cbxRemoteType.ItemIndex = 0 then
      FConnectionType := ctLAN
    else FConnectionType := ctInternet;
    FRemoteHost := cbxServer.Text;
  end else begin
    FConnectionType := ctLocal;
    FRemoteHost := '';
  end;
end;

procedure TDBISAMOpenDatabase.LoadDatabases;
begin
  btnSelect.Visible := rbLocal.Checked;
  cbxServer.Enabled := rbRemote.Checked;
  lblServer.Enabled := rbRemote.Checked;

  with cbxAvailableDatabases do
  begin
    if rbRemote.Checked then begin
      DBISAMSession.Active := False;
      Items.Clear;
      Style := csDropDownList;
      if (cbxServer.Text = '') and (cbxServer.Items.Count > 0) then
        cbxServer.ItemIndex := 0;
      if cbxServer.Text <> '' then
      begin
        DBISAMSession.SessionType := stRemote;
        DBISAMSession.RemoteHost := cbxServer.Text;
        {$IFDEF DBISAM_V4}
        if cbxRemoteType.ItemIndex = 0 then
          DBISAMSession.RemoteCompression := 0
        else DBISAMSession.RemoteCompression := INTERNET_COMPRESSION;
        {$ELSE}
        if cbxRemoteType.ItemIndex = 0 then
          DBISAMSession.RemoteType := rtLAN
        else DBISAMSession.RemoteType := rtInternet;
        {$ENDIF}
        try
          DBISAMSession.Active := True;
          DBISAMSession.GetRemoteDatabaseNames(Items);
        except
          // We don't care about exceptions here
        end;
      end;
      FLastServer := cbxServer.Text;
      DBISAMSession.Active := False;
    end else begin
      if (FConnectionType = ctLocal) and (FDatabaseName <> '') then
        MRUDatabases.Add(FDatabaseName);
      Style := csDropDown;
      Items.Assign(MRUDatabases);
    end;
    ItemIndex := Items.IndexOf(FDatabaseName);
    if ItemIndex < 0 then ItemIndex := 0;
  end;
  cbxAvailableDatabasesChange(nil);
end;


procedure TDBISAMOpenDatabase.btnSelectClick(Sender: TObject);
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

procedure TDBISAMOpenDatabase.ConnectionTypeChange(Sender: TObject);
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

procedure TDBISAMOpenDatabase.cbxServerChange(Sender: TObject);
begin
  if rbRemote.checked and (cbxServer.Text <> FLastServer) then
    LoadDatabases;
end;

procedure TDBISAMOpenDatabase.cbxAvailableDatabasesChange(Sender: TObject);
begin
  // Database Changed
end;

procedure TDBISAMOpenDatabase.FormCloseQuery(Sender: TObject;
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

{ TDBISAMOpenDatabaseDialog }

constructor TDBISAMOpenDatabaseDialog.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FDatabaseURL := '';
  FDBISAMSession := nil;
  FMRUServers := TStringList.Create;
  TStringList(FMRUServers).Duplicates := dupIgnore;
  TStringList(FMRUServers).Sorted := True;
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
  FTitle := '';
end;

destructor TDBISAMOpenDatabaseDialog.Destroy;
begin
  inherited Destroy;
  FMRUServers.Free;
  FMRUDatabases.Free;
end;

function TDBISAMOpenDatabaseDialog.Execute: Boolean;
begin
  with TDBISAMOpenDatabase.Create(Application.MainForm) do
  try
    if FTitle <> '' then
      Caption := FTitle;
    MRUServers.Assign(Self.MRUServers);
    MRUDatabases.Assign(Self.MRUDatabases);
    Result := Execute(FDBISAMSession, FDatabaseURL);
    if Result then begin
      Self.MRUServers.Assign(MRUServers);
      Self.MRUDatabases.Assign(MRUDatabases);
    end;
  finally
    Free;
  end;
end;

procedure TDBISAMOpenDatabaseDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FDBISAMSession) and (Operation = opRemove) then
    FDBISAMSession := nil;
  inherited;
end;

procedure TDBISAMOpenDatabaseDialog.SetMRUDatabases(const Value: TStrings);
begin
  FMRUDatabases.Assign(Value);
end;

procedure TDBISAMOpenDatabaseDialog.SetMRUServers(const Value: TStrings);
begin
  FMRUServers.Assign(Value);
end;

end.
