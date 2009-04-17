(******************************************************************************)
(*
(*  Context Database Extensions Suite (ElevateDB)
(*
(*  Open EDB Database Dialog component.
(*  Contains:
(*      TEDBOpenDatabase
(*      TEDBOpenDatabaseDialog
(*
(*  Copyright (c) 2004-2006 Michael Baytalsky
(*
(******************************************************************************)
unit EDBOpenDatabase;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, EDBComps, Db;

type
  TEDBOpenDatabase = class(TForm)
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
    EDBSession: TEDBSession;

    procedure LoadDatabases; virtual;
    procedure DataToForm; virtual;
    procedure FormToData; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function Execute(AEDBSession: TEDBSession; var DatabaseURL: String): Boolean;

    property MRUServers: TStrings read FMRUServers;
    property MRUDatabases: TStrings read FMRUDatabases;
  end;

  {:$ TEDBOpenDatabaseDialog displays database selection dialog. }
  {:: TEDBOpenDatabaseDialog displays a modal dialog box for selecting and opening EDB Databases. }
  {:: The dialog does not appear at runtime until it is activated by a call to the Execute method. }
  {:: When the user clicks OK, the dialog closes and the selected database path is stored }
  {:: in the DatabaseURL property.}
  TEDBOpenDatabaseDialog = class(TComponent)
  protected
    FDatabaseURL: String;
    FEDBSession: TEDBSession;
    FMRUServers: TStrings;
    FMRUDatabases: TStrings;
    FTitle: String;

    procedure SetMRUDatabases(const Value: TStrings);
    procedure SetMRUServers(const Value: TStrings);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Creates an instance of TEDBOpenDatabaseDialog component. }
    constructor Create(Owner: TComponent); override;
    {:$ Destroys the instance of TEDBOpenDatabaseDialog component. }
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
    property EDBSession: TEDBSession read FEDBSession write FEDBSession;
    {:$ Specifies the title for the open database dialog. }
    {:: Default title is 'Open Database'. }
    property Title: String read FTitle write FTitle;
  end;

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

uses ShellAPI, ShlObj, EDBExt, dbSchema;

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

{ TEDBOpenDatabase }

constructor TEDBOpenDatabase.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FMRUServers := TStringList.Create;
  TStringList(FMRUServers).Duplicates := dupIgnore;
  TStringList(FMRUServers).Sorted := True;
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
  EDBSession := TEDBSession.Create(Self);
end;

destructor TEDBOpenDatabase.Destroy;
begin
  inherited Destroy;
  FMRUServers.Free;
  FMRUDatabases.Free;
end;

function TEDBOpenDatabase.Execute(AEDBSession: TEDBSession;
  var DatabaseURL: String): Boolean;
begin
  EDBSession.SessionName := 'OpenDatabase';
  if AEDBSession <> nil then
  begin
    EDBSession.LoginUser := AEDBSession.LoginUser;
    EDBSession.LoginPassword := AEDBSession.LoginPassword;
    EDBSession.RemotePort := AEDBSession.RemotePort;
    EDBSession.OnLogin := AEDBSession.OnLogin;
  end;
  DecodeDatabaseURL(DatabaseURL, FConnectionType, FRemoteHost, FDatabaseName);
  DataToForm;
  Result := ShowModal = mrOK;
  if Result then
    DatabaseURL := EncodeDatabaseURL(FConnectionType, FRemoteHost, FDatabaseName);
end;

procedure TEDBOpenDatabase.DataToForm;
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

procedure TEDBOpenDatabase.FormToData;
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

procedure TEDBOpenDatabase.LoadDatabases;
var
  P: Integer;
  RemoteHost, RemotePort: String;
begin
  btnSelect.Visible := rbLocal.Checked;
  cbxServer.Enabled := rbRemote.Checked;
  lblServer.Enabled := rbRemote.Checked;

  with cbxAvailableDatabases do
  begin
    if rbRemote.Checked then begin
      EDBSession.Connected := False;
      Items.Clear;
      Style := csDropDownList;
      if (cbxServer.Text = '') and (cbxServer.Items.Count > 0) then
        cbxServer.ItemIndex := 0;
      if cbxServer.Text <> '' then
      begin
        EDBSession.SessionType := stRemote;
        RemoteHost := Trim(cbxServer.Text);

        P := Pos(':', RemoteHost);
        if P > 0 then
        begin
          RemotePort := copy(RemoteHost, P+1, MaxInt);
          RemoteHost := copy(RemoteHost, 1, P - 1);
          EDBSession.RemotePort := StrToIntDef(RemotePort, EDBExt.DEFAULT_PORT);
        end;

        EDBSession.RemoteHost := RemoteHost;
        if cbxRemoteType.ItemIndex = 0 then
          EDBSession.RemoteCompression := 0
        else EDBSession.RemoteCompression := INTERNET_COMPRESSION;
        try
          EDBSession.Connected := True;
          EDBSession.GetDatabaseNames(Items);
        except
          // We don't care about exceptions here
        end;
      end;
      FLastServer := cbxServer.Text;
      EDBSession.Connected := False;
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


procedure TEDBOpenDatabase.btnSelectClick(Sender: TObject);
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

procedure TEDBOpenDatabase.ConnectionTypeChange(Sender: TObject);
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

procedure TEDBOpenDatabase.cbxServerChange(Sender: TObject);
begin
  if rbRemote.checked and (cbxServer.Text <> FLastServer) then
    LoadDatabases;
end;

procedure TEDBOpenDatabase.cbxAvailableDatabasesChange(Sender: TObject);
begin
  // Database Changed
end;

procedure TEDBOpenDatabase.FormCloseQuery(Sender: TObject;
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

{ TEDBOpenDatabaseDialog }

constructor TEDBOpenDatabaseDialog.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FDatabaseURL := '';
  FEDBSession := nil;
  FMRUServers := TStringList.Create;
  TStringList(FMRUServers).Duplicates := dupIgnore;
  TStringList(FMRUServers).Sorted := True;
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
  FTitle := '';
end;

destructor TEDBOpenDatabaseDialog.Destroy;
begin
  inherited Destroy;
  FMRUServers.Free;
  FMRUDatabases.Free;
end;

function TEDBOpenDatabaseDialog.Execute: Boolean;
begin
  with TEDBOpenDatabase.Create(Application.MainForm) do
  try
    if FTitle <> '' then
      Caption := FTitle;
    MRUServers.Assign(Self.MRUServers);
    MRUDatabases.Assign(Self.MRUDatabases);
    Result := Execute(FEDBSession, FDatabaseURL);
    if Result then begin
      Self.MRUServers.Assign(MRUServers);
      Self.MRUDatabases.Assign(MRUDatabases);
    end;
  finally
    Free;
  end;
end;

procedure TEDBOpenDatabaseDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FEDBSession) and (Operation = opRemove) then
    FEDBSession := nil;
  inherited;
end;

procedure TEDBOpenDatabaseDialog.SetMRUDatabases(const Value: TStrings);
begin
  FMRUDatabases.Assign(Value);
end;

procedure TEDBOpenDatabaseDialog.SetMRUServers(const Value: TStrings);
begin
  FMRUServers.Assign(Value);
end;

end.
