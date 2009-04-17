unit fBDEOpenDatabase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  CtxBDEAdapter, Dialogs, ExtCtrls, DB, BDEExt, dbSchema, DBTables, StdCtrls;

type
  TfrmBDEOpenDatabase = class(TForm)
    cbxAliasDriver: TComboBox;
    Label3: TLabel;
    rgConnectionType: TRadioGroup;
    pnlPath: TPanel;
    lblPathDSN: TLabel;
    edtPath: TEdit;
    btnBrowse: TButton;
    cbxODBC: TComboBox;
    Label1: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    OpenDialog: TOpenDialog;
    procedure cbxAliasDriverChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgConnectionTypeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FConnectionStr: String;
    FSelectFolder: Boolean;
    FKeyParam: String;
  public
    { Public declarations }
  end;

var
  frmBDEOpenDatabase: TfrmBDEOpenDatabase;

  function SelectDatabase(var ConnectionStr: OleVariant): Boolean;

resourcestring
  SDriverAliasEmpty = 'Please select a valid database driver or alias';
  SDriverIsNotSupported = 'This type of driver is not supported';
  
implementation

{$R *.dfm}

uses FileCtrl, dbEngProfile, Registry;

function SelectDatabase(var ConnectionStr: OleVariant): Boolean;
begin
  with TfrmBDEOpenDatabase.Create(nil) do
  try
    FConnectionStr := VarToStr(ConnectionStr);
    Result := ShowModal = mrOK;
    if Result then
      ConnectionStr := FConnectionStr;
  finally
    Free;
  end;
end;

procedure TfrmBDEOpenDatabase.cbxAliasDriverChange(Sender: TObject);
var
  List: TStringList;
begin
  if not pnlPath.Visible or (cbxAliasDriver.ItemIndex < 0) then exit;
  List := TStringList.Create;
  try
    DBTables.Session.GetDriverParams(cbxAliasDriver.Text, List);
    FKeyParam := GetBDEKeyParam(List);
    if AnsiSameText(FKeyParam, 'ODBC DSN') then
    begin
      edtPath.Visible := False;
      btnBrowse.Visible := False;
      cbxODBC.Visible := True;
    end else begin
      edtPath.Visible := True;
      btnBrowse.Visible := True;
      cbxODBC.Visible := False;
      FSelectFolder := AnsiSameText(FKeyParam, 'PATH');
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmBDEOpenDatabase.btnBrowseClick(Sender: TObject);
var
  Directory: String;
begin
  if FSelectFolder then
  begin
    Directory := edtPath.Text;
    if SelectDirectory('Select Database Directory', '', Directory) then
      edtPath.Text := Directory;
  end else begin
    if OpenDialog.Execute then
      edtPath.Text := OpenDialog.FileName;
  end;
end;

procedure TfrmBDEOpenDatabase.FormCreate(Sender: TObject);
var
  List: TStrings;
begin
  List := TStringList.Create;
  with TRegIniFile.Create('Software\ODBC\ODBC.INI') do
  try
    ReadSection('ODBC Data Sources', List);
    cbxODBC.Items.AddStrings(List);
    RootKey := HKey_Local_Machine;
    OpenKey('Software\ODBC\ODBC.INI', True);
    ReadSection('ODBC Data Sources', List);
    cbxODBC.Items.AddStrings(List);
  finally
    Free;
    List.Free;
  end;
end;

procedure TfrmBDEOpenDatabase.FormShow(Sender: TObject);
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  if cbxODBC.Items.Count > 0 then
    cbxODBC.ItemIndex := 0;

  DecodeDatabaseURL(FConnectionStr, ConnectionType, RemoteHost, DatabaseName);
  // Something like Decode database url
  if AnsiSameText(ConnectionType, 'BDE') then
  begin
    rgConnectionType.ItemIndex := 0;
    rgConnectionTypeClick(nil);
    with cbxAliasDriver do
      ItemIndex := Items.IndexOf(DatabaseName);
  end else
  begin
    rgConnectionType.ItemIndex := 1;
    rgConnectionTypeClick(nil);
    with cbxAliasDriver do
      ItemIndex := Items.IndexOf(ConnectionType);
    cbxAliasDriverChange(nil);
    if cbxODBC.Visible then
      with cbxODBC do
        ItemIndex := Items.IndexOf(DatabaseName)
    else edtPath.Text := DatabaseName;
  end;
end;

procedure TfrmBDEOpenDatabase.rgConnectionTypeClick(Sender: TObject);
begin
  // Fill Aliases/Drivers depending on selection
  if rgConnectionType.ItemIndex = 0 then
    DBTables.Session.GetAliasNames(cbxAliasDriver.Items)
  else DBTables.Session.GetDriverNames(cbxAliasDriver.Items);

  pnlPath.Visible := rgConnectionType.ItemIndex = 1;
  if cbxAliasDriver.Items.Count > 0 then
    cbxAliasDriver.ItemIndex := 0;
  cbxAliasDriverChange(nil);
end;

procedure TfrmBDEOpenDatabase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  if ModalResult = mrOK then
  begin
    if cbxAliasDriver.ItemIndex < 0 then
      raise Exception.Create(SDriverAliasEmpty);

    // Something like Encode database url
    if rgConnectionType.ItemIndex = 0 then
    begin
      ConnectionType := 'BDE';
      RemoteHost := 'ALIAS';
      DatabaseName := cbxAliasDriver.Text;
    end else
    begin
      if FKeyParam = '' then
        raise Exception.Create(SDriverIsNotSupported);

      ConnectionType := cbxAliasDriver.Text;
      RemoteHost := FKeyParam;
      if cbxODBC.Visible then
        DatabaseName := cbxODBC.Text
      else DatabaseName := edtPath.Text;
    end;
    FConnectionStr := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
  end;
end;

end.
