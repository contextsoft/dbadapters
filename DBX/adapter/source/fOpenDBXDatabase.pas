unit fOpenDBXDatabase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SQLExpr, dbSchema, DBXpress, DB;

type
  TfrmOpenDBXDatabase = class(TForm)
    Label1: TLabel;
    cbxDriverName: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    edtHostName: TEdit;
    edtDatabaseName: TEdit;
    Bevel1: TBevel;
    SQLConnection1: TSQLConnection;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FConnectionStr: String;
  public
    { Public declarations }
  end;

var
  frmOpenDBXDatabase: TfrmOpenDBXDatabase;

  function SelectDBXDatabase(var ConnectionStr: String): Boolean;

resourcestring
  SNoDriverSelected = 'Please select a driver';
  SInvalidDatabaseName = 'Please specify a valid database name';

implementation

{$R *.dfm}

function SelectDBXDatabase(var ConnectionStr: String): Boolean;
begin
  with TfrmOpenDBXDatabase.Create(nil) do
  try
    FConnectionStr := ConnectionStr;
    Result := ShowModal = mrOK;
    if Result then
      ConnectionStr := FConnectionStr;
  finally
    Free;
  end;
end;

procedure TfrmOpenDBXDatabase.FormCreate(Sender: TObject);
begin
  GetDriverNames(cbxDriverName.Items, False);
end;

procedure TfrmOpenDBXDatabase.FormShow(Sender: TObject);
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  DecodeDatabaseURL(FConnectionStr, ConnectionType, RemoteHost, DatabaseName);
  with cbxDriverName do ItemIndex := Items.IndexOf(ConnectionType);
  edtHostName.Text := RemoteHost;
  edtDatabaseName.Text := DatabaseName;
end;

procedure TfrmOpenDBXDatabase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  if ModalResult = mrOK then
  begin
    if cbxDriverName.ItemIndex < 0 then
      raise Exception.Create(SNoDriverSelected);

    ConnectionType := cbxDriverName.Text;
    DatabaseName := Trim(edtDatabaseName.Text);
    RemoteHost := Trim(edtHostName.Text);

    if DatabaseName = '' then
      raise Exception.Create(SInvalidDatabaseName);

    FConnectionStr := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
  end;
end;

end.
