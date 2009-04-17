unit fMySQLOpenDatabase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DBAccess, MyDacVcl, DB, MyAccess;

type
  TfrmMySQLOpenDatabase = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edtHost: TEdit;
    edtDatabase: TEdit;
    Bevel1: TBevel;
    MyConnection1: TMyConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMySQLOpenDatabase: TfrmMySQLOpenDatabase;

  function EditConnectionString(var ConnectionStr: String): Boolean;

implementation

{$R *.dfm}

uses dbSchema;

function EditConnectionString(var ConnectionStr: String): Boolean;
var
  ConnectionType, RemoteHost, DatabaseName: String;
begin
  with TfrmMySQLOpenDatabase.Create(nil) do
  try
    DecodeDatabaseURL(ConnectionStr, ConnectionType, RemoteHost, DatabaseName);
    edtHost.Text := RemoteHost;
    edtDatabase.Text := DatabaseName;
    Result := ShowModal = mrOK;
    if Result then
      ConnectionStr := EncodeDatabaseURL('MySQL', edtHost.Text, edtDatabase.Text);
  finally
    Free;
  end;
end;

end.
