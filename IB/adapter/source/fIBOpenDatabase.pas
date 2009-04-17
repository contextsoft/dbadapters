unit fIBOpenDatabase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, IBDatabase, ExtCtrls, StdCtrls;

type
  TfrmIBOpenDatabase = class(TForm)
    rbLocal: TRadioButton;
    rbRemote: TRadioButton;
    Label1: TLabel;
    edtServer: TEdit;
    Label2: TLabel;
    cbxProtocol: TComboBox;
    Label3: TLabel;
    edtDatabase: TEdit;
    btnBrowse: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    OpenDialog: TOpenDialog;
    IBDatabase1: TIBDatabase;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ConnectionTypeChanged(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FConnectionStr: String;

    procedure DataToForm;
    procedure FormToData;
    procedure UpdateControls;
  end;

var
  frmIBOpenDatabase: TfrmIBOpenDatabase;


  function EditConnectionString(var ConnectionStr: String): Boolean;

implementation

{$R *.dfm}

uses dbSchema;

{ TCP: localhost:mydb, NamedPipes: //localhost/mydb, SPX: localhost@mydb }

function EditConnectionString(var ConnectionStr: String): Boolean;
begin
  with TfrmIBOpenDatabase.Create(nil) do
  try
    FConnectionStr := ConnectionStr;
    DataToForm;
    Result := ShowModal = mrOK;
    if Result then
      ConnectionStr := FConnectionStr;
  finally
    Free;
  end;
end;

procedure TfrmIBOpenDatabase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
    FormToData;
end;

procedure TfrmIBOpenDatabase.FormToData;
begin
  if rbRemote.Checked then
  case cbxProtocol.ItemIndex of
    0: begin
      // TCP
      FConnectionStr := edtServer.Text + ':' + edtDatabase.Text;
    end;
    1: begin
      // NamedPipes
      FConnectionStr := '//' + edtServer.Text + '/' + edtDatabase.Text;
    end;
    2: begin
      // SPX
      FConnectionStr := edtServer.Text + '@' + edtDatabase.Text;
    end;
  end else
    FConnectionStr := edtDatabase.Text;
end;


procedure TfrmIBOpenDatabase.DataToForm;
var
  P: Integer;
begin
  if AnsiPos('//', FConnectionStr) = 1 then
  begin
    cbxProtocol.ItemIndex := 1; // NamedPipes
    rbRemote.Checked := True;
    P := 3;
    edtServer.Text := dbSchema.NextToken(FConnectionStr, '/', P);
    edtDatabase.Text := copy(FConnectionStr, P, MaxInt);
  end else begin
    P := AnsiPos('@', FConnectionStr);
    if P > 0 then
    begin
      rbRemote.Checked := True;
      cbxProtocol.ItemIndex := 2; // SPX
      edtServer.Text := copy(FConnectionStr, 1, P - 1);
      edtDatabase.Text := copy(FConnectionStr, P + 1, MaxInt);
    end else begin
      cbxProtocol.ItemIndex := 0; // TCP
      P := AnsiPos(':', FConnectionStr);
      if (P > 0) and (P < Length(FConnectionStr)) and (FConnectionStr[P + 1] <> '\' ) then
      begin
        // remote
        rbRemote.Checked := True;
        edtServer.Text := copy(FConnectionStr, 1, P - 1);
        edtDatabase.Text := copy(FConnectionStr, P + 1, MaxInt);
      end else begin
        // local
        rbLocal.Checked := True;
        edtServer.Text := '';
        edtDatabase.Text := FConnectionStr;
      end;
    end;
  end;
  UpdateControls;
end;

procedure TfrmIBOpenDatabase.UpdateControls;
begin
  edtServer.Enabled := rbRemote.Checked;
  cbxProtocol.Enabled := rbRemote.Checked;
end;

procedure TfrmIBOpenDatabase.ConnectionTypeChanged(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmIBOpenDatabase.btnBrowseClick(Sender: TObject);
begin
  OpenDialog.FileName := edtDatabase.Text;
  if OpenDialog.Execute then
    edtDatabase.Text := OpenDialog.FileName;
end;

end.
