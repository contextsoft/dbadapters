unit fEditCompanyInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, DBCtrls, Db, DBISAMTb, DBISAMExt, Buttons,
  ExtCtrls;

type
  TfrmEditCompanyInfo = class(TForm)
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    Label7: TLabel;
    DBEdit7: TDBEdit;
    Label8: TLabel;
    DBEdit8: TDBEdit;
    Label9: TLabel;
    DBEdit9: TDBEdit;
    Label10: TLabel;
    DBEdit10: TDBEdit;
    Panel1: TPanel;
    Bevel1: TBevel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dsData: TDataSource;
    tblData: TDBISAMTableExt;
    tblDataCompanyName: TStringField;
    tblDataStreetAddress: TStringField;
    tblDataCity: TStringField;
    tblDataState: TStringField;
    tblDataZip: TStringField;
    tblDataCountry: TStringField;
    tblDataPhone: TStringField;
    tblDataFax: TStringField;
    tblDataEmail: TStringField;
    tblDataEID: TStringField;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SaveChanges(Confirm: Boolean = True);
  end;

var
  frmEditCompanyInfo: TfrmEditCompanyInfo;

implementation

{$R *.DFM}

resourcestring
  SConfirmSaveChanges = 'Would you like to save changes?';

procedure TfrmEditCompanyInfo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  SaveChanges(ModalResult <> mrOK);
end;

procedure TfrmEditCompanyInfo.SaveChanges(Confirm: Boolean);
begin
  if tblData.Modified then
  begin
    if Confirm then
      case MessageDlg(SConfirmSaveChanges, mtConfirmation, [mbYes,mbNo,mbCancel], 0)
      of
        mrYes: tblData.Post;
        mrNo: tblData.Cancel;
        else Abort;
      end
    else tblData.Post;
  end;
end;

procedure TfrmEditCompanyInfo.FormShow(Sender: TObject);
begin
  if tblData.EOF then
    tblData.Insert;
end;

procedure TfrmEditCompanyInfo.FormCreate(Sender: TObject);
begin
  tblData.Active := True;
end;

end.
