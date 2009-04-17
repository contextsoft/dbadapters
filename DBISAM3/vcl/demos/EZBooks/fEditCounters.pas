unit fEditCounters;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBISAMTb, DBISAMExt, StdCtrls, Buttons, ExtCtrls, Mask, DBCtrls;

type
  TfrmCounters = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    tblData: TDBISAMTableExt;
    dsData: TDataSource;
    tblDataCustomerID: TIntegerField;
    tblDataItemID: TIntegerField;
    tblDataOrderID: TIntegerField;
    tblDataPaymentID: TIntegerField;
    tblDataOrderLineID: TIntegerField;
    tblDataPaymentLineID: TIntegerField;
    GroupBox1: TGroupBox;
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
    tblDataSnapshotID: TIntegerField;
    Label7: TLabel;
    DBEdit7: TDBEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCounters: TfrmCounters;

implementation

{$R *.DFM}

procedure TfrmCounters.FormCreate(Sender: TObject);
begin
  tblData.Active := True;
end;

procedure TfrmCounters.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) and tblData.Modified then
    tblData.Post;
  if tblData.State in dsEditModes then
    tblData.Cancel;
end;

end.
