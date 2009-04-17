unit fEditPayments;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fEdit, Db, StdCtrls, Buttons, ExtCtrls, DBCtrls, Mask, Grids, DBGrids,
  dbSchema, nxdb, nxdbext, DbMemDS, dbDocument;

type
  TfrmEditPayment = class(TfrmEditObject)
    tblDataCustomerID: TIntegerField;
    tblDataDateCreated: TDateField;
    tblDataFormOfPayment: TStringField;
    tblDataReference: TStringField;
    tblDataTotalAmount: TCurrencyField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    tblDataCustomer: TStringField;
    Label7: TLabel;
    DBLookupComboBox1: TDBLookupComboBox;
    tblDataFOP: TStringField;
    Label2: TLabel;
    DBLookupComboBox2: TDBLookupComboBox;
    DBGrid1: TDBGrid;
    Label4: TLabel;
    docPayment: TDBDocument;
    Label8: TLabel;
    DBEdit2: TDBEdit;
    Label9: TLabel;
    DBEdit4: TDBEdit;
    tblPaymentLines: TDbMemDataset;
    dsPaymentLines: TDataSource;
    tblPaymentLinesPaymentID: TIntegerField;
    tblPaymentLinesOrderID: TIntegerField;
    tblPaymentLinesAmount: TCurrencyField;
    btnSelectCustomer: TBitBtn;
    tblPaymentLinesBalance: TCurrencyField;
    tblPaymentLinesDescription: TStringField;
    tblOrders: TnxTableExt;
    tblDataCreditedToAccount: TCurrencyField;
    tblDataAppliedToOrders: TCurrencyField;
    DBNavigator1: TDBNavigator;
    tblPaymentLinesPaymentLineID: TIntegerField;
    tblDataPaymentID: TIntegerField;
    procedure btnSelectCustomerClick(Sender: TObject);
    procedure dsDataDataChange(Sender: TObject; Field: TField);
    procedure dsPaymentLinesStateChange(Sender: TObject);
    procedure tblPaymentLinesBeforePost(DataSet: TDataSet);
    procedure LockMaster(DataSet: TDataSet);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  protected
    procedure InternalNew; override;
    procedure InternalOpen; override;
    procedure InternalSave; override;
  public
    { Public declarations }
    class function KeyFields: String; override;
    class function TableName: String; override;
    class function DisplayName: String; override;
    function IsModified: Boolean; override;
  end;

var
  frmEditPayment: TfrmEditPayment;

implementation

uses dReferences, fBrowseCustomers, dbExtUtils, fMain;

{$R *.DFM}

{ TfrmEditPayment }

class function TfrmEditPayment.DisplayName: String;
begin
  Result := 'payment';
end;

class function TfrmEditPayment.KeyFields: String;
begin
  Result := 'PaymentID';
end;

class function TfrmEditPayment.TableName: String;
begin
  Result := 'Payments';
end;

procedure TfrmEditPayment.InternalNew;
begin
  docPayment.New;
  tblDataDateCreated.Value := Date;
end;

procedure TfrmEditPayment.InternalOpen;
begin
  docPayment.Active := False;
  docPayment.Open(KeyValue);
end;

procedure TfrmEditPayment.InternalSave;
begin
  tblPaymentLines.Filtered := True;
  try
    docPayment.Save;
    KeyValue := tblData.FieldValues[KeyFields];
  finally
    tblPaymentLines.Filtered := False;
  end;
end;

procedure TfrmEditPayment.btnSelectCustomerClick(Sender: TObject);
begin
  TfrmBrowseCustomers.Select(tblDataCustomerID);
end;

procedure TfrmEditPayment.dsDataDataChange(Sender: TObject; Field: TField);
begin
  if Field = tblDataCustomerID then
    CopyDataSet(tblOrders, tblPaymentLines)
  else if Field = tblDataTotalAmount then
    tblDataCreditedToAccount.Value := tblDataTotalAmount.Value - tblDataAppliedToOrders.Value;
  docPayment.Modified := True;
end;

procedure TfrmEditPayment.dsPaymentLinesStateChange(Sender: TObject);
begin
  if (tblPaymentLines.State = dsBrowse) and docPayment.Active then
  begin
    tblData.Edit;
    tblDataAppliedToOrders.Value := CalcSum(tblPaymentLinesAmount);
    tblDataCreditedToAccount.Value := tblDataTotalAmount.Value - tblDataAppliedToOrders.Value;
  end;
end;

procedure TfrmEditPayment.tblPaymentLinesBeforePost(DataSet: TDataSet);
begin
  inherited;
  if (tblPaymentLines.State = dsInsert) and (tblPaymentLinesPaymentLineID.IsNull) then
    tblPaymentLinesPaymentLineID.AsInteger := dmReferences.GetNextID('PaymentLineID');
  docPayment.Modified := True;
end;

function TfrmEditPayment.IsModified: Boolean;
begin
  Result := docPayment.Modified;
end;

procedure TfrmEditPayment.LockMaster(DataSet: TDataSet);
begin
  inherited;
  if docPayment.Active then
    tblData.Edit;
end;

procedure TfrmEditPayment.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) and (tblPaymentLines.State in dsEditModes) then
    tblPaymentLines.Post
  else if tblPaymentLines.State in dsEditModes then
    tblPaymentLines.Cancel;
  inherited;
end;

end.
