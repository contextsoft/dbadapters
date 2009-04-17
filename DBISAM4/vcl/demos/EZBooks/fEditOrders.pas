unit fEditOrders;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fEdit, Db, DBISAMTb, DBISAMExt, StdCtrls, Buttons, ExtCtrls, DBCtrls,
  Grids, DBGrids, ComCtrls, Mask, dbDocument, dbExtUtils, dbSchema;

type
  TfrmEditOrders = class(TfrmEditObject)
    tblDataDateCreated: TDateField;
    tblDataCustomerID: TIntegerField;
    tblDataStatus: TSmallintField;
    tblDataNotes: TMemoField;
    tblDataPayOffDate: TDateField;
    tblDataSalesRep: TStringField;
    tblDataTax: TFloatField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    Label6: TLabel;
    DBEdit5: TDBEdit;
    Label7: TLabel;
    Label8: TLabel;
    DBEdit7: TDBEdit;
    PageControl: TPageControl;
    tsDetails: TTabSheet;
    tsPayments: TTabSheet;
    grdOrderLines: TDBGrid;
    grdPayments: TDBGrid;
    btnSelectCustomer: TBitBtn;
    DBEdit6: TDBEdit;
    DBEdit9: TDBEdit;
    Label5: TLabel;
    Label9: TLabel;
    DBEdit10: TDBEdit;
    Label10: TLabel;
    DBEdit11: TDBEdit;
    DBMemo1: TDBMemo;
    Label11: TLabel;
    tblOrderLines: TDBISAMTableExt;
    tblPaymentLines: TDBISAMTableExt;
    dsOrderLines: TDataSource;
    dsPaymentLines: TDataSource;
    tblOrderLinesOrderID: TIntegerField;
    tblOrderLinesItemID: TIntegerField;
    tblOrderLinesDescription: TStringField;
    tblOrderLinesSalePrice: TCurrencyField;
    tblOrderLinesQuantity: TFloatField;
    tblOrderLinesTaxable: TBooleanField;
    tblOrderLinesCharges: TCurrencyField;
    tblPaymentLinesPaymentID: TIntegerField;
    tblPaymentLinesOrderID: TIntegerField;
    tblPaymentLinesAmount: TCurrencyField;
    tblPayments: TDBISAMTableExt;
    tblPaymentLinesPaymentDate: TDateField;
    tblPaymentLinesFormOfPayment: TStringField;
    tblPaymentLinesReference: TStringField;
    tblPaymentsCustomerID: TIntegerField;
    tblPaymentsDateCreated: TDateField;
    tblPaymentsFormOfPayment: TStringField;
    tblPaymentsReference: TStringField;
    tblPaymentsTotalAmount: TCurrencyField;
    tblDataOrderStatus: TStringField;
    DBLookupComboBox2: TDBLookupComboBox;
    tblDataTaxTypeLookup: TStringField;
    DBLookupComboBox1: TDBLookupComboBox;
    tblDataCustomer: TStringField;
    docOrder: TDBDocument;
    tblOrderLinesSalePriceLookup: TCurrencyField;
    tblOrderLinesTaxableLookup: TBooleanField;
    Label12: TLabel;
    DBEdit4: TDBEdit;
    tblDataDescription: TStringField;
    Panel2: TPanel;
    DBNavigator1: TDBNavigator;
    tblOrderLinesOrderLineID: TIntegerField;
    tblOrderLinesTaxableSale: TCurrencyField;
    tblOrderLinesTax: TCurrencyField;
    tblDataTotalTax: TCurrencyField;
    tblDataTotalSales: TCurrencyField;
    tblDataTotalCharges: TCurrencyField;
    tblDataTotalBalance: TCurrencyField;
    tblDataTaxLookup: TFloatField;
    tblPaymentLinesPaymentLineID: TIntegerField;
    tblDataOrderID: TIntegerField;
    tblPaymentsPaymentID: TIntegerField;
    tblPaymentsCreditedToAccount: TCurrencyField;
    tblPaymentsAppliedToOrders: TCurrencyField;
    tblDataPaymentLookup: TCurrencyField;
    tblDataTaxType: TStringField;
    procedure btnSelectCustomerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dsOrderLinesDataChange(Sender: TObject; Field: TField);
    procedure dsOrderLinesStateChange(Sender: TObject);
    procedure tblOrderLinesCalcFields(DataSet: TDataSet);
    procedure tblDataCalcFields(DataSet: TDataSet);
    procedure dsDataDataChange(Sender: TObject; Field: TField);
    procedure tblOrderLinesBeforePost(DataSet: TDataSet);
    procedure LockMaster(DataSet: TDataSet);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure grdPaymentsDblClick(Sender: TObject);
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
  frmEditOrders: TfrmEditOrders;

implementation

uses dReferences, fBrowseCustomers, fEditPayments;

{$R *.DFM}

class function TfrmEditOrders.DisplayName: String;
begin
  Result := 'order';
end;

class function TfrmEditOrders.KeyFields: String;
begin
  Result := 'OrderID';
end;

class function TfrmEditOrders.TableName: String;
begin
  Result := 'Orders';
end;

procedure TfrmEditOrders.InternalNew;
begin
  docOrder.New;
  tblDataDateCreated.Value := Date;
end;

procedure TfrmEditOrders.InternalOpen;
begin
  docOrder.Active := False;
  docOrder.Open(KeyValue);
end;

procedure TfrmEditOrders.InternalSave;
begin
  docOrder.Save;
  KeyValue := tblData.FieldValues[KeyFields];
end;

procedure TfrmEditOrders.FormCreate(Sender: TObject);
begin
  inherited;
  tblPaymentLines.Active := True;
  PageControl.ActivePageIndex := 0;
end;

procedure TfrmEditOrders.btnSelectCustomerClick(Sender: TObject);
begin
  TfrmBrowseCustomers.Select(tblDataCustomerID);
end;

procedure TfrmEditOrders.dsOrderLinesDataChange(Sender: TObject;
  Field: TField);
begin
  if Field = tblOrderLinesItemID then
  begin
    tblOrderLinesSalePrice.Assign(tblOrderLinesSalePriceLookup);
    tblOrderLinesTaxable.Assign(tblOrderLinesTaxableLookup);
  end else if (Field = tblOrderLinesSalePrice) or (Field = tblOrderLinesQuantity) then
    tblOrderLinesCharges.Value := tblOrderLinesSalePrice.Value * tblOrderLinesQuantity.Value;
end;

procedure TfrmEditOrders.dsOrderLinesStateChange(Sender: TObject);
begin
  if (tblOrderLines.State = dsBrowse) and docOrder.Active then
  begin
    tblData.Edit;
    tblDataTotalSales.Value := CalcSum(tblOrderLinesCharges);
    tblDataTotalTax.Value := CalcSum(tblOrderLinesTax);
    tblDataTotalCharges.Value :=
      tblDataTotalTax.Value + tblDataTotalSales.Value;
  end;
end;

procedure TfrmEditOrders.tblOrderLinesCalcFields(DataSet: TDataSet);
begin
  if tblOrderLinesTaxable.Value then
    tblOrderLinesTaxableSale.Value := tblOrderLinesCharges.Value
  else tblOrderLinesTaxableSale.Value := 0;
  tblOrderLinesTax.Value := tblOrderLinesTaxableSale.Value * tblDataTax.Value / 100;
end;

procedure TfrmEditOrders.tblDataCalcFields(DataSet: TDataSet);
begin
  tblDataTotalBalance.Value :=
    tblDataTotalCharges.Value - tblDataPaymentLookup.Value;
end;

procedure TfrmEditOrders.dsDataDataChange(Sender: TObject; Field: TField);
begin
  inherited;
  if Field = tblDataTaxType then
  begin
    tblDataTax.Value := tblDataTaxLookup.Value;
    dsOrderLinesStateChange(nil);
  end;
  docOrder.Modified := True;
end;

procedure TfrmEditOrders.tblOrderLinesBeforePost(DataSet: TDataSet);
begin
  inherited;
  if (tblOrderLines.State = dsInsert) and (tblOrderLinesOrderLineID.IsNull) then
    tblOrderLinesOrderLineID.AsInteger := dmReferences.GetNextID('OrderLineID');
  docOrder.Modified := True;
end;

function TfrmEditOrders.IsModified: Boolean;
begin
  Result := docOrder.Modified;
end;

procedure TfrmEditOrders.LockMaster(DataSet: TDataSet);
begin
  inherited;
  if docOrder.Active then
    tblData.Edit;
end;

procedure TfrmEditOrders.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult = mrOK) and (tblOrderLines.State in dsEditModes) then
    tblOrderLines.Post
  else if tblOrderLines.State in dsEditModes then
    tblOrderLines.Cancel;
  inherited;
end;

procedure TfrmEditOrders.grdPaymentsDblClick(Sender: TObject);
begin
  // Display the payment
  if not tblPaymentLinesPaymentID.IsNull then
  with TfrmEditPayment.Create(Self) do
  try
    BrowseDataSet := Self.tblPaymentLines;
    if Edit then
    begin
      Self.tblPaymentLines.Refresh;
      Self.tblPayments.Refresh;
      Self.tblData.Refresh;
    end;
  finally
    Free;
  end;
end;

end.


