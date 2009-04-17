unit fBrowsePayments;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fBrowse, ImgList, ActnList, Menus, Db, DBISAMTb, DBISAMExt, Grids,
  DBGrids, ComCtrls, StdCtrls, Buttons, ToolWin;

type
  TfrmBrowsePayments = class(TfrmBrowse)
    qryBrowseCustomerID: TIntegerField;
    qryBrowseDateCreated: TDateField;
    qryBrowseFormOfPayment: TStringField;
    qryBrowseReference: TStringField;
    qryBrowseTotalAmount: TCurrencyField;
    qryBrowseCustomer: TStringField;
    qryBrowseFOP: TStringField;
    qryBrowsePaymentID: TIntegerField;
    qryBrowseCreditedToAccount: TCurrencyField;
    qryBrowseAppliedToOrders: TCurrencyField;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowsePayments: TfrmBrowsePayments;

implementation

uses fEditPayments, dReferences;

{$R *.DFM}

procedure TfrmBrowsePayments.FormCreate(Sender: TObject);
begin
  FEditFormClass := TfrmEditPayment;
  inherited;
end;

end.
