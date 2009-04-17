unit fBrowseOrders;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fBrowse, ImgList, ActnList, Menus, Db, DBISAMTb, DBISAMExt, Grids,
  DBGrids, ComCtrls, StdCtrls, Buttons, ToolWin;

type
  TfrmBrowseOrders = class(TfrmBrowse)
    qryBrowseDateCreated: TDateField;
    qryBrowseCustomerID: TIntegerField;
    qryBrowseStatus: TSmallintField;
    qryBrowseNotes: TMemoField;
    qryBrowsePayOffDate: TDateField;
    qryBrowseTaxType: TStringField;
    qryBrowseSalesRep: TStringField;
    qryBrowseTax: TFloatField;
    qryBrowseCustomer: TStringField;
    qryBrowseStatus_: TStringField;
    qryBrowseDescription: TStringField;
    qryBrowseOrderID: TIntegerField;
    qryBrowseTotalTax: TCurrencyField;
    qryBrowseTotalSales: TCurrencyField;
    qryBrowseTotalCharges: TCurrencyField;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowseOrders: TfrmBrowseOrders;

implementation

uses fEditOrders, dReferences;

{$R *.DFM}

procedure TfrmBrowseOrders.FormCreate(Sender: TObject);
begin
  FEditFormClass := TfrmEditOrders;
  inherited;
end;

end.
