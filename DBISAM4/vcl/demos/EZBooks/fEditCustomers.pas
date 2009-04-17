unit fEditCustomers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fEdit, Db, DBISAMTb, DBISAMExt, StdCtrls, Buttons, ExtCtrls, Mask,
  DBCtrls;

type
  TfrmEditCustomers = class(TfrmEditObject)
    tblDataCompanyName: TStringField;
    tblDataLastName: TStringField;
    tblDataFirstName: TStringField;
    tblDataInitial: TStringField;
    tblDataCareOf: TStringField;
    tblDataStreetNo: TStringField;
    tblDataStreet: TStringField;
    tblDataCity: TStringField;
    tblDataState: TStringField;
    tblDataZip: TStringField;
    tblDataHomePhone: TStringField;
    tblDataWorkPhone: TStringField;
    tblDataMobilePhone: TStringField;
    tblDataFax: TStringField;
    tblDataTaxType: TStringField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
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
    Label11: TLabel;
    DBEdit11: TDBEdit;
    Label12: TLabel;
    DBEdit12: TDBEdit;
    Label13: TLabel;
    DBEdit13: TDBEdit;
    Label14: TLabel;
    DBEdit14: TDBEdit;
    Label15: TLabel;
    DBEdit15: TDBEdit;
    Label16: TLabel;
    DBEdit16: TDBEdit;
    Label17: TLabel;
    DBLookupComboBox1: TDBLookupComboBox;
    tblDataTaxTypeLookup: TStringField;
    tblDataReferredBy: TIntegerField;
    tblDataReferredByLookup: TStringField;
    Label2: TLabel;
    DBLookupComboBox2: TDBLookupComboBox;
    btnSelectCustomer: TBitBtn;
    tblDataCustomerID: TIntegerField;
    procedure btnSelectCustomerClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function KeyFields: String; override;
    class function TableName: String; override;
    class function DisplayName: String; override;
  end;

var
  frmEditCustomers: TfrmEditCustomers;

implementation

uses dReferences, fBrowseCustomers;

{$R *.DFM}

{ TfrmEditCustomers }

class function TfrmEditCustomers.DisplayName: String;
begin
  Result := 'customer';
end;

class function TfrmEditCustomers.KeyFields: String;
begin
  Result := 'CustomerID';
end;

class function TfrmEditCustomers.TableName: String;
begin
  Result := 'Customers';
end;

procedure TfrmEditCustomers.btnSelectCustomerClick(Sender: TObject);
begin
  TfrmBrowseCustomers.Select(tblDataReferredBy);
end;

end.
