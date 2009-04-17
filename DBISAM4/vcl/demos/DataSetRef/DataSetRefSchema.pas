(******************************************************************************)
(*  This file contains pascal declarations imported from database schema
(*  This file will be written by the 'Update Schema Declaration' procedure
(*  of TDatabaseSchema component editor.
(*  Changes to this file will be discarded during the update process.
(*
(*  Database Schema: DataSetRefSchema
(*  Version: N/A
(******************************************************************************)
unit DataSetRefSchema;

interface

uses Classes, DB, dbSchema;

const
  { Enumerations }
  ENUMERATIONS_COUNT = 9;
  { Scheduled Operations }
  { Location Types }
  enStoreOffice = '1'; { Store/Office }
  enVault = '2'; { Vault }
  enWarehouse = '3'; { Warehouse }
  enContractor = '4'; { Contractor }
  enPartner = '5'; { Partner }
  enMe = '6'; { Me }
  { Item Types }
  enGarment = 1; { Garment }
  enAccessory = 2; { Accessory }
  enSkins = 3; { Skins }
  { User Types }
  { Account Types }
  enAsset = 'Asset'; { Asset }
  enEquity = 'Equity'; { Equity }
  enLiability = 'Liability'; { Liability }
  enIncome = 'Income'; { Income }
  enExpense = 'Expense'; { Expense }
  { Line Groups }
  enSale = 'Sale'; { Sale }
  enConsignment = 'Consignment'; { Consignment }
  enLayaway = 'Lay-a-way'; { Lay-a-way }
  enStorage = 'Storage'; { Storage }
  enCleaning = 'Cleaning'; { Cleaning }
  enLabor = 'Labor'; { Labor }
  enOtherCharge = 'Other Charge'; { Other Charge }
  enDeposit = 'Deposit'; { Deposit }
  enPayment = 'Payment'; { Payment }
  { Document Types }
  dtInvoice = 1; { Simple Invoice }
  dtSaleReceipt = 2; { Sale Receipt }
  dtServiceReceipt = 3; { Service Receipt }
  dtMemo = 4; { Memo }
  dtCreditMemo = 5; { Credit Memo }
  dtPayment = 6; { Payment }
  dtQuote = 7; { Quote }
  dtAdjustment = 8; { Adjustment }
  { Document Statuses }
  { Document Entry Types }
  detInvoice = 'Invoice'; { Invoice }
  detSaleReceipt = 'Sale Receipt'; { Sale Receipt }
  detServiceReceipt = 'Service Receipt'; { Service Receipt }
  detRefundExchange = 'Refund/Exchange'; { Refund/Exchange }
  detAdjustment = 'Adjustment'; { Adjustment }
  detPayment = 'Payment'; { Payment }
type
  { Tables Declarations }

  TOrders = class (TDataSetReference)
  protected
    FOrderID: TAutoIncField;
    FCustomerID: TIntegerField;
    FDescription: TMemoField;
  published
    property OrderID: TAutoIncField read FOrderID write FOrderID;
    property CustomerID: TIntegerField read FCustomerID write FCustomerID;
    property Description: TMemoField read FDescription write FDescription;
  end;

  TCustomers = class (TDataSetReference)
  protected
    FCustomerID: TAutoIncField;
    FReferredBy: TIntegerField;
    FCompanyName: TStringField;
    FLastName: TStringField;
    FFirstName: TStringField;
    FInitial: TStringField;
    FCareOf: TStringField;
    FStreetNo: TStringField;
    FStreet: TStringField;
    FCity: TStringField;
    FState: TStringField;
    FZip: TStringField;
    FHomePhone: TStringField;
    FWorkPhone: TStringField;
    FMobilePhone: TStringField;
    FFax: TStringField;
    FTaxType: TStringField;
  published
    property CustomerID: TAutoIncField read FCustomerID write FCustomerID;
    property ReferredBy: TIntegerField read FReferredBy write FReferredBy;
    property CompanyName: TStringField read FCompanyName write FCompanyName;
    property LastName: TStringField read FLastName write FLastName;
    property FirstName: TStringField read FFirstName write FFirstName;
    property Initial: TStringField read FInitial write FInitial;
    property CareOf: TStringField read FCareOf write FCareOf;
    property StreetNo: TStringField read FStreetNo write FStreetNo;
    property Street: TStringField read FStreet write FStreet;
    property City: TStringField read FCity write FCity;
    property State: TStringField read FState write FState;
    property Zip: TStringField read FZip write FZip;
    property HomePhone: TStringField read FHomePhone write FHomePhone;
    property WorkPhone: TStringField read FWorkPhone write FWorkPhone;
    property MobilePhone: TStringField read FMobilePhone write FMobilePhone;
    property Fax: TStringField read FFax write FFax;
    property TaxType: TStringField read FTaxType write FTaxType;
  end;

implementation

end.
