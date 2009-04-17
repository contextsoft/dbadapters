(******************************************************************************)
(*  This file contains pascal declarations imported from database schema
(*  This file will be written by the 'Update Schema Declaration' procedure
(*  of TDatabaseSchema component editor.
(*  Changes to this file will be discarded during the update process.
(*
(*  Database Schema: EZBooksDecl
(*  Version: 1.2
(******************************************************************************)
unit EZBooksDecl;

interface

uses Classes, DB, dbSchema;

const
  { Enumerations }
  { Order Statuses }
  osOpen = 0; { Open order }
  osProcessing = 1; { Processing order }
  osComplete = 2; { Complete order }

type
  { Tables Declarations }

  TTaxTypes = class (TDataSetReference)
  protected
    FTaxType: TStringField;
    FTax: TFloatField;
  published
    property TaxType: TStringField read FTaxType write FTaxType;
    property Tax: TFloatField read FTax write FTax;
  end;

  TCustomers = class (TDataSetReference)
  protected
    FCustomerID: TIntegerField;
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
    FReferredBy: TIntegerField;
  published
    property CustomerID: TIntegerField read FCustomerID write FCustomerID;
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
    property ReferredBy: TIntegerField read FReferredBy write FReferredBy;
  end;

  TOrderLines = class (TDataSetReference)
  protected
    FOrderLineID: TIntegerField;
    FOrderID: TIntegerField;
    FItemID: TIntegerField;
    FDescription: TStringField;
    FSalePrice: TCurrencyField;
    FQuantity: TFloatField;
    FTaxable: TBooleanField;
    FCharges: TCurrencyField;
  published
    property OrderLineID: TIntegerField read FOrderLineID write FOrderLineID;
    property OrderID: TIntegerField read FOrderID write FOrderID;
    property ItemID: TIntegerField read FItemID write FItemID;
    property Description: TStringField read FDescription write FDescription;
    property SalePrice: TCurrencyField read FSalePrice write FSalePrice;
    property Quantity: TFloatField read FQuantity write FQuantity;
    property Taxable: TBooleanField read FTaxable write FTaxable;
    property Charges: TCurrencyField read FCharges write FCharges;
  end;

  TOrders = class (TDataSetReference)
  protected
    FOrderID: TIntegerField;
    FDateCreated: TDateField;
    FCustomerID: TIntegerField;
    FDescription: TStringField;
    FStatus: TSmallintField;
    FNotes: TMemoField;
    FPayOffDate: TDateField;
    FTaxType: TStringField;
    FSalesRep: TStringField;
    FTax: TFloatField;
    FTotalTax: TCurrencyField;
    FTotalSales: TCurrencyField;
    FTotalCharges: TCurrencyField;
  published
    property OrderID: TIntegerField read FOrderID write FOrderID;
    property DateCreated: TDateField read FDateCreated write FDateCreated;
    property CustomerID: TIntegerField read FCustomerID write FCustomerID;
    property Description: TStringField read FDescription write FDescription;
    property Status: TSmallintField read FStatus write FStatus;
    property Notes: TMemoField read FNotes write FNotes;
    property PayOffDate: TDateField read FPayOffDate write FPayOffDate;
    property TaxType: TStringField read FTaxType write FTaxType;
    property SalesRep: TStringField read FSalesRep write FSalesRep;
    property Tax: TFloatField read FTax write FTax;
    property TotalTax: TCurrencyField read FTotalTax write FTotalTax;
    property TotalSales: TCurrencyField read FTotalSales write FTotalSales;
    property TotalCharges: TCurrencyField read FTotalCharges write FTotalCharges;
  end;

  TOrderTotals = class (TDataSetReference)
  protected
    FOrderID: TIntegerField;
    FTotalCharges: TCurrencyField;
    FTotalPayment: TCurrencyField;
    FTotalBalance: TCurrencyField;
  published
    property OrderID: TIntegerField read FOrderID write FOrderID;
    property TotalCharges: TCurrencyField read FTotalCharges write FTotalCharges;
    property TotalPayment: TCurrencyField read FTotalPayment write FTotalPayment;
    property TotalBalance: TCurrencyField read FTotalBalance write FTotalBalance;
  end;

  TPaymentLines = class (TDataSetReference)
  protected
    FPaymentLineID: TIntegerField;
    FPaymentID: TIntegerField;
    FOrderID: TIntegerField;
    FAmount: TCurrencyField;
  published
    property PaymentLineID: TIntegerField read FPaymentLineID write FPaymentLineID;
    property PaymentID: TIntegerField read FPaymentID write FPaymentID;
    property OrderID: TIntegerField read FOrderID write FOrderID;
    property Amount: TCurrencyField read FAmount write FAmount;
  end;

  TPayments = class (TDataSetReference)
  protected
    FPaymentID: TIntegerField;
    FCustomerID: TIntegerField;
    FDateCreated: TDateField;
    FFormOfPayment: TStringField;
    FReference: TStringField;
    FTotalAmount: TCurrencyField;
    FCreditedToAccount: TCurrencyField;
    FAppliedToOrders: TCurrencyField;
  published
    property PaymentID: TIntegerField read FPaymentID write FPaymentID;
    property CustomerID: TIntegerField read FCustomerID write FCustomerID;
    property DateCreated: TDateField read FDateCreated write FDateCreated;
    property FormOfPayment: TStringField read FFormOfPayment write FFormOfPayment;
    property Reference: TStringField read FReference write FReference;
    property TotalAmount: TCurrencyField read FTotalAmount write FTotalAmount;
    property CreditedToAccount: TCurrencyField read FCreditedToAccount write FCreditedToAccount;
    property AppliedToOrders: TCurrencyField read FAppliedToOrders write FAppliedToOrders;
  end;

  TFormOfPayments = class (TDataSetReference)
  protected
    FFormOfPayment: TStringField;
    FDescription: TStringField;
  published
    property FormOfPayment: TStringField read FFormOfPayment write FFormOfPayment;
    property Description: TStringField read FDescription write FDescription;
  end;

  TItems = class (TDataSetReference)
  protected
    FItemID: TIntegerField;
    FDescription: TStringField;
    FSalePrice: TCurrencyField;
    FTaxable: TBooleanField;
  published
    property ItemID: TIntegerField read FItemID write FItemID;
    property Description: TStringField read FDescription write FDescription;
    property SalePrice: TCurrencyField read FSalePrice write FSalePrice;
    property Taxable: TBooleanField read FTaxable write FTaxable;
  end;

  TCompanyInfo = class (TDataSetReference)
  protected
    FCompanyName: TStringField;
    FStreetAddress: TStringField;
    FCity: TStringField;
    FState: TStringField;
    FZip: TStringField;
    FCountry: TStringField;
    FPhone: TStringField;
    FFax: TStringField;
    FEmail: TStringField;
    FEID: TStringField;
  published
    property CompanyName: TStringField read FCompanyName write FCompanyName;
    property StreetAddress: TStringField read FStreetAddress write FStreetAddress;
    property City: TStringField read FCity write FCity;
    property State: TStringField read FState write FState;
    property Zip: TStringField read FZip write FZip;
    property Country: TStringField read FCountry write FCountry;
    property Phone: TStringField read FPhone write FPhone;
    property Fax: TStringField read FFax write FFax;
    property Email: TStringField read FEmail write FEmail;
    property EID: TStringField read FEID write FEID;
  end;

  TCounters = class (TDataSetReference)
  protected
    FCustomerID: TIntegerField;
    FItemID: TIntegerField;
    FOrderID: TIntegerField;
    FPaymentID: TIntegerField;
    FOrderLineID: TIntegerField;
    FPaymentLineID: TIntegerField;
    FSnapshotID: TIntegerField;
  published
    property CustomerID: TIntegerField read FCustomerID write FCustomerID;
    property ItemID: TIntegerField read FItemID write FItemID;
    property OrderID: TIntegerField read FOrderID write FOrderID;
    property PaymentID: TIntegerField read FPaymentID write FPaymentID;
    property OrderLineID: TIntegerField read FOrderLineID write FOrderLineID;
    property PaymentLineID: TIntegerField read FPaymentLineID write FPaymentLineID;
    property SnapshotID: TIntegerField read FSnapshotID write FSnapshotID;
  end;

implementation

end.
