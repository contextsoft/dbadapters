(******************************************************************************)
(*  This file contains pascal declarations imported from database schema
(*  This file will be written by the 'Update Schema Declaration' procedure
(*  of TDatabaseSchema component editor.
(*  Changes to this file will be discarded during the update process.
(*
(*  Database Schema: Database
(*  Version: N/A
(******************************************************************************)
unit Database;

interface

uses Classes, DB, dbSchema;

const
  { Enumerations }
  ENUMERATIONS_COUNT = 0;
type
  { Tables Declarations }

  TCustomers = class (TDataSetReference)
  protected
    FCustomerID: TIntegerField;
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
    property CustomerID: TIntegerField read FCustomerID write FCustomerID;
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

  TOrders = class (TDataSetReference)
  protected
    FOrderID: TIntegerField;
    FDateCreated: TDateField;
    FCustomerID: TIntegerField;
    FStatus: TSmallintField;
    FNotes: TMemoField;
    FPayOffDate: TDateField;
    FTaxType: TStringField;
    FSalesRep: TStringField;
    FTax: TFloatField;
  published
    property OrderID: TIntegerField read FOrderID write FOrderID;
    property DateCreated: TDateField read FDateCreated write FDateCreated;
    property CustomerID: TIntegerField read FCustomerID write FCustomerID;
    property Status: TSmallintField read FStatus write FStatus;
    property Notes: TMemoField read FNotes write FNotes;
    property PayOffDate: TDateField read FPayOffDate write FPayOffDate;
    property TaxType: TStringField read FTaxType write FTaxType;
    property SalesRep: TStringField read FSalesRep write FSalesRep;
    property Tax: TFloatField read FTax write FTax;
  end;

implementation

end.
