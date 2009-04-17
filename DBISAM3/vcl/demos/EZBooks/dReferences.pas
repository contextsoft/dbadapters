unit dReferences;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBISAMTb, DBISAMExt, dbEnum, dbSchemaEnum;

(*
  This data module is a singleton - i.e. it's one for the application
  and is only used for referential and lookup purposes. The tables
  placed here all have AllowAutoOpen property set to true, so they
  don't require to be explicitly opened. Since most of the information
  about fields (like their descriptions, default values, edit formats,
  etc.) are defined within the database schema, it's not necessary to
  define persistent fields here. However, this also could be done
  if necessary.

  The idea is to only place objects that are shared by the entire
  application into the data module. The only reason I decided to
  put database & schema components on the main form is to make them
  more accessible in design-time. In the real application, they
  of course should have been placed into the data module.

  In this little demo project all the business logic is defined
  within the database schema and/or the forms, working with
  particular objects, so all the actual tables/queries are placed there.
  In the big application it would probably make sence to move all the
  business logic related stuff into the data module. However, I'd not
  advise to move here all the table, unless it is absolutely necessary
  in order to be able to replace TDBISAMxxx components with some other
  type of components. Keeping all tables within the singleton data
  module will render application unable to create two instances of the
  same form.
*)


type
  TdmReferences = class(TDataModule)
    tblTaxTypes: TDBISAMTableExt;
    enOrderStatuses: TDBSchemaEnum;
    tblItems: TDBISAMTableExt;
    tblCustomers: TDBISAMTableExt;
    tblFormOfPayments: TDBISAMTableExt;
    tblOrderTotals: TDBISAMTableExt;
    tblOrders: TDBISAMTableExt;
    tblCounters: TDBISAMTableExt;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetNextID(const FieldName: String): Integer;
  end;

var
  dmReferences: TdmReferences;

implementation

uses fMain;

{$R *.DFM}

{ TdmReferences }

function TdmReferences.GetNextID(const FieldName: String): Integer;
begin
  { ID generation rountine }
  with tblCounters do
  begin
    Refresh;
    Edit; // lock before doing anything
    try
      Result := FieldByName(FieldName).AsInteger;
      FieldByName(FieldName).AsInteger := Result + 1;
      Post;
      Result := Result + TDBISAMDatabaseExt(Database).SnapshotID * 10000000;
    except
      Cancel;
      raise;
    end;
  end;
end;

procedure TdmReferences.DataModuleCreate(Sender: TObject);
begin
  frmEZBooksMain.DBISAMEvents.HookUpToDataSets(Self);
  tblCounters.Active := True;
  if tblCounters.EOF then
    tblCounters.InsertRecord([]);
end;

end.
