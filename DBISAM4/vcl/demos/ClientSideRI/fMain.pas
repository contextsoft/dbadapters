unit fMain;

interface

uses
  Windows, Messages, SysUtils, {$IFnDEF VER130} Variants, {$ENDIF} Classes,
  Graphics, Controls, Forms, Dialogs, dbSchema, DB, dbisamtb, DBISAMExt,
  StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids;

type
  TfrmMainForm = class(TForm)
    DatabaseExt: TDBISAMDatabaseExt;
    DatabaseSchema: TDatabaseSchema;
    tblCustomers: TDBISAMTableExt;
    tblOrders: TDBISAMTableExt;
    dsCustomers: TDataSource;
    dsOrders: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Splitter1: TSplitter;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    DBNavigator2: TDBNavigator;
    Label1: TLabel;
    Label2: TLabel;
    tblCustomersCustomerID: TAutoIncField;
    tblCustomersLastName: TStringField;
    tblCustomersFirstName: TStringField;
    tblCustomersStreet: TStringField;
    tblCustomersCity: TStringField;
    tblCustomersState: TStringField;
    tblCustomersZip: TStringField;
    tblOrdersOrderID: TAutoIncField;
    tblOrdersCustomerID: TIntegerField;
    tblOrdersTotalCharges: TCurrencyField;
    tblOrdersTotalPayments: TCurrencyField;
    tblOrdersDescription: TStringField;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

implementation

{$R *.dfm}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  DatabaseExt.Directory := ExtractFilePath(Application.ExeName) + 'Data';

  DatabaseExt.Connected := True;
  UpdateDatabase(DatabaseExt);
  tblCustomers.Active := True;
  tblOrders.Active := True;
end;

end.
