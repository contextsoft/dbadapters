unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, DBGrids, DBCtrls, Db, dbEnum, StdCtrls,
  DBISAMTb, DBISAMExt, dbSchemaEnum, dbSchema, Mask, ExtCtrls;

type
  TfrmEnumDemo = class(TForm)
    tblOrders: TDBISAMTableExt;
    dsOrders: TDataSource;
    DBGrid2: TDBGrid;
    tblOrdersOrderID: TStringField;
    tblOrdersOrderStatusDisplay: TStringField;
    enOrderStatuses: TDBSchemaEnum;
    DatabaseSchema: TDatabaseSchema;
    DBLookupComboBox1: TDBLookupComboBox;
    DBEdit1: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DBEdit2: TDBEdit;
    DBNavigator1: TDBNavigator;
    tblOrdersCustomer: TStringField;
    tblOrdersOrderStatus: TSmallintField;
    pnlEditing: TPanel;
    pnlHint: TPanel;
    tblOrdersOrderStatusDescr: TStringField;
    tblOrdersOrderStatusShortDescr: TStringField;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEnumDemo: TfrmEnumDemo;

implementation

{$R *.DFM}

uses dbExtUtils;


procedure TfrmEnumDemo.FormCreate(Sender: TObject);
begin
  tblOrders.DatabaseName := GetRelativePath('Data');
  tblOrders.Active := True;
end;

end.

