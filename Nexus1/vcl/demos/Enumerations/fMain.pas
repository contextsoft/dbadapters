unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, DBGrids, DBCtrls, Db, dbEnum, StdCtrls,
  dbSchemaEnum, dbSchema, Mask, ExtCtrls, nxsdServerEngine, nxsrServerEngine,
  nxdb, nxllComponent, nxdbext;

type
  TfrmEnumDemo = class(TForm)
    tblOrders: TnxTableExt;
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
    dbMain: TnxDatabaseExt;
    nxSession: TnxSession;
    nxServerEngine: TnxServerEngine;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEnumDemo: TfrmEnumDemo;

implementation

uses
  dbExtUtils, nx1xAllEngines;

{$R *.DFM}

procedure TfrmEnumDemo.FormCreate(Sender: TObject);
begin
  dbMain.AliasPath := GetRelativePath('Data');
  tblOrders.Active := True;
end;

end.

