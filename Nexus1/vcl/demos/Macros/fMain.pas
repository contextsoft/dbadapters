unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, DBGrids, Db, dbSchema, ExtCtrls, nxsrSqlEngineBase,
  nxsqlEngine, nxsdServerEngine, nxsrServerEngine, nxdb, nxllComponent, nxdbext;

type
  TfrmMacorsDemo = class(TForm)
    pnlSQL: TPanel;
    Query: TnxQueryExt;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    Panel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    cbxOrderBy: TComboBox;
    btnExecute: TButton;
    QueryCustomerID: TIntegerField;
    QueryReferredBy: TIntegerField;
    QueryCompanyName: TStringField;
    QueryLastName: TStringField;
    QueryFirstName: TStringField;
    QueryInitial: TStringField;
    QueryCareOf: TStringField;
    QueryStreetNo: TStringField;
    QueryStreet: TStringField;
    QueryCity: TStringField;
    QueryState: TStringField;
    QueryZip: TStringField;
    QueryHomePhone: TStringField;
    QueryWorkPhone: TStringField;
    QueryMobilePhone: TStringField;
    QueryFax: TStringField;
    QueryTaxType: TStringField;
    dbMain: TnxDatabaseExt;
    nxSession: TnxSession;
    nxServerEngine: TnxServerEngine;
    nxSqlEngine: TnxSqlEngine;
    procedure btnExecuteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMacorsDemo: TfrmMacorsDemo;

implementation

uses
  dbExtUtils, nx1xAllEngines;

{$R *.DFM}

procedure TfrmMacorsDemo.btnExecuteClick(Sender: TObject);
begin
  Query.Active := False;
  Query.MacroByName('orderby').Value := cbxOrderBy.Text;
  Query.Active := True;
end;

procedure TfrmMacorsDemo.FormShow(Sender: TObject);
begin
  cbxOrderBy.ItemIndex := 0;
end;

procedure TfrmMacorsDemo.FormCreate(Sender: TObject);
begin
  dbMain.AliasPath := GetRelativePath('Data');
end;

end.
