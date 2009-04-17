unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, DBISAMTb, DBISAMExt, Db, ComCtrls, StdCtrls, ExtCtrls,
  dbSchema, DataSetRefSchema;

type
  TfrmDataSetRef = class(TForm)
    dbMain: TDBISAMDatabaseExt;
    DatabaseSchema: TDatabaseSchema;
    Panel1: TPanel;
    Label1: TLabel;
    PageControl: TPageControl;
    tsTable: TTabSheet;
    tsQuery: TTabSheet;
    dsTable: TDataSource;
    tblCustomers: TDBISAMTableExt;
    qryCustomers: TDBISAMQueryExt;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    dsQuery: TDataSource;
    Panel2: TPanel;
    btnAddress: TButton;
    procedure btnAddressClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowAddress(Customer: TCustomers);
  end;

var
  frmDataSetRef: TfrmDataSetRef;

implementation


{$R *.DFM}

{ TForm1 }

procedure TfrmDataSetRef.FormShow(Sender: TObject);
begin
  dbMain.Connected := False;
  dbMain.Directory := ExtractFilePath(Application.ExeName) + '\Data';
  dbMain.Connected := True;
  tblCustomers.Active := True;
  qryCustomers.Active := True;
end;

procedure TfrmDataSetRef.btnAddressClick(Sender: TObject);
var
  Customer: TCustomers;
begin
  // Reference is created from dataset.
  // Fields are automatically mapped, so no FieldByName is required.
  if PageControl.ActivePageIndex = 0 then
    Customer := TCustomers.Create(tblCustomers)
  else Customer := TCustomers.Create(qryCustomers);
  try
    // Reference is passed to a procedure...
    ShowAddress(Customer);
  finally
    Customer.Free; // The reference is destroyed, but the
                   // underlying dataset is not.
  end;
end;

procedure TfrmDataSetRef.ShowAddress(Customer: TCustomers);
var
  Addr: String;
begin
  with Customer do
    Addr := Format('%s %s'+#13#10+'%s %s'+#13#10+'%s, %s, %s', [
      FirstName.Value,
      LastName.Value,
      StreetNo.Value,
      Street.Value,
      City.Value,
      State.Value,
      Zip.Value]);
  ShowMessage(Addr);
end;

end.
