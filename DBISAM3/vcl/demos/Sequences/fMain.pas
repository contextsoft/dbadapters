unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  dbSequence, DBCtrls, ExtCtrls, Grids, DBGrids, Db, ComCtrls, DBISAMTb,
  DBISAMExt, dbSchema, StdCtrls;

type
  TfrmSequencesDemo = class(TForm)
    DBSequences: TDBSequences;
    DatabaseSchema: TDatabaseSchema;
    tblCounters: TDBISAMTableExt;
    Database: TDBISAMDatabaseExt;
    PageControl1: TPageControl;
    tsCustomers: TTabSheet;
    tsItems: TTabSheet;
    tblCustomers: TDBISAMTableExt;
    tblItems: TDBISAMTableExt;
    dsCustomers: TDataSource;
    dsItems: TDataSource;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    DBNavigator2: TDBNavigator;
    DBGrid2: TDBGrid;
    tblCountersItemID: TIntegerField;
    tblCountersCustomerID: TIntegerField;
    btnNextCustomerID: TButton;
    btnNextItemID: TButton;
    procedure tblCustomersAfterInsert(DataSet: TDataSet);
    procedure tblItemsAfterInsert(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure tblCustomersBeforeCancel(DataSet: TDataSet);
    procedure tblItemsBeforeCancel(DataSet: TDataSet);
    procedure btnNextCustomerIDClick(Sender: TObject);
    procedure btnNextItemIDClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSequencesDemo: TfrmSequencesDemo;

implementation

{$R *.DFM}

uses dbExtUtils;

procedure TfrmSequencesDemo.FormCreate(Sender: TObject);
begin
  Database.Connected := False;
  Database.Directory := GetRelativePath('Data');
  Database.Connected := True;
  tblItems.Active := True;
  tblCustomers.Active := True;
end;

procedure TfrmSequencesDemo.tblCustomersAfterInsert(DataSet: TDataSet);
begin
  DBSequences.NextValue(tblCustomers.FieldByName('CustomerID'));
end;

procedure TfrmSequencesDemo.tblItemsAfterInsert(DataSet: TDataSet);
begin
  DBSequences.NextValue(tblItems.FieldByName('ItemID'));
end;

procedure TfrmSequencesDemo.tblCustomersBeforeCancel(DataSet: TDataSet);
begin
  // Release the ID value, so it can be reused
  if tblCustomers.State = dsInsert then
    DBSequences.Release(tblCustomers.FieldByName('CustomerID'));
end;

procedure TfrmSequencesDemo.tblItemsBeforeCancel(DataSet: TDataSet);
begin
  // Release the ID value, so it can be reused
  if tblItems.State = dsInsert then
    DBSequences.Release(tblItems.FieldByName('ItemID'));
end;

procedure TfrmSequencesDemo.btnNextCustomerIDClick(Sender: TObject);
var
  Value: String;
begin
  Value := IntToStr(DBSequences.CurValue['CustomerID']);
  if InputQuery('Next Customer ID', 'Next Value: ', Value) then
    DBSequences.CurValue['CustomerID'] := StrToInt(Value);
end;

procedure TfrmSequencesDemo.btnNextItemIDClick(Sender: TObject);
var
  Value: String;
begin
  Value := IntToStr(DBSequences.CurValue['ItemID']);
  if InputQuery('Next Item ID', 'Next Value: ', Value) then
    DBSequences.CurValue['ItemID'] := StrToInt(Value);
end;

end.
