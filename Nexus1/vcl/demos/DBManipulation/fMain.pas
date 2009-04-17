unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, ExtCtrls, nxOpenDatabase, Grids, DBGrids, Menus, ComCtrls,
  ActnList, ToolWin, ImgList, dbSchema, DBCtrls, DBActns, nxdb, nxdbext,
  nxllComponent, nxsdServerEngine, nxsrServerEngine, dbManager;

type
  TfrmMainForm = class(TForm)
    MainMenu: TMainMenu;
    Actions: TActionList;
    Database: TnxDatabaseExt;
    DBSchema: TDatabaseSchema;
    Images: TImageList;
    ToolBar1: TToolBar;
    actOpenDatabase: TAction;
    actNewDatabase: TAction;
    actSaveDatabase: TAction;
    actExit: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    File1: TMenuItem;
    NewDatabase1: TMenuItem;
    OpenDatabase1: TMenuItem;
    SaveDatabaseToFile1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    tblCustomers: TnxTableExt;
    dsCustomers: TDataSource;
    tblOrders: TnxTableExt;
    dsOrders: TDataSource;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolButton6: TToolButton;
    Panel2: TPanel;
    DBGrid2: TDBGrid;
    Splitter1: TSplitter;
    DBGrid1: TDBGrid;
    DataSetCancel1: TDataSetCancel;
    DataSetDelete1: TDataSetDelete;
    DataSetEdit1: TDataSetEdit;
    DataSetFirst1: TDataSetFirst;
    DataSetInsert1: TDataSetInsert;
    DataSetLast1: TDataSetLast;
    DataSetNext1: TDataSetNext;
    DataSetPost1: TDataSetPost;
    DataSetPrior1: TDataSetPrior;
    DataSetRefresh1: TDataSetRefresh;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    Customers1: TMenuItem;
    First1: TMenuItem;
    Last1: TMenuItem;
    Prior1: TMenuItem;
    Next1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Edit1: TMenuItem;
    Edit2: TMenuItem;
    Post1: TMenuItem;
    Cancel1: TMenuItem;
    Delete1: TMenuItem;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    N4: TMenuItem;
    Refresh1: TMenuItem;
    actImportDatabase: TAction;
    ImportDatabase1: TMenuItem;
    N5: TMenuItem;
    nxSession: TnxSession;
    OpenDatabaseDialog: TnxOpenDatabaseDialog;
    nxServerEngine: TnxServerEngine;
    DBManager: TDBManager;
    procedure FormShow(Sender: TObject);
    procedure actOpenDatabaseExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actSaveDatabaseExecute(Sender: TObject);
    procedure actNewDatabaseExecute(Sender: TObject);
    procedure actImportDatabaseExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

implementation

uses
  dbExtUtils, nx1xAllEngines;

{$R *.DFM}

procedure TfrmMainForm.FormShow(Sender: TObject);
begin
  SaveDialog.FileName := ChangeFileExt(Application.ExeName, '.dbd');
  Database.AliasPath := GetRelativePath('Data');
  Database.Connected := True;
  tblCustomers.Active := True;
  tblOrders.Active := True;
end;

procedure TfrmMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMainForm.actOpenDatabaseExecute(Sender: TObject);
begin
  OpenDatabaseDialog.DatabaseURL := Database.DatabaseURL;
  if OpenDatabaseDialog.Execute then
  begin
    Database.Session.Active := False;
    Database.DatabaseURL := OpenDatabaseDialog.DatabaseURL;
    Database.Connected := True;
    tblCustomers.Active := True;
    tblOrders.Active := True;
  end;
end;

procedure TfrmMainForm.actSaveDatabaseExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    DBManager.SaveDatabaseToFile(SaveDialog.FileName);
end;

procedure TfrmMainForm.actNewDatabaseExecute(Sender: TObject);
begin
  if OpenDatabaseDialog.Execute then
  begin
    Database.Session.Active := False;
    Database.DatabaseURL := OpenDatabaseDialog.DatabaseURL;
    Database.Connected := True;
    // Creates database according to schema
    Database.CreateNewDatabase(True, False, False);
    // Load "default" data from last saved file...
    DBManager.LoadDatabaseFromFile(SaveDialog.FileName);
    // Re-open datasets
    tblCustomers.Active := True;
    tblOrders.Active := True;
  end;
end;

procedure TfrmMainForm.actImportDatabaseExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DBManager.LoadDatabaseFromFile(OpenDialog.FileName);
    Database.RefreshTables;
  end;
end;

end.
