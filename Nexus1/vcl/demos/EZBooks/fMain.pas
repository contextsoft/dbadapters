unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ToolWin, ComCtrls, Menus, dbSchema, AppEvnts, DB, ImgList, ExtCtrls,
  StdCtrls, nxdb, nxllComponent, nxdbext,
  nxsrSqlEngineBase, nxsqlEngine, nxsdServerEngine, nxsrServerEngine,
  nxOpenDatabase, dbDocument, dbManager;

type
  TfrmEZBooksMain = class(TForm)
    mnuMain: TMainMenu;
    Actions: TActionList;
    Images: TImageList;
    dbMain: TnxDatabaseExt;
    dsMain: TDatabaseSchema;
    File2: TMenuItem;
    Exit2: TMenuItem;
    N3: TMenuItem;
    SaveAs2: TMenuItem;
    Save2: TMenuItem;
    Open2: TMenuItem;
    New2: TMenuItem;
    ToolBar1: TToolBar;
    sMain: TnxSession;
    actOpenDatabase: TAction;
    actNewDatabase: TAction;
    actSynchronize: TAction;
    actExportToFile: TAction;
    actImportFromFile: TAction;
    actExit: TAction;
    actViewCustomers: TAction;
    actViewOrders: TAction;
    actViewFormsOfPayment: TAction;
    actViewTaxTypes: TAction;
    actEnterOrder: TAction;
    actEnterPayment: TAction;
    actViewItems: TAction;
    N1: TMenuItem;
    ImportFromFile1: TMenuItem;
    View1: TMenuItem;
    Customers1: TMenuItem;
    Orders1: TMenuItem;
    N2: TMenuItem;
    SalableItems1: TMenuItem;
    FormsofPayment1: TMenuItem;
    TaxTypes1: TMenuItem;
    Actions1: TMenuItem;
    EnterOrder1: TMenuItem;
    EnterPayment1: TMenuItem;
    actEnterCustomer: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    EnterCustomer1: TMenuItem;
    actAbout: TAction;
    Help1: TMenuItem;
    About1: TMenuItem;
    OpenDBDialog: TnxOpenDatabaseDialog;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    actViewPayments: TAction;
    Payments1: TMenuItem;
    ToolButton13: TToolButton;
    dtOrder: TDBDocumentType;
    dtPayment: TDBDocumentType;
    actViewCompanyInfo: TAction;
    CompanyInfo1: TMenuItem;
    actViewCounters: TAction;
    IDGenerators1: TMenuItem;
    N4: TMenuItem;
    actCreateSnapshot: TAction;
    dbDest: TnxDatabaseExt;
    sDest: TnxSession;
    N5: TMenuItem;
    CreateDatabaseSnapshot1: TMenuItem;
    actViewObjects: TAction;
    Objects1: TMenuItem;
    Label1: TLabel;
    Shape1: TShape;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    StatusBar: TStatusBar;
    ProgressBar: TProgressBar;
    nxServerEngine: TnxServerEngine;
    nxSqlEngine: TnxSqlEngine;
    DBManager: TDBManager;
    procedure actExitExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actViewCustomersExecute(Sender: TObject);
    procedure actEnterCustomerExecute(Sender: TObject);
    procedure actNewDatabaseExecute(Sender: TObject);
    procedure actOpenDatabaseExecute(Sender: TObject);
    procedure actViewTaxTypesExecute(Sender: TObject);
    procedure actViewFormsOfPaymentExecute(Sender: TObject);
    procedure actViewItemsExecute(Sender: TObject);
    procedure actViewOrdersExecute(Sender: TObject);
    procedure actEnterOrderExecute(Sender: TObject);
    procedure actViewPaymentsExecute(Sender: TObject);
    procedure actEnterPaymentExecute(Sender: TObject);
    procedure actViewCompanyInfoExecute(Sender: TObject);
    procedure actExportToFileExecute(Sender: TObject);
    procedure actImportFromFileExecute(Sender: TObject);
    procedure actViewCountersExecute(Sender: TObject);
    procedure actCreateSnapshotExecute(Sender: TObject);
    procedure actSynchronizeExecute(Sender: TObject);
    procedure actViewObjectsExecute(Sender: TObject);
    procedure dbMainProgress(Sender: TnxDatabaseExt;
      const Operation: String; PercentDone: Byte; var Abort: Boolean);
  private
    { Private declarations }
    procedure DoOnHint(Sender: TObject);
    procedure OpenUpdateDatabase(Database: TnxDatabaseExt);
  public
    { Public declarations }
    procedure RefreshViews;
  end;

var
  frmEZBooksMain: TfrmEZBooksMain;

implementation

uses
{$IFDEF VER130}
  FileCtrl,
{$ENDIF}
  fAbout, fBrowseCustomers, fEditCustomers, fBrowseTaxTypes,
  fBrowseFormOfPayments, fBrowseItems, fBrowseOrders, fEditOrders,
  fBrowsePayments, fEditPayments, fEditCompanyInfo, fBrowse, fEditCounters,
  fBrowseObjects, dbExtUtils, dReferences;

{$R *.DFM}

procedure TfrmEZBooksMain.FormCreate(Sender: TObject);
begin
  Application.OnHint := DoOnHint;
  dbMain.AliasPath := GetRelativePath('Data');
  OpenUpdateDatabase(dbMain);
  RefreshViews;
end;

procedure TfrmEZBooksMain.DoOnHint(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Application.Hint;
end;

procedure TfrmEZBooksMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmEZBooksMain.actAboutExecute(Sender: TObject);
begin
  with TfrmAboutBox.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmEZBooksMain.actNewDatabaseExecute(Sender: TObject);
var
  DefaultDataPath: String;
begin
  // New Database
  OpenDBDialog.DatabaseURL := dbMain.DatabaseURL;
  OpenDBDialog.Title := 'New Database';
  if OpenDBDialog.Execute then
  begin
    dbMain.Session.Active := False;
    dbMain.DatabaseURL := OpenDBDialog.DatabaseURL;
    dbMain.Open;
    // Create new database within a selected directory
    dbMain.CreateNewDatabase;
    // Path to the file with default data (company info record and taxes)
    DefaultDataPath := GetRelativePath('default.dbd');
    if FileExists(DefaultDataPath) then
      DBManager.LoadDatabaseFromFile(DefaultDataPath);
    RefreshViews;
  end;
end;

procedure TfrmEZBooksMain.actOpenDatabaseExecute(Sender: TObject);
begin
  // Open database
  OpenDBDialog.DatabaseURL := dbMain.DatabaseURL;
  OpenDBDialog.Title := 'Open Database';
  if OpenDBDialog.Execute then
  begin
    dbMain.Session.Active := False;
    dbMain.DatabaseURL := OpenDBDialog.DatabaseURL;
    OpenUpdateDatabase(dbMain);
    RefreshViews;
  end;
end;

procedure TfrmEZBooksMain.actCreateSnapshotExecute(Sender: TObject);
var
  MainDBURL: String;
  SnapshotID: Integer;
begin
  // Create a Snapshot of another database
  MainDBURL := dbMain.DatabaseURL;

  OpenDBDialog.Title := 'New Database Snapshot';
  if OpenDBDialog.Execute then
  begin
    if AnsiSameText(OpenDBDialog.DatabaseURL, MainDBURL) then
      DatabaseError('You have selected the same database as main database. Please select a different database.');

    // Get Next Snapshot ID. Must be unique for each snapshot.
    SnapshotID := dmReferences.GetNextID('SnapshotID');

    // Close all sessions
    dbMain.Session.Close;
    dbDest.Session.Close;
    try
      // Select main database
      dbDest.DatabaseURL := MainDBURL;
      OpenUpdateDatabase(dbDest);
      // This is the most simple way of creating a snapshot
      // Current database will be erased if it existed
      dbMain.DatabaseURL := OpenDBDialog.DatabaseURL;
      // Create new directory if necessary
      if dbMain.Session.ServerEngine is TnxServerEngine then
        ForceDirectories(dbMain.AliasPath);
      dbMain.Open;
      // Create empty database
      dbMain.CreateNewDatabase;
      dbMain.SnapshotID := SnapshotID; // Generated above.
      dbMain.ReplicationID := -1;
      DBManager.SynchronizeWith(dbDest);
    finally
      dbDest.Session.Close;
      RefreshViews;
    end;
  end;
end;

procedure TfrmEZBooksMain.actSynchronizeExecute(Sender: TObject);
begin
  // Synchronize with another database
  OpenDBDialog.Title := 'Select Main Database';
  if OpenDBDialog.Execute then
  begin
    dbDest.Session.Close;
    try
      dbDest.DatabaseURL := OpenDBDialog.DatabaseURL;
      OpenUpdateDatabase(dbDest);
      DBManager.SynchronizeWith(dbDest);
    finally
      dbDest.Session.Close;
      RefreshViews;
    end;
  end;
end;

procedure TfrmEZBooksMain.RefreshViews;
var
  I: Integer;
begin
  Caption := Format('%s - %s', [Application.Title, dbMain.DatabaseURL]);
  for I := 0 to MDIChildCount - 1 do
    if MDIChildren[I].InheritsFrom(TfrmBrowse) then
      TfrmBrowse(MDIChildren[I]).RefreshBrowse;
  dbMain.RefreshTables;

  { Only for snapshots }
  actSynchronize.Enabled := dbMain.SnapshotID <> 0;
  { Only for the main database }
  actCreateSnapshot.Enabled := dbMain.SnapshotID = 0;
end;

procedure TfrmEZBooksMain.actExportToFileExecute(Sender: TObject);
begin
  // Export database to file
  SaveDialog.Title := 'Export Database';
  if SaveDialog.Execute then
    DBManager.SaveDatabaseToFile(SaveDialog.FileName);
end;

procedure TfrmEZBooksMain.actImportFromFileExecute(Sender: TObject);
begin
  // Import database from file
  OpenDialog.Title := 'Import Database';
  if OpenDialog.Execute then
    DBManager.LoadDatabaseFromFile(OpenDialog.FileName);
end;

procedure TfrmEZBooksMain.actViewCustomersExecute(Sender: TObject);
begin
  TfrmBrowseCustomers.Browse;
end;

procedure TfrmEZBooksMain.actEnterCustomerExecute(Sender: TObject);
begin
  with TfrmEditCustomers.Create(Self) do
  try
    Insert;
  finally
    Free;
  end;
end;

procedure TfrmEZBooksMain.actViewTaxTypesExecute(Sender: TObject);
begin
  TfrmBrowseTaxTypes.Browse;
end;

procedure TfrmEZBooksMain.actViewFormsOfPaymentExecute(Sender: TObject);
begin
  TfrmBrowseFormOfPayments.Browse;
end;

procedure TfrmEZBooksMain.actViewItemsExecute(Sender: TObject);
begin
  TfrmBrowseItems.Browse;
end;

procedure TfrmEZBooksMain.actViewOrdersExecute(Sender: TObject);
begin
  TfrmBrowseOrders.Browse;
end;

procedure TfrmEZBooksMain.actEnterOrderExecute(Sender: TObject);
begin
  with TfrmEditOrders.Create(Self) do
  try
    Insert;
  finally
    Free;
  end;
end;

procedure TfrmEZBooksMain.actViewPaymentsExecute(Sender: TObject);
begin
  TfrmBrowsePayments.Browse;
end;

procedure TfrmEZBooksMain.actEnterPaymentExecute(Sender: TObject);
begin
  with TfrmEditPayment.Create(Self) do
  try
    Insert;
  finally
    Free;
  end;
end;

procedure TfrmEZBooksMain.actViewCompanyInfoExecute(Sender: TObject);
begin
  with TfrmEditCompanyInfo.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmEZBooksMain.actViewCountersExecute(Sender: TObject);
begin
  with TfrmCounters.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmEZBooksMain.actViewObjectsExecute(Sender: TObject);
begin
  TfrmBrowseObjects.Create(Self);
end;

procedure TfrmEZBooksMain.dbMainProgress(Sender: TnxDatabaseExt;
  const Operation: String; PercentDone: Byte; var Abort: Boolean);
begin
  ProgressBar.Position := PercentDone;
  if PercentDone < 100 then
  begin
    Screen.Cursor := crHourGlass;
    StatusBar.Panels[0].Text := Operation;
  end else begin
    Screen.Cursor := crDefault;
    ProgressBar.Position := 0;
    StatusBar.Panels[0].Text := 'Done';
  end;
end;

procedure TfrmEZBooksMain.OpenUpdateDatabase(Database: TnxDatabaseExt);
begin
  Database.Open;
  // If version is not current (or newer), run update scripts stored in schema
  if not Database.IsVersionCurrent(True) then
    Database.UpdateDatabase;
end;

end.


