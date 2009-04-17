unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dbSchema, DB, DBTables, BDEExt, Grids, DBGrids, ComCtrls,
  StdCtrls, nxllComponent, nxdb, nxDBExt, nxsrSqlEngineBase, nxsqlEngine,
  nxsdServerEngine, nxsrServerEngine, nxseAutoComponent, dbEngProfile;

type
  TfrmVersionDemo = class(TForm)
    DatabaseSchema: TDatabaseSchema;
    DatabaseExt: TnxDatabaseExt;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    lblStatus: TLabel;
    ProgressBar: TProgressBar;
    edtLog: TMemo;
    Button1: TButton;
    Label1: TLabel;
    edtDatabase: TEdit;
    Button2: TButton;
    Label2: TLabel;
    lblVersion: TLabel;
    nxSession1: TnxSession;
    nxQuery1: TnxQuery;
    nxseAllEngines1: TnxseAllEngines;
    nxServerEngine1: TnxServerEngine;
    nxSqlEngine1: TnxSqlEngine;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure OnDatabaseProgress(Sender: TObject; const Status: String; PercentDone: Byte; var Abort: Boolean);
    procedure ReOpenDatabase;
    procedure GetDatabaseVersion;
  end;

var
  frmVersionDemo: TfrmVersionDemo;

implementation

uses FileCtrl;

{$R *.dfm}

procedure TfrmVersionDemo.FormCreate(Sender: TObject);
begin
  edtDatabase.Text := ExtractFilePath(Application.ExeName) + 'Data';
  GetDatabaseVersion;
end;

procedure TfrmVersionDemo.OnDatabaseProgress(Sender: TObject; const Status: String;
  PercentDone: Byte; var Abort: Boolean);
begin
  ProgressBar.Position := PercentDone;
  lblStatus.Caption := Status;
  lblStatus.Update;
  edtLog.Lines.Add(Status);
end;

procedure TfrmVersionDemo.Button1Click(Sender: TObject);
var
  Directory: string;
begin
  Directory := edtDatabase.Text;
  if SelectDirectory(Directory, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
    edtDatabase.Text := Directory;
    GetDatabaseVersion;
  end;
end;

procedure TfrmVersionDemo.GetDatabaseVersion;
begin
  DatabaseExt.Connected := False;
  try
    DatabaseExt.DatabaseURL := edtDatabase.Text; // 'STANDARD:\\PATH\' +
    DatabaseExt.Connected := True;
  finally
    lblVersion.Caption := Format('Required Version: %s;   Actual Version: %s',
      [DatabaseSchema.VersionLabel, DatabaseExt.VersionLabel]);
    DatabaseExt.Connected := False;
  end;
end;

procedure TfrmVersionDemo.ReOpenDatabase;
begin
  DatabaseExt.Connected := False;
  DatabaseExt.DatabaseURL := edtDatabase.Text; // 'STANDARD:\\PATH\' +
  DatabaseExt.Connected := True;
  edtLog.Lines.Clear;
  if not DatabaseExt.IsVersionCurrent(True) then
    UpdateDatabase(DatabaseExt, OnDatabaseProgress)
  else begin
    ProgressBar.Position := 0;
    lblStatus.Caption := 'Done.';
    edtLog.Lines.Add(lblStatus.Caption + ' Database structure is current.');
  end;
  lblVersion.Caption := Format('Required Version: %s;   Actual Version: %s',
    [DatabaseSchema.VersionLabel, DatabaseExt.VersionLabel]);

  nxQuery1.Active := True;
end;

procedure TfrmVersionDemo.Button2Click(Sender: TObject);
begin
  ReOpenDatabase;
end;

end.
