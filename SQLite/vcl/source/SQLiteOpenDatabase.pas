(******************************************************************************)
(*
(*  Context Database Extensions Suite (SQLite)
(*
(*  Open SQLite Database Dialog component.
(*  Contains:
(*      TSQLiteOpenDatabase
(*      TSQLiteOpenDatabaseDialog
(*
(*  Copyright (c) 2006 Michael Baytalsky
(*
(******************************************************************************)
unit SQLiteOpenDatabase;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Db, dbSchema, dbExtUtils, SQLiteDBExt, Dialogs;

type
  TSQLiteOpenDatabase = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cbxDatabases: TComboBox;
    btnSelect: TButton;
    lblDatabase: TLabel;
    OpenDialog: TOpenDialog;

    procedure btnSelectClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    FDatabaseName: String;
    FMRUDatabases: TStrings;
    procedure LoadDatabases; virtual;
    procedure DataToForm; virtual;
    procedure FormToData; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function Execute(var DatabaseURL: String): Boolean;
    property MRUDatabases: TStrings read FMRUDatabases;
  end;

  {:$ TSQLiteOpenDatabaseDialog displays database selection dialog. }
  {:: TSQLiteOpenDatabaseDialog displays a modal dialog box for selecting and opening SQLite Databases. }
  {:: The dialog does not appear at runtime until it is activated by a call to the Execute method. }
  {:: When the user clicks OK, the dialog closes and the selected database path is stored }
  {:: in the DatabaseURL property.}
  TSQLiteOpenDatabaseDialog = class(TComponent)
  protected
    FDatabaseURL: String;
    FMRUServers: TStrings;
    FMRUDatabases: TStrings;
    FTitle: String;

    procedure SetMRUDatabases(const Value: TStrings);
  public
    {:$ Creates an instance of TnxOpenDatabaseDialog component. }
    constructor Create(Owner: TComponent); override;
    {:$ Destroys the instance of TnxOpenDatabaseDialog component. }
    destructor Destroy; override;
    {:$ Executes modal open database dialog. }
    {:: Returns True if the user pressed OK to select a database. The result }
    {:: of users input is stored in the DatabaseURL property. }
    function Execute: Boolean;
    {:$ Contains a list of most recently user (MRU) database names. }
    property MRUDatabases: TStrings read FMRUDatabases write SetMRUDatabases;
  published
    {:$ Returns the URL for the selected database. }
    property DatabaseURL: String read FDatabaseURL write FDatabaseURL;
    {:$ Specifies the title for the open database dialog. }
    {:: Default title is 'Open Database'. }
    property Title: String read FTitle write FTitle;
  end;

resourcestring
  SServerNameEmpty = 'Server name can not be empty.';
  SDatabasePathEmpty = 'Database path can not be empty.';
  SDatabaseNameEmpty = 'Database name can not be empty.';

implementation

uses ShlObj;

{$R *.DFM}

{ TSQLiteOpenDatabase }

constructor TSQLiteOpenDatabase.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
end;

destructor TSQLiteOpenDatabase.Destroy;
begin
  inherited Destroy;
  FMRUDatabases.Free;
end;

function TSQLiteOpenDatabase.Execute(var DatabaseURL: String): Boolean;
var
  CT, RH: string;
begin
  dbSchema.DecodeDatabaseURL(DatabaseURL, CT, RH, FDatabaseName);
  DataToForm;
  Result := ShowModal = mrOK;
  if Result then
    DatabaseURL := dbSchema.EncodeDatabaseURL('', '', FDatabaseName);
end;

procedure TSQLiteOpenDatabase.DataToForm;
begin
  LoadDatabases;
end;

procedure TSQLiteOpenDatabase.FormToData;
begin
  FDatabaseName := cbxDatabases.Text;
end;

procedure TSQLiteOpenDatabase.LoadDatabases;
begin
  with cbxDatabases do
  begin
    if FDatabaseName <> '' then
      MRUDatabases.Add(FDatabaseName);
    Style := csDropDown;
    Items.Assign(MRUDatabases);
    ItemIndex := Items.IndexOf(FDatabaseName);
    if ItemIndex < 0 then
      ItemIndex := 0;
  end;
end;

procedure TSQLiteOpenDatabase.btnSelectClick(Sender: TObject);
var
  S: String;
begin
  S := cbxDatabases.Text;
  OpenDialog.FileName := S;
  if not OpenDialog.Execute then
    Exit;
  S := OpenDialog.FileName;
  with cbxDatabases do
  begin
    if S <> '' then
      MRUDatabases.Add(S);
    Items.Assign(MRUDatabases);
    ItemIndex := Items.IndexOf(S);
  end;
end;

procedure TSQLiteOpenDatabase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    FormToData;
    if FDatabaseName = '' then
      DatabaseError(SDatabaseNameEmpty);
  end;
end;

{ TSQLiteOpenDatabaseDialog }

constructor TSQLiteOpenDatabaseDialog.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FDatabaseURL := '';
  FMRUDatabases := TStringList.Create;
  TStringList(FMRUDatabases).Duplicates := dupIgnore;
  TStringList(FMRUDatabases).Sorted := True;
  FTitle := '';
end;

destructor TSQLiteOpenDatabaseDialog.Destroy;
begin
  inherited Destroy;
  FMRUServers.Free;
  FMRUDatabases.Free;
end;

function TSQLiteOpenDatabaseDialog.Execute: Boolean;
begin
  with TSQLiteOpenDatabase.Create(Application.MainForm) do
  try
    if FTitle <> '' then
      Caption := FTitle;
    MRUDatabases.Assign(Self.MRUDatabases);
    Result := Execute(FDatabaseURL);
    if Result then
      Self.MRUDatabases.Assign(MRUDatabases);
  finally
    Free;
  end;
end;

procedure TSQLiteOpenDatabaseDialog.SetMRUDatabases(const Value: TStrings);
begin
  FMRUDatabases.Assign(Value);
end;

end.
