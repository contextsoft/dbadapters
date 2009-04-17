unit fEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Db, DBISAMTb, DBISAMExt, dbSchema;

type
  TfrmEditObject = class(TForm)
    Panel1: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    Bevel1: TBevel;
    tblData: TDBISAMTableExt;
    dsData: TDataSource;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tblDataBeforePost(DataSet: TDataSet);
  protected
    { Protected declarations }
    FKeyValue: Variant;
    FBrowseDataSet: TDataSet;

    procedure InternalNew; virtual;
    procedure InternalOpen; virtual;
    procedure InternalSave; virtual;
    procedure InternalCancel; virtual;
    procedure InternalDelete; virtual;
  public
    { Public declarations }
    class function KeyFields: String; virtual;
    class function TableName: String; virtual;
    class function DisplayName: String; virtual;

    function Insert: Boolean; virtual;
    function Edit: Boolean; virtual;
    function Delete: Boolean; virtual;

    function ConfirmDelete: Boolean; virtual;
    function IsModified: Boolean; virtual;
    procedure OpenTable; virtual;
    procedure LocateCurrentRecord;
    procedure SaveChanges(Confirm: Boolean = True);

    property KeyValue: Variant read FKeyValue write FKeyValue;
    property BrowseDataSet: TDataSet read FBrowseDataSet write FBrowseDataSet;
  end;

  TEditFormClass = class of TfrmEditObject;

var
  frmEditObject: TfrmEditObject;

implementation

uses fMain, dReferences;

{$R *.DFM}

resourcestring
  SConfirmSaveChanges = 'Would you like to save changes to current %s?';
  SConfirmDelete = 'Delete selected %s';
  SRecordNotFound = '%s not found. It is possible that it has been deleted by another user. Please click refresh and try again.';

{ TfrmEditObject }

class function TfrmEditObject.TableName: String;
begin
  Result := '';
end;

class function TfrmEditObject.KeyFields: String;
begin
  Result := '';
end;

class function TfrmEditObject.DisplayName: String;
begin
  Result := TableName;
end;

function TfrmEditObject.Delete: Boolean;
begin
  OpenTable;
  LocateCurrentRecord;
  Result := ConfirmDelete;
  if Result then
    InternalDelete;
end;

function TfrmEditObject.Edit: Boolean;
begin
  OpenTable;
  LocateCurrentRecord;
  Result := ShowModal = mrOK;
end;

function TfrmEditObject.Insert: Boolean;
begin
  OpenTable;
  InternalNew;
  BrowseDataSet := nil;
  Result := ShowModal = mrOK;
end;

procedure TfrmEditObject.OpenTable;
begin
  if not tblData.Active then
  begin
    tblData.TableName := TableName;
    tblData.Active := True;
  end;
end;

function TfrmEditObject.ConfirmDelete: Boolean;
begin
  Result := MessageDlg(Format(SConfirmDelete, [DisplayName]), mtConfirmation, [mbOK,mbCancel], 0) = mrOk;
end;

procedure TfrmEditObject.LocateCurrentRecord;
begin
  if Assigned(BrowseDataSet) then
  begin
    KeyValue := BrowseDataSet.FieldValues[KeyFields];
    btnPrev.Enabled := BrowseDataSet.RecNo <> 1;
    btnNext.Enabled := BrowseDataSet.RecNo <> BrowseDataSet.RecordCount;
  end;
  InternalOpen;
end;

procedure TfrmEditObject.btnNextClick(Sender: TObject);
begin
  // Make sure to save or discard changes
  SaveChanges;
  // Promote to the next record
  BrowseDataSet.Refresh;
  BrowseDataSet.Next;
  // Locate that record
  LocateCurrentRecord;
end;

procedure TfrmEditObject.btnPrevClick(Sender: TObject);
begin
  // Make sure to save or discard changes
  SaveChanges;
  // Promote to the next record
  BrowseDataSet.Refresh;
  BrowseDataSet.Prior;
  // Locate that record
  LocateCurrentRecord;
end;

procedure TfrmEditObject.SaveChanges(Confirm: Boolean = True);
begin
  if tblData.State in dsEditModes then
  begin
    tblData.UpdateRecord;
    if IsModified then
    begin
      if Confirm then
        case MessageDlg(Format(SConfirmSaveChanges, [DisplayName]),
          mtConfirmation, [mbYes,mbNo,mbCancel], 0)
        of
          mrYes: InternalSave;
          mrNo: InternalCancel;
          else Abort;
        end
      else InternalSave;
    end else
      InternalCancel;
  end;
end;

procedure TfrmEditObject.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  SaveChanges(ModalResult <> mrOK);
end;

procedure TfrmEditObject.FormShow(Sender: TObject);
begin
  btnPrev.Visible := Assigned(BrowseDataSet);
  btnNext.Visible := Assigned(BrowseDataSet);
end;

procedure TfrmEditObject.FormCreate(Sender: TObject);
begin
  frmEZBooksMain.DBISAMEvents.HookUpToDataSets(Self);
end;

procedure TfrmEditObject.InternalCancel;
begin
  tblData.Cancel;
end;

procedure TfrmEditObject.InternalNew;
begin
  tblData.Insert;
end;

procedure TfrmEditObject.InternalOpen;
begin
  if not tblData.Locate(KeyFields, KeyValue, []) then
    DatabaseErrorFmt(SRecordNotFound, [DisplayName]);
end;

procedure TfrmEditObject.InternalSave;
begin
  tblData.Post;
  KeyValue := tblData.FieldValues[KeyFields];
end;

procedure TfrmEditObject.InternalDelete;
begin
  tblData.Delete;
end;

procedure TfrmEditObject.tblDataBeforePost(DataSet: TDataSet);
var
  KeyField: TField;
begin
  with tblData do
  begin
    KeyField := FieldByName(KeyFields);
    if (KeyField is TIntegerField) and (State = dsInsert) and (KeyField.AsInteger = 0) then
      KeyField.AsInteger := dmReferences.GetNextID(KeyFields);
  end;
end;

function TfrmEditObject.IsModified: Boolean;
begin
  Result := tblData.Modified;
end;

end.
