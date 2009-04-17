unit fBrowse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFnDEF VER130}
  Variants,
{$ENDIF}
  Db, DBISAMTb, DBISAMExt, Grids, DBGrids, ToolWin, ComCtrls, ExtCtrls,
  DBCtrls, fEdit, Menus, StdCtrls, Buttons, ImgList, ActnList;

type
  TfrmBrowse = class(TForm)
    ToolBar: TToolBar;
    dsBrowse: TDataSource;
    qryBrowse: TDBISAMQueryExt;
    StatusBar: TStatusBar;
    popBrowse: TPopupMenu;
    mnuEdit: TMenuItem;
    ToolButton1: TToolButton;
    btnSelect: TBitBtn;
    grdBrowse: TDBGrid;
    Actions: TActionList;
    actEdit: TAction;
    actInsert: TAction;
    actDelete: TAction;
    Images: TImageList;
    Insert1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    actRefresh: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure grdBrowseDblClick(Sender: TObject);
    procedure grdBrowseTitleClick(Column: TColumn);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure qryBrowseAfterOpen(DataSet: TDataSet);
    procedure actEditExecute(Sender: TObject);
    procedure actInsertExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
  private
    { Private declarations }
  protected
    FEditFormClass: TEditFormClass;
    FKeyValue: Variant;
  public
    { Public declarations }
    procedure UpdateState; virtual;
    procedure RefreshBrowse; virtual;
    property KeyValue: Variant read FKeyValue write FKeyValue;

    class procedure Browse; virtual;
    class function Select(var AKeyValue: Variant): Boolean; overload; virtual;
    class function Select(Field: TField): Boolean; overload; virtual; 
  end;

var
  frmBrowse: TfrmBrowse;

implementation

{$R *.DFM}

resourcestring
  SRecordCount = '%d records';

class procedure TfrmBrowse.Browse;
var
  PrevInstance: TfrmBrowse;
begin
  PrevInstance := TfrmBrowse(Application.MainForm.FindComponent(copy(ClassName, 2, 100)));
  if Assigned(PrevInstance) then
    PrevInstance.BringToFront
  else
    with Create(Application.MainForm) do
    begin
      FormStyle := fsMDIChild;
      Visible := True;
    end;
end;

class function TfrmBrowse.Select(var AKeyValue: Variant): Boolean;
begin
  with Create(Application.MainForm) do
  try
    FKeyValue := AKeyValue;
    Result := ShowModal = mrOK;
    if Result then
      AKeyValue := qryBrowse.FieldValues[FEditFormClass.KeyFields];
  finally
    Free;
  end;
end;

class function TfrmBrowse.Select(Field: TField): Boolean;
var
  NewValue: Variant;
begin
  with Create(Application.MainForm) do
  try
    FKeyValue := Field.AsVariant;
    Result := ShowModal = mrOK;
    if Result then
    begin
      NewValue := qryBrowse.FieldValues[FEditFormClass.KeyFields];
      if NewValue <> Field.AsVariant then
      begin
        Field.DataSet.Edit;
        Field.AsVariant := NewValue;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TfrmBrowse.FormCreate(Sender: TObject);
begin
  FKeyValue := NULL;
  qryBrowse.MacroByName('fields').Value := '*';
  qryBrowse.MacroByName('table').Value := FEditFormClass.TableName;
  qryBrowse.MacroByName('where').Value := '';
  qryBrowse.MacroByName('orderby').Value := '';
end;

procedure TfrmBrowse.UpdateState;
begin
  actEdit.Enabled := qryBrowse.Active and (not qryBrowse.EOF);
  actInsert.Enabled := qryBrowse.Active;
  actDelete.Enabled := actEdit.Enabled;

  if qryBrowse.Active then
    StatusBar.SimpleText := Format(SRecordCount, [qryBrowse.RecordCount])
  else StatusBar.SimpleText := '';
end;

procedure TfrmBrowse.grdBrowseDblClick(Sender: TObject);
begin
  if fsModal in FormState then
    ModalResult := mrOK
  else if actEdit.Enabled then
    actEditExecute(nil);
end;

procedure TfrmBrowse.RefreshBrowse;
begin
  Screen.Cursor := crHourGlass;
  try
    qryBrowse.Active := False;
    qryBrowse.Active := True;
    // Locate
    if not (VarIsEmpty(FKeyValue) or VarIsNull(FKeyValue)) then
      qryBrowse.Locate(FEditFormClass.KeyFields, FKeyValue, []);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmBrowse.grdBrowseTitleClick(Column: TColumn);
begin
  if Column.Field.FieldKind = fkData then
  begin
    qryBrowse.MacroByName('orderby').Value := 'order by ' + Column.Field.FieldName;
    RefreshBrowse;
  end;
end;


procedure TfrmBrowse.FormShow(Sender: TObject);
begin
  btnSelect.Visible := fsModal in FormState;
  RefreshBrowse;
end;

procedure TfrmBrowse.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmBrowse.qryBrowseAfterOpen(DataSet: TDataSet);
begin
  UpdateState;
end;

procedure TfrmBrowse.actEditExecute(Sender: TObject);
begin
  with FEditFormClass.Create(Self) do
  try
    BrowseDataSet := qryBrowse;
    if Edit then
    begin
      Self.KeyValue := KeyValue;
      RefreshBrowse;
    end;
  finally
    Free;
  end;
end;

procedure TfrmBrowse.actInsertExecute(Sender: TObject);
begin
  with FEditFormClass.Create(Self) do
  try
    if Insert then
    begin
      Self.KeyValue := KeyValue;
      RefreshBrowse;
    end;
  finally
    Free;
  end;
end;

procedure TfrmBrowse.actDeleteExecute(Sender: TObject);
begin
  with FEditFormClass.Create(Self) do
  try
    BrowseDataSet := qryBrowse;
    if Delete then
    begin
      Self.KeyValue := NULL;
      RefreshBrowse;
    end;
  finally
    Free;
  end;
end;

procedure TfrmBrowse.actRefreshExecute(Sender: TObject);
begin
  if qryBrowse.Active then
    FKeyValue := qryBrowse.FieldValues[FEditFormClass.KeyFields]
  else FKeyValue := NULL;
  RefreshBrowse;
end;

end.
