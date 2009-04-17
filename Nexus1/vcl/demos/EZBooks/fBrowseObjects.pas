unit fBrowseObjects;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, Grids, DBGrids, Menus, nxdb, nxdbext;

type
  TfrmBrowseObjects = class(TForm)
    grdBrowse: TDBGrid;
    tblObjects: TnxTableExt;
    dsObjects: TDataSource;
    tblObjectsObjectType: TStringField;
    tblObjectsObjectKey: TStringField;
    tblObjectsReplicationID: TIntegerField;
    tblObjectsSnapshotID: TIntegerField;
    tblObjectsTimestamp: TDateTimeField;
    tblObjectsChangeType: TIntegerField;
    tblObjectsChangeStatus: TIntegerField;
    tblObjectsUserName: TStringField;
    tblObjectsObjectData: TBlobField;
    popBrowse: TPopupMenu;
    popRefresh: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure tblObjectsChangeTypeGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
    procedure tblObjectsChangeStatusGetText(Sender: TField;
      var Text: String; DisplayText: Boolean);
    procedure popRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowseObjects: TfrmBrowseObjects;

implementation

uses fMain;

{$R *.DFM}

procedure TfrmBrowseObjects.FormShow(Sender: TObject);
begin
  tblObjects.Refresh;
end;

procedure TfrmBrowseObjects.tblObjectsChangeTypeGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  if not Sender.IsNull then
    Text := Format('%'+Sender.AsString + ':s', ['Deleted', 'ModifiedReference', 'ModifiedContent', 'Modified', 'Inserted']);
end;

procedure TfrmBrowseObjects.tblObjectsChangeStatusGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  if not Sender.IsNull then
    Text := Format('%'+Sender.AsString + ':s', ['Active', 'Inactive', 'Confirm', 'Confirmed']);
end;

procedure TfrmBrowseObjects.popRefreshClick(Sender: TObject);
begin
  tblObjects.Refresh;
end;

procedure TfrmBrowseObjects.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
