unit fBrowseItems;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fBrowse, ImgList, ActnList, Menus, Db, DBISAMTb, DBISAMExt, Grids,
  DBGrids, ComCtrls, StdCtrls, Buttons, ToolWin;

type
  TfrmBrowseItems = class(TfrmBrowse)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowseItems: TfrmBrowseItems;

implementation

uses fEditItems;

{$R *.DFM}

procedure TfrmBrowseItems.FormCreate(Sender: TObject);
begin
  FEditFormClass := TfrmEditItem;
  inherited;
end;

end.
