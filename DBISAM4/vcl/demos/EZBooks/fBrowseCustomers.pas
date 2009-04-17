unit fBrowseCustomers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fBrowse, Menus, Db, DBISAMTb, DBISAMExt, ComCtrls, Grids, DBGrids,
  ExtCtrls, DBCtrls, ToolWin, StdCtrls, Buttons, ImgList, ActnList;

type
  TfrmBrowseCustomers = class(TfrmBrowse)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowseCustomers: TfrmBrowseCustomers;

implementation

uses fEditCustomers;

{$R *.DFM}

procedure TfrmBrowseCustomers.FormCreate(Sender: TObject);
begin
  FEditFormClass := TfrmEditCustomers;
  inherited;
end;

end.
