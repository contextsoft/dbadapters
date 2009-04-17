unit fBrowseFormOfPayments;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fBrowse, ImgList, ActnList, Menus, Db, Grids, DBGrids, ComCtrls, StdCtrls,
  Buttons, ToolWin, nxdb, nxdbext;

type
  TfrmBrowseFormOfPayments = class(TfrmBrowse)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowseFormOfPayments: TfrmBrowseFormOfPayments;

implementation

uses fEditFormOfPayments;

{$R *.DFM}

procedure TfrmBrowseFormOfPayments.FormCreate(Sender: TObject);
begin
  FEditFormClass := TfrmEditFormOfPayments;
  inherited;
end;

end.
