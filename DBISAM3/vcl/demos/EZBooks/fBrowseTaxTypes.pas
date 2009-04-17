unit fBrowseTaxTypes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fBrowse, ImgList, ActnList, Menus, Db, DBISAMTb, DBISAMExt, Grids,
  DBGrids, ComCtrls, StdCtrls, Buttons, ToolWin;

type
  TfrmBrowseTaxTypes = class(TfrmBrowse)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowseTaxTypes: TfrmBrowseTaxTypes;

implementation

uses fEditTaxTypes;

{$R *.DFM}

procedure TfrmBrowseTaxTypes.FormCreate(Sender: TObject);
begin
  FEditFormClass := TfrmEditTaxTypes;
  inherited;
end;

end.
