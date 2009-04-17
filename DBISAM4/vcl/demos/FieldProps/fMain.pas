unit fMain;

interface

uses
  Windows, Messages, SysUtils, {$IFnDEF VER130}Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, dbExtUtils, dbSchema, DBISAMTb,
  DBISAMExt, ExtCtrls;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    btnExecute: TButton;
    DatabaseSchema1: TDatabaseSchema;
    DBISAMDatabaseExt1: TDBISAMDatabaseExt;
    DBISAMQueryExt1: TDBISAMQueryExt;
    DataSource1: TDataSource;
    Label1: TLabel;
    Shape1: TShape;
    procedure btnExecuteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnExecuteClick(Sender: TObject);
begin
  DBISAMDatabaseExt1.Directory := GetRelativePath('..\EZBooks\Data');
  DBISAMQueryExt1.Active := False;
  DBISAMQueryExt1.Active := True;
end;

end.
