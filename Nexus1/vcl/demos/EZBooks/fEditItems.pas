unit fEditItems;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fEdit, Db, StdCtrls, Buttons, ExtCtrls, DBCtrls, Mask, nxdb, nxdbext;

type
  TfrmEditItem = class(TfrmEditObject)
    tblDataDescription: TStringField;
    tblDataSalePrice: TCurrencyField;
    tblDataTaxable: TBooleanField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    DBCheckBox1: TDBCheckBox;
    tblDataItemID: TIntegerField;
  private
    { Private declarations }
  public
    { Public declarations }
    class function KeyFields: String; override;
    class function TableName: String; override;
    class function DisplayName: String; override;
  end;

var
  frmEditItem: TfrmEditItem;

implementation

{$R *.DFM}

{ TfrmEditItem }

class function TfrmEditItem.DisplayName: String;
begin
  Result := 'item';
end;

class function TfrmEditItem.KeyFields: String;
begin
  Result := 'ItemID';
end;

class function TfrmEditItem.TableName: String;
begin
  Result := 'Items';
end;

end.
