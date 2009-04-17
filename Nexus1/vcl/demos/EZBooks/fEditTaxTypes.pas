unit fEditTaxTypes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fEdit, Db, StdCtrls, Buttons, ExtCtrls, Mask, DBCtrls, nxdb, nxdbext;

type
  TfrmEditTaxTypes = class(TfrmEditObject)
    tblDataTaxType: TStringField;
    tblDataTax: TFloatField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
  private
    { Private declarations }
  public
    { Public declarations }
    class function KeyFields: String; override;
    class function TableName: String; override;
    class function DisplayName: String; override;
  end;

var
  frmEditTaxTypes: TfrmEditTaxTypes;

implementation

{$R *.DFM}

{ TfrmEditObject1 }

class function TfrmEditTaxTypes.DisplayName: String;
begin
  Result := 'type of tax';
end;

class function TfrmEditTaxTypes.KeyFields: String;
begin
  Result := 'TaxType';
end;

class function TfrmEditTaxTypes.TableName: String;
begin
  Result := 'TaxTypes';
end;

end.
