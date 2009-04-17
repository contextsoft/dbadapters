unit fEditFormOfPayments;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fEdit, Db, StdCtrls, Buttons, ExtCtrls, Mask, DBCtrls, nxdb, nxdbext;

type
  TfrmEditFormOfPayments = class(TfrmEditObject)
    tblDataFormOfPayment: TStringField;
    tblDataDescription: TStringField;
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
  frmEditFormOfPayments: TfrmEditFormOfPayments;

implementation

{$R *.DFM}

{ TfrmEditFormOfPayments }

class function TfrmEditFormOfPayments.DisplayName: String;
begin
  Result := 'form of payments';
end;

class function TfrmEditFormOfPayments.KeyFields: String;
begin
  Result := 'FormOfPayment';
end;

class function TfrmEditFormOfPayments.TableName: String;
begin
  Result := 'FormOfPayments';
end;

end.
