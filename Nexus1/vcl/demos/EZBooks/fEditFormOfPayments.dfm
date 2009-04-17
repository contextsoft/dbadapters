inherited frmEditFormOfPayments: TfrmEditFormOfPayments
  Caption = 'Forms of payment'
  ClientHeight = 139
  ClientWidth = 380
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 28
    Top = 20
    Width = 78
    Height = 13
    Caption = 'Form of payment'
    FocusControl = DBEdit1
  end
  object Label2: TLabel [1]
    Left = 28
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Description'
    FocusControl = DBEdit2
  end
  inherited Panel1: TPanel
    Left = 292
    Height = 139
    TabOrder = 2
    inherited Bevel1: TBevel
      Height = 139
    end
  end
  object DBEdit1: TDBEdit [3]
    Left = 28
    Top = 36
    Width = 64
    Height = 21
    DataField = 'FormOfPayment'
    DataSource = dsData
    TabOrder = 0
  end
  object DBEdit2: TDBEdit [4]
    Left = 28
    Top = 80
    Width = 244
    Height = 21
    DataField = 'Description'
    DataSource = dsData
    TabOrder = 1
  end
  inherited tblData: TnxTableExt
    TableName = 'FormOfPayments'
    AutoFieldsProperties = True
    Left = 216
    Top = 12
    object tblDataFormOfPayment: TStringField
      DisplayLabel = 'Form of payment'
      FieldName = 'FormOfPayment'
      Origin = 'FormOfPayments.FormOfPayment'
      Size = 10
    end
    object tblDataDescription: TStringField
      FieldName = 'Description'
      Origin = 'FormOfPayments.Description'
      Size = 40
    end
  end
  inherited dsData: TDataSource
    Left = 230
    Top = 28
  end
end
