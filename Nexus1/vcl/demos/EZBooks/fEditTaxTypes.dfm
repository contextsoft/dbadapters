inherited frmEditTaxTypes: TfrmEditTaxTypes
  Left = 425
  Top = 276
  Caption = 'Edit Tax Type'
  ClientHeight = 138
  ClientWidth = 308
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 32
    Top = 44
    Width = 48
    Height = 13
    Caption = 'Tax Type:'
    FocusControl = DBEdit1
  end
  object Label2: TLabel [1]
    Left = 32
    Top = 72
    Width = 57
    Height = 13
    Caption = 'Tax Amount'
    FocusControl = DBEdit2
  end
  inherited Panel1: TPanel
    Left = 220
    Height = 138
    TabOrder = 2
    inherited Bevel1: TBevel
      Height = 138
    end
  end
  object DBEdit1: TDBEdit [3]
    Left = 104
    Top = 40
    Width = 64
    Height = 21
    DataField = 'TaxType'
    DataSource = dsData
    TabOrder = 0
  end
  object DBEdit2: TDBEdit [4]
    Left = 104
    Top = 68
    Width = 64
    Height = 21
    DataField = 'Tax'
    DataSource = dsData
    TabOrder = 1
  end
  inherited tblData: TnxTableExt
    TableName = 'TaxTypes'
    Left = 180
    Top = 52
    object tblDataTaxType: TStringField
      FieldName = 'TaxType'
      Size = 10
    end
    object tblDataTax: TFloatField
      FieldName = 'Tax'
    end
  end
  inherited dsData: TDataSource
    Left = 194
    Top = 68
  end
end
