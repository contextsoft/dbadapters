inherited frmEditItem: TfrmEditItem
  Caption = 'Edit Item'
  ClientHeight = 164
  ClientWidth = 398
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 24
    Top = 16
    Width = 30
    Height = 13
    Caption = 'Item #'
    FocusControl = DBEdit1
  end
  object Label2: TLabel [1]
    Left = 24
    Top = 60
    Width = 53
    Height = 13
    Caption = 'Description'
    FocusControl = DBEdit2
  end
  object Label3: TLabel [2]
    Left = 24
    Top = 104
    Width = 48
    Height = 13
    Caption = 'Sale Price'
    FocusControl = DBEdit3
  end
  inherited Panel1: TPanel
    Left = 310
    Height = 164
    TabOrder = 4
    inherited Bevel1: TBevel
      Height = 164
    end
  end
  object DBEdit1: TDBEdit [4]
    Left = 24
    Top = 32
    Width = 64
    Height = 21
    DataField = 'ItemID'
    DataSource = dsData
    TabOrder = 0
  end
  object DBEdit2: TDBEdit [5]
    Left = 24
    Top = 76
    Width = 265
    Height = 21
    DataField = 'Description'
    DataSource = dsData
    TabOrder = 1
  end
  object DBEdit3: TDBEdit [6]
    Left = 24
    Top = 120
    Width = 64
    Height = 21
    DataField = 'SalePrice'
    DataSource = dsData
    TabOrder = 2
  end
  object DBCheckBox1: TDBCheckBox [7]
    Left = 108
    Top = 124
    Width = 97
    Height = 17
    Caption = 'Taxable'
    DataField = 'Taxable'
    DataSource = dsData
    TabOrder = 3
    ValueChecked = 'True'
    ValueUnchecked = 'False'
  end
  inherited tblData: TnxTableExt
    TableName = 'Items'
    AutoFieldsProperties = True
    Left = 264
    Top = 20
    object tblDataItemID: TIntegerField
      FieldName = 'ItemID'
      Required = True
    end
    object tblDataDescription: TStringField
      FieldName = 'Description'
      Origin = 'Items.Description'
      Size = 80
    end
    object tblDataSalePrice: TCurrencyField
      DisplayLabel = 'Sale Price'
      FieldName = 'SalePrice'
      Origin = 'Items.SalePrice'
    end
    object tblDataTaxable: TBooleanField
      DefaultExpression = 'True'
      FieldName = 'Taxable'
      Origin = 'Items.Taxable'
    end
  end
  inherited dsData: TDataSource
    Left = 278
    Top = 36
  end
end
