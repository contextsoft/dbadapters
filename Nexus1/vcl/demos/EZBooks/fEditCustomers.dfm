inherited frmEditCustomers: TfrmEditCustomers
  ActiveControl = DBEdit1
  Caption = 'Edit Customer'
  ClientHeight = 393
  ClientWidth = 432
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 16
    Top = 12
    Width = 55
    Height = 13
    Caption = 'CustomerID'
    FocusControl = DBEdit1
  end
  object Label3: TLabel [1]
    Left = 16
    Top = 56
    Width = 72
    Height = 13
    Caption = 'CompanyName'
    FocusControl = DBEdit3
  end
  object Label4: TLabel [2]
    Left = 164
    Top = 96
    Width = 48
    Height = 13
    Caption = 'LastName'
    FocusControl = DBEdit4
  end
  object Label5: TLabel [3]
    Left = 16
    Top = 96
    Width = 47
    Height = 13
    Caption = 'FirstName'
    FocusControl = DBEdit5
  end
  object Label6: TLabel [4]
    Left = 124
    Top = 96
    Width = 24
    Height = 13
    Caption = 'Initial'
    FocusControl = DBEdit6
  end
  object Label7: TLabel [5]
    Left = 16
    Top = 136
    Width = 33
    Height = 13
    Caption = 'CareOf'
    FocusControl = DBEdit7
  end
  object Label8: TLabel [6]
    Left = 16
    Top = 176
    Width = 42
    Height = 13
    Caption = 'StreetNo'
    FocusControl = DBEdit8
  end
  object Label9: TLabel [7]
    Left = 84
    Top = 176
    Width = 28
    Height = 13
    Caption = 'Street'
    FocusControl = DBEdit9
  end
  object Label10: TLabel [8]
    Left = 16
    Top = 216
    Width = 17
    Height = 13
    Caption = 'City'
    FocusControl = DBEdit10
  end
  object Label11: TLabel [9]
    Left = 192
    Top = 216
    Width = 25
    Height = 13
    Caption = 'State'
    FocusControl = DBEdit11
  end
  object Label12: TLabel [10]
    Left = 240
    Top = 216
    Width = 15
    Height = 13
    Caption = 'Zip'
    FocusControl = DBEdit12
  end
  object Label13: TLabel [11]
    Left = 16
    Top = 256
    Width = 59
    Height = 13
    Caption = 'HomePhone'
    FocusControl = DBEdit13
  end
  object Label14: TLabel [12]
    Left = 148
    Top = 256
    Width = 57
    Height = 13
    Caption = 'WorkPhone'
    FocusControl = DBEdit14
  end
  object Label15: TLabel [13]
    Left = 16
    Top = 296
    Width = 62
    Height = 13
    Caption = 'MobilePhone'
    FocusControl = DBEdit15
  end
  object Label16: TLabel [14]
    Left = 148
    Top = 296
    Width = 17
    Height = 13
    Caption = 'Fax'
    FocusControl = DBEdit16
  end
  object Label17: TLabel [15]
    Left = 16
    Top = 336
    Width = 42
    Height = 13
    Caption = 'TaxType'
  end
  object Label2: TLabel [16]
    Left = 88
    Top = 12
    Width = 55
    Height = 13
    Caption = 'Referred by'
    FocusControl = DBLookupComboBox2
  end
  inherited Panel1: TPanel
    Left = 344
    Height = 393
    TabOrder = 18
    inherited Bevel1: TBevel
      Height = 393
    end
  end
  object DBEdit1: TDBEdit [18]
    Left = 16
    Top = 28
    Width = 64
    Height = 21
    DataField = 'CustomerID'
    DataSource = dsData
    TabOrder = 0
  end
  object DBEdit3: TDBEdit [19]
    Left = 16
    Top = 72
    Width = 305
    Height = 21
    DataField = 'CompanyName'
    DataSource = dsData
    TabOrder = 3
  end
  object DBEdit4: TDBEdit [20]
    Left = 164
    Top = 112
    Width = 157
    Height = 21
    DataField = 'LastName'
    DataSource = dsData
    TabOrder = 4
  end
  object DBEdit5: TDBEdit [21]
    Left = 16
    Top = 112
    Width = 101
    Height = 21
    DataField = 'FirstName'
    DataSource = dsData
    TabOrder = 5
  end
  object DBEdit6: TDBEdit [22]
    Left = 124
    Top = 112
    Width = 34
    Height = 21
    DataField = 'Initial'
    DataSource = dsData
    TabOrder = 6
  end
  object DBEdit7: TDBEdit [23]
    Left = 16
    Top = 152
    Width = 305
    Height = 21
    DataField = 'CareOf'
    DataSource = dsData
    TabOrder = 7
  end
  object DBEdit8: TDBEdit [24]
    Left = 16
    Top = 192
    Width = 65
    Height = 21
    DataField = 'StreetNo'
    DataSource = dsData
    TabOrder = 8
  end
  object DBEdit9: TDBEdit [25]
    Left = 88
    Top = 192
    Width = 233
    Height = 21
    DataField = 'Street'
    DataSource = dsData
    TabOrder = 9
  end
  object DBEdit10: TDBEdit [26]
    Left = 16
    Top = 232
    Width = 169
    Height = 21
    DataField = 'City'
    DataSource = dsData
    TabOrder = 10
  end
  object DBEdit11: TDBEdit [27]
    Left = 192
    Top = 232
    Width = 41
    Height = 21
    DataField = 'State'
    DataSource = dsData
    TabOrder = 11
  end
  object DBEdit12: TDBEdit [28]
    Left = 240
    Top = 232
    Width = 81
    Height = 21
    DataField = 'Zip'
    DataSource = dsData
    TabOrder = 12
  end
  object DBEdit13: TDBEdit [29]
    Left = 16
    Top = 272
    Width = 124
    Height = 21
    DataField = 'HomePhone'
    DataSource = dsData
    TabOrder = 13
  end
  object DBEdit14: TDBEdit [30]
    Left = 148
    Top = 272
    Width = 124
    Height = 21
    DataField = 'WorkPhone'
    DataSource = dsData
    TabOrder = 14
  end
  object DBEdit15: TDBEdit [31]
    Left = 16
    Top = 312
    Width = 124
    Height = 21
    DataField = 'MobilePhone'
    DataSource = dsData
    TabOrder = 15
  end
  object DBEdit16: TDBEdit [32]
    Left = 148
    Top = 312
    Width = 124
    Height = 21
    DataField = 'Fax'
    DataSource = dsData
    TabOrder = 16
  end
  object DBLookupComboBox1: TDBLookupComboBox [33]
    Left = 16
    Top = 352
    Width = 145
    Height = 21
    DataField = 'TaxTypeLookup'
    DataSource = dsData
    TabOrder = 17
  end
  object DBLookupComboBox2: TDBLookupComboBox [34]
    Left = 88
    Top = 28
    Width = 233
    Height = 21
    DataField = 'ReferredByLookup'
    DataSource = dsData
    TabOrder = 2
  end
  object btnSelectCustomer: TBitBtn [35]
    Left = 248
    Top = 3
    Width = 72
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Select...'
    TabOrder = 1
    OnClick = btnSelectCustomerClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF008CC00CC88CC0
      0CC8880700888807008888007088880070888807008888070088880070888800
      7088830700388307003880000008800000088070000880700008807000088070
      00088078800880788008807FF008807FF0088708807887088078888B9888888B
      98888889B8888889B88888800888888008888888888888888888}
    Margin = 4
  end
  inherited tblData: TnxTableExt
    TableName = 'Customers'
    AutoFieldsProperties = True
    Left = 368
    Top = 164
    object tblDataCustomerID: TIntegerField
      DefaultExpression = '0'
      FieldName = 'CustomerID'
      Required = True
    end
    object tblDataCompanyName: TStringField
      FieldName = 'CompanyName'
      Origin = 'Customers.CompanyName'
      Size = 40
    end
    object tblDataLastName: TStringField
      FieldName = 'LastName'
      Origin = 'Customers.LastName'
      Size = 40
    end
    object tblDataFirstName: TStringField
      FieldName = 'FirstName'
      Origin = 'Customers.FirstName'
      Size = 40
    end
    object tblDataInitial: TStringField
      FieldName = 'Initial'
      Origin = 'Customers.Initial'
      Size = 5
    end
    object tblDataCareOf: TStringField
      FieldName = 'CareOf'
      Origin = 'Customers.CareOf'
    end
    object tblDataStreetNo: TStringField
      FieldName = 'StreetNo'
      Origin = 'Customers.StreetNo'
    end
    object tblDataStreet: TStringField
      FieldName = 'Street'
      Origin = 'Customers.Street'
      Size = 60
    end
    object tblDataCity: TStringField
      FieldName = 'City'
      Origin = 'Customers.City'
      Size = 80
    end
    object tblDataState: TStringField
      FieldName = 'State'
      Origin = 'Customers.State'
      Size = 15
    end
    object tblDataZip: TStringField
      FieldName = 'Zip'
      Origin = 'Customers.Zip'
    end
    object tblDataHomePhone: TStringField
      FieldName = 'HomePhone'
      Origin = 'Customers.HomePhone'
    end
    object tblDataWorkPhone: TStringField
      FieldName = 'WorkPhone'
      Origin = 'Customers.WorkPhone'
    end
    object tblDataMobilePhone: TStringField
      FieldName = 'MobilePhone'
      Origin = 'Customers.MobilePhone'
    end
    object tblDataFax: TStringField
      FieldName = 'Fax'
      Origin = 'Customers.Fax'
    end
    object tblDataTaxType: TStringField
      FieldName = 'TaxType'
      Origin = 'Customers.TaxType'
    end
    object tblDataTaxTypeLookup: TStringField
      FieldKind = fkLookup
      FieldName = 'TaxTypeLookup'
      LookupDataSet = dmReferences.tblTaxTypes
      LookupKeyFields = 'TaxType'
      LookupResultField = 'TaxType'
      KeyFields = 'TaxType'
      Lookup = True
    end
    object tblDataReferredBy: TIntegerField
      FieldName = 'ReferredBy'
      Origin = 'Customers.ReferredBy'
    end
    object tblDataReferredByLookup: TStringField
      FieldKind = fkLookup
      FieldName = 'ReferredByLookup'
      LookupDataSet = dmReferences.tblCustomers
      LookupKeyFields = 'CustomerID'
      LookupResultField = 'CompanyName'
      KeyFields = 'ReferredBy'
      Size = 80
      Lookup = True
    end
  end
  inherited dsData: TDataSource
    Left = 382
    Top = 180
  end
end
