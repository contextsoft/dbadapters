object frmEditCompanyInfo: TfrmEditCompanyInfo
  Left = 343
  Top = 284
  BorderStyle = bsDialog
  Caption = 'Company Info'
  ClientHeight = 245
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Company '
    FocusControl = DBEdit1
  end
  object Label2: TLabel
    Left = 12
    Top = 60
    Width = 69
    Height = 13
    Caption = 'Street Address'
    FocusControl = DBEdit2
  end
  object Label3: TLabel
    Left = 12
    Top = 84
    Width = 17
    Height = 13
    Caption = 'City'
    FocusControl = DBEdit3
  end
  object Label4: TLabel
    Left = 12
    Top = 108
    Width = 25
    Height = 13
    Caption = 'State'
    FocusControl = DBEdit4
  end
  object Label5: TLabel
    Left = 12
    Top = 132
    Width = 36
    Height = 13
    Caption = 'Country'
    FocusControl = DBEdit5
  end
  object Label6: TLabel
    Left = 12
    Top = 164
    Width = 31
    Height = 13
    Caption = 'Phone'
    FocusControl = DBEdit6
  end
  object Label7: TLabel
    Left = 12
    Top = 188
    Width = 17
    Height = 13
    Caption = 'Fax'
    FocusControl = DBEdit7
  end
  object Label8: TLabel
    Left = 12
    Top = 216
    Width = 25
    Height = 13
    Caption = 'Email'
    FocusControl = DBEdit8
  end
  object Label9: TLabel
    Left = 224
    Top = 8
    Width = 18
    Height = 13
    Caption = 'EID'
    FocusControl = DBEdit9
  end
  object Label10: TLabel
    Left = 180
    Top = 108
    Width = 15
    Height = 13
    Caption = 'Zip'
    FocusControl = DBEdit10
  end
  object DBEdit1: TDBEdit
    Left = 12
    Top = 24
    Width = 185
    Height = 21
    DataField = 'CompanyName'
    DataSource = dsData
    TabOrder = 0
  end
  object DBEdit2: TDBEdit
    Left = 96
    Top = 56
    Width = 185
    Height = 21
    DataField = 'StreetAddress'
    DataSource = dsData
    TabOrder = 2
  end
  object DBEdit3: TDBEdit
    Left = 96
    Top = 80
    Width = 185
    Height = 21
    DataField = 'City'
    DataSource = dsData
    TabOrder = 3
  end
  object DBEdit4: TDBEdit
    Left = 96
    Top = 104
    Width = 69
    Height = 21
    DataField = 'State'
    DataSource = dsData
    TabOrder = 4
  end
  object DBEdit5: TDBEdit
    Left = 96
    Top = 128
    Width = 185
    Height = 21
    DataField = 'Country'
    DataSource = dsData
    TabOrder = 6
  end
  object DBEdit6: TDBEdit
    Left = 52
    Top = 160
    Width = 124
    Height = 21
    DataField = 'Phone'
    DataSource = dsData
    TabOrder = 7
  end
  object DBEdit7: TDBEdit
    Left = 52
    Top = 184
    Width = 124
    Height = 21
    DataField = 'Fax'
    DataSource = dsData
    TabOrder = 8
  end
  object DBEdit8: TDBEdit
    Left = 52
    Top = 208
    Width = 297
    Height = 21
    DataField = 'Email'
    DataSource = dsData
    TabOrder = 9
  end
  object DBEdit9: TDBEdit
    Left = 224
    Top = 24
    Width = 124
    Height = 21
    DataField = 'EID'
    DataSource = dsData
    TabOrder = 1
  end
  object DBEdit10: TDBEdit
    Left = 204
    Top = 104
    Width = 77
    Height = 21
    DataField = 'Zip'
    DataSource = dsData
    TabOrder = 5
  end
  object Panel1: TPanel
    Left = 368
    Top = 0
    Width = 88
    Height = 245
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 10
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 5
      Height = 245
      Align = alLeft
      Shape = bsLeftLine
    end
    object btnOK: TBitBtn
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      Kind = bkOK
      Margin = 4
    end
    object btnCancel: TBitBtn
      Left = 8
      Top = 36
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
      Margin = 4
    end
  end
  object dsData: TDataSource
    DataSet = tblData
    Left = 406
    Top = 120
  end
  object tblData: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'CompanyInfo'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 388
    Top = 96
    object tblDataCompanyName: TStringField
      FieldName = 'CompanyName'
      Origin = 'CompanyInfo.CompanyName'
      Required = True
      Size = 60
    end
    object tblDataStreetAddress: TStringField
      FieldName = 'StreetAddress'
      Origin = 'CompanyInfo.StreetAddress'
      Size = 80
    end
    object tblDataCity: TStringField
      FieldName = 'City'
      Origin = 'CompanyInfo.City'
      Size = 60
    end
    object tblDataState: TStringField
      FieldName = 'State'
      Origin = 'CompanyInfo.State'
    end
    object tblDataZip: TStringField
      FieldName = 'Zip'
      Origin = 'CompanyInfo.Zip'
      Size = 10
    end
    object tblDataCountry: TStringField
      FieldName = 'Country'
      Origin = 'CompanyInfo.Country'
      Size = 60
    end
    object tblDataPhone: TStringField
      FieldName = 'Phone'
      Origin = 'CompanyInfo.Phone'
    end
    object tblDataFax: TStringField
      FieldName = 'Fax'
      Origin = 'CompanyInfo.Fax'
    end
    object tblDataEmail: TStringField
      FieldName = 'Email'
      Origin = 'CompanyInfo.Email'
      Size = 80
    end
    object tblDataEID: TStringField
      FieldName = 'EID'
      Origin = 'CompanyInfo.EID'
    end
  end
end
