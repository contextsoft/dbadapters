object frmMacorsDemo: TfrmMacorsDemo
  Left = 279
  Top = 218
  Width = 571
  Height = 374
  Caption = 'SQL Macros Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSQL: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 45
    Align = alTop
    BevelOuter = bvLowered
    Color = clInfoBk
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 6
      Width = 424
      Height = 34
      AutoSize = False
      Caption = 'SQL:'#13#10'select * from customers order by <%orderby%>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 77
    Width = 563
    Height = 263
    Align = alClient
    DataSource = DataSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'CustomerID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        Width = 72
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstName'
        Width = 77
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'City'
        Width = 76
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StreetNo'
        Width = 49
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Street'
        Width = 72
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'State'
        Width = 38
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Zip'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'HomePhone'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'WorkPhone'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'MobilePhone'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Fax'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TaxType'
        Visible = True
      end>
  end
  object Panel: TPanel
    Left = 0
    Top = 45
    Width = 563
    Height = 32
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 2
    object Label2: TLabel
      Left = 13
      Top = 10
      Width = 43
      Height = 13
      Caption = 'Order by:'
    end
    object cbxOrderBy: TComboBox
      Left = 75
      Top = 6
      Width = 159
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'CustomerID'
        'LastName'
        'FirstName'
        'LastName,FirstName'
        'City,LastName,FirstName'
        'Street,StreetNo'
        'State,City,CustomerID'
        'Zip,CustomerID')
    end
    object btnExecute: TButton
      Left = 246
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 1
      OnClick = btnExecuteClick
    end
  end
  object Query: TnxQueryExt
    Database = dbMain
    RequestLive = True
    SQL.Strings = (
      'select * from customers order by <%orderby%>')
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Macros = <
      item
        Name = 'orderby'
      end>
    MacroBegin = '<%'
    MacroEnd = '%>'
    AllowMacros = True
    Left = 147
    Top = 137
    object QueryCustomerID: TIntegerField
      FieldName = 'CustomerID'
    end
    object QueryReferredBy: TIntegerField
      FieldName = 'ReferredBy'
    end
    object QueryCompanyName: TStringField
      FieldName = 'CompanyName'
      Size = 40
    end
    object QueryLastName: TStringField
      FieldName = 'LastName'
      Size = 40
    end
    object QueryFirstName: TStringField
      FieldName = 'FirstName'
      Size = 40
    end
    object QueryInitial: TStringField
      FieldName = 'Initial'
      Size = 5
    end
    object QueryCareOf: TStringField
      FieldName = 'CareOf'
    end
    object QueryStreetNo: TStringField
      FieldName = 'StreetNo'
    end
    object QueryStreet: TStringField
      FieldName = 'Street'
      Size = 60
    end
    object QueryCity: TStringField
      FieldName = 'City'
      Size = 80
    end
    object QueryState: TStringField
      FieldName = 'State'
      Size = 15
    end
    object QueryZip: TStringField
      FieldName = 'Zip'
    end
    object QueryHomePhone: TStringField
      FieldName = 'HomePhone'
    end
    object QueryWorkPhone: TStringField
      FieldName = 'WorkPhone'
    end
    object QueryMobilePhone: TStringField
      FieldName = 'MobilePhone'
    end
    object QueryFax: TStringField
      FieldName = 'Fax'
    end
    object QueryTaxType: TStringField
      FieldName = 'TaxType'
    end
  end
  object DataSource: TDataSource
    Left = 191
    Top = 140
  end
  object dbMain: TnxDatabaseExt
    Session = nxSession
    SystemTableName = 'System'
    ObjectsTableName = 'Objects'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers]
    MaxCachedTables = 10
    DatabaseName = 'dbMain'
    Left = 116
    Top = 108
  end
  object nxSession: TnxSession
    ServerEngine = nxServerEngine
    Left = 156
    Top = 96
  end
  object nxServerEngine: TnxServerEngine
    SqlEngine = nxSqlEngine
    Options = []
    Left = 196
    Top = 80
  end
  object nxSqlEngine: TnxSqlEngine
    Left = 232
    Top = 64
  end
end
