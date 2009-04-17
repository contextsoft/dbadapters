object Form1: TForm1
  Left = 228
  Top = 191
  Width = 630
  Height = 374
  Caption = 'Auto Field Properties Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    622
    340)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 16
    Top = 8
    Width = 456
    Height = 61
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clInfoBk
    Shape = stRoundRect
  end
  object Label1: TLabel
    Left = 24
    Top = 12
    Width = 444
    Height = 53
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'This demo illustrates the usage of AutoFieldsProperties property' +
      ' of TDBISAMQuery/Table components. If this property is set to Tr' +
      'ue and the database is connected to schema, field'#39's properties (' +
      'like DisplayLabel, DisplayWidth, EditMask, etc.) will be automat' +
      'ically set from schema definitions.'
    Transparent = True
    WordWrap = True
  end
  object DBGrid1: TDBGrid
    Left = 16
    Top = 76
    Width = 591
    Height = 251
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object btnExecute: TButton
    Left = 482
    Top = 24
    Width = 122
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Execute Query'
    TabOrder = 1
    OnClick = btnExecuteClick
  end
  object DatabaseSchema1: TDatabaseSchema
    TargetDB = 'DBISAM4'
    TableDefs = <
      item
        Name = 'Orders'
        FieldDefs = <
          item
            Name = 'OrderID'
            Description = 'Order #'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            DisplayWidth = 5
            ItemID = 44
          end
          item
            Name = 'DateCreated'
            Description = 'Created'
            Required = True
            Attributes = [faRequired]
            DataType = ftDate
            DisplayWidth = 10
            ItemID = 45
          end
          item
            Name = 'CustomerID'
            Description = 'Customer #'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            DisplayWidth = 5
            ItemID = 46
          end
          item
            Name = 'Description'
            DataType = ftString
            Size = 80
            DisplayWidth = 15
            ItemID = 47
          end
          item
            Name = 'Status'
            Required = True
            Attributes = [faRequired]
            DataType = ftSmallint
            DisplayWidth = 5
            ItemID = 48
          end
          item
            Name = 'Notes'
            DataType = ftMemo
            DisplayWidth = 10
            ItemID = 49
          end
          item
            Name = 'PayOffDate'
            DataType = ftDate
            DisplayWidth = 10
            ItemID = 50
          end
          item
            Name = 'TaxType'
            Required = True
            Attributes = [faRequired]
            DataType = ftString
            Size = 10
            DisplayWidth = 10
            ItemID = 51
          end
          item
            Name = 'SalesRep'
            DataType = ftString
            Size = 20
            DisplayWidth = 5
            ItemID = 52
          end
          item
            Name = 'Tax'
            DataType = ftFloat
            DisplayWidth = 5
            ItemID = 53
          end
          item
            Name = 'TotalTax'
            Required = True
            Attributes = [faRequired]
            DataType = ftCurrency
            DisplayWidth = 5
            ItemID = 54
          end
          item
            Name = 'TotalSales'
            Required = True
            Attributes = [faRequired]
            DataType = ftCurrency
            DisplayWidth = 5
            ItemID = 55
          end
          item
            Name = 'TotalCharges'
            Required = True
            Attributes = [faRequired]
            DataType = ftCurrency
            DisplayWidth = 5
            ItemID = 56
          end>
        IndexDefs = <
          item
            IndexFields = <
              item
                Name = 'OrderID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 57
          end
          item
            Name = 'ByCustomer'
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 58
          end
          item
            Name = 'ByTaxType'
            IndexFields = <
              item
                Name = 'TaxType'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 59
          end>
        ItemID = 60
      end
      item
        Name = 'Customers'
        FieldDefs = <
          item
            Name = 'CustomerID'
            Description = 'Customer #'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            DisplayWidth = 50
            ItemID = 91
          end
          item
            Name = 'CompanyName'
            Description = 'Company'
            Required = True
            Attributes = [faRequired]
            DataType = ftString
            Size = 40
            DisplayWidth = 50
            ItemID = 92
          end
          item
            Name = 'LastName'
            Description = 'Last Name'
            Required = True
            Attributes = [faRequired]
            DataType = ftString
            Size = 40
            DisplayWidth = 30
            ItemID = 93
          end
          item
            Name = 'FirstName'
            Description = 'First Name'
            DataType = ftString
            Size = 40
            DisplayWidth = 30
            ItemID = 94
          end
          item
            Name = 'Initial'
            DataType = ftString
            Size = 5
            DisplayWidth = 5
            ItemID = 95
          end
          item
            Name = 'CareOf'
            Description = 'C\O'
            DataType = ftString
            Size = 40
            DisplayWidth = 10
            ItemID = 96
          end
          item
            Name = 'StreetNo'
            DataType = ftString
            Size = 20
            DisplayWidth = 5
            ItemID = 97
          end
          item
            Name = 'Street'
            DataType = ftString
            Size = 60
            DisplayWidth = 10
            ItemID = 98
          end
          item
            Name = 'City'
            DataType = ftString
            Size = 80
            DisplayWidth = 10
            ItemID = 99
          end
          item
            Name = 'State'
            DataType = ftString
            Size = 15
            DisplayWidth = 5
            ItemID = 100
          end
          item
            Name = 'Zip'
            DataType = ftString
            Size = 12
            DisplayWidth = 5
            ItemID = 101
          end
          item
            Name = 'HomePhone'
            Description = 'Home Phone'
            DataType = ftString
            Size = 20
            DisplayWidth = 10
            EditMask = '(000) 000-000'
            ItemID = 102
          end
          item
            Name = 'WorkPhone'
            Description = 'Work Phone'
            DataType = ftString
            Size = 20
            DisplayWidth = 10
            EditMask = '(000) 000-000'
            ItemID = 103
          end
          item
            Name = 'MobilePhone'
            Description = 'Mobile Phone'
            DataType = ftString
            Size = 20
            DisplayWidth = 10
            EditMask = '(000) 000-000'
            ItemID = 104
          end
          item
            Name = 'Fax'
            DataType = ftString
            Size = 20
            DisplayWidth = 10
            EditMask = '(000) 000-000'
            ItemID = 105
          end
          item
            Name = 'TaxType'
            Required = True
            Attributes = [faRequired]
            DataType = ftString
            Size = 10
            DisplayWidth = 5
            ItemID = 106
          end
          item
            Name = 'ReferredBy'
            DataType = ftInteger
            DisplayWidth = 5
            ItemID = 107
          end>
        IndexDefs = <
          item
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 108
          end
          item
            Name = 'ByName'
            IndexFields = <
              item
                Name = 'LastName'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end
              item
                Name = 'FirstName'
                Descending = False
                CaseInsensitive = False
                ItemID = 2
              end>
            Options = [ixCaseInsensitive]
            ItemID = 109
          end
          item
            Name = 'ByTaxType'
            IndexFields = <
              item
                Name = 'TaxType'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 110
          end>
        ItemID = 111
      end>
    SchemaName = 'Database'
    Left = 260
    Top = 160
    SchemaGUID = '{2E4462F9-77E8-4BFA-A4C5-56B282D490E4}'
  end
  object DBISAMDatabaseExt1: TDBISAMDatabaseExt
    EngineVersion = '3.24'
    DatabaseName = 'MainDB'
    Directory = 'E:\projects\sdk\Context\DBExt\demos\DBISAM\EZBooks\Data'
    SessionName = 'Default'
    Schema = DatabaseSchema1
    SystemTableName = 'System'
    ObjectsTableName = 'Objects'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers]
    MaxCachedTables = 10
    Left = 260
    Top = 212
  end
  object DBISAMQueryExt1: TDBISAMQueryExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'MainDB'
    EngineVersion = '3.24'
    MaxRowCount = -1
    SQL.Strings = (
      'select * from '
      
        '  orders join customers on orders.CustomerID = customers.Custome' +
        'rID')
    Params = <>
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Macros = <>
    MacroBegin = '<%'
    MacroEnd = '%>'
    Left = 368
    Top = 212
  end
  object DataSource1: TDataSource
    DataSet = DBISAMQueryExt1
    Left = 456
    Top = 212
  end
end
