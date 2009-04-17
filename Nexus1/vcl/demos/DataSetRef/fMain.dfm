object frmDataSetRef: TfrmDataSetRef
  Left = 279
  Top = 140
  Width = 483
  Height = 407
  Caption = 'DataSet Reference Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 475
    Height = 49
    Align = alTop
    BevelOuter = bvLowered
    Color = clInfoBk
    TabOrder = 0
    object Label1: TLabel
      Left = 14
      Top = 5
      Width = 379
      Height = 41
      AutoSize = False
      Caption = 
        'The same routine is used to display address both for Query and T' +
        'able.'#13#10'DataSet References are declared automatically, using comp' +
        'onent menu'#13#10'of the DatabaseSchema component (Update Schema Decla' +
        'rations...).'
      WordWrap = True
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 81
    Width = 475
    Height = 292
    ActivePage = tsTable
    Align = alClient
    TabOrder = 1
    object tsTable: TTabSheet
      Caption = 'Customers Table'
      object DBGrid2: TDBGrid
        Left = 0
        Top = 0
        Width = 467
        Height = 264
        Align = alClient
        DataSource = dsTable
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object tsQuery: TTabSheet
      Caption = 'Customers Query'
      ImageIndex = 1
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 467
        Height = 268
        Align = alClient
        DataSource = dsQuery
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 49
    Width = 475
    Height = 32
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object btnAddress: TButton
      Left = 12
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Address...'
      TabOrder = 0
      OnClick = btnAddressClick
    end
  end
  object dbMain: TnxDatabaseExt
    Session = nxSession
    AliasPath = 
      'D:\Program Files\Borland\Delphi6\Projects\dbisamext\nxdemos\Data' +
      'SetRef\Data'
    Schema = DatabaseSchema
    SystemTableName = 'System'
    ObjectsTableName = 'Objects'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers]
    MaxCachedTables = 10
    DatabaseName = 'dbMain'
    Left = 92
    Top = 129
  end
  object DatabaseSchema: TDatabaseSchema
    TargetDB = 'Nexus1'
    Relationships = <
      item
        Name = 'Orders1_Customer'
        DetailTableName = 'Orders'
        DetailKeyFields = 'CustomerID'
        DetailRelationName = 'Customer'
        MasterTableName = 'Customers'
        MasterKeyFields = 'CustomerID'
        MasterRelationName = 'Orders1'
        MasterRecordOptional = False
        RequireRecordErrorMessage = 
          'Field CustomerID in the Orders table does not contain a valid or' +
          ' null reference to the Customers table.'
        ItemID = 1
      end
      item
        Name = 'Orders_Customers'
        DetailTableName = 'Orders'
        DetailKeyFields = 'CustomerID'
        DetailRelationName = 'Customers'
        MasterTableName = 'Customers'
        MasterKeyFields = 'CustomerID'
        MasterRelationName = 'Orders'
        MasterRecordOptional = False
        DeleteAction = raError
        UpdateAction = raCascade
        ItemID = 2
      end>
    TableDefs = <
      item
        Name = 'Orders'
        Description = 'Orders'
        FieldDefs = <
          item
            Name = 'OrderID'
            Description = 'Order #'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            ItemID = 3
          end
          item
            Name = 'CustomerID'
            Description = 'Customer #'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 4
          end
          item
            Name = 'Description'
            Description = 'Order Descr.'
            DataType = ftMemo
            ItemID = 5
          end>
        IndexDefs = <
          item
            Name = 'ByCustomerID'
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 6
          end
          item
            IndexFields = <
              item
                Name = 'OrderID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 7
          end>
        TypePrefix = 'T'
        Category = 'Documents'
        ItemID = 8
      end
      item
        Name = 'Customers'
        Description = 'Customers'
        FieldDefs = <
          item
            Name = 'CustomerID'
            Description = 'CustomerID'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            ItemID = 9
          end
          item
            Name = 'ReferredBy'
            Description = 'ReferredBy'
            DataType = ftInteger
            ItemID = 10
          end
          item
            Name = 'CompanyName'
            Description = 'CompanyName'
            DataType = ftString
            Size = 40
            ItemID = 11
          end
          item
            Name = 'LastName'
            Description = 'LastName'
            DataType = ftString
            Size = 40
            ItemID = 12
          end
          item
            Name = 'FirstName'
            Description = 'FirstName'
            DataType = ftString
            Size = 40
            ItemID = 13
          end
          item
            Name = 'Initial'
            Description = 'Initial'
            DataType = ftString
            Size = 5
            ItemID = 14
          end
          item
            Name = 'CareOf'
            Description = 'CareOf'
            DataType = ftString
            Size = 20
            ItemID = 15
          end
          item
            Name = 'StreetNo'
            Description = 'StreetNo'
            DataType = ftString
            Size = 20
            ItemID = 16
          end
          item
            Name = 'Street'
            Description = 'Street'
            DataType = ftString
            Size = 60
            ItemID = 17
          end
          item
            Name = 'City'
            Description = 'City'
            DataType = ftString
            Size = 80
            ItemID = 18
          end
          item
            Name = 'State'
            Description = 'State'
            DataType = ftString
            Size = 15
            ItemID = 19
          end
          item
            Name = 'Zip'
            Description = 'Zip'
            DataType = ftString
            Size = 20
            ItemID = 20
          end
          item
            Name = 'HomePhone'
            Description = 'HomePhone'
            DataType = ftString
            Size = 20
            ItemID = 21
          end
          item
            Name = 'WorkPhone'
            Description = 'WorkPhone'
            DataType = ftString
            Size = 20
            ItemID = 22
          end
          item
            Name = 'MobilePhone'
            Description = 'MobilePhone'
            DataType = ftString
            Size = 20
            ItemID = 23
          end
          item
            Name = 'Fax'
            Description = 'Fax'
            DataType = ftString
            Size = 20
            ItemID = 24
          end
          item
            Name = 'TaxType'
            Description = 'TaxType'
            DataType = ftString
            Size = 20
            ItemID = 25
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
            ItemID = 26
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
            ItemID = 27
          end>
        TypePrefix = 'T'
        Category = 'References'
        ItemID = 28
      end>
    Enumerations = <
      item
        Name = 'LocationTypes'
        Description = 'Location Types'
        Items.Strings = (
          '1=Store/Office'
          '2=Vault'
          '3=Warehouse'
          '4=Contractor'
          '5=Partner'
          '6=Me')
        DisplayLabel = 'Location Types'
        TypePrefix = 'en'
        ItemID = 29
      end
      item
        Name = 'ItemTypes'
        Description = 'Item Types'
        Items.Strings = (
          '1=Garment'
          '2=Accessory'
          '3=Skins')
        DisplayLabel = 'Item Types'
        TypePrefix = 'en'
        IntConsts = True
        ItemID = 30
      end
      item
        Name = 'UserTypes'
        Description = 'User Types'
        DisplayLabel = 'User Types'
        TypePrefix = 'en'
        ItemID = 31
      end
      item
        Name = 'AccountTypes'
        Description = 'Account Types'
        Items.Strings = (
          'Asset'
          'Equity'
          'Liability'
          'Income'
          'Expense')
        DisplayLabel = 'Account Types'
        TypePrefix = 'en'
        ItemID = 32
      end
      item
        Name = 'LineGroups'
        Description = 'Line Groups'
        Items.Strings = (
          'Sale'
          'Consignment'
          'Lay-a-way'
          'Storage'
          'Cleaning'
          'Labor'
          'Other Charge'
          'Deposit'
          'Payment')
        DisplayLabel = 'Line Groups'
        TypePrefix = 'en'
        ItemID = 33
      end
      item
        Name = 'DocTypes'
        Description = 'Document Types'
        Items.Strings = (
          '1=Invoice'
          '2=Sale Receipt'
          '3=Service Receipt'
          '4=Memo'
          '5=Credit Memo'
          '6=Payment'
          '7=Quote'
          '8=Adjustment')
        Descriptions.Strings = (
          '1=Simple Invoice')
        DisplayLabel = 'Document Types'
        TypePrefix = 'dt'
        IntConsts = True
        ItemID = 34
      end
      item
        Name = 'DocStatuses'
        Description = 'Document Statuses'
        DisplayLabel = 'Document Statuses'
        TypePrefix = 'ds'
        ItemID = 35
      end
      item
        Name = 'DocEntryTypes'
        Description = 'Document Entry Types'
        Items.Strings = (
          'Invoice'
          'Sale Receipt'
          'Service Receipt'
          'Refund/Exchange'
          'Adjustment'
          'Payment')
        DisplayLabel = 'Document Entry Types'
        TypePrefix = 'det'
        ItemID = 36
      end>
    SchemaName = 'DataSetRefSchema'
    Left = 155
    Top = 154
    SchemaGUID = '{41F2252D-0C17-43CB-AD8B-ABDBBBEE8746}'
  end
  object dsTable: TDataSource
    DataSet = tblCustomers
    Left = 101
    Top = 218
  end
  object tblCustomers: TnxTableExt
    Database = dbMain
    TableName = 'Customers'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 85
    Top = 201
  end
  object qryCustomers: TnxQueryExt
    Database = dbMain
    SQL.Strings = (
      'select * from customers')
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Macros = <>
    MacroBegin = '<%'
    MacroEnd = '%>'
    Left = 245
    Top = 188
  end
  object dsQuery: TDataSource
    DataSet = qryCustomers
    Left = 261
    Top = 204
  end
  object nxSession: TnxSession
    ServerEngine = nxServerEngine
    Left = 136
    Top = 108
  end
  object nxServerEngine: TnxServerEngine
    SqlEngine = nxSqlEngine1
    Options = []
    Left = 172
    Top = 108
  end
  object nxSqlEngine1: TnxSqlEngine
    Left = 208
    Top = 108
  end
end
