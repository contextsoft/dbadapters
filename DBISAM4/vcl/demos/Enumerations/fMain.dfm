object frmEnumDemo: TfrmEnumDemo
  Left = 274
  Top = 164
  Width = 522
  Height = 375
  Caption = 'Enumeration Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid2: TDBGrid
    Left = 0
    Top = 135
    Width = 514
    Height = 206
    Align = alClient
    DataSource = dsOrders
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object pnlHint: TPanel
    Left = 0
    Top = 0
    Width = 514
    Height = 67
    Align = alTop
    BevelOuter = bvLowered
    Color = clInfoBk
    TabOrder = 1
    object Label3: TLabel
      Left = 10
      Top = 8
      Width = 520
      Height = 51
      AutoSize = False
      Caption = 
        'This program demonstrates use of the enumerations declared'#13#10'with' +
        'in database schema. Each enumeration could be exposed'#13#10'as a data' +
        'set using TDBSchemEnum or TDBEnumeration components'#13#10'and then us' +
        'ed for lookup fields as shown for OrderStatus.'
    end
  end
  object pnlEditing: TPanel
    Left = 0
    Top = 67
    Width = 514
    Height = 68
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 2
    object Label1: TLabel
      Left = 9
      Top = 42
      Width = 39
      Height = 13
      Caption = 'Order #:'
    end
    object Label2: TLabel
      Left = 318
      Top = 42
      Width = 33
      Height = 13
      Caption = 'Status:'
    end
    object Label4: TLabel
      Left = 129
      Top = 43
      Width = 47
      Height = 13
      Caption = 'Customer:'
    end
    object DBLookupComboBox1: TDBLookupComboBox
      Left = 362
      Top = 38
      Width = 145
      Height = 21
      DataField = 'OrderStatusDisplay'
      DataSource = dsOrders
      TabOrder = 3
    end
    object DBEdit1: TDBEdit
      Left = 58
      Top = 39
      Width = 62
      Height = 21
      DataField = 'OrderID'
      DataSource = dsOrders
      TabOrder = 1
    end
    object DBEdit2: TDBEdit
      Left = 182
      Top = 39
      Width = 121
      Height = 21
      DataField = 'Customer'
      DataSource = dsOrders
      TabOrder = 2
    end
    object DBNavigator1: TDBNavigator
      Left = 10
      Top = 6
      Width = 240
      Height = 25
      DataSource = dsOrders
      TabOrder = 0
    end
  end
  object tblOrders: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'Data'
    EngineVersion = '3.24'
    FieldDefs = <
      item
        Name = 'OrderID'
        DataType = ftString
        Size = 8
      end
      item
        Name = 'Customer'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'OrderStatus'
        Attributes = [faRequired]
        DataType = ftSmallint
      end>
    TableName = 'Orders'
    StoreDefs = True
    OptimizedLookups = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers]
    Left = 88
    Top = 177
    object tblOrdersOrderID: TStringField
      DisplayLabel = 'Order #'
      FieldName = 'OrderID'
      Origin = 'Orders.OrderID'
      Size = 8
    end
    object tblOrdersCustomer: TStringField
      FieldName = 'Customer'
      Origin = 'Orders.Customer'
    end
    object tblOrdersOrderStatus: TSmallintField
      DefaultExpression = '0'
      DisplayLabel = 'Order Status'
      FieldName = 'OrderStatus'
      Origin = 'Orders.OrderStatus'
      Required = True
      Visible = False
    end
    object tblOrdersOrderStatusDisplay: TStringField
      DisplayLabel = 'Order Status'
      FieldKind = fkLookup
      FieldName = 'OrderStatusDisplay'
      LookupDataSet = enOrderStatuses
      LookupKeyFields = 'Key'
      LookupResultField = 'Value'
      KeyFields = 'OrderStatus'
      Size = 30
      Lookup = True
    end
    object tblOrdersOrderStatusDescr: TStringField
      FieldKind = fkLookup
      FieldName = 'OrderStatusDescr'
      LookupDataSet = enOrderStatuses
      LookupKeyFields = 'Key'
      LookupResultField = 'Description'
      KeyFields = 'OrderStatus'
      Size = 40
      Lookup = True
    end
    object tblOrdersOrderStatusShortDescr: TStringField
      FieldKind = fkLookup
      FieldName = 'OrderStatusShortDescr'
      LookupDataSet = enOrderStatuses
      LookupKeyFields = 'Key'
      LookupResultField = 'ShortDescription'
      KeyFields = 'OrderStatus'
      Size = 15
      Lookup = True
    end
  end
  object dsOrders: TDataSource
    DataSet = tblOrders
    Left = 132
    Top = 186
  end
  object enOrderStatuses: TDBSchemaEnum
    Active = True
    Enumeration = 'OrderStatuses'
    Schema = DatabaseSchema
    Left = 276
    Top = 153
  end
  object DatabaseSchema: TDatabaseSchema
    TargetDB = 'DBISAM4'
    TableDefs = <
      item
        Name = 'Orders'
        Description = 'Orders'
        FieldDefs = <
          item
            Name = 'OrderID'
            Description = 'Order #'
            DataType = ftString
            Size = 8
            ItemID = 1
          end
          item
            Name = 'Customer'
            Description = 'Customer'
            DataType = ftString
            Size = 20
            ItemID = 2
          end
          item
            Name = 'OrderStatus'
            Description = 'Status'
            Required = True
            Attributes = [faRequired]
            DataType = ftSmallint
            Enumeration = 'OrderStatuses'
            DefaultExpression = '0'
            ItemID = 3
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
            ItemID = 4
          end>
        TypePrefix = 'T'
        ItemID = 5
      end>
    Enumerations = <
      item
        Name = 'OrderStatuses'
        Description = 'OrderStatuses'
        Items.Strings = (
          '0=Open'
          '1=Processing'
          '2=Complete')
        Descriptions.Strings = (
          '0=Open Order'
          '1=Order in Processing'
          '2=Order Complete')
        ShortDescriptions.Strings = (
          '0=Open'
          '1=Proc.'
          '2=Compl.')
        DisplayLabel = 'OrderStatuses'
        TypePrefix = 'en'
        ItemID = 6
      end>
    SchemaName = 'Database'
    Left = 223
    Top = 209
    SchemaGUID = '{9F1E5663-BB3E-41F7-AEB1-60F5031687A5}'
  end
end
