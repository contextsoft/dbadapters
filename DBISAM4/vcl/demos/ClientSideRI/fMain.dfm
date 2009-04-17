object frmMainForm: TfrmMainForm
  Left = 284
  Top = 224
  Width = 571
  Height = 376
  Caption = 'DBISAM Client-Side RI Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 176
    Width = 563
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 33
    Width = 563
    Height = 143
    Align = alTop
    DataSource = dsCustomers
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'CustomerID'
        Width = 58
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        Width = 88
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstName'
        Width = 76
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Street'
        Width = 92
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'City'
        Width = 75
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'State'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Zip'
        Visible = True
      end>
  end
  object DBGrid2: TDBGrid
    Left = 0
    Top = 207
    Width = 563
    Height = 135
    Align = alClient
    DataSource = dsOrders
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'OrderID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CustomerID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Width = 251
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TotalCharges'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TotalPayments'
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 84
      Height = 19
      Caption = 'Customers'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -16
      Font.Name = 'Tacoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DBNavigator1: TDBNavigator
      Left = 336
      Top = 8
      Width = 200
      Height = 18
      DataSource = dsCustomers
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 179
    Width = 563
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label2: TLabel
      Left = 8
      Top = 5
      Width = 52
      Height = 19
      Caption = 'Orders'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -16
      Font.Name = 'Tacoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DBNavigator2: TDBNavigator
      Left = 343
      Top = 5
      Width = 200
      Height = 18
      DataSource = dsOrders
      TabOrder = 0
    end
  end
  object DatabaseExt: TDBISAMDatabaseExt
    EngineVersion = '3.24'
    DatabaseName = 'MainDB'
    Directory = 'E:\projects\sdk\Context\DBExt\demos\DBISAM\ClientSideRI\Data\'
    SessionName = 'Default'
    Schema = DatabaseSchema
    SystemTableName = 'System'
    ObjectsTableName = 'Objects'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers]
    MaxCachedTables = 10
    Left = 96
    Top = 64
  end
  object DatabaseSchema: TDatabaseSchema
    TargetDB = 'DBISAM4'
    Updates = <
      item
        Description = 'Created: Customers, Orders; '
        SQLScript = 
          #13#10'CREATE TABLE "Customers" ('#13#10'  "CustomerID" AUTOINC NOT NULL,'#13#10 +
          '  "LastName" VARCHAR(50),'#13#10'  "FirstName" VARCHAR(50),'#13#10'  "Street' +
          '" VARCHAR(80),'#13#10'  "City" VARCHAR(80),'#13#10'  "State" VARCHAR(8),'#13#10'  ' +
          '"Zip" VARCHAR(12),'#13#10'  CONSTRAINT "PK_Customers" PRIMARY KEY ("Cu' +
          'stomerID")'#13#10#13#10'  DESCRIPTION '#39'Customers'#39#13#10')'#13#10'-- GO --'#13#10#13#10'CREATE I' +
          'NDEX "ByLastNameFirstName" ON "Customers"("LastName", "FirstName' +
          '")'#13#10'-- GO --'#13#10#13#10'CREATE TABLE "Orders" ('#13#10'  "OrderID" AUTOINC NOT' +
          ' NULL,'#13#10'  "CustomerID" INT NOT NULL,'#13#10'  "Description" VARCHAR(12' +
          '0),'#13#10'  "TotalCharges" MONEY NOT NULL,'#13#10'  "TotalPayments" MONEY N' +
          'OT NULL,'#13#10'  CONSTRAINT "PK_Orders" PRIMARY KEY ("OrderID")'#13#10#13#10')'#13 +
          #10'-- GO --'#13#10#13#10'CREATE INDEX "ByCustomerID" ON "Orders"("CustomerID' +
          '")'#13#10'-- GO --'#13#10
        Iterate = False
        VersionLabel = '1.1'
        ItemID = 27
      end
      item
        Description = 'Altered: Customers, Orders, Orders_Customers; '
        Iterate = False
        VersionLabel = '1.2'
        ItemID = 28
      end
      item
        Iterate = False
        VersionLabel = '1.3'
        ItemID = 29
      end
      item
        Description = 'Altered: Customers, Orders, Orders_Customers; '
        Iterate = False
        VersionLabel = '1.4'
        ItemID = 30
      end
      item
        Description = 'Altered: Orders; '
        SQLScript = 
          '-- ## TargetDB: DBISAM4;'#13#10#13#10'ALTER TABLE "Orders" '#13#10'  REDEFINE "D' +
          'escription" VARCHAR(200) DESCRIPTION '#39'Comments'#39';'#13#10
        Iterate = False
        VersionLabel = '1.5'
        ItemID = 31
      end>
    Relationships = <
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
        ItemID = 13
      end>
    TableDefs = <
      item
        Name = 'Customers'
        Description = 'Customers'
        FieldDefs = <
          item
            Name = 'CustomerID'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            ItemID = 1
          end
          item
            Name = 'LastName'
            DataType = ftString
            Size = 50
            ItemID = 2
          end
          item
            Name = 'FirstName'
            DataType = ftString
            Size = 50
            ItemID = 3
          end
          item
            Name = 'Street'
            DataType = ftString
            Size = 80
            ItemID = 4
          end
          item
            Name = 'City'
            DataType = ftString
            Size = 80
            ItemID = 5
          end
          item
            Name = 'State'
            DataType = ftString
            Size = 8
            ItemID = 6
          end
          item
            Name = 'Zip'
            DataType = ftString
            Size = 12
            ItemID = 7
          end>
        IndexDefs = <
          item
            Name = 'PK_Customers'
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 9
          end
          item
            Name = 'ByLastNameFirstName'
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
            ItemID = 12
          end>
        ItemID = 14
      end
      item
        Name = 'Orders'
        FieldDefs = <
          item
            Name = 'OrderID'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            ItemID = 15
          end
          item
            Name = 'CustomerID'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 16
          end
          item
            Name = 'Description'
            Description = 'Comments'
            DataType = ftString
            Size = 200
            ItemID = 17
          end
          item
            Name = 'TotalCharges'
            Required = True
            Attributes = [faRequired]
            DataType = ftCurrency
            ItemID = 18
          end
          item
            Name = 'TotalPayments'
            Required = True
            Attributes = [faRequired]
            DataType = ftCurrency
            ItemID = 19
          end>
        IndexDefs = <
          item
            Name = 'PK_Orders'
            IndexFields = <
              item
                Name = 'OrderID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 21
          end
          item
            Name = 'ByCustomerID'
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 23
          end>
        ItemID = 25
      end>
    SchemaName = 'Database'
    Left = 248
    Top = 88
    SchemaGUID = '{BBE87048-9A9C-460D-869D-CFEABA5BC357}'
  end
  object tblCustomers: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'MainDB'
    EngineVersion = '3.24'
    TableName = 'Customers'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 136
    Top = 40
    object tblCustomersCustomerID: TAutoIncField
      FieldName = 'CustomerID'
    end
    object tblCustomersLastName: TStringField
      FieldName = 'LastName'
      Size = 50
    end
    object tblCustomersFirstName: TStringField
      FieldName = 'FirstName'
      Size = 50
    end
    object tblCustomersStreet: TStringField
      FieldName = 'Street'
      Size = 80
    end
    object tblCustomersCity: TStringField
      FieldName = 'City'
      Size = 80
    end
    object tblCustomersState: TStringField
      FieldName = 'State'
      Size = 8
    end
    object tblCustomersZip: TStringField
      FieldName = 'Zip'
      Size = 12
    end
  end
  object tblOrders: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'MainDB'
    EngineVersion = '3.24'
    IndexName = 'ByCustomerID'
    MasterFields = 'CustomerID'
    MasterSource = dsCustomers
    TableName = 'Orders'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 184
    Top = 56
    object tblOrdersOrderID: TAutoIncField
      FieldName = 'OrderID'
      Origin = 'Orders.OrderID'
    end
    object tblOrdersCustomerID: TIntegerField
      FieldName = 'CustomerID'
      Origin = 'Orders.CustomerID'
      Required = True
    end
    object tblOrdersDescription: TStringField
      DisplayLabel = 'Comments'
      FieldName = 'Description'
      Origin = 'Orders.Description'
      Size = 200
    end
    object tblOrdersTotalCharges: TCurrencyField
      FieldName = 'TotalCharges'
      Origin = 'Orders.TotalCharges'
      Required = True
    end
    object tblOrdersTotalPayments: TCurrencyField
      FieldName = 'TotalPayments'
      Origin = 'Orders.TotalPayments'
      Required = True
    end
  end
  object dsCustomers: TDataSource
    DataSet = tblCustomers
    Left = 144
    Top = 48
  end
  object dsOrders: TDataSource
    DataSet = tblOrders
    Left = 200
    Top = 64
  end
end
