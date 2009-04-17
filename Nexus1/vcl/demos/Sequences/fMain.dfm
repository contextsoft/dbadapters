object frmSequencesDemo: TfrmSequencesDemo
  Left = 276
  Top = 223
  Width = 606
  Height = 375
  Caption = 'Sequences Demo'
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
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 581
    Height = 332
    ActivePage = tsCustomers
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsCustomers: TTabSheet
      Caption = 'Customers'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 33
        Width = 573
        Height = 271
        Align = alClient
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
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'LastName'
            Width = 84
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'FirstName'
            Width = 87
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'StreetNo'
            Width = 77
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Street'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'City'
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
          end>
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 573
        Height = 33
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 1
        object DBNavigator1: TDBNavigator
          Left = 8
          Top = 4
          Width = 240
          Height = 25
          DataSource = dsCustomers
          Flat = True
          TabOrder = 0
        end
        object btnNextCustomerID: TButton
          Left = 272
          Top = 4
          Width = 117
          Height = 25
          Caption = 'Set Next CustomerID'
          TabOrder = 1
          OnClick = btnNextCustomerIDClick
        end
      end
    end
    object tsItems: TTabSheet
      Caption = 'Items'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 573
        Height = 33
        Align = alTop
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object DBNavigator2: TDBNavigator
          Left = 8
          Top = 4
          Width = 240
          Height = 25
          DataSource = dsItems
          Flat = True
          TabOrder = 0
        end
        object btnNextItemID: TButton
          Left = 272
          Top = 4
          Width = 89
          Height = 25
          Caption = 'Set Next ItemID'
          TabOrder = 1
          OnClick = btnNextItemIDClick
        end
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 33
        Width = 573
        Height = 271
        Align = alClient
        DataSource = dsItems
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'ItemID'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Description'
            Width = 255
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'SalePrice'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Taxable'
            Visible = True
          end>
      end
    end
  end
  object DBSequences: TDBSequences
    SequenceTable = tblCounters
    Left = 296
    Top = 152
  end
  object DatabaseSchema: TDatabaseSchema
    TargetDB = 'Nexus1'
    Updates = <
      item
        Description = 'Version 1.1'
        Iterate = False
        VersionLabel = '1.1'
        ItemID = 1
      end>
    TableDefs = <
      item
        Name = 'Items'
        Description = 'Items'
        FieldDefs = <
          item
            Name = 'ItemID'
            Description = 'ItemID'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            DefaultExpression = '0'
            ItemID = 2
          end
          item
            Name = 'Description'
            Description = 'Description'
            DataType = ftString
            Size = 80
            ItemID = 3
          end
          item
            Name = 'SalePrice'
            Description = 'SalePrice'
            Required = True
            Attributes = [faRequired]
            DataType = ftCurrency
            ItemID = 4
          end
          item
            Name = 'Taxable'
            Description = 'Taxable'
            Required = True
            Attributes = [faRequired]
            DataType = ftBoolean
            DefaultExpression = 'True'
            ItemID = 5
          end>
        IndexDefs = <
          item
            Name = '<Primary>'
            IndexFields = <
              item
                Name = 'ItemID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 6
          end>
        TypePrefix = 'T'
        ItemID = 7
      end
      item
        Name = 'Counters'
        Description = 'Counters'
        FieldDefs = <
          item
            Name = 'ItemID'
            Description = 'ItemID'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            DefaultExpression = '1'
            ItemID = 8
          end
          item
            Name = 'CustomerID'
            Description = 'CustomerID'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            DefaultExpression = '1'
            ItemID = 9
          end>
        Replicate = False
        TypePrefix = 'T'
        ItemID = 10
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
            DataType = ftInteger
            DefaultExpression = '0'
            ItemID = 11
          end
          item
            Name = 'LastName'
            Description = 'LastName'
            Required = True
            Attributes = [faRequired]
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
            Name = 'StreetNo'
            Description = 'StreetNo'
            DataType = ftString
            Size = 15
            ItemID = 14
          end
          item
            Name = 'Street'
            Description = 'Street'
            DataType = ftString
            Size = 60
            ItemID = 15
          end
          item
            Name = 'City'
            Description = 'City'
            DataType = ftString
            Size = 40
            ItemID = 16
          end
          item
            Name = 'State'
            Description = 'State'
            DataType = ftString
            Size = 15
            ItemID = 17
          end
          item
            Name = 'Zip'
            Description = 'Zip'
            DataType = ftString
            Size = 12
            ItemID = 18
          end
          item
            Name = 'HomePhone'
            Description = 'HomePhone'
            DataType = ftString
            Size = 20
            ItemID = 19
          end
          item
            Name = 'WorkPhone'
            Description = 'WorkPhone'
            DataType = ftString
            Size = 20
            ItemID = 20
          end
          item
            Name = 'MobilePhone'
            Description = 'MobilePhone'
            DataType = ftString
            Size = 20
            ItemID = 21
          end
          item
            Name = 'Fax'
            Description = 'Fax'
            DataType = ftString
            Size = 20
            ItemID = 22
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
            ItemID = 23
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
            ItemID = 24
          end>
        ObjectType = 'Customers222'
        TypePrefix = 'T'
        ItemID = 25
      end>
    SchemaName = 'Database'
    Left = 296
    Top = 104
    SchemaGUID = '{0E198846-5445-4AD9-96E8-68A8EEDBBAFC}'
  end
  object tblCounters: TnxTableExt
    Database = Database
    TableName = 'Counters'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 224
    Top = 152
    object tblCountersItemID: TIntegerField
      FieldName = 'ItemID'
      Origin = 'Counters.ItemID'
      Required = True
    end
    object tblCountersCustomerID: TIntegerField
      FieldName = 'CustomerID'
      Origin = 'Counters.CustomerID'
      Required = True
    end
  end
  object Database: TnxDatabaseExt
    Session = nxSession
    AliasPath = 
      'D:\Program Files\Borland\Delphi6\Projects\dbisamext\nxdemos\Sequ' +
      'ences\Data'
    Schema = DatabaseSchema
    SystemTableName = 'System'
    ObjectsTableName = 'Objects'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers]
    MaxCachedTables = 10
    DatabaseName = 'Database'
    Left = 224
    Top = 104
  end
  object tblCustomers: TnxTableExt
    Database = Database
    AfterInsert = tblCustomersAfterInsert
    BeforeCancel = tblCustomersBeforeCancel
    TableName = 'Customers'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 156
    Top = 152
  end
  object tblItems: TnxTableExt
    Database = Database
    AfterInsert = tblItemsAfterInsert
    BeforeCancel = tblItemsBeforeCancel
    TableName = 'Items'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 156
    Top = 104
  end
  object dsCustomers: TDataSource
    DataSet = tblCustomers
    Left = 168
    Top = 160
  end
  object dsItems: TDataSource
    DataSet = tblItems
    Left = 172
    Top = 112
  end
  object nxSession: TnxSession
    ServerEngine = nxServerEngine
    Left = 252
    Top = 68
  end
  object nxServerEngine: TnxServerEngine
    Options = []
    Left = 276
    Top = 40
  end
end
