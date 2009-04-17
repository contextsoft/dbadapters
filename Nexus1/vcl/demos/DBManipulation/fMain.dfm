object frmMainForm: TfrmMainForm
  Left = 306
  Top = 200
  Width = 591
  Height = 425
  Caption = 'Database Manipulation Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 583
    Height = 26
    AutoSize = True
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
    Images = Images
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = actNewDatabase
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Action = actOpenDatabase
    end
    object ToolButton3: TToolButton
      Left = 46
      Top = 0
      Action = actSaveDatabase
    end
    object ToolButton4: TToolButton
      Left = 69
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton7: TToolButton
      Left = 77
      Top = 0
      Action = DataSetFirst1
    end
    object ToolButton8: TToolButton
      Left = 100
      Top = 0
      Action = DataSetPrior1
    end
    object ToolButton13: TToolButton
      Left = 123
      Top = 0
      Action = DataSetNext1
    end
    object ToolButton14: TToolButton
      Left = 146
      Top = 0
      Action = DataSetLast1
    end
    object ToolButton15: TToolButton
      Left = 169
      Top = 0
      Action = DataSetRefresh1
    end
    object ToolButton17: TToolButton
      Left = 192
      Top = 0
      Width = 8
      Caption = 'ToolButton17'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object ToolButton9: TToolButton
      Left = 200
      Top = 0
      Action = DataSetEdit1
    end
    object ToolButton16: TToolButton
      Left = 223
      Top = 0
      Action = DataSetInsert1
    end
    object ToolButton12: TToolButton
      Left = 246
      Top = 0
      Action = DataSetDelete1
    end
    object ToolButton18: TToolButton
      Left = 269
      Top = 0
      Width = 8
      Caption = 'ToolButton18'
      ImageIndex = 8
      Style = tbsSeparator
    end
    object ToolButton10: TToolButton
      Left = 277
      Top = 0
      Action = DataSetPost1
    end
    object ToolButton11: TToolButton
      Left = 300
      Top = 0
      Action = DataSetCancel1
    end
    object ToolButton6: TToolButton
      Left = 323
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 331
      Top = 0
      Action = actExit
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 26
    Width = 583
    Height = 345
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 0
      Top = 223
      Width = 583
      Height = 2
      Cursor = crVSplit
      Align = alBottom
    end
    object DBGrid2: TDBGrid
      Left = 0
      Top = 0
      Width = 583
      Height = 223
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
          Width = 63
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'ReferredBy'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'CompanyName'
          Width = 97
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'LastName'
          Width = 67
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'FirstName'
          Width = 63
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Initial'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'CareOf'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'StreetNo'
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
        end
        item
          Expanded = False
          FieldName = 'TaxType'
          Visible = True
        end>
    end
    object DBGrid1: TDBGrid
      Left = 0
      Top = 225
      Width = 583
      Height = 120
      Align = alBottom
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
          Width = 50
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'DateCreated'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'CustomerID'
          Width = 61
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Status'
          Width = 53
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Notes'
          Width = 63
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'PayOffDate'
          Width = 71
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'SalesRep'
          Width = 93
          Visible = True
        end>
    end
  end
  object MainMenu: TMainMenu
    Images = Images
    Left = 18
    Top = 82
    object File1: TMenuItem
      Caption = '&File'
      object NewDatabase1: TMenuItem
        Action = actNewDatabase
      end
      object OpenDatabase1: TMenuItem
        Action = actOpenDatabase
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object SaveDatabaseToFile1: TMenuItem
        Action = actSaveDatabase
      end
      object ImportDatabase1: TMenuItem
        Action = actImportDatabase
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object Customers1: TMenuItem
      Caption = '&Customers'
      object First1: TMenuItem
        Action = DataSetFirst1
      end
      object Last1: TMenuItem
        Action = DataSetLast1
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Prior1: TMenuItem
        Action = DataSetPrior1
      end
      object Next1: TMenuItem
        Action = DataSetNext1
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Edit1: TMenuItem
        Action = DataSetInsert1
      end
      object Edit2: TMenuItem
        Action = DataSetEdit1
      end
      object Delete1: TMenuItem
        Action = DataSetDelete1
      end
      object Post1: TMenuItem
        Action = DataSetPost1
      end
      object Cancel1: TMenuItem
        Action = DataSetCancel1
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Refresh1: TMenuItem
        Action = DataSetRefresh1
      end
    end
  end
  object Actions: TActionList
    Images = Images
    Left = 74
    Top = 81
    object actOpenDatabase: TAction
      Category = 'File'
      Caption = '&Open Database...'
      Hint = 'Open existing database'
      ImageIndex = 2
      ShortCut = 16463
      OnExecute = actOpenDatabaseExecute
    end
    object actNewDatabase: TAction
      Category = 'File'
      Caption = '&New Database...'
      Hint = 'Create new database from template'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = actNewDatabaseExecute
    end
    object actSaveDatabase: TAction
      Category = 'File'
      Caption = '&Export Database To File...'
      Hint = 'Save (Export) the database to single external file'
      ImageIndex = 1
      ShortCut = 16453
      OnExecute = actSaveDatabaseExecute
    end
    object actImportDatabase: TAction
      Category = 'File'
      Caption = '&Import Database From File...'
      Hint = 'Import database from a single file...'
      ShortCut = 16457
      OnExecute = actImportDatabaseExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = '&Exit'
      Hint = 'Exit Application'
      ImageIndex = 6
      OnExecute = actExitExecute
    end
    object DataSetCancel1: TDataSetCancel
      Category = 'Customers'
      Caption = '&Cancel'
      Hint = 'Cancel'
      ImageIndex = 7
    end
    object DataSetDelete1: TDataSetDelete
      Category = 'Customers'
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 8
    end
    object DataSetEdit1: TDataSetEdit
      Category = 'Customers'
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 9
    end
    object DataSetFirst1: TDataSetFirst
      Category = 'Customers'
      Caption = '&First'
      Hint = 'First'
      ImageIndex = 10
    end
    object DataSetInsert1: TDataSetInsert
      Category = 'Customers'
      Caption = '&Insert'
      Hint = 'Insert'
      ImageIndex = 11
    end
    object DataSetLast1: TDataSetLast
      Category = 'Customers'
      Caption = '&Last'
      Hint = 'Last'
      ImageIndex = 12
    end
    object DataSetNext1: TDataSetNext
      Category = 'Customers'
      Caption = '&Next'
      Hint = 'Next'
      ImageIndex = 13
    end
    object DataSetPost1: TDataSetPost
      Category = 'Customers'
      Caption = 'P&ost'
      Hint = 'Post'
      ImageIndex = 14
    end
    object DataSetPrior1: TDataSetPrior
      Category = 'Customers'
      Caption = '&Prior'
      Hint = 'Prior'
      ImageIndex = 15
    end
    object DataSetRefresh1: TDataSetRefresh
      Category = 'Customers'
      Caption = '&Refresh'
      Hint = 'Refresh'
      ImageIndex = 16
      ShortCut = 116
    end
  end
  object Database: TnxDatabaseExt
    Session = nxSession
    AliasPath = 'E:\projects\sdk\Context\DBExt\demos\NX\DBManipulation\Data'
    Schema = DBSchema
    SystemTableName = 'System'
    ObjectsTableName = 'Objects'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers]
    MaxCachedTables = 10
    DatabaseName = 'Database'
    Left = 16
    Top = 131
  end
  object DBSchema: TDatabaseSchema
    TargetDB = 'Nexus1'
    Updates = <
      item
        Description = 'Created: Customers, Orders; '
        SQLScript = 
          #13#10'CREATE TABLE "Customers" ('#13#10'  "CustomerID" INT NOT NULL,'#13#10'  "R' +
          'eferredBy" INT,'#13#10'  "CompanyName" VARCHAR(40),'#13#10'  "LastName" VARC' +
          'HAR(40),'#13#10'  "FirstName" VARCHAR(40),'#13#10'  "Initial" VARCHAR(5),'#13#10' ' +
          ' "CareOf" VARCHAR(20),'#13#10'  "StreetNo" VARCHAR(20),'#13#10'  "Street" VA' +
          'RCHAR(60),'#13#10'  "City" VARCHAR(80),'#13#10'  "State" VARCHAR(15),'#13#10'  "Zi' +
          'p" VARCHAR(20),'#13#10'  "HomePhone" VARCHAR(20),'#13#10'  "WorkPhone" VARCH' +
          'AR(20),'#13#10'  "MobilePhone" VARCHAR(20),'#13#10'  "Fax" VARCHAR(20),'#13#10'  "' +
          'TaxType" VARCHAR(20),'#13#10'  PRIMARY KEY (CustomerID)'#13#10');'#13#10'-- GO --'#13 +
          #10#13#10'CREATE INDEX "ByName" ON "Customers"(LastName, FirstName);'#13#10'-' +
          '- GO --'#13#10#13#10'CREATE TABLE "Orders" ('#13#10'  "OrderID" INT NOT NULL,'#13#10' ' +
          ' "DateCreated" DATE,'#13#10'  "CustomerID" INT NOT NULL,'#13#10'  "Status" S' +
          'MALLINT,'#13#10'  "Notes" TEXT,'#13#10'  "PayOffDate" DATE,'#13#10'  "TaxType" VAR' +
          'CHAR(10),'#13#10'  "SalesRep" VARCHAR(20),'#13#10'  "Tax" FLOAT,'#13#10'  PRIMARY ' +
          'KEY (OrderID)'#13#10');'#13#10'-- GO --'#13#10#13#10'CREATE INDEX "ByCustomer" ON "Ord' +
          'ers"(CustomerID);'#13#10'-- GO --'#13#10
        Iterate = False
        VersionLabel = '1.1'
        ItemID = 38
      end>
    Relationships = <
      item
        Name = 'Customers_ReferredBy'
        DetailTableName = 'Customers'
        DetailKeyFields = 'ReferredBy'
        DetailRelationName = 'ReferredBy'
        MasterTableName = 'Customers'
        MasterKeyFields = 'CustomerID'
        MasterRelationName = 'Customers'
        DeleteAction = raNullify
        UpdateAction = raCascade
        ItemID = 21
      end
      item
        Name = 'Orders_Customer'
        DetailTableName = 'Orders'
        DetailKeyFields = 'CustomerID'
        DetailRelationName = 'Customer'
        MasterTableName = 'Customers'
        MasterKeyFields = 'CustomerID'
        MasterRelationName = 'Orders'
        MasterRecordOptional = False
        DeleteAction = raError
        UpdateAction = raCascade
        ItemID = 37
      end>
    TableDefs = <
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
            ItemID = 1
          end
          item
            Name = 'ReferredBy'
            Description = 'ReferredBy'
            DataType = ftInteger
            ItemID = 2
          end
          item
            Name = 'CompanyName'
            Description = 'CompanyName'
            DataType = ftString
            Size = 40
            ItemID = 3
          end
          item
            Name = 'LastName'
            Description = 'LastName'
            DataType = ftString
            Size = 40
            ItemID = 4
          end
          item
            Name = 'FirstName'
            Description = 'FirstName'
            DataType = ftString
            Size = 40
            ItemID = 5
          end
          item
            Name = 'Initial'
            Description = 'Initial'
            DataType = ftString
            Size = 5
            ItemID = 6
          end
          item
            Name = 'CareOf'
            Description = 'CareOf'
            DataType = ftString
            Size = 20
            ItemID = 7
          end
          item
            Name = 'StreetNo'
            Description = 'StreetNo'
            DataType = ftString
            Size = 20
            ItemID = 8
          end
          item
            Name = 'Street'
            Description = 'Street'
            DataType = ftString
            Size = 60
            ItemID = 9
          end
          item
            Name = 'City'
            Description = 'City'
            DataType = ftString
            Size = 80
            ItemID = 10
          end
          item
            Name = 'State'
            Description = 'State'
            DataType = ftString
            Size = 15
            ItemID = 11
          end
          item
            Name = 'Zip'
            Description = 'Zip'
            DataType = ftString
            Size = 20
            ItemID = 12
          end
          item
            Name = 'HomePhone'
            Description = 'HomePhone'
            DataType = ftString
            Size = 20
            ItemID = 13
          end
          item
            Name = 'WorkPhone'
            Description = 'WorkPhone'
            DataType = ftString
            Size = 20
            ItemID = 14
          end
          item
            Name = 'MobilePhone'
            Description = 'MobilePhone'
            DataType = ftString
            Size = 20
            ItemID = 15
          end
          item
            Name = 'Fax'
            Description = 'Fax'
            DataType = ftString
            Size = 20
            ItemID = 16
          end
          item
            Name = 'TaxType'
            Description = 'TaxType'
            DataType = ftString
            Size = 20
            ItemID = 17
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
            ItemID = 18
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
            ItemID = 19
          end>
        TypePrefix = 'T'
        ItemID = 23
      end
      item
        Name = 'Orders'
        Description = 'Orders'
        FieldDefs = <
          item
            Name = 'OrderID'
            Description = 'OrderID'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 24
          end
          item
            Name = 'DateCreated'
            Description = 'DateCreated'
            DataType = ftDate
            ItemID = 25
          end
          item
            Name = 'CustomerID'
            Description = 'CustomerID'
            Required = True
            Attributes = [faRequired]
            DataType = ftInteger
            ItemID = 26
          end
          item
            Name = 'Status'
            Description = 'Status'
            DataType = ftSmallint
            ItemID = 27
          end
          item
            Name = 'Notes'
            Description = 'Notes'
            DataType = ftMemo
            ItemID = 28
          end
          item
            Name = 'PayOffDate'
            Description = 'PayOffDate'
            DataType = ftDate
            ItemID = 29
          end
          item
            Name = 'TaxType'
            Description = 'TaxType'
            DataType = ftString
            Size = 10
            ItemID = 30
          end
          item
            Name = 'SalesRep'
            Description = 'SalesRep'
            DataType = ftString
            Size = 20
            ItemID = 31
          end
          item
            Name = 'Tax'
            Description = 'Tax'
            DataType = ftFloat
            ItemID = 32
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
            ItemID = 33
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
            ItemID = 34
          end>
        TypePrefix = 'T'
        ItemID = 36
      end>
    SchemaName = 'Database'
    Left = 80
    Top = 136
    SchemaGUID = '{5C6F5868-82B5-4D44-AD32-38D2851F3030}'
  end
  object Images: TImageList
    Left = 118
    Top = 81
    Bitmap = {
      494C010111001300040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000005000000001002000000000000050
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000080808000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      00000000000000000000000000000000000000000000BFBFBF00BFBFBF00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00000000000000000000000000000000000000
      800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000000000000000000000000000000000000000BFBFBF00BFBFBF00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00000000000000000000000000000000000000
      80000000800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8000000080000000800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00000000000000000000000000FFFFFF00BFBFBF00FFFF
      FF00BFBFBF00FFFFFF00BFBFBF00FFFFFF00BFBFBF00FFFFFF00BFBFBF00FFFF
      FF00BFBFBF00FFFFFF00BFBFBF00000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000000000000000BFBFBF00FFFFFF00BFBF
      BF00FFFFFF00BFBFBF00FFFFFF00BFBFBF00FFFFFF00BFBFBF00FFFFFF00BFBF
      BF00FFFFFF000000FF00FFFFFF00000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF00000000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00000000000000000000000000FFFFFF00BFBFBF00FFFF
      FF00BFBFBF00FFFFFF00BFBFBF00FFFFFF00BFBFBF00FFFFFF00BFBFBF00FFFF
      FF00BFBFBF00FFFFFF00BFBFBF00000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      8000000080000000800000FFFF000000000000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000BFBF
      BF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      800000008000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000000000BFBFBF00FFFFFF0000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8000FFFF0000000080000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8000FFFF0000FFFF00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007F7F7F000000
      00007F7F7F007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      00007F7F7F007F7F7F007F7F7F0000FFFF0000FFFF007F7F7F007F7F7F007F7F
      7F007F7F7F0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF000000
      0000BFBFBF00BFBFBF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF007F7F7F000000FF007F7F7F00FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF00BFBF
      BF00BFBFBF00BFBFBF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF000000FF000000FF000000FF0000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF007F7F7F000000FF007F7F7F00FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000000000FF
      FF00BFBFBF0000FFFF00BFBFBF0000FFFF00BFBFBF0000FFFF00BFBFBF0000FF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF000000FF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000007F7F7F00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      000000FFFF00BFBFBF0000FFFF00BFBFBF0000FFFF00BFBFBF0000FFFF00BFBF
      BF0000FFFF00000000000000000000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF000000FF007F7F7F0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF000000000000FFFF0000FFFF0000FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000BFBFBF0000000000FF000000FF000000FF00
      00000000FF00FF000000FF00000000000000000000000000000000FFFF00FFFF
      FF000000000000FFFF00BFBFBF0000FFFF00BFBFBF0000FFFF00BFBFBF0000FF
      FF00BFBFBF0000FFFF00000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF000000FF000000FF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000FFFF0000FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF0000FFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000FF000000FF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      00000000000000000000FFFFFF0000000000FFFFFF00000000000000FF000000
      FF000000FF000000FF000000FF0000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF007F7F7F007F7F7F0000FFFF00FFFFFF007F7F7F000000FF000000FF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000000000000000000000000000000000000FFFFFF0000FFFF00FFFFFF0000FF
      FF000000FF000000FF00FFFFFF0000FFFF007F7F7F000000FF000000FF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000000000FF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF000000FF000000FF007F7F7F00FFFFFF007F7F7F000000FF000000FF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF0000FFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF000000FF000000FF000000FF000000FF000000FF00FFFFFF0000FF
      FF00FFFFFF0000FFFF000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF00000000000000000000000000FFFFFF0000000000BFBF
      BF00FFFFFF0000000000FFFFFF000000000000000000000000007F7F7F000000
      FF000000FF000000FF00000000000000000000000000000000007F7F7F000000
      00000000000000000000000000007F7F7F000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF000000FF000000FF000000FF00FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000FFFF0000FFFF000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      00000000000000FFFF0000FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF000000000000000000000000000000000000FFFF0000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000500000000100010000000000800200000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      FFFF000000000000FC7F000000000000F0FF000000000000F1FF000000000000
      E3FF000000000000E7FF000000000000E707000000000000E387000000000000
      E107000000000000F007000000000000F837000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFE7E7F9FFF1FFFF9F
      E1E7F87FE0FFFE1FE067F81FC47FF81FE007F80FCE3FF01FE067F81FFF1FF81F
      E1E7F87FFF8FFE1FE7E7F9FFFFC7FF9FFFFFFFFFFFE7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7FFFFFFFFFE7E7FC7F
      FFFFE007E787FC7FE007F00FE607E00FE007F81FE007E00FE007FC3FE607E00F
      FFFFFE7FE787FC7FFFFFFFFFE7E7FC7FFFFFFFFFFFFFFC7FFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDF8001C007FFFFFFCF0000C007FFFF
      FFC70000C007FFFF00030000C007FFFF00010000C007F3E700000000C007F1C7
      00010000C007F88F00030000C007FC1F0007E007C007FE3F000FE007C007FC1F
      001FE007C007F88F007FE007C007F1C700FFE00FC007F3E701FFE01FC007FFFF
      03FFE03FC007FFFFFFFFE07FC007FFFFFF7EFF00FFFFFFFF9001FF00FFFFF83F
      C003FF00FFFFE00FE003FF00FFFFC007E0030000C00F8003E003000080078003
      E00300008003000100010000800100018000002380010001E0070001800F0001
      E00F0000800F0001E00F0023801F8003E0270063C0FF8003C07300C3C0FFC007
      9E790107FFFFE00F7EFE03FFFFFFF83F00000000000000000000000000000000
      000000000000}
  end
  object tblCustomers: TnxTableExt
    Database = Database
    TableName = 'Customers'
    IndexName = 'ByName'
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 194
    Top = 125
  end
  object dsCustomers: TDataSource
    DataSet = tblCustomers
    Left = 258
    Top = 125
  end
  object tblOrders: TnxTableExt
    Database = Database
    TableName = 'Orders'
    IndexName = 'ByCustomer'
    MasterFields = 'CustomerID'
    MasterSource = dsCustomers
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 199
    Top = 176
  end
  object dsOrders: TDataSource
    DataSet = tblOrders
    Left = 262
    Top = 176
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.dbd'
    Filter = 'Database Data (*.dbd)|*.*'
    Left = 280
    Top = 80
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.dbd'
    Filter = 'Database Data (*.dbd)|*.*'
    Left = 344
    Top = 80
  end
  object nxSession: TnxSession
    ServerEngine = nxServerEngine
    Left = 16
    Top = 184
  end
  object OpenDatabaseDialog: TnxOpenDatabaseDialog
    NxSession = nxSession
    Left = 192
    Top = 80
  end
  object nxServerEngine: TnxServerEngine
    Options = []
    Left = 84
    Top = 196
  end
  object DBManager: TDBManager
    Database = Database
    Left = 136
    Top = 162
  end
end
