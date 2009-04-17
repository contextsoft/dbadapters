object frmVersionDemo: TfrmVersionDemo
  Left = 338
  Top = 237
  Width = 715
  Height = 484
  Caption = 'Schema Versions Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    707
    450)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 8
    Top = 283
    Width = 40
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus'
  end
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Database:'
  end
  object Label2: TLabel
    Left = 127
    Top = 60
    Width = 566
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Press "Open Database" to open an existing database from the path' +
      ' above. If the database does not exist or has old structure, it ' +
      'will be updated automatically.'
    Color = clInfoBk
    ParentColor = False
    WordWrap = True
  end
  object lblVersion: TLabel
    Left = 8
    Top = 40
    Width = 3
    Height = 13
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 94
    Width = 686
    Height = 184
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 299
    Width = 687
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
  end
  object edtLog: TMemo
    Left = 8
    Top = 324
    Width = 687
    Height = 113
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object Button1: TButton
    Left = 628
    Top = 10
    Width = 65
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Select...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object edtDatabase: TEdit
    Left = 64
    Top = 11
    Width = 557
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 8
    Top = 62
    Width = 113
    Height = 25
    Caption = 'Open Database...'
    TabOrder = 5
    OnClick = Button2Click
  end
  object DatabaseSchema: TDatabaseSchema
    TargetDB = 'Nexus2'
    Updates = <
      item
        Description = 'Created: Customers; '
        SQLScript = 
          '-- ## TargetDB: Nexus2;'#13#10#13#10'CREATE TABLE "Customers" '#13#10'  DESCRIPT' +
          'ION '#39'Customers'#39#13#10'('#13#10'  "CustomerID" AUTOINC DEFAULT 0 NOT NULL,'#13#10 +
          '  "CompanyName" VARCHAR(40) NOT NULL,'#13#10'  "LastName" VARCHAR(40) ' +
          'NOT NULL,'#13#10'  "FirstName" VARCHAR(40),'#13#10'  "Initial" VARCHAR(5),'#13#10 +
          '  "StreetNo" VARCHAR(20),'#13#10'  "Street" VARCHAR(60),'#13#10'  "City" VAR' +
          'CHAR(80),'#13#10'  "State" VARCHAR(15),'#13#10'  "Zip" VARCHAR(12),'#13#10'  "Home' +
          'Phone" VARCHAR(20),'#13#10'  "WorkPhone" VARCHAR(20),'#13#10'  "CareOf" VARC' +
          'HAR(40),'#13#10'  "MobilePhone" VARCHAR(20),'#13#10'  "Fax" VARCHAR(20),'#13#10'  ' +
          'PRIMARY KEY ("CustomerID")'#13#10');'#13#10#13#10'CREATE INDEX "ByName" ON "Cust' +
          'omers"("LastName","FirstName");'#13#10
        Iterate = False
        VersionLabel = '1.1'
        ItemID = 22
      end
      item
        Description = 'Altered: Customers; '
        SQLScript = 
          '-- ## TargetDB: Nexus2;'#13#10#13#10'CREATE INDEX "ByCity" ON "Customers"(' +
          '"City");'#13#10
        Iterate = False
        VersionLabel = '1.2'
        ItemID = 24
      end
      item
        Description = 'Altered: Customers; '
        SQLScript = 
          '-- ## TargetDB: Nexus2;'#13#10#13#10'ALTER TABLE "Customers"'#13#10'  ALTER  "La' +
          'stName"  DROP CONSTRAINT NOT NULL;'#13#10#13#10'ALTER TABLE "Customers"'#13#10' ' +
          ' ALTER  "Fax"  ADD CONSTRAINT NOT NULL;'#13#10
        Iterate = False
        VersionLabel = '1.3'
        ItemID = 26
      end
      item
        Description = 'Altered: Customers; '
        SQLScript = 
          '-- ## TargetDB: Nexus2;'#13#10#13#10'CREATE TABLE "temp_Customers" ('#13#10'  "C' +
          'ustomerID" AUTOINC DEFAULT 0 NOT NULL,'#13#10'  "CompanyName" VARCHAR(' +
          '40) NOT NULL,'#13#10'  "LastName" VARCHAR(40),'#13#10'  "FirstName" VARCHAR(' +
          '40),'#13#10'  "Initial" VARCHAR(5),'#13#10'  "StreetNo" VARCHAR(20),'#13#10'  "Str' +
          'eet" VARCHAR(60),'#13#10'  "City" VARCHAR(80),'#13#10'  "State" VARCHAR(15),' +
          #13#10'  "Zip" VARCHAR(12),'#13#10'  "HomePhone" VARCHAR(20),'#13#10'  "WorkPhone' +
          '" VARCHAR(20),'#13#10'  "CareOf" VARCHAR(40),'#13#10'  "MobilePhone" VARCHAR' +
          '(20),'#13#10'  "Fax" VARCHAR(20) NOT NULL'#13#10');'#13#10#13#10'INSERT INTO "temp_Cus' +
          'tomers" SELECT * FROM "Customers";'#13#10#13#10'DROP TABLE "Customers";'#13#10#13 +
          #10'CREATE TABLE "Customers" '#13#10'  DESCRIPTION '#39'Customers'#39#13#10'('#13#10'  "Cus' +
          'tomerID" AUTOINC DEFAULT 0 NOT NULL,'#13#10'  "CompanyName" VARCHAR(40' +
          ') NOT NULL,'#13#10'  "LastName" VARCHAR(40),'#13#10'  "FirstName" VARCHAR(40' +
          '),'#13#10'  "Initial" VARCHAR(5),'#13#10'  "StreetNo" VARCHAR(20),'#13#10'  "Stree' +
          't" VARCHAR(60),'#13#10'  "City" VARCHAR(80),'#13#10'  "State" VARCHAR(15),'#13#10 +
          '  "Zip" VARCHAR(12),'#13#10'  "HomePhone" VARCHAR(20),'#13#10'  "WorkPhone" ' +
          'VARCHAR(20),'#13#10'  "CareOf" VARCHAR(40),'#13#10'  "MobilePhone" VARCHAR(2' +
          '0),'#13#10'  "Fax" VARCHAR(40) NOT NULL,'#13#10'  PRIMARY KEY ("CustomerID")' +
          #13#10');'#13#10#13#10'CREATE INDEX "ByName" ON "Customers"("LastName","FirstNa' +
          'me");'#13#10#13#10'CREATE INDEX "ByCity" ON "Customers"("City");'#13#10#13#10'INSERT' +
          ' INTO "Customers"('#13#10'  "CustomerID", "CompanyName", "LastName", "' +
          'FirstName", "Initial", "StreetNo", "Street", "City", "State", "Z' +
          'ip", "HomePhone", "WorkPhone", "CareOf", "MobilePhone", "Fax"'#13#10')' +
          ' SELECT'#13#10'  "CustomerID", "CompanyName", "LastName", "FirstName",' +
          ' "Initial", "StreetNo", "Street", "City", "State", "Zip", "HomeP' +
          'hone", "WorkPhone", "CareOf", "MobilePhone", "Fax"'#13#10'FROM "temp_C' +
          'ustomers";'#13#10#13#10'DROP TABLE "temp_Customers";'#13#10
        Iterate = False
        VersionLabel = '1.4'
        ItemID = 27
      end>
    TableDefs = <
      item
        Name = 'Customers'
        Description = 'Customers'
        FieldDefs = <
          item
            Name = 'CustomerID'
            Description = 'Customer #'
            Required = True
            Attributes = [faRequired]
            DataType = ftAutoInc
            DefaultExpression = '0'
            Identity = True
            ItemID = 1
          end
          item
            Name = 'CompanyName'
            Description = 'Company'
            Required = True
            Attributes = [faRequired]
            DataType = ftString
            Size = 40
            ItemID = 2
          end
          item
            Name = 'LastName'
            Description = 'Last Name'
            DataType = ftString
            Size = 40
            ItemID = 3
          end
          item
            Name = 'FirstName'
            Description = 'First Name'
            DataType = ftString
            Size = 40
            ItemID = 4
          end
          item
            Name = 'Initial'
            Description = 'Initial'
            DataType = ftString
            Size = 5
            ItemID = 5
          end
          item
            Name = 'StreetNo'
            Description = 'Street #'
            DataType = ftString
            Size = 20
            ItemID = 6
          end
          item
            Name = 'Street'
            Description = 'Street'
            DataType = ftString
            Size = 60
            ItemID = 7
          end
          item
            Name = 'City'
            Description = 'City'
            DataType = ftString
            Size = 80
            ItemID = 8
          end
          item
            Name = 'State'
            Description = 'State'
            DataType = ftString
            Size = 15
            ItemID = 9
          end
          item
            Name = 'Zip'
            Description = 'Zip'
            DataType = ftString
            Size = 12
            ItemID = 10
          end
          item
            Name = 'HomePhone'
            Description = 'HomePhone'
            DataType = ftString
            Size = 20
            ItemID = 11
          end
          item
            Name = 'WorkPhone'
            Description = 'WorkPhone'
            DataType = ftString
            Size = 20
            ItemID = 12
          end
          item
            Name = 'CareOf'
            Description = 'C/O'
            DataType = ftString
            Size = 40
            ItemID = 18
          end
          item
            Name = 'MobilePhone'
            Description = 'MobilePhone'
            DataType = ftString
            Size = 20
            ItemID = 19
          end
          item
            Name = 'Fax'
            Description = 'Fax'
            Required = True
            Attributes = [faRequired]
            DataType = ftString
            Size = 40
            ItemID = 20
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
            ItemID = 13
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
            ItemID = 14
          end
          item
            Name = 'ByCity'
            IndexFields = <
              item
                Name = 'City'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            ItemID = 23
          end>
        ObjectType = 'Customers222'
        TypePrefix = 'T'
        Category = 'References'
        ItemID = 16
      end>
    SchemaName = 'Database'
    Left = 64
    Top = 168
    SchemaGUID = '{7736A6A6-3CD0-43AE-99FD-F62F0896EDD7}'
  end
  object DatabaseExt: TnxDatabaseExt
    Session = nxSession1
    AliasPath = 
      'E:\D7\SDK\Context\DBExt\adapters\Nexus2\demos\SchemaVersions\Dat' +
      'a'
    Schema = DatabaseSchema
    SystemTableName = 'SysTable'
    DatabaseName = 'MainDB'
    Left = 144
    Top = 168
  end
  object DataSource1: TDataSource
    DataSet = nxQuery1
    Left = 376
    Top = 176
  end
  object nxSession1: TnxSession
    ServerEngine = nxServerEngine1
    Left = 208
    Top = 176
  end
  object nxQuery1: TnxQuery
    Database = DatabaseExt
    SQL.Strings = (
      'select * from customers')
    Left = 344
    Top = 160
  end
  object nxseAllEngines1: TnxseAllEngines
    Left = 512
    Top = 168
  end
  object nxServerEngine1: TnxServerEngine
    SqlEngine = nxSqlEngine1
    Options = []
    TableExtension = 'nx1'
    Left = 512
    Top = 112
  end
  object nxSqlEngine1: TnxSqlEngine
    ActiveDesigntime = True
    Left = 592
    Top = 112
  end
end
