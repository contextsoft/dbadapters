inherited frmEditPayment: TfrmEditPayment
  Left = 376
  Top = 185
  Width = 515
  Height = 396
  BorderStyle = bsSizeable
  Caption = 'Enter Payment'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 12
    Top = 11
    Width = 51
    Height = 13
    Caption = 'Payment #'
    FocusControl = DBEdit1
  end
  object Label3: TLabel [1]
    Left = 84
    Top = 11
    Width = 23
    Height = 13
    Caption = 'Date'
    FocusControl = DBEdit3
  end
  object Label5: TLabel [2]
    Left = 120
    Top = 55
    Width = 50
    Height = 13
    Caption = 'Reference'
    FocusControl = DBEdit5
  end
  object Label6: TLabel [3]
    Left = 340
    Top = 55
    Width = 60
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Amount Paid'
    FocusControl = DBEdit6
  end
  object Label7: TLabel [4]
    Left = 156
    Top = 11
    Width = 44
    Height = 13
    Caption = 'Customer'
    FocusControl = DBLookupComboBox1
  end
  object Label2: TLabel [5]
    Left = 12
    Top = 55
    Width = 78
    Height = 13
    Caption = 'Form of payment'
    FocusControl = DBLookupComboBox2
  end
  object Label4: TLabel [6]
    Left = 12
    Top = 100
    Width = 127
    Height = 13
    Caption = 'Payments applied to orders'
    FocusControl = DBLookupComboBox2
  end
  object Label8: TLabel [7]
    Left = 232
    Top = 341
    Width = 96
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Credited to account:'
    FocusControl = DBEdit2
  end
  object Label9: TLabel [8]
    Left = 194
    Top = 316
    Width = 134
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Amount paid towards orders:'
    FocusControl = DBEdit4
  end
  inherited Panel1: TPanel
    Left = 419
    Height = 369
    TabOrder = 10
    inherited Bevel1: TBevel
      Height = 369
    end
  end
  object DBEdit1: TDBEdit [10]
    Left = 12
    Top = 27
    Width = 64
    Height = 21
    DataField = 'PaymentID'
    DataSource = dsData
    TabOrder = 0
  end
  object DBEdit3: TDBEdit [11]
    Left = 84
    Top = 27
    Width = 64
    Height = 21
    DataField = 'DateCreated'
    DataSource = dsData
    TabOrder = 1
  end
  object DBEdit5: TDBEdit [12]
    Left = 120
    Top = 71
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DataField = 'Reference'
    DataSource = dsData
    TabOrder = 5
  end
  object DBEdit6: TDBEdit [13]
    Left = 340
    Top = 71
    Width = 64
    Height = 21
    Anchors = [akTop, akRight]
    DataField = 'TotalAmount'
    DataSource = dsData
    TabOrder = 6
  end
  object DBLookupComboBox1: TDBLookupComboBox [14]
    Left = 155
    Top = 27
    Width = 249
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DataField = 'Customer'
    DataSource = dsData
    TabOrder = 2
  end
  object DBLookupComboBox2: TDBLookupComboBox [15]
    Left = 12
    Top = 71
    Width = 101
    Height = 21
    DataField = 'FOP'
    DataSource = dsData
    TabOrder = 4
  end
  object DBGrid1: TDBGrid [16]
    Left = 12
    Top = 116
    Width = 392
    Height = 187
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dsPaymentLines
    TabOrder = 7
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'OrderID'
        ReadOnly = True
        Width = 51
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Width = 167
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Balance'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Amount'
        Visible = True
      end>
  end
  object DBEdit2: TDBEdit [17]
    Left = 340
    Top = 337
    Width = 64
    Height = 21
    Anchors = [akRight, akBottom]
    DataField = 'CreditedToAccount'
    DataSource = dsData
    TabOrder = 9
  end
  object DBEdit4: TDBEdit [18]
    Left = 340
    Top = 312
    Width = 64
    Height = 21
    Anchors = [akRight, akBottom]
    DataField = 'AppliedToOrders'
    DataSource = dsData
    TabOrder = 8
  end
  object btnSelectCustomer: TBitBtn [19]
    Left = 331
    Top = 4
    Width = 73
    Height = 22
    Anchors = [akTop, akRight]
    Caption = 'Select...'
    TabOrder = 3
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
  object DBNavigator1: TDBNavigator [20]
    Left = 12
    Top = 307
    Width = 140
    Height = 21
    DataSource = dsPaymentLines
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbEdit, nbPost, nbCancel]
    Anchors = [akLeft, akBottom]
    Flat = True
    TabOrder = 11
  end
  inherited tblData: TnxTableExt
    TableName = 'Payments'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 236
    Top = 144
    object tblDataPaymentID: TIntegerField
      FieldName = 'PaymentID'
      Required = True
    end
    object tblDataCustomerID: TIntegerField
      DisplayLabel = 'Customer #'
      FieldName = 'CustomerID'
      Origin = 'Payments.CustomerID'
      Required = True
    end
    object tblDataDateCreated: TDateField
      DisplayLabel = 'Created'
      FieldName = 'DateCreated'
      Origin = 'Payments.DateCreated'
      EditMask = '00/00/00'
    end
    object tblDataFormOfPayment: TStringField
      DisplayLabel = 'Form of payment'
      FieldName = 'FormOfPayment'
      Origin = 'Payments.FormOfPayment'
      Required = True
      Size = 10
    end
    object tblDataReference: TStringField
      FieldName = 'Reference'
      Origin = 'Payments.Reference'
    end
    object tblDataTotalAmount: TCurrencyField
      DefaultExpression = '0'
      DisplayLabel = 'Total Amount'
      FieldName = 'TotalAmount'
      Origin = 'Payments.TotalAmount'
      Required = True
    end
    object tblDataCustomer: TStringField
      FieldKind = fkLookup
      FieldName = 'Customer'
      LookupDataSet = dmReferences.tblCustomers
      LookupKeyFields = 'CustomerID'
      LookupResultField = 'CompanyName'
      KeyFields = 'CustomerID'
      Size = 80
      Lookup = True
    end
    object tblDataFOP: TStringField
      FieldKind = fkLookup
      FieldName = 'FOP'
      LookupDataSet = dmReferences.tblFormOfPayments
      LookupKeyFields = 'FormOfPayment'
      LookupResultField = 'Description'
      KeyFields = 'FormOfPayment'
      Lookup = True
    end
    object tblDataCreditedToAccount: TCurrencyField
      FieldName = 'CreditedToAccount'
      Origin = 'Payments.CreditedToAccount'
      Required = True
    end
    object tblDataAppliedToOrders: TCurrencyField
      FieldName = 'AppliedToOrders'
      Origin = 'Payments.AppliedToOrders'
      Required = True
    end
  end
  inherited dsData: TDataSource
    OnDataChange = dsDataDataChange
    Left = 250
    Top = 156
  end
  object docPayment: TDBDocument
    DocumentType = 'Payment'
    DatabaseName = 'DBMAIN'
    MasterTable = tblData
    DataSets = <
      item
        Name = 'PaymentLines'
        TableName = 'PaymentLines'
        DocumentIDField = 'PaymentID'
        ItemIDField = 'PaymentLineID'
        DataSet = tblPaymentLines
        DataSets = <>
      end>
    Left = 219
    Top = 208
  end
  object tblPaymentLines: TDbMemDataSet
    AutoCalcFields = False
    Filter = 'Amount <> 0'
    BeforeInsert = LockMaster
    BeforeEdit = LockMaster
    BeforePost = tblPaymentLinesBeforePost
    BeforeDelete = LockMaster
    FieldDefs = <
      item
        Name = 'PaymentLineID'
        DataType = ftInteger
      end
      item
        Name = 'PaymentID'
        DataType = ftInteger
      end
      item
        Name = 'OrderID'
        DataType = ftInteger
      end
      item
        Name = 'Amount'
        DataType = ftCurrency
      end>
    Left = 300
    Top = 184
    Data = {
      010004000D005061796D656E744C696E6549440300000009005061796D656E74
      49440300000007004F726465724944030000000600416D6F756E740700000000
      0000000000}
    object tblPaymentLinesPaymentLineID: TIntegerField
      FieldName = 'PaymentLineID'
    end
    object tblPaymentLinesPaymentID: TIntegerField
      DisplayLabel = 'Payment #'
      FieldName = 'PaymentID'
      Origin = 'PaymentLines.PaymentID'
    end
    object tblPaymentLinesOrderID: TIntegerField
      DisplayLabel = 'Order #'
      FieldName = 'OrderID'
      Origin = 'PaymentLines.OrderID'
      Required = True
    end
    object tblPaymentLinesAmount: TCurrencyField
      DefaultExpression = '0'
      FieldName = 'Amount'
      Origin = 'PaymentLines.Amount'
      Required = True
    end
    object tblPaymentLinesBalance: TCurrencyField
      FieldKind = fkLookup
      FieldName = 'Balance'
      LookupDataSet = dmReferences.tblOrderTotals
      LookupKeyFields = 'OrderID'
      LookupResultField = 'TotalBalance'
      KeyFields = 'OrderID'
      Lookup = True
    end
    object tblPaymentLinesDescription: TStringField
      FieldKind = fkLookup
      FieldName = 'Description'
      LookupDataSet = dmReferences.tblOrders
      LookupKeyFields = 'OrderID'
      LookupResultField = 'Description'
      KeyFields = 'OrderID'
      Size = 80
      Lookup = True
    end
  end
  object dsPaymentLines: TDataSource
    DataSet = tblPaymentLines
    OnStateChange = dsPaymentLinesStateChange
    Left = 312
    Top = 192
  end
  object tblOrders: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'Orders'
    IndexName = 'ByCustomer'
    MasterFields = 'CustomerID'
    MasterSource = dsData
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 163
    Top = 208
  end
end
