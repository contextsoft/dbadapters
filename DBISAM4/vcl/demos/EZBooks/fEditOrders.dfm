inherited frmEditOrders: TfrmEditOrders
  Left = 292
  Top = 201
  Width = 533
  Height = 482
  BorderStyle = bsSizeable
  Caption = 'Edit Order'
  OldCreateOrder = True
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 12
    Top = 8
    Width = 36
    Height = 13
    Caption = 'Order #'
    FocusControl = DBEdit1
  end
  object Label2: TLabel [1]
    Left = 88
    Top = 8
    Width = 23
    Height = 13
    Caption = 'Date'
    FocusControl = DBEdit2
  end
  object Label3: TLabel [2]
    Left = 164
    Top = 8
    Width = 44
    Height = 13
    Caption = 'Customer'
    FocusControl = DBEdit3
  end
  object Label4: TLabel [3]
    Left = 88
    Top = 48
    Width = 30
    Height = 13
    Caption = 'Status'
  end
  object Label6: TLabel [4]
    Left = 12
    Top = 48
    Width = 47
    Height = 13
    Caption = 'Pay off by'
    FocusControl = DBEdit5
  end
  object Label7: TLabel [5]
    Left = 236
    Top = 357
    Width = 21
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Tax:'
  end
  object Label8: TLabel [6]
    Left = 256
    Top = 48
    Width = 52
    Height = 13
    Caption = 'Sales Rep.'
    FocusControl = DBEdit7
  end
  object Label5: TLabel [7]
    Left = 236
    Top = 381
    Width = 69
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Total Charges:'
  end
  object Label9: TLabel [8]
    Left = 236
    Top = 405
    Width = 76
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Total Payments:'
  end
  object Label10: TLabel [9]
    Left = 236
    Top = 429
    Width = 84
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Total Balance:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label11: TLabel [10]
    Left = 12
    Top = 357
    Width = 66
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Special Notes'
  end
  object Label12: TLabel [11]
    Left = 12
    Top = 88
    Width = 56
    Height = 13
    Caption = 'Description:'
    FocusControl = DBEdit5
  end
  object PageControl: TPageControl [12]
    Left = 11
    Top = 132
    Width = 415
    Height = 214
    ActivePage = tsDetails
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 8
    object tsDetails: TTabSheet
      Caption = 'Order Details'
      object Panel2: TPanel
        Left = 0
        Top = 164
        Width = 407
        Height = 22
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object DBNavigator1: TDBNavigator
          Left = 0
          Top = 1
          Width = 200
          Height = 21
          DataSource = dsOrderLines
          Flat = True
          TabOrder = 0
        end
      end
      object grdOrderLines: TDBGrid
        Left = 0
        Top = 0
        Width = 407
        Height = 164
        Align = alClient
        DataSource = dsOrderLines
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Description'
            Width = 152
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'SalePrice'
            Width = 71
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Quantity'
            Width = 45
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Charges'
            Width = 54
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Taxable'
            Visible = True
          end>
      end
    end
    object tsPayments: TTabSheet
      Caption = 'Payments'
      ImageIndex = 1
      object grdPayments: TDBGrid
        Left = 0
        Top = 0
        Width = 407
        Height = 186
        Hint = 'Double click the record to view payment details'
        Align = alClient
        DataSource = dsPaymentLines
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = grdPaymentsDblClick
        Columns = <
          item
            Expanded = False
            FieldName = 'PaymentDate'
            Width = 66
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'FormOfPayment'
            Width = 100
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Reference'
            Width = 115
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Amount'
            Width = 74
            Visible = True
          end>
      end
    end
  end
  inherited Panel1: TPanel
    Left = 437
    Height = 448
    TabOrder = 15
    inherited Bevel1: TBevel
      Height = 448
    end
  end
  object DBEdit1: TDBEdit [14]
    Left = 12
    Top = 24
    Width = 69
    Height = 21
    DataField = 'OrderID'
    DataSource = dsData
    TabOrder = 0
  end
  object DBEdit2: TDBEdit [15]
    Left = 88
    Top = 24
    Width = 69
    Height = 21
    DataField = 'DateCreated'
    DataSource = dsData
    TabOrder = 1
  end
  object DBEdit3: TDBEdit [16]
    Left = 164
    Top = 24
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DataField = 'Customer'
    DataSource = dsData
    TabOrder = 2
  end
  object DBEdit5: TDBEdit [17]
    Left = 12
    Top = 64
    Width = 69
    Height = 21
    DataField = 'PayOffDate'
    DataSource = dsData
    TabOrder = 4
  end
  object DBEdit7: TDBEdit [18]
    Left = 256
    Top = 64
    Width = 168
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DataField = 'SalesRep'
    DataSource = dsData
    TabOrder = 6
  end
  object btnSelectCustomer: TBitBtn [19]
    Left = 352
    Top = 23
    Width = 72
    Height = 22
    Hint = 'Select existing or enter a new customer'
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
  object DBEdit6: TDBEdit [20]
    Left = 332
    Top = 353
    Width = 88
    Height = 21
    Anchors = [akRight, akBottom]
    DataField = 'TotalTax'
    DataSource = dsData
    ReadOnly = True
    TabOrder = 11
  end
  object DBEdit9: TDBEdit [21]
    Left = 332
    Top = 377
    Width = 88
    Height = 21
    Anchors = [akRight, akBottom]
    DataField = 'TotalCharges'
    DataSource = dsData
    ReadOnly = True
    TabOrder = 12
  end
  object DBEdit10: TDBEdit [22]
    Left = 332
    Top = 401
    Width = 88
    Height = 21
    Anchors = [akRight, akBottom]
    DataField = 'PaymentLookup'
    DataSource = dsData
    ReadOnly = True
    TabOrder = 13
  end
  object DBEdit11: TDBEdit [23]
    Left = 332
    Top = 425
    Width = 88
    Height = 21
    Anchors = [akRight, akBottom]
    DataField = 'TotalBalance'
    DataSource = dsData
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 14
  end
  object DBMemo1: TDBMemo [24]
    Left = 12
    Top = 375
    Width = 213
    Height = 68
    Anchors = [akLeft, akRight, akBottom]
    DataField = 'Notes'
    DataSource = dsData
    TabOrder = 9
  end
  object DBLookupComboBox2: TDBLookupComboBox [25]
    Left = 88
    Top = 64
    Width = 161
    Height = 21
    DataField = 'OrderStatus'
    DataSource = dsData
    TabOrder = 5
  end
  object DBLookupComboBox1: TDBLookupComboBox [26]
    Left = 264
    Top = 353
    Width = 65
    Height = 21
    Anchors = [akRight, akBottom]
    DataField = 'TaxTypeLookup'
    DataSource = dsData
    TabOrder = 10
  end
  object DBEdit4: TDBEdit [27]
    Left = 12
    Top = 104
    Width = 412
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DataField = 'Description'
    DataSource = dsData
    TabOrder = 7
  end
  inherited tblData: TDBISAMTableExt
    OnCalcFields = tblDataCalcFields
    TableName = 'Orders'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 348
    Top = 172
    object tblDataOrderID: TIntegerField
      FieldName = 'OrderID'
      Required = True
    end
    object tblDataDateCreated: TDateField
      DisplayLabel = 'Created'
      FieldName = 'DateCreated'
      Origin = 'Orders.DateCreated'
      EditMask = '00/00/00'
    end
    object tblDataCustomerID: TIntegerField
      DisplayLabel = 'Customer #'
      FieldName = 'CustomerID'
      Origin = 'Orders.CustomerID'
      Required = True
    end
    object tblDataDescription: TStringField
      FieldName = 'Description'
      Size = 80
    end
    object tblDataStatus: TSmallintField
      FieldName = 'Status'
      Origin = 'Orders.Status'
      Required = True
    end
    object tblDataNotes: TMemoField
      FieldName = 'Notes'
      Origin = 'Orders.Notes'
      BlobType = ftMemo
    end
    object tblDataPayOffDate: TDateField
      DisplayLabel = 'Pay off by'
      FieldName = 'PayOffDate'
      Origin = 'Orders.PayOffDate'
    end
    object tblDataSalesRep: TStringField
      DisplayLabel = 'Sales Rep.'
      FieldName = 'SalesRep'
      Origin = 'Orders.SalesRep'
    end
    object tblDataTax: TFloatField
      FieldName = 'Tax'
      Origin = 'Orders.Tax'
    end
    object tblDataTaxType: TStringField
      FieldName = 'TaxType'
      Required = True
      Size = 10
    end
    object tblDataOrderStatus: TStringField
      FieldKind = fkLookup
      FieldName = 'OrderStatus'
      LookupDataSet = dmReferences.enOrderStatuses
      LookupKeyFields = 'Key'
      LookupResultField = 'Value'
      KeyFields = 'Status'
      Lookup = True
    end
    object tblDataTaxTypeLookup: TStringField
      FieldKind = fkLookup
      FieldName = 'TaxTypeLookup'
      LookupDataSet = dmReferences.tblTaxTypes
      LookupKeyFields = 'TaxType'
      LookupResultField = 'TaxType'
      KeyFields = 'TaxType'
      Lookup = True
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
    object tblDataTotalTax: TCurrencyField
      FieldName = 'TotalTax'
      Required = True
    end
    object tblDataTotalSales: TCurrencyField
      FieldName = 'TotalSales'
      Required = True
    end
    object tblDataTotalCharges: TCurrencyField
      FieldName = 'TotalCharges'
      Required = True
    end
    object tblDataTotalBalance: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'TotalBalance'
      Calculated = True
    end
    object tblDataTaxLookup: TFloatField
      FieldKind = fkLookup
      FieldName = 'TaxLookup'
      LookupDataSet = dmReferences.tblTaxTypes
      LookupKeyFields = 'TaxType'
      LookupResultField = 'Tax'
      KeyFields = 'TaxType'
      Lookup = True
    end
    object tblDataPaymentLookup: TCurrencyField
      FieldKind = fkLookup
      FieldName = 'PaymentLookup'
      LookupDataSet = dmReferences.tblOrderTotals
      LookupKeyFields = 'OrderID'
      LookupResultField = 'TotalPayment'
      KeyFields = 'OrderID'
      Lookup = True
    end
  end
  inherited dsData: TDataSource
    OnDataChange = dsDataDataChange
    Left = 362
    Top = 184
  end
  object tblOrderLines: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    BeforeInsert = LockMaster
    BeforeEdit = LockMaster
    BeforePost = tblOrderLinesBeforePost
    BeforeDelete = LockMaster
    OnCalcFields = tblOrderLinesCalcFields
    DatabaseName = 'Memory'
    SessionName = 'Default'
    EngineVersion = '4.25 Build 3'
    FieldDefs = <
      item
        Name = 'OrderLineID'
        Attributes = [faRequired]
        DataType = ftInteger
        CharCase = fcNoChange
        Compression = 0
      end
      item
        Name = 'OrderID'
        DataType = ftInteger
        CharCase = fcNoChange
        Compression = 0
      end
      item
        Name = 'ItemID'
        Attributes = [faRequired]
        DataType = ftInteger
        CharCase = fcNoChange
        Compression = 0
      end
      item
        Name = 'Description'
        DataType = ftString
        Size = 100
        CharCase = fcNoChange
        Compression = 0
      end
      item
        Name = 'SalePrice'
        Attributes = [faRequired]
        DataType = ftCurrency
        CharCase = fcNoChange
        Compression = 0
      end
      item
        Name = 'Quantity'
        Attributes = [faRequired]
        DataType = ftFloat
        CharCase = fcNoChange
        Compression = 0
      end
      item
        Name = 'Taxable'
        Attributes = [faRequired]
        DataType = ftBoolean
        CharCase = fcNoChange
        Compression = 0
      end
      item
        Name = 'Charges'
        Attributes = [faRequired]
        DataType = ftCurrency
        CharCase = fcNoChange
        Compression = 0
      end>
    IndexDefs = <
      item
        Name = 'tblOrderLinesIndex1'
        Fields = 'OrderLineID'
        Compression = icNone
      end>
    TableName = 'OrderLines'
    StoreDefs = True
    AllowAutoOpen = True
    Temporary = True
    AutoFieldsProperties = True
    UpdateOptions = []
    Left = 279
    Top = 172
    object tblOrderLinesOrderLineID: TIntegerField
      FieldName = 'OrderLineID'
    end
    object tblOrderLinesOrderID: TIntegerField
      DisplayLabel = 'Order #'
      FieldName = 'OrderID'
      Origin = 'OrderLines.OrderID'
    end
    object tblOrderLinesItemID: TIntegerField
      DisplayLabel = 'Item #'
      FieldName = 'ItemID'
      Origin = 'OrderLines.ItemID'
      Required = True
    end
    object tblOrderLinesDescription: TStringField
      FieldKind = fkLookup
      FieldName = 'Description'
      LookupDataSet = dmReferences.tblItems
      LookupKeyFields = 'ItemID'
      LookupResultField = 'Description'
      KeyFields = 'ItemID'
      Origin = 'OrderLines.Description'
      Size = 100
      Lookup = True
    end
    object tblOrderLinesSalePrice: TCurrencyField
      FieldName = 'SalePrice'
      Origin = 'OrderLines.SalePrice'
      Required = True
    end
    object tblOrderLinesQuantity: TFloatField
      DefaultExpression = '1'
      FieldName = 'Quantity'
      Origin = 'OrderLines.Quantity'
      Required = True
    end
    object tblOrderLinesTaxable: TBooleanField
      DefaultExpression = 'True'
      FieldName = 'Taxable'
      Origin = 'OrderLines.Taxable'
      Required = True
    end
    object tblOrderLinesCharges: TCurrencyField
      FieldName = 'Charges'
      Origin = 'OrderLines.Charges'
      Required = True
    end
    object tblOrderLinesSalePriceLookup: TCurrencyField
      FieldKind = fkLookup
      FieldName = 'SalePriceLookup'
      LookupDataSet = dmReferences.tblItems
      LookupKeyFields = 'ItemID'
      LookupResultField = 'SalePrice'
      KeyFields = 'ItemID'
      Lookup = True
    end
    object tblOrderLinesTaxableLookup: TBooleanField
      FieldKind = fkLookup
      FieldName = 'TaxableLookup'
      LookupDataSet = dmReferences.tblItems
      LookupKeyFields = 'ItemID'
      LookupResultField = 'Taxable'
      KeyFields = 'ItemID'
      Lookup = True
    end
    object tblOrderLinesTaxableSale: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'TaxableSale'
      Calculated = True
    end
    object tblOrderLinesTax: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'Tax'
      Calculated = True
    end
  end
  object tblPaymentLines: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '4.25 Build 3'
    IndexFieldNames = 'OrderID'
    MasterFields = 'OrderID'
    MasterSource = dsData
    TableName = 'PaymentLines'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 279
    Top = 232
    object tblPaymentLinesPaymentLineID: TIntegerField
      FieldName = 'PaymentLineID'
      Required = True
    end
    object tblPaymentLinesPaymentID: TIntegerField
      DisplayLabel = 'Payment #'
      FieldName = 'PaymentID'
      Origin = 'PaymentLines.PaymentID'
      Required = True
    end
    object tblPaymentLinesOrderID: TIntegerField
      DisplayLabel = 'Order #'
      FieldName = 'OrderID'
      Origin = 'PaymentLines.OrderID'
      Required = True
    end
    object tblPaymentLinesAmount: TCurrencyField
      FieldName = 'Amount'
      Origin = 'PaymentLines.Amount'
      Required = True
    end
    object tblPaymentLinesPaymentDate: TDateField
      DisplayLabel = 'Date'
      FieldKind = fkLookup
      FieldName = 'PaymentDate'
      LookupDataSet = tblPayments
      LookupKeyFields = 'PaymentID'
      LookupResultField = 'DateCreated'
      KeyFields = 'PaymentID'
      Lookup = True
    end
    object tblPaymentLinesFormOfPayment: TStringField
      DisplayLabel = 'Form Of Payment'
      FieldKind = fkLookup
      FieldName = 'FormOfPayment'
      LookupDataSet = tblPayments
      LookupKeyFields = 'PaymentID'
      LookupResultField = 'FormOfPayment'
      KeyFields = 'PaymentID'
      Lookup = True
    end
    object tblPaymentLinesReference: TStringField
      FieldKind = fkLookup
      FieldName = 'Reference'
      LookupDataSet = tblPayments
      LookupKeyFields = 'PaymentID'
      LookupResultField = 'Reference'
      KeyFields = 'PaymentID'
      Lookup = True
    end
  end
  object dsOrderLines: TDataSource
    DataSet = tblOrderLines
    OnStateChange = dsOrderLinesStateChange
    OnDataChange = dsOrderLinesDataChange
    Left = 288
    Top = 184
  end
  object dsPaymentLines: TDataSource
    DataSet = tblPaymentLines
    Left = 292
    Top = 244
  end
  object tblPayments: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '4.25 Build 3'
    TableName = 'Payments'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 355
    Top = 236
    object tblPaymentsPaymentID: TIntegerField
      FieldName = 'PaymentID'
      Required = True
    end
    object tblPaymentsCustomerID: TIntegerField
      DisplayLabel = 'Customer #'
      FieldName = 'CustomerID'
      Origin = 'Payments.CustomerID'
      Required = True
    end
    object tblPaymentsDateCreated: TDateField
      DisplayLabel = 'Created'
      FieldName = 'DateCreated'
      Origin = 'Payments.DateCreated'
    end
    object tblPaymentsFormOfPayment: TStringField
      DisplayLabel = 'Form of payment'
      FieldName = 'FormOfPayment'
      Origin = 'Payments.FormOfPayment'
      Required = True
      Size = 10
    end
    object tblPaymentsReference: TStringField
      FieldName = 'Reference'
      Origin = 'Payments.Reference'
    end
    object tblPaymentsTotalAmount: TCurrencyField
      DisplayLabel = 'Total Amount'
      FieldName = 'TotalAmount'
      Origin = 'Payments.TotalAmount'
      Required = True
    end
    object tblPaymentsCreditedToAccount: TCurrencyField
      FieldName = 'CreditedToAccount'
      Required = True
    end
    object tblPaymentsAppliedToOrders: TCurrencyField
      FieldName = 'AppliedToOrders'
      Required = True
    end
  end
  object docOrder: TDBDocument
    DocumentType = 'Order'
    DatabaseName = 'DBMAIN'
    MasterTable = tblData
    DataSets = <
      item
        Name = 'OrderLines'
        TableName = 'OrderLines'
        DocumentIDField = 'OrderID'
        ItemIDField = 'OrderLineID'
        DataSet = tblOrderLines
        DataSets = <>
      end>
    Left = 199
    Top = 172
  end
end
