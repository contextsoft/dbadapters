object dmReferences: TdmReferences
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 345
  Top = 182
  Height = 367
  Width = 514
  object tblTaxTypes: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'TaxTypes'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 44
    Top = 24
  end
  object enOrderStatuses: TDBSchemaEnum
    Active = True
    Enumeration = 'OrderStatuses'
    Schema = frmEZBooksMain.dsMain
    Left = 44
    Top = 76
  end
  object tblItems: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'Items'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 128
    Top = 24
  end
  object tblCustomers: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'Customers'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 128
    Top = 76
  end
  object tblFormOfPayments: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'FormOfPayments'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 128
    Top = 128
  end
  object tblOrderTotals: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'OrderTotals'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 44
    Top = 128
  end
  object tblOrders: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'Orders'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 199
    Top = 28
  end
  object tblCounters: TnxTableExt
    Database = frmEZBooksMain.dbMain
    TableName = 'Counters'
    AllowAutoOpen = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 200
    Top = 92
  end
end
