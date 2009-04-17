object dmReferences: TdmReferences
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 345
  Top = 182
  Height = 367
  Width = 514
  object tblTaxTypes: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
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
  object tblItems: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'Items'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 128
    Top = 24
  end
  object tblCustomers: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'Customers'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 128
    Top = 76
  end
  object tblFormOfPayments: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'FormOfPayments'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 128
    Top = 128
  end
  object tblOrderTotals: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'OrderTotals'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 44
    Top = 128
  end
  object tblOrders: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'Orders'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 199
    Top = 28
  end
  object tblCounters: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'Counters'
    AllowAutoOpen = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 200
    Top = 92
  end
end
