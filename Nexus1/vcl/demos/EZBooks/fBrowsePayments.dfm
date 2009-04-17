inherited frmBrowsePayments: TfrmBrowsePayments
  Width = 527
  Caption = 'Browse Payments'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited ToolBar: TToolBar
    Width = 519
  end
  inherited StatusBar: TStatusBar
    Width = 519
  end
  inherited grdBrowse: TDBGrid
    Width = 519
    Columns = <
      item
        Expanded = False
        FieldName = 'PaymentID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Customer'
        Width = 127
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DateCreated'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FOP'
        Width = 86
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Reference'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TotalAmount'
        Visible = True
      end>
  end
  inherited qryBrowse: TnxQueryExt
    AutoFieldsProperties = True
    Macros = <
      item
        Name = 'fields'
        Value = '*'
      end
      item
        Name = 'table'
        Value = 'payments'
      end
      item
        Name = 'where'
      end
      item
        Name = 'orderby'
      end>
    object qryBrowsePaymentID: TIntegerField
      FieldName = 'PaymentID'
      Origin = 'payments.PaymentID'
      Required = True
    end
    object qryBrowseCustomerID: TIntegerField
      DisplayLabel = 'Customer #'
      FieldName = 'CustomerID'
      Origin = 'payments.CustomerID'
      Required = True
    end
    object qryBrowseDateCreated: TDateField
      DisplayLabel = 'Created'
      FieldName = 'DateCreated'
      Origin = 'payments.DateCreated'
      EditMask = '00/00/00'
    end
    object qryBrowseFormOfPayment: TStringField
      DisplayLabel = 'Form of payment'
      FieldName = 'FormOfPayment'
      Origin = 'payments.FormOfPayment'
      Required = True
      Size = 10
    end
    object qryBrowseReference: TStringField
      FieldName = 'Reference'
      Origin = 'payments.Reference'
    end
    object qryBrowseTotalAmount: TCurrencyField
      DefaultExpression = '0'
      DisplayLabel = 'Total Amount'
      FieldName = 'TotalAmount'
      Origin = 'payments.TotalAmount'
      Required = True
    end
    object qryBrowseCustomer: TStringField
      FieldKind = fkLookup
      FieldName = 'Customer'
      LookupDataSet = dmReferences.tblCustomers
      LookupKeyFields = 'CustomerID'
      LookupResultField = 'CompanyName'
      KeyFields = 'CustomerID'
      Size = 40
      Lookup = True
    end
    object qryBrowseFOP: TStringField
      DisplayLabel = 'Form of payment'
      FieldKind = fkLookup
      FieldName = 'FOP'
      LookupDataSet = dmReferences.tblFormOfPayments
      LookupKeyFields = 'FormOfPayment'
      LookupResultField = 'Description'
      KeyFields = 'FormOfPayment'
      Size = 10
      Lookup = True
    end
    object qryBrowseCreditedToAccount: TCurrencyField
      FieldName = 'CreditedToAccount'
      Origin = 'payments.CreditedToAccount'
      Required = True
    end
    object qryBrowseAppliedToOrders: TCurrencyField
      FieldName = 'AppliedToOrders'
      Origin = 'payments.AppliedToOrders'
      Required = True
    end
  end
end
