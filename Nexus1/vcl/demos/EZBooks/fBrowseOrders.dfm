inherited frmBrowseOrders: TfrmBrowseOrders
  Left = 334
  Width = 641
  Caption = 'Browse Orders'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited ToolBar: TToolBar
    Width = 633
  end
  inherited StatusBar: TStatusBar
    Width = 633
  end
  inherited grdBrowse: TDBGrid
    Width = 633
    Columns = <
      item
        Expanded = False
        FieldName = 'OrderID'
        Width = 55
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DateCreated'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Width = 121
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Customer'
        Width = 97
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Status_'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PayOffDate'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TaxType'
        Visible = False
      end
      item
        Expanded = False
        FieldName = 'SalesRep'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Tax'
        Visible = False
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
        Value = 'orders'
      end
      item
        Name = 'where'
      end
      item
        Name = 'orderby'
      end>
    object qryBrowseOrderID: TIntegerField
      FieldName = 'OrderID'
      Required = True
    end
    object qryBrowseDateCreated: TDateField
      DisplayLabel = 'Created'
      FieldName = 'DateCreated'
      Origin = 'orders.DateCreated'
      EditMask = '00/00/00'
    end
    object qryBrowseCustomer: TStringField
      FieldKind = fkLookup
      FieldName = 'Customer'
      LookupDataSet = dmReferences.tblCustomers
      LookupKeyFields = 'CustomerID'
      LookupResultField = 'CompanyName'
      KeyFields = 'CustomerID'
      Size = 80
      Lookup = True
    end
    object qryBrowseCustomerID: TIntegerField
      DisplayLabel = 'Customer #'
      FieldName = 'CustomerID'
      Origin = 'orders.CustomerID'
      Required = True
    end
    object qryBrowseDescription: TStringField
      FieldName = 'Description'
      Origin = 'orders.Description'
      Size = 80
    end
    object qryBrowseStatus: TSmallintField
      DefaultExpression = '0'
      FieldName = 'Status'
      Origin = 'orders.Status'
      Required = True
    end
    object qryBrowseNotes: TMemoField
      FieldName = 'Notes'
      Origin = 'orders.Notes'
      BlobType = ftMemo
    end
    object qryBrowsePayOffDate: TDateField
      DisplayLabel = 'Pay off by'
      FieldName = 'PayOffDate'
      Origin = 'orders.PayOffDate'
    end
    object qryBrowseTaxType: TStringField
      DisplayLabel = 'Type of tax'
      FieldName = 'TaxType'
      Origin = 'orders.TaxType'
      Required = True
      Size = 5
    end
    object qryBrowseSalesRep: TStringField
      DisplayLabel = 'Sales Rep.'
      FieldName = 'SalesRep'
      Origin = 'orders.SalesRep'
    end
    object qryBrowseTax: TFloatField
      FieldName = 'Tax'
      Origin = 'orders.Tax'
    end
    object qryBrowseStatus_: TStringField
      DisplayLabel = 'Status'
      FieldKind = fkLookup
      FieldName = 'Status_'
      LookupDataSet = dmReferences.enOrderStatuses
      LookupKeyFields = 'Key'
      LookupResultField = 'Value'
      KeyFields = 'Status'
      Size = 10
      Lookup = True
    end
    object qryBrowseTotalTax: TCurrencyField
      FieldName = 'TotalTax'
      Required = True
    end
    object qryBrowseTotalSales: TCurrencyField
      FieldName = 'TotalSales'
      Required = True
    end
    object qryBrowseTotalCharges: TCurrencyField
      FieldName = 'TotalCharges'
      Required = True
    end
  end
end
