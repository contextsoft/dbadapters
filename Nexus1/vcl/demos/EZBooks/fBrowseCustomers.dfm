inherited frmBrowseCustomers: TfrmBrowseCustomers
  Left = 327
  Top = 198
  Width = 606
  Height = 395
  Caption = 'Browse Customers'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited ToolBar: TToolBar
    Width = 598
  end
  inherited StatusBar: TStatusBar
    Top = 345
    Width = 598
  end
  inherited grdBrowse: TDBGrid
    Width = 598
    Height = 319
    Columns = <
      item
        Expanded = False
        FieldName = 'CustomerID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CompanyName'
        Width = 115
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        Width = 72
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstName'
        Width = 79
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StreetNo'
        Width = 49
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Street'
        Width = 76
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'City'
        Width = 65
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'State'
        Width = 38
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Zip'
        Width = 37
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
      end
      item
        Expanded = False
        FieldName = 'CareOf'
        Width = 54
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Initial'
        Visible = True
      end>
  end
  inherited qryBrowse: TnxQueryExt
    AutoFieldsProperties = True
  end
end
