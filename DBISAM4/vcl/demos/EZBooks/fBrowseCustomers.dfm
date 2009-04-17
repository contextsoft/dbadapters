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
    Top = 348
    Width = 598
  end
  inherited grdBrowse: TDBGrid
    Width = 598
    Height = 322
    Columns = <
      item
        Expanded = False
        FieldName = 'CustomerID'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CompanyName'
        PickList.Strings = ()
        Width = 115
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        PickList.Strings = ()
        Width = 72
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstName'
        PickList.Strings = ()
        Width = 79
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StreetNo'
        PickList.Strings = ()
        Width = 49
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Street'
        PickList.Strings = ()
        Width = 76
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'City'
        PickList.Strings = ()
        Width = 65
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'State'
        PickList.Strings = ()
        Width = 38
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Zip'
        PickList.Strings = ()
        Width = 37
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'HomePhone'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'WorkPhone'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'MobilePhone'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Fax'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TaxType'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CareOf'
        PickList.Strings = ()
        Width = 54
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Initial'
        PickList.Strings = ()
        Visible = True
      end>
  end
  inherited qryBrowse: TDBISAMQueryExt
    AutoDisplayLabels = True
    AutoFieldsProperties = True
  end
end
