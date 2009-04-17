inherited frmBrowseItems: TfrmBrowseItems
  Caption = 'Browse Items'
  PixelsPerInch = 96
  TextHeight = 13
  inherited grdBrowse: TDBGrid
    Columns = <
      item
        Expanded = False
        FieldName = 'ItemID'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        PickList.Strings = ()
        Width = 271
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SalePrice'
        PickList.Strings = ()
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Taxable'
        PickList.Strings = ()
        Visible = True
      end>
  end
  inherited qryBrowse: TDBISAMQueryExt
    Macros = <
      item
        Name = 'fields'
        Value = '*'
      end
      item
        Name = 'table'
        Value = 'items'
      end
      item
        Name = 'where'
      end
      item
        Name = 'orderby'
      end>
  end
end
