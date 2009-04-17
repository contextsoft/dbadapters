inherited frmBrowseItems: TfrmBrowseItems
  Caption = 'Browse Items'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited grdBrowse: TDBGrid
    Columns = <
      item
        Expanded = False
        FieldName = 'ItemID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Width = 271
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SalePrice'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Taxable'
        Visible = True
      end>
  end
  inherited qryBrowse: TnxQueryExt
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
