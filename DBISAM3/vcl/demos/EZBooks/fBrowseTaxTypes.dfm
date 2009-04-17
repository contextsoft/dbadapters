inherited frmBrowseTaxTypes: TfrmBrowseTaxTypes
  Width = 372
  Height = 361
  Caption = 'Browse Tax Types'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited ToolBar: TToolBar
    Width = 364
  end
  inherited StatusBar: TStatusBar
    Top = 314
    Width = 364
  end
  inherited grdBrowse: TDBGrid
    Width = 364
    Height = 288
    Columns = <
      item
        Expanded = False
        FieldName = 'TaxType'
        PickList.Strings = ()
        Width = 110
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Tax'
        PickList.Strings = ()
        Width = 85
        Visible = True
      end>
  end
  inherited qryBrowse: TDBISAMQueryExt
    AutoDisplayLabels = True
    AutoFieldsProperties = True
    Macros = <
      item
        Name = 'fields'
        Value = '*'
      end
      item
        Name = 'table'
        Value = 'taxtypes'
      end
      item
        Name = 'where'
      end
      item
        Name = 'orderby'
      end>
  end
end
