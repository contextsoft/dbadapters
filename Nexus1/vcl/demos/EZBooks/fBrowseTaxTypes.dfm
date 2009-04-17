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
    Top = 311
    Width = 364
  end
  inherited grdBrowse: TDBGrid
    Width = 364
    Height = 285
    Columns = <
      item
        Expanded = False
        FieldName = 'TaxType'
        Width = 110
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Tax'
        Width = 85
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
