object SQLiteOpenDatabase: TSQLiteOpenDatabase
  Left = 432
  Top = 248
  BorderStyle = bsDialog
  Caption = 'Open Database'
  ClientHeight = 83
  ClientWidth = 410
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lblDatabase: TLabel
    Left = 8
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Database:'
    FocusControl = cbxDatabases
  end
  object btnOK: TButton
    Tag = 265
    Left = 245
    Top = 53
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Tag = 266
    Left = 327
    Top = 53
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbxDatabases: TComboBox
    Tag = 263
    Left = 60
    Top = 12
    Width = 265
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object btnSelect: TButton
    Tag = 264
    Left = 327
    Top = 10
    Width = 75
    Height = 25
    Caption = '&Select...'
    TabOrder = 3
    OnClick = btnSelectClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 8
    Top = 48
  end
end
