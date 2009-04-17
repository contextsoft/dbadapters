object frmBDEOpenDatabase: TfrmBDEOpenDatabase
  Left = 454
  Top = 276
  BorderStyle = bsDialog
  Caption = 'Open BDE Database'
  ClientHeight = 248
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 12
    Top = 15
    Width = 325
    Height = 180
    Shape = bsFrame
  end
  object Label3: TLabel
    Left = 24
    Top = 100
    Width = 89
    Height = 13
    Caption = 'Alias/Driver Name:'
  end
  object Label1: TLabel
    Left = 24
    Top = 32
    Width = 84
    Height = 13
    Caption = 'Connection Type:'
  end
  object cbxAliasDriver: TComboBox
    Left = 122
    Top = 98
    Width = 199
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cbxAliasDriverChange
  end
  object rgConnectionType: TRadioGroup
    Left = 122
    Top = 25
    Width = 199
    Height = 57
    ItemIndex = 0
    Items.Strings = (
      'BDE Alias'
      'BDE/ODBC Driver')
    TabOrder = 0
    OnClick = rgConnectionTypeClick
  end
  object pnlPath: TPanel
    Left = 15
    Top = 124
    Width = 315
    Height = 59
    BevelOuter = bvNone
    TabOrder = 2
    object lblPathDSN: TLabel
      Left = 8
      Top = 14
      Width = 86
      Height = 13
      Caption = 'Path/ODBC DSN:'
    end
    object edtPath: TEdit
      Left = 107
      Top = 10
      Width = 199
      Height = 21
      TabOrder = 1
    end
    object btnBrowse: TButton
      Left = 231
      Top = 36
      Width = 75
      Height = 23
      Caption = '&Browse...'
      TabOrder = 2
      OnClick = btnBrowseClick
    end
    object cbxODBC: TComboBox
      Left = 107
      Top = 10
      Width = 200
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 173
    Top = 209
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 257
    Top = 209
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'All Files (*.*)|*.*|MS Access (*.mdb)|*.mdb|Interbase (*.gdb)|*.' +
      'gdb'
    Left = 28
    Top = 192
  end
end
