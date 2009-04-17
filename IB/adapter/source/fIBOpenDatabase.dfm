object frmIBOpenDatabase: TfrmIBOpenDatabase
  Left = 279
  Top = 396
  BorderStyle = bsDialog
  Caption = 'Edit Database Connection'
  ClientHeight = 190
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 9
    Top = 9
    Width = 385
    Height = 128
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 45
    Width = 34
    Height = 13
    Caption = 'Server:'
  end
  object Label2: TLabel
    Left = 192
    Top = 43
    Width = 42
    Height = 13
    Caption = 'Protocol:'
  end
  object Label3: TLabel
    Left = 16
    Top = 77
    Width = 49
    Height = 13
    Caption = 'Database:'
  end
  object rbLocal: TRadioButton
    Left = 72
    Top = 16
    Width = 57
    Height = 17
    Caption = '&Local'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = ConnectionTypeChanged
  end
  object rbRemote: TRadioButton
    Left = 136
    Top = 16
    Width = 65
    Height = 17
    Caption = '&Remote'
    TabOrder = 1
    OnClick = ConnectionTypeChanged
  end
  object edtServer: TEdit
    Left = 72
    Top = 40
    Width = 105
    Height = 21
    TabOrder = 2
  end
  object cbxProtocol: TComboBox
    Left = 240
    Top = 40
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Text = 'TCP'
    Items.Strings = (
      'TCP'
      'NamedPipes'
      'SPX')
  end
  object edtDatabase: TEdit
    Left = 72
    Top = 72
    Width = 313
    Height = 21
    TabOrder = 4
  end
  object btnBrowse: TButton
    Left = 312
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Browse...'
    TabOrder = 5
    OnClick = btnBrowseClick
  end
  object btnOk: TButton
    Left = 216
    Top = 152
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object btnCancel: TButton
    Left = 304
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Firebird Database (*.fdb)|*.fdb|InterBase Database (*.gdb)|*.gdb' +
      '|All Files (*.*)|*.*'
    Left = 112
    Top = 112
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 'e:\mydab'
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 56
    Top = 128
  end
end
