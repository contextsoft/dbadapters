object EDBOpenDatabase: TEDBOpenDatabase
  Left = 408
  Top = 348
  ActiveControl = cbxAvailableDatabases
  BorderStyle = bsDialog
  Caption = 'Open Database'
  ClientHeight = 194
  ClientWidth = 410
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Tag = 265
    Left = 243
    Top = 161
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Tag = 266
    Left = 325
    Top = 161
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object gbxConnectionType: TGroupBox
    Left = 7
    Top = 8
    Width = 393
    Height = 57
    Caption = 'Connection Type'
    TabOrder = 0
    object rbLocal: TRadioButton
      Tag = 261
      Left = 16
      Top = 24
      Width = 153
      Height = 17
      Caption = 'Direct Access (File Sharing)'
      TabOrder = 0
      OnClick = ConnectionTypeChange
    end
    object rbRemote: TRadioButton
      Tag = 262
      Left = 185
      Top = 25
      Width = 91
      Height = 17
      Caption = 'Client/Server'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = ConnectionTypeChange
    end
    object cbxRemoteType: TComboBox
      Left = 280
      Top = 22
      Width = 92
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'LAN'
        'Internet')
    end
  end
  object gbxAvailableDatabases: TGroupBox
    Left = 7
    Top = 71
    Width = 393
    Height = 78
    Caption = 'Available Databases'
    TabOrder = 1
    object lblServer: TLabel
      Left = 18
      Top = 23
      Width = 34
      Height = 13
      Caption = 'Server:'
    end
    object cbxAvailableDatabases: TComboBox
      Tag = 263
      Left = 17
      Top = 48
      Width = 278
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbxAvailableDatabasesChange
      OnDropDown = cbxServerChange
    end
    object btnSelect: TButton
      Tag = 264
      Left = 303
      Top = 46
      Width = 75
      Height = 25
      Caption = '&Select...'
      TabOrder = 2
      OnClick = btnSelectClick
    end
    object cbxServer: TComboBox
      Left = 61
      Top = 19
      Width = 234
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnExit = cbxServerChange
    end
  end
end
