object frmOpenDBXDatabase: TfrmOpenDBXDatabase
  Left = 333
  Top = 327
  BorderStyle = bsDialog
  Caption = 'Configure dbExpress Connection'
  ClientHeight = 157
  ClientWidth = 378
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
    Left = 8
    Top = 12
    Width = 361
    Height = 97
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 20
    Top = 28
    Width = 62
    Height = 13
    Caption = 'Driver Name:'
  end
  object Label2: TLabel
    Left = 20
    Top = 52
    Width = 56
    Height = 13
    Caption = 'Host Name:'
  end
  object Label3: TLabel
    Left = 20
    Top = 76
    Width = 80
    Height = 13
    Caption = 'Database Name:'
  end
  object cbxDriverName: TComboBox
    Left = 104
    Top = 24
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 212
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 292
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object edtHostName: TEdit
    Left = 104
    Top = 48
    Width = 161
    Height = 21
    TabOrder = 1
  end
  object edtDatabaseName: TEdit
    Left = 104
    Top = 72
    Width = 249
    Height = 21
    TabOrder = 2
  end
  object SQLConnection1: TSQLConnection
    DriverName = 'MySQL'
    GetDriverFunc = 'getSQLDriverMYSQL'
    LibraryName = 'dbexpmysql.dll'
    LoadParamsOnConnect = True
    LoginPrompt = False
    Params.Strings = (
      'HostName=ServerName'
      'Database=DBNAME'
      'User_Name=user'
      'Password=password'
      'BlobSize=-1'
      'ErrorResourceFile='
      'LocaleCode=0000')
    VendorLib = 'libmysql.dll'
    Left = 56
    Top = 96
  end
end
