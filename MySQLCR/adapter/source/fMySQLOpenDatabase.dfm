object frmMySQLOpenDatabase: TfrmMySQLOpenDatabase
  Left = 423
  Top = 277
  BorderStyle = bsDialog
  Caption = 'Connect to MySQL Database'
  ClientHeight = 130
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 9
    Top = 8
    Width = 272
    Height = 81
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 22
    Top = 27
    Width = 25
    Height = 13
    Caption = 'Host:'
  end
  object Label2: TLabel
    Left = 22
    Top = 58
    Width = 49
    Height = 13
    Caption = 'Database:'
  end
  object btnOK: TButton
    Left = 112
    Top = 96
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 200
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object edtHost: TEdit
    Left = 82
    Top = 21
    Width = 185
    Height = 21
    TabOrder = 0
    Text = 'localhost'
  end
  object edtDatabase: TEdit
    Left = 82
    Top = 53
    Width = 185
    Height = 21
    TabOrder = 1
  end
  object MyConnection1: TMyConnection
    Database = 'test'
    Port = 3307
    Username = 'root'
    Password = 'root'
    Server = 'localhost'
    Left = 40
    Top = 64
  end
end
