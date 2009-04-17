object frmAboutBox: TfrmAboutBox
  Left = 422
  Top = 234
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 213
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 32
      Top = 24
      Width = 32
      Height = 32
      AutoSize = True
      Center = True
      Picture.Data = {
        055449636F6E0000010001002020100000000000E80200001600000028000000
        2000000040000000010004000000000080020000000000000000000000000000
        0000000000000000000080000080000000808000800000008000800080800000
        80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF0000000000000000000000000000000000000000000000000000000000
        000000000000000B3B3B3B33330000BB3B3000000000B3B3B3B3333333030BB3
        3033000000033330000000000030BB0B3B3330000003333B8B8383333033BBBB
        3333300000003333333333330B3B3BB33B3B3B0000000B3B3B3333330B3B33B3
        BBB3330000B3B3B3B3B333330B3B333BBB333B0003333000000000000B3B33BB
        BBB3B3000300000B3B3B33330B3B333BB0B33B000030B3B3B3B3B3330B3B333B
        BBB3B30003033333333333330B33333BBB3B3B000003000B888383830BB33333
        333BB000000033330000000000B33B3333BB300000033000B3B3B3B3B0BB3333
        0BBB000000000B3B3B3B3B3B3B0BB33B3BB00000000033333333333333300B33
        3300000000003000BBB838383830003000000000000003333380000000000000
        0000000000003338000B3B3B3B3B3B000000000000000330B3B3B3B3B3B3B3B3
        3000000000000003333FFFFFF33333333300000000000003088BBBB3B3B3B300
        030000000000000033333BBBBB3B3B33300000000000000333B3B3BBBBB3B3B3
        3300000000000000333B3BBBBBBB333330000000000000000003B3B3BFFFFB00
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000FFFFFFFFFE003C1FF000000FE0000007C0000003C0000003C0000001
        C0000001800000010000000100000001000000010000000180000003C0000003
        C0000007E000000FE000001FE00000FFE00000FFE000007FF000003FF800001F
        FC00001FFC00001FFC00001FFE00003FFF00007FFFE003FFFFFFFFFFFFFFFFFF
        FFFFFFFF}
      Stretch = True
      IsControl = True
    end
    object ProductName: TLabel
      Left = 108
      Top = 20
      Width = 92
      Height = 13
      Caption = 'EZ Books Demo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Version: TLabel
      Left = 108
      Top = 44
      Width = 53
      Height = 13
      Caption = 'Version 1.0'
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 80
      Width = 177
      Height = 13
      Caption = 'Copyright (c) 2004, Michael Baytalsky'
      IsControl = True
    end
    object Comments: TLabel
      Left = 8
      Top = 104
      Width = 254
      Height = 39
      Caption = 
        'All rights reserved.'#13#10'This program is part of Database Ex' +
        'tensions Suite (Nexus).'
      WordWrap = True
      IsControl = True
    end
  end
  object OKButton: TButton
    Left = 111
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
