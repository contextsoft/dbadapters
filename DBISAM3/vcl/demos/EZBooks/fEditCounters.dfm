object frmCounters: TfrmCounters
  Left = 393
  Top = 225
  BorderStyle = bsDialog
  Caption = 'Counters'
  ClientHeight = 224
  ClientWidth = 311
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 223
    Top = 0
    Width = 88
    Height = 224
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 5
      Height = 224
      Align = alLeft
      Shape = bsLeftLine
    end
    object btnOK: TBitBtn
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      Kind = bkOK
      Margin = 4
    end
    object btnCancel: TBitBtn
      Left = 8
      Top = 36
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
      Margin = 4
    end
  end
  object GroupBox1: TGroupBox
    Left = 12
    Top = 12
    Width = 197
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Next Values (Generators) '
    TabOrder = 0
    object Label1: TLabel
      Left = 20
      Top = 24
      Width = 54
      Height = 13
      Caption = 'Customer #'
      FocusControl = DBEdit1
    end
    object Label2: TLabel
      Left = 20
      Top = 44
      Width = 30
      Height = 13
      Caption = 'Item #'
      FocusControl = DBEdit2
    end
    object Label3: TLabel
      Left = 20
      Top = 72
      Width = 36
      Height = 13
      Caption = 'Order #'
      FocusControl = DBEdit3
    end
    object Label4: TLabel
      Left = 20
      Top = 120
      Width = 51
      Height = 13
      Caption = 'Payment #'
      FocusControl = DBEdit4
    end
    object Label5: TLabel
      Left = 20
      Top = 96
      Width = 59
      Height = 13
      Caption = 'Order Line #'
      FocusControl = DBEdit5
    end
    object Label6: TLabel
      Left = 20
      Top = 144
      Width = 74
      Height = 13
      Caption = 'Payment Line #'
      FocusControl = DBEdit6
    end
    object Label7: TLabel
      Left = 20
      Top = 168
      Width = 55
      Height = 13
      Caption = 'Snapshot #'
      FocusControl = DBEdit7
    end
    object DBEdit1: TDBEdit
      Left = 108
      Top = 20
      Width = 64
      Height = 21
      DataField = 'CustomerID'
      DataSource = dsData
      TabOrder = 0
    end
    object DBEdit2: TDBEdit
      Left = 108
      Top = 44
      Width = 64
      Height = 21
      DataField = 'ItemID'
      DataSource = dsData
      TabOrder = 1
    end
    object DBEdit3: TDBEdit
      Left = 108
      Top = 68
      Width = 64
      Height = 21
      DataField = 'OrderID'
      DataSource = dsData
      TabOrder = 2
    end
    object DBEdit4: TDBEdit
      Left = 108
      Top = 116
      Width = 64
      Height = 21
      DataField = 'PaymentID'
      DataSource = dsData
      TabOrder = 4
    end
    object DBEdit5: TDBEdit
      Left = 108
      Top = 92
      Width = 64
      Height = 21
      DataField = 'OrderLineID'
      DataSource = dsData
      TabOrder = 3
    end
    object DBEdit6: TDBEdit
      Left = 108
      Top = 140
      Width = 64
      Height = 21
      DataField = 'PaymentLineID'
      DataSource = dsData
      TabOrder = 5
    end
    object DBEdit7: TDBEdit
      Left = 108
      Top = 164
      Width = 64
      Height = 21
      DataField = 'SnapshotID'
      DataSource = dsData
      TabOrder = 6
    end
  end
  object tblData: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'Counters'
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableTriggers, uoEnableChangeTracking]
    Left = 236
    Top = 96
    object tblDataCustomerID: TIntegerField
      DefaultExpression = '1'
      DisplayLabel = 'Customer #'
      FieldName = 'CustomerID'
      Origin = 'Counters.CustomerID'
      Required = True
    end
    object tblDataItemID: TIntegerField
      DefaultExpression = '1'
      DisplayLabel = 'Item #'
      FieldName = 'ItemID'
      Origin = 'Counters.ItemID'
      Required = True
    end
    object tblDataOrderID: TIntegerField
      DefaultExpression = '1'
      DisplayLabel = 'Order #'
      FieldName = 'OrderID'
      Origin = 'Counters.OrderID'
      Required = True
    end
    object tblDataPaymentID: TIntegerField
      DefaultExpression = '1'
      DisplayLabel = 'Payment #'
      FieldName = 'PaymentID'
      Origin = 'Counters.PaymentID'
      Required = True
    end
    object tblDataOrderLineID: TIntegerField
      DefaultExpression = '1'
      DisplayLabel = 'Order Line #'
      FieldName = 'OrderLineID'
      Origin = 'Counters.OrderLineID'
      Required = True
    end
    object tblDataPaymentLineID: TIntegerField
      DefaultExpression = '1'
      DisplayLabel = 'Payment Line #'
      FieldName = 'PaymentLineID'
      Origin = 'Counters.PaymentLineID'
      Required = True
    end
    object tblDataSnapshotID: TIntegerField
      DefaultExpression = '1'
      DisplayLabel = 'Snapshot #'
      FieldName = 'SnapshotID'
      Origin = 'Counters.SnapshotID'
      Required = True
    end
  end
  object dsData: TDataSource
    DataSet = tblData
    Left = 250
    Top = 112
  end
end
