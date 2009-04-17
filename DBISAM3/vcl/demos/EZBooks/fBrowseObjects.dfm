object frmBrowseObjects: TfrmBrowseObjects
  Left = 315
  Top = 177
  Width = 705
  Height = 409
  Caption = 'Browse Objects'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grdBrowse: TDBGrid
    Left = 0
    Top = 0
    Width = 697
    Height = 382
    Align = alClient
    DataSource = dsObjects
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    PopupMenu = popBrowse
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ObjectType'
        Width = 111
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ObjectKey'
        Width = 55
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ReplicationID'
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SnapshotID'
        Width = 61
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Timestamp'
        Width = 124
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ChangeType'
        Width = 69
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ChangeStatus'
        Width = 73
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'UserName'
        Width = 78
        Visible = True
      end>
  end
  object tblObjects: TDBISAMTableExt
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBMAIN'
    SessionName = 'Main'
    EngineVersion = '3.24'
    TableName = 'Objects'
    AllowAutoOpen = True
    AutoFieldsProperties = True
    UpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking]
    Left = 204
    Top = 108
    object tblObjectsObjectType: TStringField
      DisplayLabel = 'Object Type'
      FieldName = 'ObjectType'
      Size = 32
    end
    object tblObjectsObjectKey: TStringField
      DisplayLabel = 'Object Key'
      FieldName = 'ObjectKey'
      Size = 64
    end
    object tblObjectsReplicationID: TIntegerField
      FieldName = 'ReplicationID'
    end
    object tblObjectsSnapshotID: TIntegerField
      FieldName = 'SnapshotID'
    end
    object tblObjectsTimestamp: TDateTimeField
      FieldName = 'Timestamp'
    end
    object tblObjectsChangeType: TIntegerField
      Alignment = taLeftJustify
      DisplayLabel = 'Change Type'
      FieldName = 'ChangeType'
      OnGetText = tblObjectsChangeTypeGetText
    end
    object tblObjectsChangeStatus: TIntegerField
      Alignment = taLeftJustify
      DisplayLabel = 'Change Status'
      FieldName = 'ChangeStatus'
      OnGetText = tblObjectsChangeStatusGetText
    end
    object tblObjectsUserName: TStringField
      DisplayLabel = 'User Name'
      FieldName = 'UserName'
      Size = 32
    end
    object tblObjectsObjectData: TBlobField
      FieldName = 'ObjectData'
      BlobType = ftBlob
    end
  end
  object dsObjects: TDataSource
    DataSet = tblObjects
    Left = 224
    Top = 124
  end
  object popBrowse: TPopupMenu
    Left = 132
    Top = 152
    object popRefresh: TMenuItem
      Caption = '&Refresh'
      ShortCut = 116
      OnClick = popRefreshClick
    end
  end
end
