object NxServer: TNxServer
  OldCreateOrder = False
  Left = 380
  Top = 320
  Height = 265
  Width = 343
  object nxServerEngine: TnxServerEngine
    ActiveRuntime = True
    SqlEngine = nxSqlEngine
    Options = []
    Left = 32
    Top = 28
  end
  object nxRemoteServerEngine: TnxRemoteServerEngine
    Left = 140
    Top = 24
  end
  object nxSqlEngine: TnxSqlEngine
    ActiveRuntime = True
    Left = 28
    Top = 84
  end
end
