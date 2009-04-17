unit dmMain;

interface

uses
  SysUtils, Classes, nxtcCOMTransport, nxtnNamedPipeTransport,
  nxllTransport, nxptBasePooledTransport, nxtwWinsockTransport,
  nxsrSqlEngineBase, nxsqlEngine, nxreRemoteServerEngine, nxllComponent,
  nxsdServerEngine, nxsrServerEngine, nxseAllEngines, nxseAutoComponent;


type
  TNxServer = class(TDataModule)
    nxServerEngine: TnxServerEngine;
    nxRemoteServerEngine: TnxRemoteServerEngine;
    nxSqlEngine: TnxSqlEngine;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NxServer: TNxServer;

implementation

{$R *.dfm}

end.
