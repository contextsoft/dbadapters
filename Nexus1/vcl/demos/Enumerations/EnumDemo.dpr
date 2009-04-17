program EnumDemo;

uses
  Forms,
  fMain in 'fMain.pas' {frmEnumDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmEnumDemo, frmEnumDemo);
  Application.Run;
end.
