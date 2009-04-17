program ClientRI;

uses
  Forms,
  fMain in 'fMain.pas' {frmMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
