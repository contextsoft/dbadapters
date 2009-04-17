program DBManipulation;

uses
  Forms,
  fMain in 'fMain.pas' {frmMainForm},
  Database in 'Database.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
