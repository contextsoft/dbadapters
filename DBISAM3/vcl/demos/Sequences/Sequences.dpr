program Sequences;

uses
  Forms,
  fMain in 'fMain.pas' {frmSequencesDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSequencesDemo, frmSequencesDemo);
  Application.Run;
end.
