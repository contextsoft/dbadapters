program DataSetRefDemo;

uses
  Forms,
  fMain in 'fMain.pas' {frmDataSetRef},
  DataSetRefSchema in 'DataSetRefSchema.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDataSetRef, frmDataSetRef);
  Application.Run;
end.
