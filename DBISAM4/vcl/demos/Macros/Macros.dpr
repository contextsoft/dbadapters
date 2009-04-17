program Macros;

uses
  Forms,
  fMain in 'fMain.pas' {frmMacorsDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMacorsDemo, frmMacorsDemo);
  Application.Run;
end.
