program EZBooks;

uses
  Forms,
  fMain in 'fMain.pas' {frmEZBooksMain},
  fAbout in 'fAbout.pas' {frmAboutBox},
  fBrowse in 'fBrowse.pas' {frmBrowse},
  fEdit in 'fEdit.pas' {frmEditObject},
  fBrowseCustomers in 'fBrowseCustomers.pas' {frmBrowseCustomers},
  fEditCustomers in 'fEditCustomers.pas' {frmEditCustomers},
  fBrowseTaxTypes in 'fBrowseTaxTypes.pas' {frmBrowseTaxTypes},
  fEditTaxTypes in 'fEditTaxTypes.pas' {frmEditTaxTypes},
  fEditFormOfPayments in 'fEditFormOfPayments.pas' {frmEditFormOfPayments},
  fBrowseFormOfPayments in 'fBrowseFormOfPayments.pas' {frmBrowseFormOfPayments},
  fEditItems in 'fEditItems.pas' {frmEditItem},
  fBrowseItems in 'fBrowseItems.pas' {frmBrowseItems},
  fEditOrders in 'fEditOrders.pas' {frmEditOrders},
  dReferences in 'dReferences.pas' {dmReferences: TDataModule},
  fBrowseOrders in 'fBrowseOrders.pas' {frmBrowseOrders},
  fEditPayments in 'fEditPayments.pas' {frmEditPayment},
  fBrowsePayments in 'fBrowsePayments.pas' {frmBrowsePayments},
  fEditCompanyInfo in 'fEditCompanyInfo.pas' {frmEditCompanyInfo},
  fEditCounters in 'fEditCounters.pas' {frmCounters},
  fBrowseObjects in 'fBrowseObjects.pas' {frmBrowseObjects},
  EZBooksDecl in 'EZBooksDecl.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'EZ Books';
  Application.CreateForm(TfrmEZBooksMain, frmEZBooksMain);
  Application.CreateForm(TdmReferences, dmReferences);
  Application.Run;
end.
