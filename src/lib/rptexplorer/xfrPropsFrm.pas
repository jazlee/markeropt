unit xfrPropsFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, xfrRptExplr, Mask, cxControls,
  cxContainer, cxEdit, cxTextEdit, Menus, cxLookAndFeelPainters, cxButtons;

type
  TfrxExplorerPropForm = class(TForm)
    Panel1: TPanel;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Image1: TImage;
    Bevel1: TBevel;
    edFileName: TcxTextEdit;
    Label1: TLabel;
    edRptID: TcxTextEdit;
    Label2: TLabel;
    edType: TEdit;
    Label4: TLabel;
    edLocation: TEdit;
    Label5: TLabel;
    edSize: TEdit;
    Label6: TLabel;
    edDate: TEdit;
    Bevel4: TBevel;
    ckReadonly: TCheckBox;
    Label7: TLabel;
    ckHidden: TCheckBox;
    ckSystem: TCheckBox;
    edTag: TcxTextEdit;
    edDesc: TcxTextEdit;
    Label3: TLabel;
    Label8: TLabel;
    btnValidate: TcxButton;
    procedure btnValidateClick(Sender: TObject);
  private
    FExplorer: TxfrReportExplorer;
    FItem: TxfrAbstractItem;
    procedure SetExplorer(const Value: TxfrReportExplorer);
    { Private declarations }
  public
    procedure UpdateState;
    property ReportExplorer: TxfrReportExplorer read FExplorer write SetExplorer;
    property Item: TxfrAbstractItem read FItem write FItem;
  end;

implementation

{$R *.dfm}

type
  TxfrReportExplorerHack = class(TxfrReportExplorer);
{ TfrxExplorerPropForm }

procedure TfrxExplorerPropForm.SetExplorer(
  const Value: TxfrReportExplorer);
begin
  FExplorer := Value;
  if FExplorer <> nil then
    UpdateState;
end;

procedure TfrxExplorerPropForm.UpdateState;
begin
  btnValidate.Enabled := not (ReportExplorer.TagAuthenticated);
  edTag.Enabled := ReportExplorer.TagAuthenticated;
end;

procedure TfrxExplorerPropForm.btnValidateClick(Sender: TObject);
begin
  if FItem is TxfrReportItem then
    TxfrReportExplorerHack(FExplorer).ValidateReport(TxfrReportItem(FItem), etTagAuth);
  TxfrReportExplorerHack(ReportExplorer).Authenticate;
  UpdateState;
end;

end.
