unit frmExtrnToolsEditU;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxContainer, cxEdit, cxLabel, cxTextEdit, cxMaskEdit,
  cxButtonEdit, Menus, cxLookAndFeelPainters, StdCtrls, cxButtons,
  ecfextrntools;

{$I CSPDefs.inc}  

type
  TfrmExtrnToolsEdit = class(TForm)
    cxLabel1: TcxLabel;
    cxTextEdit1: TcxTextEdit;
    edProgName: TcxButtonEdit;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxTextEdit2: TcxTextEdit;
    cxLabel4: TcxLabel;
    cxTextEdit3: TcxTextEdit;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    OpenDialog1: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure edProgNamePropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure btnOKClick(Sender: TObject);
  private
    FItem: TExternalToolItem;
  public
    procedure AssignItem(AItem: TExternalToolItem); 
  end;

var
  frmExtrnToolsEdit: TfrmExtrnToolsEdit;

implementation

uses ecflangutil;

{$R *.dfm}

{ TfrmExtrnToolsEdit }

procedure TfrmExtrnToolsEdit.AssignItem(AItem: TExternalToolItem);
begin
  ReconstructLanguage(Self);
  FItem := AItem;
end;

procedure TfrmExtrnToolsEdit.btnOKClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if Trim(cxTextEdit1.Text) = EmptyStr then
    raise Exception.Create('Title must be supplied');
  ModalResult := mrOk;
end;

procedure TfrmExtrnToolsEdit.edProgNamePropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  {$IFDEF DELPHI9}
  if OpenDialog1.Execute(Self.Handle) then
  {$ELSE}
  if OpenDialog1.Execute then
  {$ENDIF}
  begin
    FItem.ProgramName := OpenDialog1.FileName;
    edProgName.Text := FItem.ProgramName;
    cxTextEdit2.Text := FItem.WorkingDir;
  end;
end;

procedure TfrmExtrnToolsEdit.FormHide(Sender: TObject);
begin
  if ModalResult = mrOk then
  begin
    FItem.Title := cxTextEdit1.Text;
    FItem.ProgramName := edProgName.Text;
    FItem.WorkingDir := cxTextEdit2.Text;
    FItem.Parameters := cxTextEdit3.Text;
  end;
end;

procedure TfrmExtrnToolsEdit.FormShow(Sender: TObject);
begin
  cxTextEdit1.Text := FItem.Title;
  edProgName.Text := FItem.ProgramName;
  cxTextEdit2.Text := FItem.WorkingDir;
  cxTextEdit3.Text := FItem.Parameters;
end;

end.
