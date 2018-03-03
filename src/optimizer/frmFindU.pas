unit frmFindU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxPC, cxControls, cxGraphics, cxMaskEdit, cxDropDownEdit,
  cxDBEdit, cxContainer, cxEdit, cxTextEdit;

type
  TfrmFind = class(TForm)
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    btnOK: TButton;
    btnCancel: TButton;
    edFindText: TcxTextEdit;
    Label1: TLabel;
    Label2: TLabel;
    edFields: TcxComboBox;
  private
    { Private declarations }
  public
    class function ShowFindDialog(Sender: TComponent;
                                  var ATextFind: string;
                                  var AFieldIndex: integer;
                                  AFields: TStrings): boolean;
  end;

var
  frmFind: TfrmFind;

implementation

{$R *.dfm}

{ TfrmFind }

class function TfrmFind.ShowFindDialog(Sender: TComponent;
  var ATextFind: string; var AFieldIndex: integer; AFields: TStrings): boolean;
begin
  Result := False;
  with TfrmFind.Create(Sender) do
  try
    edFindText.Text := ATextFind;
    edFields.Properties.Items.Assign(AFields);
    if AFields.Count > 0 then
      edFields.ItemIndex := 0;
    Result := ShowModal = mrOk;
    ATextFind := edFindText.Text;
    AFieldIndex := edFields.ItemIndex;
  finally
    Free;
  end;
end;

end.
