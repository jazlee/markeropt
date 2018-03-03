unit xfrFViewerFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, frxSynMemo, StdCtrls;

type
  TxfrFileViewerForm = class(TForm)
    Panel1: TPanel;
    CodePanel: TPanel;
    StatusBar1: TStatusBar;
    btnCancel: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FCodeWindow: TfrxSyntaxMemo;
  public
    property CodeWindow: TfrxSyntaxMemo read FCodeWindow;

    function Execute: boolean;
  end;

implementation

{$R *.dfm}

function TxfrFileViewerForm.Execute: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TxfrFileViewerForm.FormCreate(Sender: TObject);
begin
  FCodeWindow := TfrxSyntaxMemo.Create(Self);
  with FCodeWindow do
  begin
    Parent := CodePanel;
    Align := alClient;
    Color := clWindow;
  end;
end;

end.
