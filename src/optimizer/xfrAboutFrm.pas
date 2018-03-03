unit xfrAboutFrm;

interface

{$I frx.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TxfrAboutForm = class(TForm)
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Bevel2: TBevel;
    Shape1: TShape;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses frxClass, frxUtils, frxRes, ShellApi, CSPAppUtil;

{$R *.DFM}

procedure TxfrAboutForm.FormCreate(Sender: TObject);
begin
  Caption := frxGet(2600);
  Label2.Caption := 'Version ' + FR_VERSION;
  Label10.Caption := #174;
//  Label1.Caption := GetAppIDClass.ProjectName; 
  {$IFDEF FR_LITE}
  Label1.Caption := 'FreeReport'; 
  {$ENDIF}
end;

procedure TxfrAboutForm.LabelClick(Sender: TObject);
begin
  case TLabel(Sender).Tag of
    1: ShellExecute(GetDesktopWindow, 'open',
      PChar(TLabel(Sender).Caption), nil, nil, sw_ShowNormal);
    2: ShellExecute(GetDesktopWindow, 'open',
      PChar('mailto:' + TLabel(Sender).Caption), nil, nil, sw_ShowNormal);
  end;
end;

procedure TxfrAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

end.



//<censored>
