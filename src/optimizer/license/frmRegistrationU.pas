unit frmRegistrationU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxGroupBox,
  StdCtrls, cxTextEdit;

type
  TfrmRegistration = class(TForm)
    cxGroupBox1: TcxGroupBox;
    btnRegister: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edMachineID: TcxTextEdit;
    Label3: TLabel;
    edRegEdit: TcxTextEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    class function DoRegister(AOwner: TComponent): boolean;
  end;

var
  frmRegistration: TfrmRegistration;

implementation
uses
  WinlicenseSDK;

{$R *.dfm}

class function TfrmRegistration.DoRegister(AOwner: TComponent): boolean;
var
  ARegKey: array[0..1024] of char;  
begin
  with TfrmRegistration.Create(AOwner) do
  try
    Result := ShowModal = mrOk;
    if Result then    
      StrCopy(ARegKey, PAnsiChar(edRegEdit.Text));
  finally
    Free;
  end;
  if Result then
  begin
    WLRegNormalKeyInstallToRegistry(ARegKey);
    MessageBox(0, 'Registration key is correct. Please, restart this application to finish the registration process', 'Success', MB_OK);
  end;
end;

procedure TfrmRegistration.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ARegKey: array[0..1024] of char;
begin
  if ModalResult = mrOk then
  begin
    StrCopy(ARegKey, PAnsiChar(edRegEdit.Text));
    if not WLRegNormalKeyCheck(ARegKey) then
    begin
      ModalResult := mrNone;
      Action := caNone;
      MessageBox(0, 'The key that you entered is invalid. Please, try it again', 'Error', MB_OK or MB_ICONERROR)
    end;
  end;
end;

procedure TfrmRegistration.FormCreate(Sender: TObject);
var
  AMachineID: array[0..100] of char;
begin
  if WLHardwareGetID(AMachineID) then
    edMachineID.Text := AMachineID;
end;

end.
