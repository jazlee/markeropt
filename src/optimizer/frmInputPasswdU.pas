unit frmInputPasswdU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, cxControls, cxContainer, cxEdit, cxTextEdit,
  DB, ACRMain, ACRTypes, CSPConsts;

type
  TfrmInputPassword = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    edPassword: TcxTextEdit;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCount: integer;
    FDatabase: TACRDatabase;
    FRetryCount: integer;
  public
    constructor Create(AOwner: TComponent); override;

    property RetryCount: integer read FRetryCount write FRetryCount;

    class function VerifyPassword(ADatabase: TACRDatabase): boolean;
  end;

var
  frmInputPassword: TfrmInputPassword;

implementation
uses
  gnugettext;

{$R *.dfm}

{ TfrmInputPassword }

constructor TfrmInputPassword.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCount := 0;
  FRetryCount := 3;
  FDatabase := TACRDatabase.Create(Self);
  FDatabase.DatabaseName := 'VERIFYDB';
end;

procedure TfrmInputPassword.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    try
      FDatabase.CryptoParams.Password := edPassword.Text;
      FDatabase.Open;
      FDatabase.Close;
    except
      if FCount < FRetryCount then
      begin
        Action := Forms.caNone;
        raise Exception.Create(_(SERRInvalidPassword));
      end else
        ModalResult := mrCancel;                  
    end;
  end;  
end;

class function TfrmInputPassword.VerifyPassword(
  ADatabase: TACRDatabase): boolean;
begin
  Result := True;
  if ADatabase.IsDatabaseEncrypted then
  begin
    with TfrmInputPassword.Create(Application.MainForm) do
    try
      FDatabase.DatabaseFileName := ADatabase.DatabaseFileName;
      if ShowModal = mrOk then
        ADatabase.CryptoParams.Password :=
          FDatabase.CryptoParams.Password
      else
        Result := False;
    finally
      Free;
    end;
  end;  
end;

end.
