unit frmProgressU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxContainer, cxEdit, cxProgressBar, StdCtrls;

type
  TfrmProgress = class(TForm)
    DBProgress: TcxProgressBar;
    TBProgress: TcxProgressBar;
    lbCaption: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    mResult : integer;
    EWindowList:pointer;
    EActiveWindow:HWND;
  public
    function MessageHook(var Msg: TMessage): Boolean;
    procedure SetIndicator(v:integer);
  end;

var
  frmProgress: TfrmProgress;

implementation

{$R *.dfm}

procedure TfrmProgress.FormClose(Sender: TObject; var Action: TCloseAction);
var time: Cardinal;
begin
 if (mResult = mrCancel) then
  begin
   action := caNone;
   mResult := mrOk;
  end
 else
  begin
   DBProgress.Position := 100;
   time := GetTickCount;
   while GetTickCount - time < 500 do
    Application.ProcessMessages;
   Application.UnhookMainWindow(MessageHook);
   EnableTaskWindows(EWindowList);
   SetActiveWindow(EActiveWindow);
  end;
end;

function TfrmProgress.MessageHook(var Msg: TMessage): Boolean;
begin
  Result := False;
end;

procedure TfrmProgress.SetIndicator(v: integer);
begin
  DBProgress.Position := v;
end;

end.
