unit frmLogU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxClasses, dxBar, ActnList, ImgList, PCLogViewer, ComCtrls;

type
  TfrmLog = class(TForm)
    LogImage: TImageList;
    LogAction: TActionList;
    LogBarManager: TdxBarManager;
    actClear: TAction;
    dxBarButton1: TdxBarButton;
    LogBarManagerBar1: TdxBar;
    procedure actClearExecute(Sender: TObject);
  private
    FLogViewer: TPCLogViewer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConstructResources;

    procedure InternalUpdateState;
  end;

var
  frmLog: TfrmLog;

implementation

uses dmMainU, IApplication, gnugettext;

{$R *.dfm}

procedure TfrmLog.actClearExecute(Sender: TObject);
begin
  FLogViewer.Clear;
end;

procedure TfrmLog.ConstructResources;
begin
  actClear.Caption := _('Clear');
//  FLogViewer.RecreateColumns;
end;

constructor TfrmLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogViewer := TPCLogViewer.Create(Self);
  FLogViewer.Parent := Self;
  FLogViewer.Align  := alClient;
  FLogViewer.Source := dmMain.PCMemoryLog;
  FLogViewer.DoubleBuffered := True;
  FLogViewer.VisibleColumns := [lvcDate, lvcTime, lvcCategory, lvcText];
  LogBarManager.Style       := bmsUseLookAndFeel;
  FLogViewer.PopupOptions.Filter  := True;
  FLogViewer.PopupOptions.Pause   := True;
  FLogViewer.PopupOptions.Earliest:= True;
  FLogViewer.PopupOptions.Copy    := True;
  ConstructResources;
end;

destructor TfrmLog.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmLog.InternalUpdateState;
{
var
  I: integer;
}
begin
{
  for I := 0 to LogAction.ActionCount-1 do
    TAction(LogAction.Actions[i]).Enabled := dmMain.Connected;
  if not dmMain.Connected then
    FLogViewer.Clear;    
}
end;

end.
