unit frmStyleTypeU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit;

type
  TfrmStyleType = class(TForm)
    ActionList: TActionList;
    actNew: TAction;
    actSave: TAction;
    actDelete: TAction;
    actClose: TAction;
    BarManager: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    stdBtnNew: TdxBarLargeButton;
    stdBtnOpen: TdxBarLargeButton;
    stdBtnDelete: TdxBarLargeButton;
    stdBtnClose: TdxBarLargeButton;
    StatusBar: TdxStatusBar;
    cxTabControl1: TcxTabControl;
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    actReff_1: TAction;
    stdBtnReff_1: TdxBarLargeButton;
    DataSource: TDataSource;
    cxDBTextEdit1: TcxDBTextEdit;
    Label1: TLabel;
    cxDBTextEdit2: TcxDBTextEdit;
    Label2: TLabel;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure ActionListStateChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
  private
    procedure InitData;

    function GetLookupValues(const AFields: string): Variant;

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class function DoBrowse(const AFields: string): Variant; overload;
  end;

var
  frmStyleType: TfrmStyleType;

implementation

uses
  ecfutils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU;

{$R *.dfm}

{ TfrmStyleType }

procedure TfrmStyleType.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmStyleType.actDeleteExecute(Sender: TObject);
begin
  if MessageDlg(SConfirmDelCancel, mtConfirmation, mbYesNo, 0) <> mrYes then
    exit;
  if DataSource.DataSet.State = dsInsert then
  begin
    DataSource.DataSet.Cancel;
    Close;
    exit;
  end
  else if DataSource.DataSet.State = dsEdit then
    DataSource.DataSet.Cancel;
  DataSource.DataSet.Delete;
end;

procedure TfrmStyleType.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmStyleType.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_STTYPE_ITEM_ID,
                  DataSource.DataSet.FieldByName('sttname').AsWideString
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStyleType.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

constructor TfrmStyleType.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitData;
  Position := poMainFormCenter;
end;

destructor TfrmStyleType.Destroy;
begin
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmStyleType.DoBrowse;
begin
  with TfrmStyleType.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'sttname;sttdesc',
            'Name;Description',
            'Type of Styles',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmStyleType.DoBrowse(const AFields: string): Variant;
begin
  with TfrmStyleType.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'sttname;sttdesc',
            'Name;Description',
            'Type of Styles',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

procedure TfrmStyleType.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    ADataset.Delete;
  end;
end;

procedure TfrmStyleType.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBTextEdit1.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStyleType.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBTextEdit1.Enabled := True;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStyleType.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := RPT_STTYPE_ALL_ID;
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStyleType.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmStyleType.GetLookupValues(const AFields: string): Variant;
var
  ACount, APos, I: integer;
  AField: string;
begin
  if Pos(';', AFields) > 0 then
  begin
    ACount := FieldCount(AFields);
    APos := 1;
    I := 0;
    Result := VarArrayCreate([0, ACount], varVariant);
    while APos <= Length(AFields) do
    begin
      AField := ExtractFieldName(AFields, APos);
      Result[I] := DataSource.DataSet.FieldValues[AField];
      Inc(I); 
    end;
  end else
    Result := DataSource.DataSet.FieldValues[AFields];
end;

procedure TfrmStyleType.InitData;
begin
  DataSource.DataSet := dmReferences.GetACRTable('styletypes');
  DataSource.DataSet.Active := True;
end;

end.
