unit frmLayingRuleItemsU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  ecfutils, cxSpinEdit;

type
  TfrmLayingRuleItems = class(TForm)
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
    Label2: TLabel;
    cxDBSpinEdit1: TcxDBSpinEdit;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure ActionListStateChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actReff_1Execute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
  private
    FParams: TECFParams;

    procedure AssignParams;
    procedure InitData;
    procedure TranslateFilter(const AFields: string; AValues: Variant);

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure SetFilterByParam;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class procedure DoBrowse(const AFields: string; const AValues: Variant); overload;
  end;

var
  frmLayingRuleItems: TfrmLayingRuleItems;

implementation

uses
  Strutils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, frmUoMU;

{$R *.dfm}

{ TfrmLayingRuleItems }

procedure TfrmLayingRuleItems.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmLayingRuleItems.actDeleteExecute(Sender: TObject);
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

procedure TfrmLayingRuleItems.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmLayingRuleItems.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_LRITEM_ITEM_ID,
                  VarArrayOf([
                    DataSource.DataSet.FieldByName('lrname').AsWideString,
                    DataSource.DataSet.FieldByName('minlyr').AsInteger
                  ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmLayingRuleItems.actReff_1Execute(Sender: TObject);
begin
//
end;

procedure TfrmLayingRuleItems.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    if DataSource.DataSet.FieldByName('lrname').AsString = '' then
      raise Exception.Create('Laying rule name must not empty');
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmLayingRuleItems.AssignParams;
var
  I: integer;
  AField: TField;
begin
  for I := 0 to FParams.Count - 1 do
  begin
    AField := DataSource.DataSet.FindField(FParams[i].Name);
    if AField <> nil then
      AField.Value := FParams[i].Value;
  end;
end;

constructor TfrmLayingRuleItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

destructor TfrmLayingRuleItems.Destroy;
begin
  DataSource.DataSet.Free;
  FParams.Free;
  inherited Destroy;
end;

class procedure TfrmLayingRuleItems.DoBrowse;
begin
  with TfrmLayingRuleItems.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'minlyr',
            'Minimum Layer',
            'Laying Rule Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class procedure TfrmLayingRuleItems.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmLayingRuleItems.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'minlyr',
            'Minimum Layer',
            'Laying Rule Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete,
            DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmLayingRuleItems.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
    ADataset.Delete;
end;

procedure TfrmLayingRuleItems.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmLayingRuleItems.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  AssignParams;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmLayingRuleItems.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_LRITEM_ALL_ID,
                  ADataset.FieldByName('lrname').AsWideString
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmLayingRuleItems.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

procedure TfrmLayingRuleItems.InitData;
begin
  DataSource.DataSet := dmReferences.GetACRTable('itmlayrule', 'pk_itmlayrule');
  DataSource.DataSet.Active := True;
end;

procedure TfrmLayingRuleItems.SetFilterByParam;

  function ConstructFilters: string;
  var
    i: integer;
    AStr: string;
  begin
    Result := '';
    for i := 0 to FParams.Count - 1 do
    begin
      AStr := Format('(%s = %s)',[FParams[I].Name, QuotedStr(FParams[I].AsString)]);
      Result := IfThen(Result = '', AStr, Result + 'and '+ AStr);
    end;
  end;

begin
  DataSource.DataSet.Filter := ConstructFilters;
  DataSource.DataSet.Filtered := True;
end;

procedure TfrmLayingRuleItems.TranslateFilter(const AFields: string; AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
