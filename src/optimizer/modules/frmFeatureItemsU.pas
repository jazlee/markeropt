unit frmFeatureItemsU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  ecfutils, cxSpinEdit;

type
  TfrmFeatureItems = class(TForm)
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
    Label3: TLabel;
    cxDBTextEdit2: TcxDBTextEdit;
    cxDBSpinEdit1: TcxDBSpinEdit;
    cxDBTextEdit3: TcxDBTextEdit;
    Label4: TLabel;
    Label5: TLabel;
    cxDBSpinEdit2: TcxDBSpinEdit;
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
  frmFeatureItems: TfrmFeatureItems;

implementation

uses
  Strutils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, frmUoMU;

{$R *.dfm}

{ TfrmFeatureItems }

procedure TfrmFeatureItems.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmFeatureItems.actDeleteExecute(Sender: TObject);
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

procedure TfrmFeatureItems.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmFeatureItems.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_FITEM_ITEM_ID,
                  VarArrayOf([
                    DataSource.DataSet.FieldByName('ftname').AsWideString,
                    DataSource.DataSet.FieldByName('fitname').AsWideString
                  ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmFeatureItems.actReff_1Execute(Sender: TObject);
begin
//
end;

procedure TfrmFeatureItems.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    if DataSource.DataSet.FieldByName('fitname').AsString = '' then
      raise Exception.Create('Feature Item name must not empty');
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmFeatureItems.AssignParams;
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

constructor TfrmFeatureItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

destructor TfrmFeatureItems.Destroy;
begin
  DataSource.DataSet.Free;
  FParams.Free;
  inherited Destroy;
end;

class procedure TfrmFeatureItems.DoBrowse;
begin
  with TfrmFeatureItems.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'fitname;fitprio;fitprio2;fitdesc',
            'Item Name;Sequence;Priority;Description',
            'Feature Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class procedure TfrmFeatureItems.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmFeatureItems.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'fitname;fitprio;fitprio2;fitdesc',
            'Item Name;Sequence;Priority;Description',
            'Feature Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmFeatureItems.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
    ADataset.Delete;
end;

procedure TfrmFeatureItems.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBTextEdit2.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmFeatureItems.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBTextEdit2.Enabled := True;
  AssignParams;
  ADataset.FieldByName('fitprio').AsInteger := 5;
  ADataset.FieldByName('fitprio2').AsInteger := 5;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmFeatureItems.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_FITEM_ALL_ID,
                  DataSource.DataSet.FieldByName('ftname').AsWideString
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmFeatureItems.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

procedure TfrmFeatureItems.InitData;
begin
  DataSource.DataSet := dmReferences.GetACRTable('itmfeatures');
  DataSource.DataSet.Active := True;
end;

procedure TfrmFeatureItems.SetFilterByParam;

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

procedure TfrmFeatureItems.TranslateFilter(const AFields: string; AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
