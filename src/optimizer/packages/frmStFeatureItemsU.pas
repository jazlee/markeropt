unit frmStFeatureItemsU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  ecfutils, cxSpinEdit;

type
  TfrmStFeatureItems = class(TForm)
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
    cxDBTextEdit4: TcxDBTextEdit;
    Label8: TLabel;
    cxDBTextEdit7: TcxDBTextEdit;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    Label6: TLabel;
    cxDBSpinEdit2: TcxDBSpinEdit;
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
    procedure DeleteItems(const ATableName: string);

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure SetFilterByParam;
    function GetLookupValues(const AFields: string): Variant;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class procedure DoBrowse(const AFields: string; const AValues: Variant); overload;
    class function DoBrowse(const AFields, AParamFields: string; const AValues: Variant): Variant; overload;
  end;

var
  frmStFeatureItems: TfrmStFeatureItems;

implementation

uses
  Strutils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, frmUoMU, 
  dmPackageU, frmStMaterialU;

{$R *.dfm}

{ TfrmStFeatureItems }

procedure TfrmStFeatureItems.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmStFeatureItems.actDeleteExecute(Sender: TObject);
begin
  if (DataSource.DataSet.IsEmpty) or
     (MessageDlg(SConfirmDelCancel, mtConfirmation, mbYesNo, 0) <> mrYes) then
    exit;
  if DataSource.DataSet.State = dsInsert then
  begin
    DataSource.DataSet.Cancel;
    Close;
    exit;
  end
  else if DataSource.DataSet.State = dsEdit then
    DataSource.DataSet.Cancel;
  DeleteItems('stmaterials');
  DeleteItems('strules');
  DataSource.DataSet.Delete;
end;

procedure TfrmStFeatureItems.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmStFeatureItems.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
      RPT_STFTITEM_ITEM_ID,
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').AsWideString,
        DataSource.DataSet.FieldByName('ftname').AsWideString,
        DataSource.DataSet.FieldByName('fitname').AsWideString
      ])
    ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStFeatureItems.actReff_1Execute(Sender: TObject);

  function ConstructFilters: string;
  var
    i: integer;
    AStr: string;
  begin
    Result := '';
    for i := 0 to FParams.Count - 1 do
    begin
      AStr := Format('(%s = %s)',[FParams[I].Name, QuotedStr(FParams[I].AsString)]);
      Result := IfThen(Result = '', AStr, Result + ' and '+ AStr);
    end;
  end;
  
var
  ADataset: TDataSet;
begin
  ADataset := dmPackage.GetACRTable('stfeature','pk_stfeature');
  try
    ADataset.Filter := ConstructFilters;
    ADataset.Filtered := True;
    ADataset.Open;
    if ADataset.FieldByName('ftgrp').Value <> 1 then
      raise Exception.Create(SERRNotYFeature);
  finally
    ADataset.Free;
  end;
  TfrmStMaterial.DoBrowse('stname;ftname;fitname',
    VarArrayOf(
      [
        DataSource.DataSet.FieldValues['stname'],
        DataSource.DataSet.FieldValues['ftname'],
        DataSource.DataSet.FieldValues['fitname']
      ]
    )
  );
end;

procedure TfrmStFeatureItems.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    if DataSource.DataSet.FieldByName('fitname').AsString = '' then
      raise Exception.Create('Item name must not empty');
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmStFeatureItems.AssignParams;
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

constructor TfrmStFeatureItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmStFeatureItems.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(stname = %s) and (ftname = %s) '+
      ' and (fitname = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('stname').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('ftname').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('fitname').AsString)
      ]
    );
    ADataItem.Filtered := True;
    ADataItem.Open;
    while not ADataItem.IsEmpty do
      ADataItem.Delete;
  finally
    ADataItem.Close;
    ADataItem.Free;
  end;
end;

destructor TfrmStFeatureItems.Destroy;
begin
  DataSource.DataSet.Free;
  FParams.Free;
  inherited Destroy;
end;

class procedure TfrmStFeatureItems.DoBrowse;
begin
  with TfrmStFeatureItems.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'fitname;fitprio;fitprio2;fitdesc',
            'Item Name;Sequence;Priority;Description',
            'Style Feature Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class procedure TfrmStFeatureItems.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmStFeatureItems.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'fitname;fitprio;fitprio2;fitdesc',
            'Item Name;Sequence;Priority;Description',
            'Style Feature Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmStFeatureItems.DoBrowse(const AFields, AParamFields: string;
  const AValues: Variant): Variant;
begin
  with TfrmStFeatureItems.Create(Application.MainForm) do
  try
    TranslateFilter(AParamFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'fitname;fitprio;fitprio2;fitdesc',
            'Item Name;Sequence;Priority;Description',
            'Style Feature Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

procedure TfrmStFeatureItems.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('stmaterials');
    DeleteItems('strules');    
    ADataset.Delete;
  end;
end;

procedure TfrmStFeatureItems.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBTextEdit2.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStFeatureItems.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBTextEdit2.Enabled := True;
  AssignParams;
  ADataset.FieldByName('fitprio').AsInteger := 5;
  ADataset.FieldByName('fitprio2').AsInteger := 5;
  ADataset.FieldByName('avgyy').AsInteger := 0;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStFeatureItems.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
      RPT_STFTITEM_ALL_ID,
      VarArrayOf([
        ADataSet.FieldByName('stname').AsWideString,
        ADataSet.FieldByName('ftname').AsWideString
      ])
    ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStFeatureItems.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmStFeatureItems.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmStFeatureItems.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('stitmfeatures', 'idx_stitmfeatures_1');
  DataSource.DataSet.Active := True;
end;

procedure TfrmStFeatureItems.SetFilterByParam;

  function ConstructFilters: string;
  var
    i: integer;
    AStr: string;
  begin
    Result := '';
    for i := 0 to FParams.Count - 1 do
    begin
      AStr := Format('(%s = %s)',[FParams[I].Name, QuotedStr(FParams[I].AsString)]);
      Result := IfThen(Result = '', AStr, Result + ' and '+ AStr);
    end;
  end;

begin
  DataSource.DataSet.Filter := ConstructFilters;
  DataSource.DataSet.Filtered := True;
end;

procedure TfrmStFeatureItems.TranslateFilter(const AFields: string; AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
