unit frmItmMaterialU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  ecfutils, cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox,
  cxRadioGroup, CSPEditUtils, cxDropDownEdit;

type
  TfrmItmMaterial = class(TForm)
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
    cxDBTextEdit3: TcxDBTextEdit;
    Label3: TLabel;
    Label5: TLabel;
    cxDBButtonEdit1: TcxDBButtonEdit;
    Label7: TLabel;
    cxDBButtonEdit2: TcxDBButtonEdit;
    cxDBTextEdit6: TcxDBTextEdit;
    Label9: TLabel;
    cxDBButtonEdit3: TcxDBButtonEdit;
    cxDBRadioGroup1: TcxDBRadioGroup;
    cxDBTextEdit8: TcxDBTextEdit;
    Label10: TLabel;
    Label8: TLabel;
    cxDBTextEdit7: TcxDBTextEdit;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    cxDBComboBox1: TcxDBComboBox;
    Label4: TLabel;
    Label6: TLabel;
    cxDBTextEdit4: TcxDBTextEdit;
    Label11: TLabel;
    cxDBTextEdit5: TcxDBTextEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure ActionListStateChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure actReff_1Execute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
  private
    FParams: TECFParams;
    FEventManager: TDatasetEventHandler;
    procedure InitData;

    function GetLookupValues(const AFields: string): Variant;
    procedure SetFilterByParam;
    procedure TranslateFilter(const AFields: string; AValues: Variant);
    procedure AssignParams;

    procedure DeleteItems(const ATableName: string);

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure DoOnMTCDChange(Sender: TObject);
    procedure DoOnLRNAMEChange(Sender: TObject);
    procedure DoOnUMCODEChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class procedure DoBrowse(const AFields: string; const AValues: Variant); overload;
    class function DoBrowse(const AFields: string): Variant; overload;
  end;

var
  frmItmMaterial: TfrmItmMaterial;

implementation

uses
  StrUtils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU, 
  frmMaterialU, frmLayingRuleU, frmUoMU, frmItmLayingRuleItemsU, gnugettext;

{$R *.dfm}

{ TfrmItmMaterial }

procedure TfrmItmMaterial.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmItmMaterial.actDeleteExecute(Sender: TObject);
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
  DeleteItems('itmrules');
  DataSource.DataSet.Delete;
end;

procedure TfrmItmMaterial.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmItmMaterial.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_MATERIAL_ITEM_ID,
                  VarArrayOf([
                      DataSource.DataSet.FieldByName('itmname').AsWideString,
                      DataSource.DataSet.FieldByName('mtcd').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmItmMaterial.actReff_1Execute(Sender: TObject);
begin
   TfrmItmLayingRuleItems.DoBrowse('itmname;mtcd',
    VarArrayOf(
      [
        DataSource.DataSet.FieldValues['itmname'],
        DataSource.DataSet.FieldValues['mtcd']
      ]
    ));
end;

procedure TfrmItmMaterial.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmItmMaterial.AssignParams;
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

constructor TfrmItmMaterial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmItmMaterial.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmMaterial.DoBrowse('mtcd;mtname;mtdesc;mtcatcd');
  DataSource.DataSet.FieldByName('mtcd').Value := AValue[0];
  DataSource.DataSet.FieldByName('mtname').Value := AValue[1];
  DataSource.DataSet.FieldByName('mtdesc').Value := AValue[2];
  DataSource.DataSet.FieldByName('mtcatcd').Value := AValue[3];
end;

procedure TfrmItmMaterial.cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmLayingRule.DoBrowse('lrname;lrdesc');
  if (DataSource.DataSet.FieldByName('lrname').AsString <> '') and
     (MessageDlg(SConfirmReplace, mtWarning, mbYesNo, 0) = mrNo) then
    exit;
  DataSource.DataSet.FieldByName('lrname').Value := AValue[0];
  DataSource.DataSet.FieldByName('lrdesc').Value := AValue[1];
end;

procedure TfrmItmMaterial.cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  cxDBButtonEdit3.DataBinding.Field.Value := TfrmUoM.DoBrowse('umcode');
end;

procedure TfrmItmMaterial.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(itmname = %s) and (mtcd = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('itmname').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('mtcd').AsString)
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

destructor TfrmItmMaterial.Destroy;
begin
  FParams.Free;
  FEventManager.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmItmMaterial.DoBrowse;
begin
  with TfrmItmMaterial.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'mtcd;mtname;mtdesc;lrname;lrdesc',
            'Code;Name;Description;Laying Rule;Laying Desc.',
            'Item Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmItmMaterial.DoBrowse(const AFields: string): Variant;
begin
  with TfrmItmMaterial.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'mtcd;mtname;mtdesc;lrname;lrdesc',
            'Code;Name;Description;Laying Rule;Laying Desc.',
            'Item Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

class procedure TfrmItmMaterial.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmItmMaterial.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'mtcd;mtname;mtdesc;lrname;lrdesc',
            'Code;Name;Description;Laying Rule;Laying Desc.',
            'Item Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmItmMaterial.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('itmrules');
    ADataset.Delete;
  end;
end;

procedure TfrmItmMaterial.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBButtonEdit1.Enabled := False;
  cxDBButtonEdit2.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmItmMaterial.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  AssignParams;
  DataSource.DataSet.FieldByName('avgyy').Value := 1;
  DataSource.DataSet.FieldByName('fbaltype').AsInteger := 0;
  DataSource.DataSet.FieldByName('fballowance').AsInteger := 0;
  DataSource.DataSet.FieldByName('mtlayout').AsString := cxDBComboBox1.Properties.Items[0];
  DataSource.DataSet.FieldByName('mtwidth').AsInteger := 0;
  cxDBButtonEdit1.Enabled := True;
  cxDBButtonEdit2.Enabled := True;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmItmMaterial.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ITMMATERIAL_ALL_ID,
                  ADataSet.FieldByName('itmname').AsWideString
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmItmMaterial.DoOnLRNAMEChange(Sender: TObject);
var
  AValue: Variant;
  ADataset, BDataset: TDataSet;
  I: integer;
  ACheck: Boolean;
begin
  ACheck := DataSource.DataSet.FieldByName('lrname').Value <> null;
  if ACheck then
  begin
    AValue := dmReferences.SelectValue('layrule','lrname', 'lrdesc',
        DataSource.DataSet.FieldByName('lrname').Value
      );
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  ADataset := dmReferences.GetACRTable('itmlayrule', 'pk_itmlayrule');
  BDataset := dmPackage.GetACRTable('itmrules','pk_itmrules');
  try
    BDataset.Filter := Format('(itmname = %s) and (mtcd = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('itmname').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('mtcd').AsString)
      ]);
    BDataset.Filtered := True;
    BDataset.Open;
    while not BDataset.IsEmpty do
      BDataset.Delete;
    BDataset.Active := False;
    if ACheck then
    begin
      ADataset.Filter := Format('(lrname = %s)',
        [
          QuotedStr(DataSource.DataSet.FieldByName('lrname').AsString)
        ]);
      ADataset.Filtered := True;
      ADataset.Open;
      DataSource.DataSet.FieldByName('lrdesc').Value := AValue;
      BDataset.Filtered := False;
      BDataset.Open;
      if not ADataset.IsEmpty then
      begin
        ADataset.First;
        while not ADataset.Eof do
        begin
          BDataset.Append;
          BDataset.FieldByName('itmname').AsString :=
            DataSource.DataSet.FieldByName('itmname').AsString;
          BDataset.FieldByName('mtcd').AsString :=
            DataSource.DataSet.FieldByName('mtcd').AsString;
          BDataset.FieldByName('minlyr').Value :=
            ADataset.FieldByName('minlyr').Value;
          BDataset.Post;
          ADataset.Next;        
        end;
      end;
    end;
  finally
    BDataset.Free;
    ADataset.Free;
  end;
end;

procedure TfrmItmMaterial.DoOnMTCDChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('mtcd').Value <> null then
  begin
    AValue := dmReferences.SelectValue('materials', 'mtcd',
        'mtcd;mtname;mtdesc;mtcatcd',
        DataSource.DataSet.FieldByName('mtcd').Value
      );
    if (not VarIsArray(AValue)) and (AValue = null) then
      raise Exception.Create(_(SERREntryHasNoReference))
    else if VarIsArray(AValue) then
    begin
      DataSource.DataSet.FieldByName('mtname').Value := AValue[1];
      DataSource.DataSet.FieldByName('mtdesc').Value := AValue[2];
      DataSource.DataSet.FieldByName('mtcatcd').Value := AValue[3];            
    end;
  end;
end;

procedure TfrmItmMaterial.DoOnUMCODEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('umcode').Value <> null then
  begin
    AValue := dmReferences.SelectValue('umcodes', 'umcode', 'umcode',
        DataSource.DataSet.FieldByName('umcode').Value
      );
    if (AValue = null) then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
end;

procedure TfrmItmMaterial.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmItmMaterial.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmItmMaterial.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('itmmaterial');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'mtcd', DoOnMTCDChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'lrname', DoOnLRNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'umcode', DoOnUMCODEChange);
end;

procedure TfrmItmMaterial.SetFilterByParam;
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

procedure TfrmItmMaterial.TranslateFilter(const AFields: string;
  AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
