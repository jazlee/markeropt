unit frmStMaterialU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  ecfutils, CSPEditUtils, cxDropDownEdit;

type
  TfrmStMaterial = class(TForm)
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
    Label4: TLabel;
    cxDBTextEdit4: TcxDBTextEdit;
    cxDBTextEdit5: TcxDBTextEdit;
    Label5: TLabel;
    Label6: TLabel;
    cxDBButtonEdit1: TcxDBButtonEdit;
    Label7: TLabel;
    cxDBButtonEdit2: TcxDBButtonEdit;
    cxDBTextEdit6: TcxDBTextEdit;
    Label9: TLabel;
    cxDBButtonEdit3: TcxDBButtonEdit;
    cxDBRadioGroup1: TcxDBRadioGroup;
    Label10: TLabel;
    cxDBTextEdit8: TcxDBTextEdit;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    Label8: TLabel;
    cxDBComboBox1: TcxDBComboBox;
    Label11: TLabel;
    cxDBTextEdit7: TcxDBTextEdit;
    Label12: TLabel;
    cxDBTextEdit9: TcxDBTextEdit;
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
  end;

var
  frmStMaterial: TfrmStMaterial;

implementation

uses
  StrUtils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU, 
  frmUoMU, frmMaterialU, frmLayingRuleU, frmStLayingRuleItemsU, gnugettext;

{$R *.dfm}

{ TfrmStMaterial }

procedure TfrmStMaterial.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmStMaterial.actDeleteExecute(Sender: TObject);
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
  DeleteItems('strules');
  DataSource.DataSet.Delete;
end;

procedure TfrmStMaterial.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmStMaterial.actReff_1Execute(Sender: TObject);
begin
  TfrmStLayingRuleItems.DoBrowse('stname;ftname;fitname;mtcd',
    VarArrayOf([
      DataSource.DataSet.FieldValues['stname'],
      DataSource.DataSet.FieldValues['ftname'],
      DataSource.DataSet.FieldValues['fitname'],
      DataSource.DataSet.FieldValues['mtcd']
    ])
  );
end;

procedure TfrmStMaterial.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmStMaterial.AssignParams;
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

constructor TfrmStMaterial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventManager := TDatasetEventHandler.Create(Self);
  FParams := TECFParams.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmStMaterial.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
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

procedure TfrmStMaterial.cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
  ADataset, BDataset: TDataSet;
  I: integer;
begin
  AValue := TfrmLayingRule.DoBrowse('lrname;lrdesc');
  if (DataSource.DataSet.FieldByName('lrname').AsString <> '') and
     (MessageDlg(SConfirmReplace, mtWarning, mbYesNo, 0) = mrNo) then
    exit;
  DataSource.DataSet.FieldByName('lrname').Value := AValue[0];
  DataSource.DataSet.FieldByName('lrdesc').Value := AValue[1];
end;

procedure TfrmStMaterial.cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  cxDBButtonEdit3.DataBinding.Field.Value := TfrmUoM.DoBrowse('umcode');
end;

procedure TfrmStMaterial.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(stname = %s) and (ftname = %s) '+
      ' and (fitname = %s) and (mtcd = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('stname').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('ftname').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('fitname').AsString),
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

destructor TfrmStMaterial.Destroy;
begin
  FEventManager.Free;
  FParams.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmStMaterial.DoBrowse;
begin
  with TfrmStMaterial.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'mtcd;mtname;mtdesc',
            'Code;Name;Description',
            'Style Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class procedure TfrmStMaterial.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmStMaterial.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'mtcd;mtname;mtdesc',
            'Code;Name;Description',
            'Style Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmStMaterial.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('strules');    
    ADataset.Delete;
  end;
end;

procedure TfrmStMaterial.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBButtonEdit1.Enabled := False;
  cxDBButtonEdit2.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStMaterial.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBButtonEdit1.Enabled := True;
  cxDBButtonEdit2.Enabled := True;
  AssignParams;
  ADataset.FieldValues['fbaltype'] := 0;
  ADataset.FieldValues['fballowance'] := 0;
  ADataset.FieldValues['mtlayout'] := cxDBComboBox1.Properties.Items[0];
  ADataset.FieldValues['mtwidth'] := 0;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStMaterial.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
      RPT_STMATERIAL_ALL_ID,
      VarArrayOf([
        ADataset.FieldByName('stname').AsWideString,
        ADataset.FieldByName('ftname').AsWideString,
        ADataset.FieldByName('itmname').AsWideString
      ])
    ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStMaterial.DoOnLRNAMEChange(Sender: TObject);
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
        DataSource.DataSet.FieldByName('lrname').AsWideString
      );
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  ADataset := dmReferences.GetACRTable('itmlayrule', 'pk_itmlayrule');
  BDataset := dmPackage.GetACRTable('strules','pk_strules');
  try
    BDataset.Filter := Format('(stname = %s) and (ftname = %s) '+
      'and (fitname = %s) and (mtcd = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('stname').AsWideString),
        QuotedStr(DataSource.DataSet.FieldByName('ftname').AsWideString),
        QuotedStr(DataSource.DataSet.FieldByName('fitname').AsWideString),
        QuotedStr(DataSource.DataSet.FieldByName('mtcd').AsWideString)
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
          QuotedStr(DataSource.DataSet.FieldByName('lrname').AsWideString)
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
          BDataset.FieldByName('stname').AsString :=
            DataSource.DataSet.FieldByName('stname').AsString;
          BDataset.FieldByName('ftname').AsString :=
            DataSource.DataSet.FieldByName('ftname').AsString;
          BDataset.FieldByName('fitname').AsString :=
            DataSource.DataSet.FieldByName('fitname').AsString;
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

procedure TfrmStMaterial.DoOnMTCDChange(Sender: TObject);
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

procedure TfrmStMaterial.DoOnUMCODEChange(Sender: TObject);
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

procedure TfrmStMaterial.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

procedure TfrmStMaterial.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('stmaterials');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'mtcd', DoOnMTCDChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'lrname', DoOnLRNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'umcode', DoOnUMCODEChange);
end;

procedure TfrmStMaterial.SetFilterByParam;
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

procedure TfrmStMaterial.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_STMATERIAL_ITEM_ID,
                  VarArrayOf([
                      DataSource.DataSet.FieldByName('ordno').AsWideString,
                      DataSource.DataSet.FieldByName('ftname').AsWideString,
                      DataSource.DataSet.FieldByName('itmname').AsWideString,
                      DataSource.DataSet.FieldByName('mtcd').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStMaterial.TranslateFilter(const AFields: string;
  AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
