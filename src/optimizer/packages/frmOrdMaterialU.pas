unit frmOrdMaterialU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  ecfutils, cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox,
  cxRadioGroup, cxDropDownEdit, cxSpinEdit, cspapputil, CSPEditUtils;

type
  TfrmOrdMaterial = class(TForm)
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
    cxDBTextEdit4: TcxDBTextEdit;
    Label4: TLabel;
    cxDBButtonEdit4: TcxDBButtonEdit;
    Label13: TLabel;
    cxDBSpinEdit2: TcxDBSpinEdit;
    Label14: TLabel;
    cxDBSpinEdit4: TcxDBSpinEdit;
    cxDBComboBox1: TcxDBComboBox;
    Label15: TLabel;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    Label6: TLabel;
    cxDBComboBox2: TcxDBComboBox;
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
    procedure cxDBButtonEdit4PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
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
  frmOrdMaterial: TfrmOrdMaterial;

implementation

uses
  StrUtils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU,
  frmMaterialU, frmLayingRuleU, frmUoMU, frmOrdLayingRuleItemsU,
  gnugettext;

{$R *.dfm}

{ TfrmOrdMaterial }

procedure TfrmOrdMaterial.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrdMaterial.actDeleteExecute(Sender: TObject);
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
  DeleteItems('orrules');
  DataSource.DataSet.Delete;
end;

procedure TfrmOrdMaterial.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmOrdMaterial.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_MATERIAL_ITEM_ID,
                  VarArrayOf([
                      DataSource.DataSet.FieldByName('ordno').AsWideString,
                      DataSource.DataSet.FieldByName('itmname').AsWideString,
                      DataSource.DataSet.FieldByName('mtcd').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmOrdMaterial.actReff_1Execute(Sender: TObject);
begin
   TfrmOrdLayingRuleItems.DoBrowse('ordno;itmname;mtcd',
    VarArrayOf(
      [
        DataSource.DataSet.FieldValues['ordno'],
        DataSource.DataSet.FieldValues['itmname'],
        DataSource.DataSet.FieldValues['mtcd']
      ]
    ));
end;

procedure TfrmOrdMaterial.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmOrdMaterial.AssignParams;
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

constructor TfrmOrdMaterial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmOrdMaterial.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
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

procedure TfrmOrdMaterial.cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
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

procedure TfrmOrdMaterial.cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  cxDBButtonEdit3.DataBinding.Field.Value := TfrmUoM.DoBrowse('umcode');
end;

procedure TfrmOrdMaterial.cxDBButtonEdit4PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AStream: TMemoryStream;
  ADSN, ADSNExt: string;
begin
  ADSN := StripDot(GetAppIDClass.Properties.GetProperty('dsnext', DSNFileExt));
  ADSNExt := Concat('.', ADSN); 
  case AButtonIndex of
    0:
      begin
        dmMain.OpenDialog.FileName := '';
        dmMain.OpenDialog.Title := Format('Open %s file',[UpperCase(ADSN)]);
        dmMain.OpenDialog.DefaultExt := ADSNExt;
        dmMain.OpenDialog.Filter := Format(
          '%s file (*%s)|*%s|All Files (*.*)|*.*',
            [
              UpperCase(ADSN),
              ADSNExt,
              ADSNExt
            ]
          );
        dmMain.OpenDialog.Options :=
          [ofHideReadOnly, ofEnableSizing, ofFileMustExist];
        if dmMain.OpenDialog.Execute(Self. Handle) then
        begin
          TcxDBButtonEdit(Sender).DataBinding.Field.AsString :=
            dmMain.OpenDialog.FileName;
                    AStream := TMemoryStream.Create;
          try
            if not FileExists(TcxDBButtonEdit(Sender).DataBinding.Field.AsString) then
              raise Exception.Create(Format(_('%s file does not exists'),
                [
                  UpperCase(ADSN)
                ])
              );
            AStream.LoadFromFile(TcxDBButtonEdit(Sender).DataBinding.Field.AsString);
            AStream.Position := 0;
            TBlobField(DataSource.DataSet.FieldByName('dsndata')).LoadFromStream(AStream);
            ShowMessage(_('File successfully attached'));          
          finally
            AStream.Free;
          end;
        end;
      end;
    1:
      begin
        AStream := TMemoryStream.Create;
        try
          if not FileExists(TcxDBButtonEdit(Sender).DataBinding.Field.AsString) then
            raise Exception.Create(Format('%s file does not exists',
              [
                UpperCase(ADSN)
              ])
            );
          AStream.LoadFromFile(TcxDBButtonEdit(Sender).DataBinding.Field.AsString);
          AStream.Position := 0;
          TBlobField(DataSource.DataSet.FieldByName('dsndata')).LoadFromStream(AStream);
          ShowMessage(_('File successfully attached'));          
        finally
          AStream.Free;
        end;
      end;
    2:
      begin
        dmMain.SaveDialog.FileName := '';
        dmMain.SaveDialog.Title := Format('Save %s file as',[UpperCase(ADSN)]);
        dmMain.SaveDialog.DefaultExt := ADSNExt;
        dmMain.SaveDialog.Filter := Format(
          '%s file (*%s)|*%s|All Files (*.*)|*.*',
            [
              UpperCase(ADSN),
              ADSNExt,
              ADSNExt
            ]
          );
        dmMain.SaveDialog.Options :=
          [ofHideReadOnly, ofEnableSizing, ofOverwritePrompt];
        if dmMain.SaveDialog.Execute(Self. Handle) then
        begin
          AStream := TMemoryStream.Create;
          try
            TBlobField(DataSource.DataSet.FieldByName('dsndata')).SaveToStream(AStream);
            AStream.Position := 0;
            if AStream.Size > 0 then
            begin
              AStream.SaveToFile(dmMain.SaveDialog.FileName);
              ShowMessage(_('Attachment successfully saved'));
            end else
              raise Exception.Create(
                Format('Current %s file is empty',
                  [
                    UpperCase(ADSN)
                  ]
                )
              );
          finally
            AStream.Free;
          end;
        end;
      end;
    3:
      begin
        if (DataSource.DataSet.FieldByName('dsnfile').AsString <> '') and
           (MessageDlg(SConfirmDeleteDSN, mtWarning, mbYesNo, 0) <> mrYes) then
          exit;
        DataSource.DataSet.FieldByName('dsnfile').Value := null;
        TBlobField(DataSource.DataSet.FieldByName('dsndata')).Clear;
        ShowMessage(_('Attachment has been successfully removed'));        
      end;
  end;
end;

procedure TfrmOrdMaterial.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(ordno = %s) and (itmname = %s) and (mtcd = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('ordno').AsString),
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

destructor TfrmOrdMaterial.Destroy;
begin
  FEventManager.Free;
  FParams.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmOrdMaterial.DoBrowse;
begin
  with TfrmOrdMaterial.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'mtcd;mtname;mtdesc;lrname;lrdesc',
            'Code;Name;Description;Laying Rule;Laying Desc.',
            'Order Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmOrdMaterial.DoBrowse(const AFields: string): Variant;
begin
  with TfrmOrdMaterial.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'mtcd;mtname;mtdesc;lrname;lrdesc',
            'Code;Name;Description;Laying Rule;Laying Desc.',
            'Order Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

class procedure TfrmOrdMaterial.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmOrdMaterial.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'mtcd;mtname;mtdesc;lrname;lrdesc',
            'Code;Name;Description;Laying Rule;Laying Desc.',
            'Order Materials',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmOrdMaterial.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('orrules');
    ADataset.Delete;
  end;
end;

procedure TfrmOrdMaterial.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBButtonEdit1.Enabled := False;
  cxDBButtonEdit2.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmOrdMaterial.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  AssignParams;
  DataSource.DataSet.FieldByName('avgyy').Value := 1;
  DataSource.DataSet.FieldByName('fbaltype').AsInteger := 0;
  DataSource.DataSet.FieldByName('fballowance').AsInteger := 0;
  DataSource.DataSet.FieldByName('maxlen').Value := 0;
  DataSource.DataSet.FieldByName('mrklen').Value := 0;
  DataSource.DataSet.FieldByName('mrkwidth').Value := 0;
  DataSource.DataSet.FieldByName('mtlayout').Value := cxDBComboBox2.Properties.Items[0];
  cxDBComboBox1.ItemIndex := 0;
  cxDBButtonEdit1.Enabled := True;
  cxDBButtonEdit2.Enabled := True;    
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmOrdMaterial.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_MATERIAL_ALL_ID,
                  VarArrayOf([
                      ADataSet.FieldByName('ordno').AsWideString,
                      ADataSet.FieldByName('itmname').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmOrdMaterial.DoOnLRNAMEChange(Sender: TObject);
var
  AValue: Variant;
  ADataset, BDataset: TDataSet;
begin  
  AValue := dmReferences.SelectValue('layrule','lrname', 'lrname;lrdesc',
      DataSource.DataSet.FieldByName('lrname').Value
    );
  if (not VarIsArray(AValue)) then
    raise Exception.Create(_(SERREntryHasNoReference));  
  ADataset := dmReferences.GetACRTable('itmlayrule', 'pk_itmlayrule');
  try
    ADataset.Filter := Format('(lrname = %s)',
      [
        QuotedStr(AValue[0])
      ]);
    ADataset.Filtered := True;
    ADataset.Open;
    BDataset := dmPackage.GetACRTable('ordrules','pk_itmrules');
    try
      BDataset.Filter := Format('(ordno = %s) and (itmname = %s) and (mtcd = %s)',
        [
          QuotedStr(DataSource.DataSet.FieldByName('ordno').AsString),
          QuotedStr(DataSource.DataSet.FieldByName('itmname').AsString),
          QuotedStr(VarToStr(DataSource.DataSet.FieldByName('mtcd').AsString))
        ]);
      BDataset.Filtered := True;
      BDataset.Open;
      while not BDataset.IsEmpty do
        BDataset.Delete;
      DataSource.DataSet.FieldByName('lrdesc').Value := AValue;
      BDataset.Active := False;
      BDataset.Filtered := False;
      BDataset.Open;
      if not ADataset.IsEmpty then
      begin
        ADataset.First;
        while not ADataset.Eof do
        begin
          BDataset.Append;
          BDataset.FieldByName('ordno').AsString :=
            DataSource.DataSet.FieldByName('ordno').AsString;
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
    finally
      BDataset.Free;
    end;
  finally
    ADataset.Free;
  end;
end;

procedure TfrmOrdMaterial.DoOnMTCDChange(Sender: TObject);
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

procedure TfrmOrdMaterial.DoOnUMCODEChange(Sender: TObject);
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

procedure TfrmOrdMaterial.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmOrdMaterial.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmOrdMaterial.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('ormaterials');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange,'mtcd', DoOnMTCDChange);
  FEventManager.RegisterFieldEvent(etFldOnChange,'lrname', DoOnLRNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange,'umcode', DoOnUMCODEChange);  
end;

procedure TfrmOrdMaterial.SetFilterByParam;
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

procedure TfrmOrdMaterial.TranslateFilter(const AFields: string;
  AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
