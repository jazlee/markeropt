unit frmOrdItemU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  ecfutils, cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox,
  cxRadioGroup, cxSpinEdit, cxDropDownEdit, cspapputil, CSPEditUtils;

type
  TfrmOrdItem = class(TForm)
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
    cxDBTextEdit3: TcxDBTextEdit;
    Label5: TLabel;
    cxDBButtonEdit1: TcxDBButtonEdit;
    Label4: TLabel;
    cxDBSpinEdit1: TcxDBSpinEdit;
    Label2: TLabel;
    cxDBButtonEdit2: TcxDBButtonEdit;
    Label6: TLabel;
    cxDBButtonEdit4: TcxDBButtonEdit;
    Label7: TLabel;
    cxDBButtonEdit5: TcxDBButtonEdit;
    Label8: TLabel;
    cxDBTextEdit4: TcxDBTextEdit;
    Label10: TLabel;
    cxDBButtonEdit6: TcxDBButtonEdit;
    Label11: TLabel;
    cxDBButtonEdit7: TcxDBButtonEdit;
    Label12: TLabel;
    cxDBButtonEdit8: TcxDBButtonEdit;
    Label9: TLabel;
    cxDBButtonEdit3: TcxDBButtonEdit;
    Label13: TLabel;
    cxDBSpinEdit2: TcxDBSpinEdit;
    Label14: TLabel;
    cxDBSpinEdit3: TcxDBSpinEdit;
    cxDBSpinEdit4: TcxDBSpinEdit;
    cxDBComboBox1: TcxDBComboBox;
    Label15: TLabel;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    cxDBTextEdit2: TcxDBTextEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure actReff_1Execute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actNewExecute(Sender: TObject);
    procedure cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit4PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit5PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit6PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit7PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit8PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure actPrintExecute(Sender: TObject);
  private
    FParams: TECFParams;
    FEventManager: TDatasetEventHandler;

    procedure InitData;

    procedure UpdateOrRules(const itmname, mtcd: string);
    procedure UpdateOrMaterials(const itmname: string);

    function GetLookupValues(const AFields: string): Variant;
    procedure SetFilterByParam;
    procedure TranslateFilter(const AFields: string; AValues: Variant);
    procedure AssignParams;

    procedure DeleteItems(const ATableName: string);

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure DoCheckBeforePost(Sender: TObject);
    procedure DoOnITMNAMEChange(Sender: TObject);
    procedure DoOnSTNAMEChange(Sender: TObject);
    procedure DoOnXFTNAMEChange(Sender: TObject);
    procedure DoOnXFITNAMEChange(Sender: TObject);
    procedure DoOnYFTNAMEChange(Sender: TObject);
    procedure DoOnYFITNAMEChange(Sender: TObject);
    procedure DoOnUMCODEChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class procedure DoBrowse(const AFields: string; const AValues: Variant); overload;
    class function DoBrowse(const AFields: string): Variant; overload;
  end;

var
  frmOrdItem: TfrmOrdItem;

implementation

uses
  StrUtils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU,
  frmUoMU, frmItmLayingRuleItemsU, frmItemU, frmStylesU, frmStFeatureU, 
  frmStFeatureItemsU, frmOrdMaterialU, gnugettext;

{$R *.dfm}

{ TfrmOrdItem }

procedure TfrmOrdItem.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrdItem.actDeleteExecute(Sender: TObject);
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
  DeleteItems('ormaterials');
  DeleteItems('orrules');
  DataSource.DataSet.Delete;
end;

procedure TfrmOrdItem.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
  actReff_1.Enabled := DataSource.State <> dsInsert;
end;

procedure TfrmOrdItem.actNewExecute(Sender: TObject);
begin
  if (DataSource.DataSet.State <> dsBrowse) then
    raise Exception.Create(SERRSaveWorkFirst);
  DataSource.DataSet.Append;
  AssignParams;
  DataSource.DataSet.FieldByName('orqty').Value := 0;
  DataSource.DataSet.FieldByName('maxlen').Value := 0;
  DataSource.DataSet.FieldByName('mrklen').Value := 0;
  DataSource.DataSet.FieldByName('mrkwidth').Value := 0;
  cxDBComboBox1.ItemIndex := 0;
  cxDBButtonEdit1.Enabled := True;
  cxDBButtonEdit2.Enabled := True;
end;

procedure TfrmOrdItem.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_ITEM_ITEM_ID,
                  VarArrayOf([
                    DataSource.DataSet.FieldByName('ordno').AsWideString,
                    DataSource.DataSet.FieldByName('itmname').AsWideString
                  ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmOrdItem.actReff_1Execute(Sender: TObject);
begin
   TfrmOrdMaterial.DoBrowse('ordno;itmname',
    VarArrayOf(
      [
        DataSource.DataSet.FieldValues['ordno'],
        DataSource.DataSet.FieldValues['itmname']
      ]
    ));
end;

procedure TfrmOrdItem.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmOrdItem.AssignParams;
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

constructor TfrmOrdItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmOrdItem.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmItem.DoBrowse(
    'itmname;itmdesc;stname;xftname;xfitname;xfitprio;xfitprio2;yftname;yfitname;umcode'
  );
  UpdateOrMaterials(AValue[0]);
  DataSource.DataSet.FieldByName('itmname').Value := AValue[0];
  DataSource.DataSet.FieldByName('itmdesc').Value := AValue[1];
  DataSource.DataSet.FieldByName('stname').Value := AValue[2];
  DataSource.DataSet.FieldByName('xftname').Value := AValue[3];
  DataSource.DataSet.FieldByName('xfitname').Value := AValue[4];
  DataSource.DataSet.FieldByName('xfitprio').Value := AValue[5];
  DataSource.DataSet.FieldByName('xfitprio2').Value := AValue[6];
  DataSource.DataSet.FieldByName('yftname').Value := AValue[7];
  DataSource.DataSet.FieldByName('yfitname').Value := AValue[8];
  DataSource.DataSet.FieldByName('umcode').Value := AValue[9];
end;

procedure TfrmOrdItem.cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmStyle.DoBrowse('stname');
  if (AValue <> DataSource.DataSet.FieldByName('stname').Value) then
  begin
    DataSource.DataSet.FieldByName('xftname').Value := null;
    DataSource.DataSet.FieldByName('xfitname').Value := null;
    DataSource.DataSet.FieldByName('xfitprio').Value := null;
    DataSource.DataSet.FieldByName('yftname').Value := null;
    DataSource.DataSet.FieldByName('yfitname').Value := null;
    DataSource.DataSet.FieldByName('stname').Value := AValue;
  end;
end;

procedure TfrmOrdItem.cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
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
            if not FileExists(cxDBButtonEdit2.DataBinding.Field.AsString) then
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

procedure TfrmOrdItem.cxDBButtonEdit4PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmStFeature.DoBrowse('ftname','stname;ftgrp',
    VarArrayOf([
      DataSource.DataSet.FieldByName('stname').Value,
      0
    ]));
  if (AValue <> DataSource.DataSet.FieldByName('xftname').Value) then
  begin
    DataSource.DataSet.FieldByName('xftname').Value := AValue;
    DataSource.DataSet.FieldByName('xfitname').Value := null;
    DataSource.DataSet.FieldByName('xfitprio').Value := null;
  end;
end;

procedure TfrmOrdItem.cxDBButtonEdit5PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmStFeatureItems.DoBrowse('fitname;fitprio','stname;ftname',
    VarArrayOf([
      DataSource.DataSet.FieldByName('stname').Value,
      DataSource.DataSet.FieldByName('xftname').Value
    ]));
  if (AValue[0] <> DataSource.DataSet.FieldByName('xftname').Value) then
  begin
    DataSource.DataSet.FieldByName('xfitname').Value := AValue[0];
    DataSource.DataSet.FieldByName('xfitprio').Value := AValue[1];
  end;
end;

procedure TfrmOrdItem.cxDBButtonEdit6PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmStFeature.DoBrowse('ftname','stname;ftgrp',
    VarArrayOf([
      DataSource.DataSet.FieldByName('stname').Value,
      1
    ]));
  if (AValue <> DataSource.DataSet.FieldByName('yftname').Value) then
  begin
    DataSource.DataSet.FieldByName('yftname').Value := AValue;
    DataSource.DataSet.FieldByName('yfitname').Value := null;
  end;
end; 

procedure TfrmOrdItem.cxDBButtonEdit7PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmStFeatureItems.DoBrowse('fitname','stname;ftname',
    VarArrayOf([
      DataSource.DataSet.FieldByName('stname').Value,
      DataSource.DataSet.FieldByName('yftname').Value
    ]));
  if (AValue <> DataSource.DataSet.FieldByName('yfitname').Value) then
    DataSource.DataSet.FieldByName('yfitname').Value := AValue;
end;

procedure TfrmOrdItem.cxDBButtonEdit8PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  DataSource.DataSet.FieldByName('umcode').Value := TfrmUoM.DoBrowse('umcode');
end;

procedure TfrmOrdItem.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(ordno = %s) and (itmname = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('ordno').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('itmname').AsString)
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

destructor TfrmOrdItem.Destroy;
begin
  FEventManager.UnregisterDataset;
  FEventManager.Free;
  FParams.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

procedure TfrmOrdItem.DoCheckBeforePost(Sender: TObject);
begin
  if DataSource.DataSet.FieldByName('orqty').AsInteger <= 0 then
    raise Exception.Create('Order quantity must be filled');
end;

procedure TfrmOrdItem.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('ormaterials');
    DeleteItems('orrules');
    ADataset.Delete;
  end;
end;

procedure TfrmOrdItem.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBButtonEdit1.Enabled := False;
  cxDBButtonEdit2.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmOrdItem.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  AssignParams;
  DataSource.DataSet.FieldByName('orqty').Value := 0;
  DataSource.DataSet.FieldByName('maxlen').Value := 0;
  DataSource.DataSet.FieldByName('mrklen').Value := 0;
  DataSource.DataSet.FieldByName('mrkwidth').Value := 0;
  cxDBComboBox1.DataBinding.Field.Value := cxDBComboBox1.Properties.Items[0];
  cxDBButtonEdit1.Enabled := True;
  cxDBButtonEdit2.Enabled := True;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmOrdItem.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
      RPT_ORDER_ITEM_ALL_ID,
      ADataset.FieldByName('ordno').Value
    ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmOrdItem.DoOnITMNAMEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('itmname').Value = null then
  begin
    DataSource.DataSet.FieldValues[
        'itmdesc;stname;xftname;xfitname;xfitprio;xfitprio2;yftname;yfitname;umcode'
      ] := VarArrayOf([
              Null, Null,Null,Null,Null,Null,Null,Null, Null
            ]);
  end else
  begin
    AValue := dmPackage.SelectValue('items', 'itmname',
      'itmdesc;stname;xftname;xfitname;xfitprio;xfitprio2;yftname;yfitname;umcode',
      DataSource.DataSet.FieldByName('itmname').Value);
    if not VarIsArray(AValue) then
      raise Exception.Create(_(SERREntryHasNoReference));
    DataSource.DataSet.FieldValues[
        'itmdesc;stname;xftname;xfitname;xfitprio;xfitprio2;yftname;yfitname;umcode'
      ] := AValue;
  end;
end;

procedure TfrmOrdItem.DoOnSTNAMEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('stname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('styles', 'stname',
      'stname',
      DataSource.DataSet.FieldByName('stname').Value);
    if (AValue = null) then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  DataSource.DataSet.FieldValues[
      'xftname;xfitname;xfitprio;xfitprio2;yftname;yfitname'
    ] := VarArrayOf([
            Null,
            Null,
            Null,
            Null,
            Null,
            Null
          ]);
end;

procedure TfrmOrdItem.DoOnUMCODEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('yftname').Value <> null then
  begin
    AValue := dmReferences.SelectValue('umcodes', 'umcode',
      'umcode', DataSource.DataSet.FieldByName('umcode').Value
    );
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
end;

procedure TfrmOrdItem.DoOnXFITNAMEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('xfitname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('stitmfeatures', 'stname;ftname;fitname',
      'fitprio;fitprio2',
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').Value,
        DataSource.DataSet.FieldByName('xftname').Value,
        DataSource.DataSet.FieldByName('xfitname').Value,
        0      
      ]));
    if not VarIsArray(AValue) then
      raise Exception.Create(_(SERREntryHasNoReference));
    DataSource.DataSet.FieldValues['xfitprio'] := AValue[0];
    DataSource.DataSet.FieldValues['xfitprio2'] := AValue[1];
  end else
  begin
    DataSource.DataSet.FieldValues['xfitprio'] := null;
    DataSource.DataSet.FieldValues['xfitprio2'] := null;
  end;
end;

procedure TfrmOrdItem.DoOnXFTNAMEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('xftname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('stfeature', 'stname;ftname;ftgrp',
      'ftname',
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').Value,
        DataSource.DataSet.FieldByName('xftname').Value,
        0      
      ]));
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  DataSource.DataSet.FieldValues['xfitname;xfitprio;xfitprio2'] :=
    VarArrayOf([
      Null,
      Null,
      Null
    ]);
end;

procedure TfrmOrdItem.DoOnYFITNAMEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('yfitname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('stitmfeatures', 'stname;ftname;fitname',
      'fitname',
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').Value,
        DataSource.DataSet.FieldByName('yftname').Value,
        DataSource.DataSet.FieldByName('yfitname').Value
      ]));
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
end;

procedure TfrmOrdItem.DoOnYFTNAMEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('yftname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('stfeature', 'stname;ftname;ftgrp',
      'ftname',
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').Value,
        DataSource.DataSet.FieldByName('yftname').Value,
        1
      ]));
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  DataSource.DataSet.FieldValues['yfitname'] := Null;
end;

procedure TfrmOrdItem.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmOrdItem.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmOrdItem.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('oritems', 'pk_oritems');
  DataSource.DataSet.Active := True;
  
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterDatasetEvent(etDsBeforePost, DoCheckBeforePost);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'stname', DoOnSTNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'itmname', DoOnITMNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'xftname', DoOnXFTNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'xfitname', DoOnXFITNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'yftname', DoOnYFTNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'yfitname', DoOnYFITNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'umcode', DoOnUMCODEChange);
end;

procedure TfrmOrdItem.SetFilterByParam;
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

procedure TfrmOrdItem.TranslateFilter(const AFields: string;
  AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

procedure TfrmOrdItem.UpdateOrMaterials(const itmname: string);
var
  ADataset, BDataset: TDataSet;
begin
  ADataset := dmPackage.GetACRTable('itmmaterial','pk_itmmaterial');
  try
    ADataset.Filter := Format('(itmname = %s)',
      [
        QuotedStr(itmname)
      ]);
    ADataset.Filtered := True;
    ADataset.Open;
    BDataset := dmPackage.GetACRTable('ormaterials', 'pk_ormaterials');
    try
      BDataset.Filter := Format('(ordno = %s) and ((itmname = %s) or (itmname = %s))',
        [
          QuotedStr(DataSource.DataSet.FieldByName('ordno').AsString),
          QuotedStr(DataSource.DataSet.FieldByName('itmname').AsString),
          QuotedStr(itmname)
        ]);
      BDataset.Filtered := True;
      BDataset.Open;
      while not BDataset.IsEmpty do
        BDataset.Delete;
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
          BDataset.FieldByName('itmname').AsString := itmname;
          BDataset.FieldByName('mtcd').AsString :=
            ADataset.FieldByName('mtcd').Value;
          BDataset.FieldByName('mtname').Value :=
            ADataset.FieldByName('mtname').Value;
          BDataset.FieldByName('mtdesc').Value :=
            ADataset.FieldByName('mtdesc').Value;
          BDataset.FieldByName('lrname').Value :=
            ADataset.FieldByName('lrname').Value;
          BDataset.FieldByName('lrdesc').Value :=
            ADataset.FieldByName('lrdesc').Value;
          BDataset.FieldByName('avgyy').Value :=
            ADataset.FieldByName('avgyy').Value;
          BDataset.FieldByName('umcode').Value :=
            ADataset.FieldByName('umcode').Value;
          BDataset.FieldByName('fballowance').Value :=
            ADataset.FieldByName('fballowance').Value;
          BDataset.FieldByName('fbaltype').Value :=
            ADataset.FieldByName('fbaltype').Value;
          BDataset.Post;
          UpdateOrRules(itmname, ADataset.FieldByName('mtcd').AsString);
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

procedure TfrmOrdItem.UpdateOrRules(const itmname, mtcd: string);
var
  ADataset, BDataset: TDataSet;
begin
  ADataset := dmPackage.GetACRTable('itmrules','pk_itmrules');
  try
    ADataset.Filter := Format('(itmname = %s) and (mtcd = %s)',
      [
        QuotedStr(itmname),
        QuotedStr(mtcd)
      ]);
    ADataset.Filtered := True;
    ADataset.Open;
    BDataset := dmPackage.GetACRTable('orrules', 'pk_orrules');
    try
      BDataset.Filter := Format('(ordno = %s) and ((itmname = %s) or (itmname = %s)) and (mtcd = %s)',
        [
          QuotedStr(DataSource.DataSet.FieldByName('ordno').AsString),
          QuotedStr(DataSource.DataSet.FieldByName('itmname').AsString),
          QuotedStr(itmname),
          QuotedStr(mtcd)
        ]);
      BDataset.Filtered := True;
      BDataset.Open;
      while not BDataset.IsEmpty do
        BDataset.Delete;
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
          BDataset.FieldByName('itmname').AsString := itmname;
          BDataset.FieldByName('mtcd').AsString := mtcd;
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

class procedure TfrmOrdItem.DoBrowse;
begin
  with TfrmOrdItem.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'itmname;itmdesc;xfitname;yfitname;orqty;xfitprio;xfitprio2',
            'Item;Description;Size;Color;Quantity;Seq;Prio',
            'Order Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmOrdItem.DoBrowse(const AFields: string): Variant;
begin
  with TfrmOrdItem.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'itmname;itmdesc;xfitname;yfitname;orqty;xfitprio;xfitprio2',
            'Item;Description;Size;Color;Quantity;Seq;Prio',
            'Order Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

class procedure TfrmOrdItem.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmOrdItem.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'itmname;itmdesc;xfitname;yfitname;orqty;xfitprio;xfitprio2',
            'Item;Description;Size;Color;Quantity;Seq;Prio',
            'Order Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

end.
