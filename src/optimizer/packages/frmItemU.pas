unit frmItemU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  CSPEditUtils;
  
type
  TfrmItem = class(TForm)
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
    cxDBTextEdit4: TcxDBTextEdit;
    Label4: TLabel;
    Label5: TLabel;
    cxDBButtonEdit1: TcxDBButtonEdit;
    Label2: TLabel;
    cxDBButtonEdit2: TcxDBButtonEdit;
    Label6: TLabel;
    cxDBTextEdit2: TcxDBTextEdit;
    Label3: TLabel;
    cxDBButtonEdit4: TcxDBButtonEdit;
    Label9: TLabel;
    cxDBButtonEdit5: TcxDBButtonEdit;
    cxDBButtonEdit6: TcxDBButtonEdit;
    Label11: TLabel;
    Label7: TLabel;
    cxDBButtonEdit3: TcxDBButtonEdit;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    Label8: TLabel;
    cxDBTextEdit3: TcxDBTextEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure ActionListStateChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit4PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit5PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit6PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure actReff_1Execute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
  private
    FEventManager: TDatasetEventHandler;
    procedure InitData;

    function GetLookupValues(const AFields: string): Variant;
    procedure DeleteItems(const ATableName: string);

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure DoOnStyleChange(Sender: TObject);
    procedure DoXFeatureChange(Sender: TObject);
    procedure DoXFeatureItemChange(Sender: TObject);
    procedure DoYFeatureChange(Sender: TObject);
    procedure DoYFeatureItemChange(Sender: TObject);
    procedure DoOnUMCodeChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class function DoBrowse(const AFields: string): Variant; overload;
  end;

var
  frmItem: TfrmItem;

implementation

uses
  ecfutils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU, 
  frmStylesU, frmStFeatureU, frmStFeatureItemsU, frmUoMU, frmItmMaterialU,
  gnugettext;

{$R *.dfm}

{ TfrmItem }

procedure TfrmItem.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmItem.actDeleteExecute(Sender: TObject);
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
  DeleteItems('itmmaterial');
  DeleteItems('itmrules');
  DataSource.DataSet.Delete;
end;

procedure TfrmItem.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmItem.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ITEM_ITEM_ID,
                  DataSource.DataSet.FieldByName('itmname').AsWideString
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmItem.actReff_1Execute(Sender: TObject);
begin
  TfrmItmMaterial.DoBrowse('itmname',
    DataSource.DataSet.FieldByName('itmname').AsString);
end;

procedure TfrmItem.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

constructor TfrmItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmItem.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmStyle.DoBrowse('stname');
  if AValue <> DataSource.DataSet.FieldByName('stname').Value then
    DataSource.DataSet.FieldByName('stname').Value := AValue;
end;

procedure TfrmItem.cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
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
    DataSource.DataSet.FieldByName('xftname').Value := AValue;
end;

procedure TfrmItem.cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmUoM.DoBrowse('umcode');
  if (AValue <> DataSource.DataSet.FieldByName('umcode').Value) then
    DataSource.DataSet.FieldByName('umcode').Value := AValue;
end;

procedure TfrmItem.cxDBButtonEdit4PropertiesButtonClick(Sender: TObject;
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

procedure TfrmItem.cxDBButtonEdit5PropertiesButtonClick(Sender: TObject;
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

procedure TfrmItem.cxDBButtonEdit6PropertiesButtonClick(Sender: TObject;
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

procedure TfrmItem.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(itmname = %s)',
      [
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

destructor TfrmItem.Destroy;
begin
  FEventManager.UnregisterDataset;
  FEventManager.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmItem.DoBrowse;
begin
  with TfrmItem.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'itmname;itmdesc;yfitname;xfitname;stname',
            'Name;Description;Color;Size;Style',
            'Browse Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmItem.DoBrowse(const AFields: string): Variant;
begin
  with TfrmItem.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'itmname;itmdesc;yfitname;xfitname;stname',
            'Name;Description;Color;Size;Style',
            'Browse Items',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

procedure TfrmItem.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('itmmaterial');
    DeleteItems('itmrules');
    ADataset.Delete;
  end;
end;

procedure TfrmItem.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBTextEdit1.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmItem.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBTextEdit1.Enabled := True;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmItem.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := RPT_ITEM_ALL_ID;
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmItem.DoXFeatureChange(Sender: TObject);
var
  AValue: Variant; 
begin
  if DataSource.DataSet.FieldByName('xftname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('stfeature', 'stname;ftgrp;ftname', 'ftname',
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').Value,
        0,
        DataSource.DataSet.FieldByName('xftname').Value
      ]));
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  DataSource.DataSet.FieldByName('xfitname').Value := null;
  DataSource.DataSet.FieldByName('xfitprio').Value := null;
end;

procedure TfrmItem.DoXFeatureItemChange(Sender: TObject);
var
  AValue: Variant; 
begin
  if DataSource.DataSet.FieldByName('xfitname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('stitmfeatures', 'stname;ftname;fitname',
      'fitname;fitprio',
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').Value,
        DataSource.DataSet.FieldByName('xftname').Value,
        DataSource.DataSet.FieldByName('xfitname').Value
      ]));
    if AValue[0] = null then
      raise Exception.Create(_(SERREntryHasNoReference));
    DataSource.DataSet.FieldByName('xfitprio').Value := AValue[1];
  end;
end;

procedure TfrmItem.DoYFeatureChange(Sender: TObject);
var
  AValue: Variant; 
begin
  if DataSource.DataSet.FieldByName('yftname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('stfeature', 'stname;ftgrp;ftname', 'ftname',
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').Value,
        1,
        DataSource.DataSet.FieldByName('yftname').Value
      ]));
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  DataSource.DataSet.FieldByName('yfitname').Value := null;
end;

procedure TfrmItem.DoYFeatureItemChange(Sender: TObject);
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

procedure TfrmItem.DoOnStyleChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('stname').Value <> null then
  begin
    AValue := dmPackage.SelectValue('styles', 'stname', 'stname',
        DataSource.DataSet.FieldByName('stname').Value
      );
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
  DataSource.DataSet.FieldByName('xftname').Value := null;
  DataSource.DataSet.FieldByName('xfitname').Value := null;
  DataSource.DataSet.FieldByName('xfitprio').Value := null;
  DataSource.DataSet.FieldByName('yftname').Value := null;
  DataSource.DataSet.FieldByName('yfitname').Value := null;
end;

procedure TfrmItem.DoOnUMCodeChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('umcode').Value <> null then
  begin
    AValue := dmReferences.SelectValue('umcodes', 'umcode', 'umcode',
        DataSource.DataSet.FieldByName('umcode').Value
      );
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
end;

procedure TfrmItem.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmItem.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmItem.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('items');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'stname', DoOnStyleChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'xftname', DoXFeatureChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'xfitname', DoXFeatureItemChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'yftname', DoYFeatureChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'yfitname', DoYFeatureItemChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'umcode', DoOnUMCodeChange);
end;

end.
