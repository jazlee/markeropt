unit frmStFeatureU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxDropDownEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  ecfutils, cxButtonEdit, CSPEditUtils;

type
  TfrmStFeature = class(TForm)
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
    Label3: TLabel;
    cxDBRadioGroup1: TcxDBRadioGroup;
    Label2: TLabel;
    cxDBButtonEdit1: TcxDBButtonEdit;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure ActionListStateChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actReff_1Execute(Sender: TObject);
    procedure cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure actPrintExecute(Sender: TObject);
  private
    FParams: TECFParams;
    FEventManager: TDatasetEventHandler;
    procedure InitData;

    procedure DeleteItems(const ATableName: string);
    procedure SetFilterByParam;
    procedure TranslateFilter(const AFields: string; AValues: Variant);
    procedure AssignParams;

    function GetLookupValues(const AFields: string): Variant;

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure DoOnFTNAMEChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class procedure DoBrowse(const AFields: string; const AValues: Variant); overload;
    class function DoBrowse(const AFields, AParamFields: string; const AValues: Variant): Variant; overload;
  end;

var
  frmStFeature: TfrmStFeature;

implementation

uses
  Strutils, CSPConsts, dmReferencesU, dmMainU, dmResU, 
  frmBrowserU, dmPackageU, frmFeatureU, frmStFeatureItemsU,
  gnugettext;

{$R *.dfm}

{ TfrmStFeature }

procedure TfrmStFeature.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmStFeature.actDeleteExecute(Sender: TObject);
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
    DeleteItems('stitmfeatures');
    DeleteItems('stmaterials');
    DeleteItems('strules');
  DataSource.DataSet.Delete;
end;

procedure TfrmStFeature.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmStFeature.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
      RPT_STFEATURE_ITEM_ID,
      VarArrayOf([
        DataSource.DataSet.FieldByName('stname').AsWideString,
        DataSource.DataSet.FieldByName('ftname').AsWideString
      ])
    ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStFeature.actReff_1Execute(Sender: TObject);
begin
  TfrmStFeatureItems.DoBrowse('stname;ftname',
    VarArrayOf([
      DataSource.DataSet.FieldValues['stname'],
      DataSource.DataSet.FieldValues['ftname']
    ])
  );
end;

procedure TfrmStFeature.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmStFeature.AssignParams;
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

constructor TfrmStFeature.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmStFeature.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmFeature.DoBrowse('ftname');
  if (cxDBButtonEdit1.DataBinding.Field.AsString <> '') and
     (MessageDlg(SConfirmReplace, mtWarning, mbYesNo, 0) = mrNo) then
    exit;
  DataSource.DataSet.FieldValues['ftname'] := AValue;
end;

procedure TfrmStFeature.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(stname = %s) and (ftname = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('stname').AsString),
        QuotedStr(DataSource.DataSet.FieldByName('ftname').AsString)
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

destructor TfrmStFeature.Destroy;
begin
  FEventManager.Free;
  FParams.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmStFeature.DoBrowse;
begin
  with TfrmStFeature.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'ftname;ftgrp;ftdesc',
            'Name;Group;Description',
            'Style Feature',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class procedure TfrmStFeature.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmStFeature.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'ftname;ftgrp;ftdesc',
            'Name;Group;Description',
            'Style Feature',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmStFeature.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('stitmfeatures');
    DeleteItems('stmaterials');
    DeleteItems('strules');
    ADataset.Delete;
  end;
end;

procedure TfrmStFeature.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBButtonEdit1.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStFeature.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBButtonEdit1.Enabled := True;
  AssignParams;
  ADataset.Fields[2].Value := 0;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStFeature.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
      RPT_STFEATURE_ALL_ID,
      ADataSet.FieldByName('stname').AsWideString
    ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStFeature.DoOnFTNAMEChange(Sender: TObject);
var
  ADataset, BDataset: TDataSet;
  AValue: Variant;
  I: integer;
begin
  AValue := dmReferences.SelectValue('features', 'ftname', 'ftname',
    DataSource.DataSet.FieldByName('ftname').AsWideString);
  if AValue = null then
    raise Exception.Create(_(SERREntryHasNoReference));
  ADataset := dmReferences.GetACRTable('features', 'pk_features');
  try
    ADataset.Open;
    if ADataset.Locate('ftname', AValue, []) then
    begin
      for I := 0 to ADataset.FieldCount - 1 do
        if ADataset.Fields[i].FieldName <> 'ftname' then
          DataSource.DataSet.FieldValues[ADataset.Fields[i].FieldName] :=
            ADataset.Fields[i].Value;
    end;
  finally
    ADataset.Free;
  end;
  ADataset := dmReferences.GetACRTable('itmfeatures', 'pk_itmfeatures');
  try
    BDataset := dmPackage.GetACRTable('stitmfeatures','pk_stitmfeatures');
    try
      if (DataSource.DataSet.FieldByName('stname').AsString <> '') and
         (DataSource.DataSet.FieldByName('ftname').OldValue <> null) then
      begin
        BDataset.Filter := Format('(stname = %s) and (ftname = %s)',
          [
            QuotedStr(DataSource.DataSet.FieldByName('stname').AsWideString),
            QuotedStr(DataSource.DataSet.FieldByName('ftname').OldValue)
          ]);
        BDataset.Filtered := True;
        BDataset.Open;
        while not BDataset.IsEmpty do
          BDataset.Delete;
      end;
      if AValue <> null then
      begin
        BDataset.Active := False;
        BDataset.Filter := Format('(stname = %s) and (ftname = %s)',
          [
            QuotedStr(DataSource.DataSet.FieldByName('stname').AsWideString),
            QuotedStr(AValue)
          ]);
        BDataset.Filtered := True;
        BDataset.Open;
        while not BDataset.IsEmpty do
          BDataset.Delete;
      end;
      BDataset.Active := False;
      BDataset.Filtered := False;
      BDataset.Open;
      ADataset.Filter := Format('(ftname = %s)',
        [
          QuotedStr(AValue)
        ]);
      ADataset.Filtered := True;
      ADataset.Open;
      if not ADataset.IsEmpty then
      begin
        ADataset.First;
        while not ADataset.Eof do
        begin
          BDataset.Append;
          BDataset.FieldByName('stname').AsString :=
            DataSource.DataSet.FieldByName('stname').AsString;
          for I := 0 to ADataset.FieldCount - 1 do
            if BDataset.FindField(ADataset.Fields[i].FieldName) <> nil then            
              BDataset.FieldByName(ADataset.Fields[i].FieldName).Value :=
                ADataset.Fields[i].Value;
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

procedure TfrmStFeature.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmStFeature.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmStFeature.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('stfeature');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'ftname', DoOnFTNAMEChange);
end;

procedure TfrmStFeature.SetFilterByParam;
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

procedure TfrmStFeature.TranslateFilter(const AFields: string;
  AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

class function TfrmStFeature.DoBrowse(const AFields, AParamFields: string;
  const AValues: Variant): Variant;
begin
  with TfrmStFeature.Create(Application.MainForm) do
  try
    TranslateFilter(AParamFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'ftname;ftgrp;ftdesc',
            'Name;Group;Description',
            'Style Feature',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

end.
