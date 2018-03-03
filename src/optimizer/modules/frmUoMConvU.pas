unit frmUoMConvU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  ecfutils, CSPEditUtils;

type
  TfrmUoMConv = class(TForm)
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
    cxDBButtonEdit1: TcxDBButtonEdit;
    cxDBTextEdit2: TcxDBTextEdit;
    cxDBRadioGroup1: TcxDBRadioGroup;
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
    procedure AssignParams;
    procedure InitData;
    procedure TranslateFilter(const AFields: string; AValues: Variant);

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure DoOnAltUomChange(Sender: TObject);

    procedure SetFilterByParam;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class procedure DoBrowse(const AFields: string; const AValues: Variant); overload;
  end;

var
  frmUoMConv: TfrmUoMConv;

implementation

uses
  Strutils, gnugettext, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, frmUoMU;

{$R *.dfm}

{ TfrmUoMConv }

procedure TfrmUoMConv.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmUoMConv.actDeleteExecute(Sender: TObject);
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

procedure TfrmUoMConv.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmUoMConv.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_UOMCONV_ITEM_ID,
                  VarArrayOf([
                      DataSource.DataSet.FieldByName('basumcode').AsWideString,
                      DataSource.DataSet.FieldByName('altumcode').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmUoMConv.actReff_1Execute(Sender: TObject);
begin
//
end;

procedure TfrmUoMConv.actSaveExecute(Sender: TObject);

  function CopyRec: variant;
  var
    i: integer;
  begin
    Result := VarArrayCreate([0, DataSource.DataSet.FieldCount], varVariant);
    for I := 0 to DataSource.DataSet.FieldCount - 1 do
      Result[I] := DataSource.DataSet.Fields[i].Value;      
  end;
  
var
  ARec: Variant;
  i: Integer;
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    if DataSource.DataSet.FieldByName('altumcode').AsString = '' then
      raise Exception.Create('Alternate U/M must not empty');
    if (DataSource.DataSet.FieldByName('altumcode').Value =
        DataSource.DataSet.FieldByName('basumcode').Value) then
      raise Exception.Create(SERRUoMCircularConversion);    
    DataSource.DataSet.Post;
    // find, edit or create reversal record
    ARec := CopyRec;
    if dmReferences.SelectValue('umconv', 'basumcode;altumcode', 'basumcode',
      VarArrayOf(
        [
          ARec[1],
          ARec[0]
        ]
      )) = null
    then
    begin
      DataSource.DataSet.Append;
      for i := 0 to DataSource.DataSet.FieldCount - 1 do
        DataSource.DataSet.Fields[i].Value := ARec[i];
      DataSource.DataSet.Fields[0].Value := ARec[1];
      DataSource.DataSet.Fields[1].Value := ARec[0];
    end else
      DataSource.DataSet.Edit;

    for i := 2 to DataSource.DataSet.FieldCount - 1 do
        DataSource.DataSet.Fields[i].Value := ARec[i];
    if ARec[3] = 1 then
      DataSource.DataSet.Fields[3].Value := 2
    else
      DataSource.DataSet.Fields[3].Value := 1;
    DataSource.DataSet.Post;
    close;
  end;
end;

procedure TfrmUoMConv.AssignParams;
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

constructor TfrmUoMConv.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventManager := TDatasetEventHandler.Create(Self);
  FParams := TECFParams.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmUoMConv.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmUoM.DoBrowse('umcode');
  if (AValue <> null) and
     (AValue = DataSource.DataSet.FieldByName('basumcode').Value) then
    raise Exception.Create(SERRUoMCircularConversion);  
  cxDBButtonEdit1.DataBinding.Field.Value := AValue;
end;

destructor TfrmUoMConv.Destroy;
begin
  FEventManager.Free;
  DataSource.DataSet.Free;
  FParams.Free;
  inherited Destroy;
end;

class procedure TfrmUoMConv.DoBrowse;
begin
  with TfrmUoMConv.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'basumcode;altumcode;umcvtfact;umcvtmeth',
            'Base U/M;Alt. U/M;Conv. Fact;Conv. Method',
            'U/M Conversion Table',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete,
            DoExternalPrint);
  finally
    Free;
  end;
end;

class procedure TfrmUoMConv.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmUoMConv.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'basumcode;altumcode;umcvtfact;umcvtmeth',
            'Base U/M;Alt. U/M;Conv. Fact;Conv. Method',
            'U/M Conversion Table',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete,
            DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmUoMConv.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
    ADataset.Delete;
end;

procedure TfrmUoMConv.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBButtonEdit1.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmUoMConv.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBButtonEdit1.Enabled := True;
  AssignParams;
  ADataset.FieldByName('umcvtmeth').AsInteger := 1;
  ADataset.FieldByName('umcvtfact').Value := 1;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmUoMConv.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_UOMCONV_ALL_ID,
                  VarArrayOf([
                      ADataSet.FieldByName('basumcode').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmUoMConv.DoOnAltUomChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('altumcode').Value <> null then
  begin
    AValue := dmReferences.SelectValue('umcodes', 'umcode', 'umcode',
        DataSource.DataSet.FieldByName('altumcode').Value
      );
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
end;

procedure TfrmUoMConv.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

procedure TfrmUoMConv.InitData;
begin
  DataSource.DataSet := dmReferences.GetACRTable('umconv');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange,'altumcode', DoOnAltUomChange);
end;

procedure TfrmUoMConv.SetFilterByParam;

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

procedure TfrmUoMConv.TranslateFilter(const AFields: string; AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
