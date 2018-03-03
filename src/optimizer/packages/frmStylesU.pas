unit frmStylesU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, CSPEditUtils;

type
  TfrmStyle = class(TForm)
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
    cxDBButtonEdit1: TcxDBButtonEdit;
    cxDBButtonEdit2: TcxDBButtonEdit;
    Label3: TLabel;
    Label4: TLabel;
    actReff_2: TAction;
    stdBtnReff_2: TdxBarLargeButton;
    actReff_3: TAction;
    stdBtnReff_3: TdxBarLargeButton;
    Label8: TLabel;
    cxDBTextEdit7: TcxDBTextEdit;
    Label5: TLabel;
    cxDBTextEdit3: TcxDBTextEdit;
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
    procedure cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure actReff_2Execute(Sender: TObject);
    procedure actReff_3Execute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
  private
    FEventManager: TDatasetEventHandler;
    procedure InitData;
    procedure DeleteItems(const ATableName: string);
    function GetLookupValues(const AFields: string): Variant;

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure DoOnSTTNAMEChange(Sender: TObject);
    procedure DoOnUMCODEChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class function DoBrowse(const AFields: string): Variant; overload;
  end;

var
  frmStyle: TfrmStyle;

implementation

uses
  ecfutils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU, 
  frmStyleTypeU, frmUoMU, frmStFeatureU, frmItmCreationU,
  gnugettext;

{$R *.dfm}

{ TfrmStyle }

procedure TfrmStyle.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmStyle.actDeleteExecute(Sender: TObject);
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
  DeleteItems('stfeature');
  DeleteItems('stitmfeatures');
  DeleteItems('stmaterials');
  DeleteItems('strules');
  DataSource.DataSet.Delete;
end;

procedure TfrmStyle.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
end;

procedure TfrmStyle.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_STYLE_ITEM_ID,
                  DataSource.DataSet.FieldByName('stname').AsWideString
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStyle.actReff_1Execute(Sender: TObject);
begin
  TfrmStyleType.DoBrowse;
end;

procedure TfrmStyle.actReff_2Execute(Sender: TObject);
begin
 TfrmStFeature.DoBrowse('stname',DataSource.DataSet.FieldValues['stname']);
end;

procedure TfrmStyle.actReff_3Execute(Sender: TObject);
begin
  TfrmItmCreation.ShowItemGeneration(Self, DataSource.DataSet);
end;

procedure TfrmStyle.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

constructor TfrmStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmStyle.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  cxDBButtonEdit1.DataBinding.Field.Value := TfrmStyleType.DoBrowse('sttname');
end;

procedure TfrmStyle.cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  cxDBButtonEdit2.DataBinding.Field.Value := TfrmUoM.DoBrowse('umcode');
end;

procedure TfrmStyle.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(stname = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('stname').AsString)
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

destructor TfrmStyle.Destroy;
begin
  FEventManager.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmStyle.DoBrowse;
begin
  with TfrmStyle.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'stname;sttname;stdesc;stitmcr',
            'Name;Group;Description;Item Created',
            'Styles',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmStyle.DoBrowse(const AFields: string): Variant;
begin
  with TfrmStyle.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'stname;sttname;stdesc;stitmcr',
            'Name;Group;Description;Item Created',
            'Styles',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

procedure TfrmStyle.DoExternalDelete(ADataset: TDataSet);
begin
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('stfeature');
    DeleteItems('stitmfeatures');
    DeleteItems('stmaterials');
    DeleteItems('strules');
    ADataset.Delete;
  end;
end;

procedure TfrmStyle.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  cxDBTextEdit1.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStyle.DoExternalNew(ADataset: TDataSet);
begin
  ADataset.Append;
  cxDBTextEdit1.Enabled := True;
  ADataset.FieldByName('stitmcr').AsBoolean := False;
  ADataset.FieldByName('itmfmt').AsString := SCSPDefaultItemFormat;
  ADataset.FieldByName('avgyy').AsInteger := 0;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmStyle.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := RPT_STYLE_ALL_ID;
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmStyle.DoOnSTTNAMEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if DataSource.DataSet.FieldByName('sttname').Value <> null then
  begin
    AValue := dmReferences.SelectValue('styletypes', 'sttname', 'sttname',
        DataSource.DataSet.FieldByName('sttname').Value
      );
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
  end;
end;

procedure TfrmStyle.DoOnUMCODEChange(Sender: TObject);
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

procedure TfrmStyle.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmStyle.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmStyle.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('styles', 'pk_styles');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'sttname', DoOnSTTNAMEChange);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'umcode', DoOnUMCODEChange);
end;

end.
