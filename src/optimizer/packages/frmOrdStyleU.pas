unit frmOrdStyleU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  ecfutils, cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox,
  cxRadioGroup, cxDropDownEdit, cxSpinEdit, cspapputil, CSPEditUtils;

type
  TfrmOrdStyle = class(TForm)
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
    Label9: TLabel;
    cxDBButtonEdit3: TcxDBButtonEdit;
    Label8: TLabel;
    cxDBTextEdit7: TcxDBTextEdit;
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
    Label11: TLabel;
    cxDBTextEdit5: TcxDBTextEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure ActionListStateChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
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

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);
    procedure DoExternalPrint(ADataset: TDataSet);

    procedure DoOnUMCODEChange(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure DoBrowse; overload;
    class procedure DoBrowse(const AFields: string; const AValues: Variant); overload;
    class function DoBrowse(const AFields: string): Variant; overload;
  end;

var
  frmOrdStyle: TfrmOrdStyle;

implementation

uses
  StrUtils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU,
  frmUoMU, gnugettext;

{$R *.dfm}

{ TfrmOrdStyle }

procedure TfrmOrdStyle.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrdStyle.actDeleteExecute(Sender: TObject);
begin
  raise Exception.Create(_(SERRNoDeletionAllowed));
end;

procedure TfrmOrdStyle.ActionListStateChange(Sender: TObject);
begin
  actNew.Enabled := False;
  actSave.Enabled := DataSource.DataSet.State in [dsEdit];
  actDelete.Enabled := False;
end;

procedure TfrmOrdStyle.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_STYLE_ITEM_ID,
                  VarArrayOf([
                      DataSource.DataSet.FieldByName('ordno').AsWideString,
                      DataSource.DataSet.FieldByName('stname').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmOrdStyle.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

constructor TfrmOrdStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TECFParams.Create(Self);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmOrdStyle.cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  cxDBButtonEdit3.DataBinding.Field.Value := TfrmUoM.DoBrowse('umcode');
end;

procedure TfrmOrdStyle.cxDBButtonEdit4PropertiesButtonClick(Sender: TObject;
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

destructor TfrmOrdStyle.Destroy;
begin
  FEventManager.Free;
  FParams.Free;
  DataSource.DataSet.Free;
  inherited Destroy;
end;

class procedure TfrmOrdStyle.DoBrowse;
begin
  with TfrmOrdStyle.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'stname;sttname;stdesc',
            'Style;Category;Description',
            'Order Styles',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

class function TfrmOrdStyle.DoBrowse(const AFields: string): Variant;
begin
  with TfrmOrdStyle.Create(Application.MainForm) do
  try
    TfrmBrowser.ShowBrowserForLookup(Application.MainForm,
            'stname;sttname;stdesc',
            'Style;Category;Description',
            'Order Styles',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
    Result := GetLookupValues(AFields);
  finally
    Free;
  end;
end;

class procedure TfrmOrdStyle.DoBrowse(const AFields: string;
  const AValues: Variant);
begin
  with TfrmOrdStyle.Create(Application.MainForm) do
  try
    TranslateFilter(AFields, AValues);
    SetFilterByParam;
    TfrmBrowser.ShowBrowser(Application.MainForm,
            'stname;sttname;stdesc',
            'Style;Category;Description',
            'Order Styles',
            DataSource,
            DoExternalNew, DoExternalEdit, DoExternalDelete, DoExternalPrint);
  finally
    Free;
  end;
end;

procedure TfrmOrdStyle.DoExternalDelete(ADataset: TDataSet);
begin
  raise Exception.Create(_(SERRNoDeletionAllowed));
end;

procedure TfrmOrdStyle.DoExternalEdit(ADataset: TDataSet);
begin
  ADataset.Edit;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
end;

procedure TfrmOrdStyle.DoExternalNew(ADataset: TDataSet);
begin
  raise Exception.Create(_(SERRNoInsertionAllowed));
end;

procedure TfrmOrdStyle.DoExternalPrint(ADataset: TDataSet);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_STYLE_ALL_ID,
                  VarArrayOf([
                      ADataSet.FieldByName('ordno').AsWideString
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;


procedure TfrmOrdStyle.DoOnUMCODEChange(Sender: TObject);
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

procedure TfrmOrdStyle.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmOrdStyle.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmOrdStyle.InitData;
begin
  DataSource.DataSet := dmPackage.GetACRTable('orstyles');
  DataSource.DataSet.Active := True;
  FEventManager.RegisterDataset(DataSource.DataSet);
  FEventManager.RegisterFieldEvent(etFldOnChange,'umcode', DoOnUMCODEChange);
end;

procedure TfrmOrdStyle.SetFilterByParam;
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

procedure TfrmOrdStyle.TranslateFilter(const AFields: string;
  AValues: Variant);
begin
  FParams.ParseParams(AFields);
  FParams.ParamValues[AFields] := AValues;
end;

end.
