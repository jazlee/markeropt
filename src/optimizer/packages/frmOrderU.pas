unit frmOrderU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, dxBar, dxBarExtItems, cxClasses, cxGraphics, cxControls,
  dxStatusBar, cxPC, DB, StdCtrls, cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  cxMaskEdit, cxButtonEdit, cxLookAndFeelPainters, cxGroupBox, cxRadioGroup,
  cxDropDownEdit, cxCalendar, cxSpinEdit, cspapputil, CSPEditUtils;

type
  TfrmOrder = class(TForm)
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
    cxDBTextEdit2: TcxDBTextEdit;
    Label3: TLabel;
    cxDBDateEdit1: TcxDBDateEdit;
    Label8: TLabel;
    cxDBSpinEdit1: TcxDBSpinEdit;
    Label6: TLabel;
    cxDBSpinEdit2: TcxDBSpinEdit;
    cxDBSpinEdit3: TcxDBSpinEdit;
    cxDBComboBox1: TcxDBComboBox;
    Label10: TLabel;
    actClear: TAction;
    actPrint: TAction;
    stdBtnPrint: TdxBarLargeButton;
    actGenerateItems: TAction;
    stdBtnGenerateItems: TdxBarLargeButton;
    actReff_2: TAction;
    stdBtnReff_2: TdxBarLargeButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSaveExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure actReff_1Execute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actClearExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actGenerateItemsExecute(Sender: TObject);
    procedure actReff_2Execute(Sender: TObject);
  private
    FEventManager: TDatasetEventHandler;
    procedure InitData;
    procedure RegisterDataset(ADataset: TDataSet);
    procedure UnregisterDataset;

    function GetLookupValues(const AFields: string): Variant;
    procedure DeleteItems(const ATableName: string);

    procedure DoExternalNew(ADataset: TDataSet);
    procedure DoExternalEdit(ADataset: TDataSet);
    procedure DoExternalDelete(ADataset: TDataSet);

    procedure DoOnCSTCODEChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoAction(const AActionID: Cardinal; ADataset: TDataSet);
  end;

var
  frmOrder: TfrmOrder;

implementation

uses
  ecfutils, dmReferencesU, dmMainU, dmResU, CSPConsts, frmBrowserU, dmPackageU,
  frmUoMU, frmCustomerU, frmOrdItemU, gnugettext, frmOrderItmCreationU, 
  frmOrdStyleU;

{$R *.dfm}

{ TfrmOrder }

procedure TfrmOrder.actClearExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
    DataSource.DataSet.FieldByName('optresult').Value := null;
end;

procedure TfrmOrder.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrder.actDeleteExecute(Sender: TObject);
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
    DeleteItems('oritems');
    DeleteItems('ormaterials');
  DataSource.DataSet.Delete;
end;

procedure TfrmOrder.actGenerateItemsExecute(Sender: TObject);
begin
  TfrmOrderItmCreation.ShowOrderItemGeneration(Self, DataSource.DataSet);
end;

procedure TfrmOrder.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actNew.Enabled := DataSource.DataSet.State = dsBrowse;
  actSave.Enabled := DataSource.DataSet.State in [dsInsert, dsEdit];
  actDelete.Enabled := true;
  actReff_1.Enabled := (DataSource.DataSet.State <> dsInsert); 
end;

procedure TfrmOrder.actPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_ID,
                  VarArrayOf([
                      Datasource.DataSet.FieldByName('ordno').AsString,
                      Datasource.DataSet.FieldByName('orddate').AsDateTime
                    ])
              ]);
  SendMessage(Application.MainForm.Handle, WM_APPMSG, CMD_EXECPRINT,
    Integer(@AValue));
end;

procedure TfrmOrder.actReff_1Execute(Sender: TObject);
begin
  TfrmOrdItem.DoBrowse('ordno',
    DataSource.DataSet.FieldByName('ordno').AsString);
end;

procedure TfrmOrder.actReff_2Execute(Sender: TObject);
begin
  dmPackage.MaintainOrderStyle(DataSource.DataSet.FieldByName('ordno').AsString);
  TfrmOrdStyle.DoBrowse('ordno',
    DataSource.DataSet.FieldByName('ordno').AsString);
end;

procedure TfrmOrder.actSaveExecute(Sender: TObject);
begin
  if DataSource.DataSet.State in [dsInsert, dsEdit] then
  begin
    DataSource.DataSet.Post;
    close;
  end;
end;

constructor TfrmOrder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventManager := TDatasetEventHandler.Create(Self);
  InitData;
  Position := poMainFormCenter;
end;

procedure TfrmOrder.cxDBButtonEdit1PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmCustomer.DoBrowse('crdid;crdname');
  DataSource.DataSet.FieldByName('cstcode').Value := AValue[0];
  DataSource.DataSet.FieldByName('cstname').Value := AValue[1];
end;

procedure TfrmOrder.cxDBButtonEdit2PropertiesButtonClick(Sender: TObject;
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

procedure TfrmOrder.cxDBButtonEdit3PropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValue: Variant;
begin
  AValue := TfrmUoM.DoBrowse('umcode');
  if (AValue <> DataSource.DataSet.FieldByName('umcode').Value) then
    DataSource.DataSet.FieldByName('umcode').Value := AValue;
end;

procedure TfrmOrder.DeleteItems(const ATableName: string);
var
  ADataItem: TDataSet;
begin
  ADataItem := dmPackage.GetACRTable(ATableName);
  try
    ADataItem.Filter := Format('(ordno = %s)',
      [
        QuotedStr(DataSource.DataSet.FieldByName('ordno').AsString)
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

destructor TfrmOrder.Destroy;
begin
  FEventManager.Free;
  inherited Destroy;
end;

procedure TfrmOrder.DoAction(const AActionID: Cardinal; ADataset: TDataSet);
begin
  case AActionID of
    CMD_ORD_NEW: DoExternalNew(ADataset);
    CMD_ORD_EDIT: DoExternalEdit(ADataset);
    CMD_ORD_DELETE: DoExternalDelete(ADataset);
  end;
end;

procedure TfrmOrder.DoExternalDelete(ADataset: TDataSet);
begin
  RegisterDataset(ADataset);
  if (ADataset.IsEmpty <> true) and
     (MessageDlg(SConfirmDelete, mtConfirmation, mbYesNoCancel, 0) = mrYes) then
  begin
    DeleteItems('oritems');
    DeleteItems('ormaterials');
    ADataset.Delete;
  end;
  UnregisterDataset;
end;

procedure TfrmOrder.DoExternalEdit(ADataset: TDataSet);
begin
  RegisterDataset(ADataset);
  ADataset.Edit;
  cxDBTextEdit1.Enabled := False;
  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
  UnregisterDataset;
end;

procedure TfrmOrder.DoExternalNew(ADataset: TDataSet);
begin
  RegisterDataset(ADataset);
  ADataset.Append;
  cxDBDateEdit1.DataBinding.Field.AsDateTime := Date;
  cxDBTextEdit1.Enabled := True;
  cxDBComboBox1.ItemIndex := 0;
  cxDBSpinEdit1.DataBinding.Field.Value := 0;
  cxDBSpinEdit2.DataBinding.Field.Value := 0;
  cxDBSpinEdit3.DataBinding.Field.Value := 0;
  cxDBComboBox1.DataBinding.Field.Value := cxDBComboBox1.Properties.Items[0]; 

  ShowModal;
  if not(ADataset.State in [dsBrowse]) then
    ADataset.Cancel;
  UnregisterDataset;
end;

procedure TfrmOrder.DoOnCSTCODEChange(Sender: TObject);
var
  AValue: Variant;
begin
  if (DataSource.DataSet.FieldValues['cstcode'] <> null) then
  begin
    AValue := dmReferences.SelectValue('cardfile','crdid', 'crdid',
      DataSource.DataSet.FieldByName('cstcode').AsWideString);
    if AValue = null then
      raise Exception.Create(_(SERREntryHasNoReference));
    DataSource.DataSet.FieldValues['cstname'] := AValue;
  end else
    DataSource.DataSet.FieldValues['cstname'] := null;  
end;

procedure TfrmOrder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (not (DataSource.DataSet.State in [dsBrowse])) and
     (DataSource.DataSet.Modified) and  
     (MessageDlg(SConfirmClose, mtWarning, mbYesNo, 0) = mrNo) then
    Action := caNone;
end;

function TfrmOrder.GetLookupValues(const AFields: string): Variant;
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

procedure TfrmOrder.InitData;
begin
{
  DataSource.DataSet := dmPackage.GetACRTable('items');
  DataSource.DataSet.Active := True;
}
end;

procedure TfrmOrder.RegisterDataset(ADataset: TDataSet);
begin
  DataSource.DataSet := ADataset;
  FEventManager.RegisterDataset(ADataset);
  FEventManager.RegisterFieldEvent(etFldOnChange, 'cstcode', DoOnCSTCODEChange);  
end;

procedure TfrmOrder.UnregisterDataset;
begin
  FEventManager.UnregisterDataset;
end;

end.
