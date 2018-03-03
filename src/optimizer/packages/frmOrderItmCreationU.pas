unit frmOrderItmCreationU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ToolWin, ActnMan, ActnCtrls, cxClasses, dxBar,
  dxBarExtItems, cxControls, cxPC, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, cxGridLevel, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, dxStatusBar,
  NativeRegXML, cxGridCustomPopupMenu, cxGridPopupMenu, Menus, dmPackageU,
  ExtCtrls, cxTextEdit, cxContainer, cxMaskEdit, cxButtonEdit, CSPConsts,
  cxSpinEdit;

type
  TItemDataSource = class(TcxCustomDataSource)
  private
    FItemRows: TItemRowCollection;
    FModified: boolean;
    FDataset: TDataSet;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create;
    destructor Destroy; override;

    property Modified: boolean read FModified;
    property ItemRows: TItemRowCollection read FItemRows;
    property Dataset: TDataSet read FDataset write FDataset;
  end;

  TfrmOrderItmCreation = class(TForm)
    ActionList: TActionList;
    actClose: TAction;
    BarManager: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    stdBtnClose: TdxBarLargeButton;
    cxTabControl1: TcxTabControl;
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    BrowseGridLevel1: TcxGridLevel;
    BrowseGrid: TcxGrid;
    StatusBar: TdxStatusBar;
    stdBtnCreate: TdxBarLargeButton;
    actCreate: TAction;
    BrowseGridTableView1: TcxGridTableView;
    cxGridPopupMenu1: TcxGridPopupMenu;
    PopupMenu1: TPopupMenu;
    Setallforcurrentrow1: TMenuItem;
    Setallforcurentcolumn1: TMenuItem;
    Setall1: TMenuItem;
    N1: TMenuItem;
    Unsetallforcurrentrow1: TMenuItem;
    Unsetallforcurrentcolumn1: TMenuItem;
    UnsetAll1: TMenuItem;
    N2: TMenuItem;
    CreateItems1: TMenuItem;
    N3: TMenuItem;
    Closewindow1: TMenuItem;
    Panel1: TPanel;
    edStyle: TcxButtonEdit;
    edStyleDesc: TcxTextEdit;
    btnPrepare: TButton;
    Label1: TLabel;
    procedure actCloseExecute(Sender: TObject);
    procedure UnsetAll1Click(Sender: TObject);
    procedure actCreateExecute(Sender: TObject);
    procedure UpdateStatus(Sender: TObject; const AText: string);
    procedure edStylePropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure btnPrepareClick(Sender: TObject);
  private
    FXFeatures: TStringList;
    FYFeatures: TStringList;
    FValues: TStringList;
    FDatasource: TItemDataSource;
    FConstructed: Boolean;
    FStyle: WideString;
    FStyleFormat: WideString;

    procedure ConstructFields;
    procedure ConstructGrid;
    procedure ConstructDatasource;

    procedure ConstructData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class procedure ShowOrderItemGeneration(AParent: TComponent; ADataset: TDataSet);

    procedure ExecuteViewer;
  end;

var
  frmOrderItmCreation: TfrmOrderItmCreation;

implementation

uses
  dmResU, dmMainU, ecfutils, gnugettext, frmStylesU;

{$R *.dfm}

procedure TfrmOrderItmCreation.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrderItmCreation.actCreateExecute(Sender: TObject);
begin
  dmPackage.CreateOrderItems(FDatasource.ItemRows, UpdateStatus);
end;

procedure TfrmOrderItmCreation.btnPrepareClick(Sender: TObject);
begin
  ConstructData;
end;

procedure TfrmOrderItmCreation.ConstructData;
var
  AValue: Variant;
begin
  if FConstructed and
    (MessageDlg(_(SCSPWarnOverwriteLost), mtConfirmation, mbYesNo, 0) <> mrYes)
    then
      exit;
  FConstructed := False;
  BrowseGridTableView1.BeginUpdate;
  try
    FXFeatures.Clear;
    FYFeatures.Clear;
    FValues.Clear;
    BrowseGridTableView1.ClearItems;
    FDatasource.FItemRows.Reset;
    FStyle := edStyle.Text;
    if FStyle = EmptyStr then
      exit;
    AValue := dmPackage.SelectValue('styles', 'stname', 'stdesc;itmfmt', FStyle); 
    if not VarIsArray(AValue) then
      raise Exception.Create(_(SERREntryHasNoReference));
    edStyleDesc.Text := AValue[0];
    ConstructFields;
    ConstructGrid;
    ConstructDatasource;
    FDatasource.ItemRows.Style := FStyle;
    FDatasource.ItemRows.StyleFormat := AValue[1]; 
    FDatasource.ItemRows.Dataset := FDatasource.Dataset;
    FDatasource.DataChanged;    
  finally
    BrowseGridTableView1.EndUpdate;
  end;
end;

procedure TfrmOrderItmCreation.ConstructDatasource;
begin
  FDatasource.ItemRows.AssignFields(FXFeatures);
  FDatasource.ItemRows.SetColors(FYFeatures, 0);
end;

procedure TfrmOrderItmCreation.ConstructFields;
var
  AVal, AFlag: integer;
  AFeatureDataset, AItemDataset: TDataSet;
  AList: TStringList;
begin
  AFeatureDataset := dmPackage.GetACRTable('stfeature', 'pk_stfeature');
  AItemDataset := dmPackage.GetACRTable('stitmfeatures', 'idx_stitmfeatures_1');
  try
    AFeatureDataset.Filter := Format('(stname = %s)',
        [
          QuotedStr(FStyle)
        ]
      );
    AFeatureDataset.Filtered := True;
    AFeatureDataset.Open;
    AFlag := 0;
    while not AFeatureDataset.Eof do
    begin
      AVal := AFeatureDataset.FieldByName('ftgrp').AsInteger + 1;
      if (AFlag and AVal) <> AVal then
      begin
        AFlag := AFlag or AVal;
        AItemDataset.Filtered := False;
        AItemDataset.Active := False;
        AItemDataset.Filter := Format('(stname = %s) and (ftname = %s)',
            [
              QuotedStr(AFeatureDataset.FieldByName('stname').AsString),
              QuotedStr(AFeatureDataset.FieldByName('ftname').AsString)
            ]
          );
        AItemDataset.Filtered := True;
        AItemDataset.Active := True;
        if AVal = 1 then
          AList := FXFeatures
        else
          AList := FYFeatures;
        AList.Clear;
        while not AItemDataset.Eof do
        begin
          AList.Add(AItemDataset.FieldByName('fitname').AsString);
          AItemDataset.Next;
        end;
      end;
      AFeatureDataset.Next;
    end;
  finally
    AFeatureDataset.Free;
    AItemDataset.Free;
  end;  
end;

procedure TfrmOrderItmCreation.ConstructGrid;
var
  i: integer;
begin
  BrowseGridTableView1.ClearItems;
  with BrowseGridTableView1.CreateColumn as TcxGridColumn do
  begin
    Caption := 'Color';
    DataBinding.ValueTypeClass := TcxStringValueType;
    PropertiesClass := TcxTextEditProperties;
    Width := 200;
    Options.Focusing := False;
    Options.Editing := False;
  end;
  for i := 0 to FXFeatures.Count - 1 do
  begin
    with BrowseGridTableView1.CreateColumn as TcxGridColumn do
    begin
      Caption := FXFeatures[i];
      DataBinding.ValueTypeClass := TcxIntegerValueType;
      PropertiesClass := TcxSpinEditProperties;
      Width := 50;
    end;
  end;    
end;

constructor TfrmOrderItmCreation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXFeatures := TStringList.Create;
  FYFeatures := TStringList.Create;
  FValues := TStringList.Create;
  FDatasource := TItemDataSource.Create;
  BrowseGridTableView1.DataController.Options :=
      [
        dcoAnsiSort, dcoCaseInsensitive, dcoAssignGroupingValues,
        dcoAssignMasterDetailKeys, dcoSaveExpanding
      ];
  BrowseGridTableView1.OptionsBehavior.IncSearch := True;
  BrowseGridTableView1.OptionsData.Deleting := False;
  BrowseGridTableView1.OptionsData.Editing := True;
  BrowseGridTableView1.OptionsData.Inserting := False;
  BrowseGridTableView1.OptionsSelection.HideFocusRectOnExit := False;
  BrowseGridTableView1.OptionsSelection.InvertSelect := False;
  BrowseGridTableView1.OptionsSelection.MultiSelect := False;
  BrowseGridTableView1.OptionsSelection.CellSelect := True;
  BrowseGridTableView1.OptionsSelection.CellMultiSelect := False;
  BrowseGridTableView1.OptionsView.ColumnAutoWidth := True;
  BrowseGridTableView1.OptionsView.GroupFooters := gfVisibleWhenExpanded;
  BrowseGridTableView1.OptionsView.Navigator := False;
  BrowseGridTableView1.DataController.Filter.Active := True;
  BrowseGridTableView1.OptionsCustomize.ColumnFiltering :=
      BrowseGridTableView1.DataController.Filter.Active;
  BrowseGridTableView1.OptionsCustomize.ColumnSorting :=
      BrowseGridTableView1.DataController.Filter.Active;
  BrowseGridTableView1.DataController.CustomDataSource := FDatasource;
  ConstructData;
end;

destructor TfrmOrderItmCreation.Destroy;
begin
  FDatasource.Free;
  FXFeatures.Free;
  FYFeatures.Free;
  FValues.Free;
  inherited Destroy;
end;

procedure TfrmOrderItmCreation.edStylePropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
var
  AValues: Variant;
begin
  AValues := TfrmStyle.DoBrowse('stname;stdesc');
  edStyle.Text := AValues[0];
  edStyleDesc.Text := AValues[1];
end;

procedure TfrmOrderItmCreation.ExecuteViewer;
begin
  ShowModal;
end;

class procedure TfrmOrderItmCreation.ShowOrderItemGeneration(AParent: TComponent;
  ADataset: TDataSet);
var
  AForm: TfrmOrderItmCreation;
begin
  AForm := TfrmOrderItmCreation.Create(AParent);
  try
    AForm.FDatasource.Dataset := ADataset;
    // AForm.ConstructFields(ADataset);
    // AForm.ConstructGrid;
    // AForm.ConstructDatasource;
    // AForm.LoadData(AMemoryStream);
    // AForm.FDatasource.ItemRows.Style := ADataset.FieldByName('stname').AsString;
    // AForm.FDatasource.ItemRows.StyleFormat := ADataset.FieldByName('itmfmt').AsString;
    // AForm.FDatasource.ItemRows.Dataset := ADataset;
    // AForm.FDatasource.DataChanged;
    // AMemoryStream.Clear;
    AForm.ExecuteViewer;
  finally
    AForm.Free;
  end;
end;

procedure TfrmOrderItmCreation.UnsetAll1Click(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
//
  end;
end;

procedure TfrmOrderItmCreation.UpdateStatus(Sender: TObject; const AText: string);
begin
  StatusBar.Panels[0].Text := AText;
end;

{ TItemDataSource }

constructor TItemDataSource.Create;
begin
  FItemRows := TItemRowCollection.Create;
end;

destructor TItemDataSource.Destroy;
begin
  FItemRows.Free;
  inherited Destroy;
end;

function TItemDataSource.GetRecordCount: Integer;
begin
  Result := FItemRows.Count;
end;

function TItemDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  AColumnId: Integer;
  AItem: TItemRow;
begin
  AItem := TItemRow(FItemRows.Items[Integer(ARecordHandle)]);
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  Result := AItem.Values[AColumnId];
end;

procedure TItemDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  AColumnId: Integer;
  AItem: TItemRow;
begin
  AItem := TItemRow(FItemRows.Items[Integer(ARecordHandle)]);
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  AItem.Values[AColumnId] := VarToStr(AValue);
end;

end.
