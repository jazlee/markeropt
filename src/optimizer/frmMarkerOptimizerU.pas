unit frmMarkerOptimizerU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxBar, dxBarExtItems, cxClasses, ActnList, cxGraphics, cxControls,
  dxStatusBar, cxPC, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxEdit, DB, cxDBData, cxGridLevel, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid, CSPCustomEngine,
  NativeXml, cxGridBandedTableView, StdCtrls, cxMaskEdit, cxDropDownEdit,
  cxCalendar, cxContainer, cxTextEdit, cxInplaceContainer, cxVGrid, cxSplitter,
  cxBlobEdit, ExtCtrls, cxSpinEdit, cxCheckBox, Menus, dxPSGlbl, dxPSUtl,
  dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev, dxPSCompsProvider,
  dxPSFillPatterns, dxPSEdgePatterns, dxPSCore, dxPScxGrid6Lnk,
  cxGridExportLink, cxMemo;

type
  TConsumptionDatasource = class(TcxCustomDataSource)
  private
    FEngineTab: TcxTabControl;
    FModified: boolean;

    function GetEngine: TCSPBaseOptimizer;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
  public
    constructor Create(AEngineTab: TcxTabControl);

    property Modified: boolean read FModified;
  end;
  
  TDistributionDatasource = class(TcxCustomDataSource)
  private
    FEngineTab: TcxTabControl;
    FModified: boolean;

    function GetEngine: TCSPBaseOptimizer;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
  public
    constructor Create(AEngineTab: TcxTabControl);

    property Modified: boolean read FModified;
  end;
   
  TfrmMarkerOptimizer = class(TForm)
    ActionList: TActionList;
    actClose: TAction;
    BarManager: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    stdBtnClose: TdxBarLargeButton;
    StatusBar: TdxStatusBar;
    EngineTabControl: TcxTabControl;
    actSave: TAction;
    actCancel: TAction;
    stdBtnSave: TdxBarLargeButton;
    stdBtnCancel: TdxBarLargeButton;
    actOptimize: TAction;
    stdBtnOptimize: TdxBarLargeButton;
    actOptimizeItem: TAction;
    dxBarDockControl1: TdxBarDockControl;
    BarManagerBar1: TdxBar;
    lblDetail: TdxBarStatic;
    actExport: TAction;
    actLoad: TAction;
    actReset: TAction;
    stdBtnReset: TdxBarButton;
    btnReoptimize: TdxBarButton;
    stdBtnExportBatch: TdxBarButton;
    stdBtnLoadXML: TdxBarButton;
    Panel1: TPanel;
    Splitter: TcxSplitter;
    Panel2: TPanel;
    Label3: TLabel;
    edOrderDesc: TcxTextEdit;
    Label2: TLabel;
    edOrderNo: TcxTextEdit;
    Label1: TLabel;
    edOrderDate: TcxDateEdit;
    actLockUnlock: TAction;
    DistPopupMenu: TPopupMenu;
    OptimizeAll1: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    Cancel1: TMenuItem;
    N2: TMenuItem;
    Reset1: TMenuItem;
    ReoptimizeWorksheet1: TMenuItem;
    N3: TMenuItem;
    ExportBatch1: TMenuItem;
    LoadXML1: TMenuItem;
    N4: TMenuItem;
    LockUnlockvalue1: TMenuItem;
    N5: TMenuItem;
    Closewindow1: TMenuItem;
    actPrint: TAction;
    stdBtnPrint: TdxBarButton;
    actExportExcel: TAction;
    stdBtnExportExcel: TdxBarButton;
    Panel3: TPanel;
    Panel4: TPanel;
    ErrorMemo: TcxMemo;
    ErrorSplitter: TcxSplitter;
    DistributionGrid: TcxGrid;
    DistributionGridBandedTableView: TcxGridBandedTableView;
    DistributionGridLevel: TcxGridLevel;
    ConsumptionGridLevel: TcxGridLevel;
    actLockRow: TAction;
    Lockunlockrow1: TMenuItem;
    TabProperties: TcxTabControl;
    PropertyGrid: TcxVerticalGrid;
    ConsumptionGridBandedTableView: TcxGridBandedTableView;
    actShowMarkerInfo: TAction;
    Showmarkerinformation1: TMenuItem;
    N6: TMenuItem;
    stdBtnShowInfo: TdxBarButton;
    procedure EngineTabControlChange(Sender: TObject);
    procedure actLockUnlockExecute(Sender: TObject);
    procedure DistributionGridBandedTableViewCustomDrawCell(
      Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure actSaveExecute(Sender: TObject);
    procedure actOptimizeExecute(Sender: TObject);
    procedure actOptimizeItemExecute(Sender: TObject);
    procedure actResetExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actPrintExecute(Sender: TObject);
    procedure actExportExcelExecute(Sender: TObject);
    procedure ConsumptionGridTableViewDataControllerSummaryAfterSummary(
      ASender: TcxDataSummary);
    procedure EngineTabControlChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure actLockRowExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actLoadExecute(Sender: TObject);
    procedure TabPropertiesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure TabPropertiesChange(Sender: TObject);
    procedure DistributionGridBandedTableViewFocusedRecordChanged(
      Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord;
      ANewItemRecordFocusingChanged: Boolean);
    procedure DistributionGridBandedTableViewCanFocusRecord(
      Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      var AAllow: Boolean);
    procedure actShowMarkerInfoExecute(Sender: TObject);
    procedure ConsumptionGridBandedTableViewDblClick(Sender: TObject);
    procedure DistributionGridBandedTableViewDblClick(Sender: TObject);

  private
    FDatasource: TDataSource;
    FDistGridDatasource: TDistributionDatasource;
    FConsGridDatasource: TConsumptionDatasource;
    FOrder: TNativeXml;
    FComponentPrinter: TdxComponentPrinter;
    FGridReportLink: TBasedxReportLink;

    procedure CreateDistributionColumns;
    procedure CreateFabricConsumptionColumns;
    procedure CreateEngineTabs;

    procedure HandlePropTabChanged(Sender: TObject);
    procedure HandlePropTabChanging(Sender: TObject; var AllowChange: Boolean);

    procedure DisplayEngineProperties(AObject: TCSPBaseOptimizer);
    procedure SaveEngineProperties(AObject: TCSPBaseOptimizer);

    procedure DisplayDetailEngineProperties(AObject: TCSPBaseOptimizer);
    procedure SaveDetailEngineProperties(AObject: TCSPBaseOptimizer);

    procedure PrepareOptimization;

    procedure LoadValues;
    procedure SaveValues;
    procedure ClearResult;

  protected
    procedure InitOrder;

  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; ADatasource: TDataSource); overload;
    destructor Destroy; override;

    class procedure ShowOptimizer(AParent: TComponent; ADatasource: TDataSource);
  end;

var
  frmMarkerOptimizer: TfrmMarkerOptimizer;

implementation

uses
  Math, CSPConsts, dmMainU, dmPackageU, dmResU, dmReferencesU, OptitexUtils,
  GNUGettext, cxDrawTextUtils, frmMarkerInfoU;

type
  TcxCustomTextEditPropertiesClass = class of TcxCustomTextEditProperties;

const
  CFabConsBandHeaderCount = 8;
  CFabConsBandHeaders: array[0..(CFabConsBandHeaderCount-1)] of string =
  (
    'General',
    'Marker Width', 'Marker Length', 'Total Marker', 'Allowance',
    'Total Length', 'Total Pcs', 'Fab. Consumption'
  );

  CFabConsValueTypeClasses: array[0..(CFabConsHeaderCount-1)] of
                            TcxValueTypeClass =
  (
    // 'stname', 'fitname', 'mtcd', 'ftname',
    TcxStringValueType, TcxStringValueType,
    TcxStringValueType, TcxStringValueType,

    // 'stpid', 'lrcnt', 'pcscnt', 'avgyy',
    TcxIntegerValueType, TcxIntegerValueType,
    TcxIntegerValueType, TcxFloatValueType,

    // 'mrkwidth', 'actwidth', 'mrklen', 'actlen', 'mrktot', 'acttot',
    TcxFloatValueType, TcxFloatValueType,
    TcxFloatValueType, TcxFloatValueType,
    TcxFloatValueType, TcxFloatValueType,

    // 'fballowance', 'fbalwlen', 'actfbalwlen', 'totlen', 'acttotlen',
    TcxStringValueType, TcxFloatValueType,  TcxFloatValueType,
    TcxFloatValueType, TcxFloatValueType,

    // 'totpcs', 'consumption', 'actconsumption'
    TcxFloatValueType, TcxFloatValueType, TcxFloatValueType
  );
  
  CFabConsPropertyTypeClasses: array[0..(CFabConsHeaderCount-1)] of
                            TcxCustomTextEditPropertiesClass =
  (
    // 'stname', 'fitname', 'mtcd', 'ftname',
    TcxTextEditProperties, TcxTextEditProperties,
    TcxTextEditProperties, TcxTextEditProperties, 

    // 'stpid', 'lrcnt', 'pcscnt', 'avgyy',
    TcxSpinEditProperties, TcxSpinEditProperties,
    TcxSpinEditProperties, TcxSpinEditProperties, 

    // 'mrkwidth', 'actwidth', 'mrklen', 'actlen', 'mrktot', 'acttot',
    TcxSpinEditProperties, TcxSpinEditProperties,
    TcxSpinEditProperties, TcxSpinEditProperties,
    TcxSpinEditProperties, TcxSpinEditProperties, 

    // 'fballowance', 'fbalwlen', 'actfbalwlen', 'totlen', 'acttotlen',
    TcxTextEditProperties, TcxSpinEditProperties, TcxSpinEditProperties, 
    TcxSpinEditProperties, TcxSpinEditProperties, 

    // 'totpcs', 'acttotpcs', 'consumption', 'actconsumption'
    TcxSpinEditProperties, TcxSpinEditProperties, TcxSpinEditProperties
    
  );  
  
{$R *.dfm}

{ TfrmMarkerOptimizer }

constructor TfrmMarkerOptimizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrder := TNativeXml.Create;
  FOrder.XmlFormat := xfReadable;
  FDatasource := nil;
  FComponentPrinter := TdxComponentPrinter.Create(Self);
  FGridReportLink := FComponentPrinter.AddLink(DistributionGrid);
  FGridReportLink.Caption := 'Report';
  FGridReportLink.PrinterPage.Orientation := poLandscape;
  FGridReportLink.PrinterPage.ScaleMode := smFit;
  DistributionGrid.ActiveLevel := DistributionGridLevel;
  FDistGridDatasource := TDistributionDatasource.Create(EngineTabControl);
  DistributionGridBandedTableView.DataController.CustomDataSource := FDistGridDatasource;
  FConsGridDatasource := TConsumptionDatasource.Create(EngineTabControl);
  ConsumptionGridBandedTableView.DataController.CustomDataSource := FConsGridDatasource;
  Splitter.CloseSplitter;
  ErrorSplitter.CloseSplitter;
end;

procedure TfrmMarkerOptimizer.actCancelExecute(Sender: TObject);
begin
  if MessageDlg(SConfifmOptClear, mtConfirmation, mbYesNo, 0) = mrYes then
    ClearResult;
end;

procedure TfrmMarkerOptimizer.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMarkerOptimizer.actExportExcelExecute(Sender: TObject);
begin
  dmMain.SaveDialog.Title := 'Export as';
  dmMain.SaveDialog.DefaultExt := '.xls';
  dmMain.SaveDialog.Filter := 'Microsoft Excel file (*.xls)|*.xls|All Files (*.*)|*.*';
  dmMain.SaveDialog.Options := [ofHideReadOnly, ofEnableSizing, ofOverwritePrompt];
  if dmMain.SaveDialog.Execute(Self.Handle) then
    ExportGridToExcel(dmMain.SaveDialog.FileName, DistributionGrid);
end;

procedure TfrmMarkerOptimizer.actExportExecute(Sender: TObject);
var
  AOptitex: TCSPOptitexExportBatch;
begin
  AOptitex := TCSPOptitexExportBatch.Create(Self);
  try
    AOptitex.Engine := FDistGridDatasource.GetEngine;
    AOptitex.DoExportBatch;
  finally
    AOptitex.Free;
  end;
end;

procedure TfrmMarkerOptimizer.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actLockUnlock.Visible := (DistributionGrid.ActiveLevel = DistributionGridLevel);
  Handled := True;
end;

procedure TfrmMarkerOptimizer.actLoadExecute(Sender: TObject);
var
  AOptitex: TCSPOptitexImportXML;
begin
  ErrorMemo.Lines.Clear;
  AOptitex := TCSPOptitexImportXML.Create(Self);
  try
    AOptitex.Engine := FDistGridDatasource.GetEngine;
    try
      AOptitex.DoImportXML;
    except
      on E:Exception do
        begin
          dmMain.Logger.Error(e);
          ErrorMemo.Lines.Text := e.Message;
          ErrorSplitter.OpenSplitter;
          Application.ShowException(E);
        end;
    end;
  finally
    FDistGridDatasource.DataChanged;
    FConsGridDatasource.DataChanged;
    AOptitex.Free;
  end;
end;

procedure TfrmMarkerOptimizer.actLockRowExecute(Sender: TObject);
var
  AView: TcxGridBandedTableView;
  AEngine: TCSPBaseOptimizer;
  ARecordIndex: integer;
  AValue: Boolean;
begin
  AView := DistributionGridBandedTableView;
  AEngine := TDistributionDatasource(AView.DataController.CustomDataSource).GetEngine;
  if AEngine <> nil then
  begin
    ARecordIndex := AView.DataController.FocusedRecordIndex;
    AValue := AEngine.RowLockStatus[ARecordIndex];
    AEngine.RowLockStatus[ARecordIndex] := not AValue;
    DistributionGridBandedTableView.Invalidate;
  end;
end;

procedure TfrmMarkerOptimizer.actLockUnlockExecute(Sender: TObject);
var
  AView: TcxGridBandedTableView;
  AEngine: TCSPBaseOptimizer;
  ARecordIndex, AColIndex: integer;
  AValue: Boolean;
begin
  AView := DistributionGridBandedTableView;
  AEngine := TDistributionDatasource(AView.DataController.CustomDataSource).GetEngine;
  if AEngine <> nil then
  begin
    ARecordIndex := AView.DataController.FocusedRecordIndex;
    AColIndex := AView.Controller.FocusedColumn.Index;
    AValue := AEngine.LockStatus[ARecordIndex, AColIndex];
    AEngine.LockStatus[ARecordIndex, AColIndex] := not AValue;
    DistributionGridBandedTableView.Invalidate;
  end;
end;

procedure TfrmMarkerOptimizer.actOptimizeExecute(Sender: TObject);
var
  I: integer;
  AEngine: TCSPBaseOptimizer;
begin
  ErrorMemo.Clear;
  PrepareOptimization;
  Screen.Cursor := crHourGlass;
  try
    for I := 0 to EngineTabControl.Tabs.Count - 1 do
    begin
      AEngine := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[I]);
      if AEngine <> nil then
      begin
        try
          AEngine.DoOptimize;
        except
          on e:exception do
          begin
            ErrorMemo.Lines.Add(e.Message);
            dmMain.Logger.Error(e);
            ErrorSplitter.OpenSplitter;
          end;
        end;
      end;
    end;
  finally
    FDistGridDatasource.DataChanged;
    FConsGridDatasource.DataChanged;
    DistributionGridBandedTableView.DataController.Groups.FullExpand;
    ConsumptionGridBandedTableView.DataController.Groups.FullExpand;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMarkerOptimizer.actOptimizeItemExecute(Sender: TObject);
var
  AEngine: TCSPBaseOptimizer;
begin
  ErrorMemo.Clear;
  PrepareOptimization;
  Screen.Cursor := crHourGlass; 
  AEngine := FDistGridDatasource.GetEngine;
  try
    if AEngine <> nil then
    begin
      try
        AEngine.DoOptimize;
      except
        on e:exception do
          begin
            dmMain.Logger.Error(e);
            ErrorMemo.Lines.Add(e.Message);
            ErrorSplitter.OpenSplitter;
            Application.ShowException(E);
          end;
      end;
    end;
  finally
    FDistGridDatasource.DataChanged;
    FConsGridDatasource.DataChanged;
    DistributionGridBandedTableView.DataController.Groups.FullExpand;
    ConsumptionGridBandedTableView.DataController.Groups.FullExpand;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMarkerOptimizer.actPrintExecute(Sender: TObject);
begin
  FGridReportLink.Preview;
end;

procedure TfrmMarkerOptimizer.actResetExecute(Sender: TObject);
begin
  FDistGridDatasource.GetEngine.DistributionRoot.NodesClear;
  FDistGridDatasource.DataChanged;
  FConsGridDatasource.GetEngine.ConsumptionRoot.NodesClear;
  FConsGridDatasource.DataChanged;
end;

procedure TfrmMarkerOptimizer.actSaveExecute(Sender: TObject);
begin
  SaveValues;
end;

procedure TfrmMarkerOptimizer.actShowMarkerInfoExecute(Sender: TObject);
var
  AView: TcxGridBandedTableView;
  AEngine: TCSPBaseOptimizer;
  ARecordIndex: integer;
  ARowNode, ANode: TXmlNode;
begin
  AView := TcxGridBandedTableView(DistributionGrid.ActiveLevel.GridView);  
  AEngine := TConsumptionDatasource(AView.DataController.CustomDataSource).GetEngine;
  if AEngine <> nil then
  begin
    ARowNode := nil;
    ARecordIndex := AView.DataController.FocusedRecordIndex;
    if (ARecordIndex < AEngine.ConsumptionRoot.NodeCount) then
      ARowNode := AEngine.ConsumptionRoot.Nodes[ARecordIndex];
    if ARowNode <> nil then
    begin
      if ARowNode.NodeCount > 0 then
      begin
        ANode := ARowNode.Nodes[0];
        TfrmMarkerInfo.ShowMarkerInfo(Self, ANode);
      end;
    end;
  end;
end;

procedure TfrmMarkerOptimizer.ClearResult;
var
  AEngine: TCSPBaseOptimizer;
  I: integer;
begin
  FDatasource.DataSet.Edit;
  try
    FDatasource.DataSet.FieldByName('optresult').Value := null;
    FDatasource.DataSet.Post;
  except
    FDatasource.DataSet.Cancel;
    raise;
  end;
  for I := 0 to EngineTabControl.Tabs.Count - 1 do
  begin
    AEngine := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[I]);
    if AEngine <> nil then
    begin
      AEngine.DistributionRoot.NodesClear;
      AEngine.ConsumptionRoot.NodesClear;
    end;
  end;
  FDistGridDatasource.DataChanged;
  FConsGridDatasource.DataChanged;
end;

procedure TfrmMarkerOptimizer.ConsumptionGridBandedTableViewDblClick(
  Sender: TObject);
begin
  actShowMarkerInfoExecute(Sender);
end;

procedure TfrmMarkerOptimizer.ConsumptionGridTableViewDataControllerSummaryAfterSummary(
  ASender: TcxDataSummary);
var
  ALevel, ACurIndex: integer;
  SV: Variant;
begin
  for ALevel:= 0 to ASender.DataController.DataControllerInfo.GroupingFieldList.Count - 1 do
  begin
    ACurIndex := 0;
    while ACurIndex < ASender.DataController.DataControllerInfo.DataGroups.Count do
    begin
      if ASender.DataController.DataControllerInfo.DataGroups[ACurIndex].Level =
        (ASender.DataController.DataControllerInfo.DataGroups.LevelCount - 1) then
      begin
        SV := ASender.DataController.DataControllerInfo.DataGroups[ACurIndex].SummaryValues;
        if VarIsArray(SV) and (VarArrayHighBound(SV, 1) = 4) then
        begin
          if (SV[0] <> null) and (SV[0] <> 0) and
             (SV[2] <> null) and (SV[2] <> 0) then
            SV[3] := RoundTo(SV[0] / SV[2], -3)
          else
            SV[3] := 0;          
          if (SV[1] <> null) and (SV[1] <> 0) and
             (SV[2] <> null) and (SV[2] <> 0) then
            SV[4] := RoundTo(SV[1] / SV[2], -3)
          else
            SV[4] := 0;
          ASender.DataController.DataControllerInfo.DataGroups[ACurIndex].
            SummaryValues := SV;
        end
      end;
      Inc(ACurIndex);
    end;
  end;
end;

constructor TfrmMarkerOptimizer.Create(AOwner: TComponent;
  ADatasource: TDataSource);
begin
  Create(AOwner);
  FDatasource := ADatasource;
end;

procedure TfrmMarkerOptimizer.CreateDistributionColumns;
var
  ARoot, ANode: TXmlNode;
  I, J: integer;
  ACap: WideString;
begin
  ARoot := FOrder.Root.FindNode('allsizes', False);
  if ARoot = nil then
    exit;
  DistributionGridBandedTableView.Bands.Clear;
  for I := 0 to ARoot.NodeCount do
    DistributionGridBandedTableView.Bands.Add;
  DistributionGridBandedTableView.ClearItems;
  for I := 0 to CDistHeaderCount - 1 do
    with DistributionGridBandedTableView.CreateColumn as TcxGridBandedColumn do
    begin
      Caption := CDistHeaders[I];
      Position.BandIndex := 0;
      Position.LineCount := 3;
      HeaderAlignmentVert := vaCenter;
      if I < 3 then
      begin
        DataBinding.ValueTypeClass := TcxStringValueType;
        PropertiesClass := TcxTextEditProperties;
        Width := 75;
        GroupIndex := I;
        Visible := False;
        SortOrder := soAscending;
        Properties.ReadOnly := True;
      end else
      if I = 3 then
      begin
        DataBinding.ValueTypeClass := TcxStringValueType;
        PropertiesClass := TcxTextEditProperties;
        Width := 0;
        Visible := False;
        Properties.ReadOnly := True;
      end else
      begin
        DataBinding.ValueTypeClass := TcxIntegerValueType;
        PropertiesClass := TcxSpinEditProperties;
        TcxSpinEditProperties(Properties).MinValue := 0;
        Width := 40;
      end;
      if I <> 5 then
      begin
        Options.Editing := False;
        Options.Focusing:= False;
      end;
    end;
  for I := 0 to ARoot.NodeCount-1 do
  begin
    ANode := ARoot.Nodes[I];
    ACap := ANode.AttributeByNameWide['fitname'];
    for J := 0 to CDistColumnSuffixCount - 1 do
    begin
      with DistributionGridBandedTableView.CreateColumn as TcxGridBandedColumn do
      begin
        if (CDistColumnSuffixes[J] = EmptyStr) then
          Caption := ACap
        else
          Caption := Format('%s %s', [ACap, CDistColumnSuffixes[J]]);
        Position.BandIndex := I+1;
        Position.LineCount := 1;
        Position.RowIndex := J;
        DataBinding.ValueTypeClass := TcxIntegerValueType;
        PropertiesClass := TcxSpinEditProperties;
        if J <> 1 then
        begin
          Properties.ReadOnly := True;
          Options.Editing     := False;
          Options.Focusing    := False;
        end;
        TcxSpinEditProperties(Properties).MinValue := 0;
        Width := 40;
      end;
    end;
  end;
  DistributionGridBandedTableView.OptionsView.ColumnAutoWidth :=
    Boolean(IfThen(DistributionGridBandedTableView.ItemCount <= 35, 1, 0));
  DistributionGridBandedTableView.OptionsView.BandHeaders := False;
end;

procedure TfrmMarkerOptimizer.CreateEngineTabs;
var
  I: integer;
  ARoot, ANode: TXmlNode;
  AClass: TCSPBaseOptimizerClass;
  AObject: TCSPBaseOptimizer;
begin
  EngineTabControl.Tabs.Clear;
  ARoot := FOrder.Root.FindNode('engines', False);
  if ARoot = nil then
    exit;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    ANode := ARoot.Nodes[I];
    if ANode <> nil then
    begin
      AClass := GetEngineClass(ANode.ReadAttributeString('name'));
      if AClass <> nil then
      begin
        AObject := AClass.Create(Self);
        AObject.SetDocument(FOrder);
        AObject.InitEngine;
        AObject.PackageValues;
        EngineTabControl.Tabs.AddObject(ANode.ReadAttributeString('name'), AObject);
      end;
    end;
  end;
end;

procedure TfrmMarkerOptimizer.CreateFabricConsumptionColumns;
var
  I: integer;
  ASummary: TcxDataSummaryItem;
  ABand: TcxGridBand;
begin
  ConsumptionGridBandedTableView.Bands.Clear;
  for I := 0 to CFabConsBandHeaderCount-1 do
  begin
    ABand := ConsumptionGridBandedTableView.Bands.Add;
    ABand.Caption := CFabConsBandHeaders[I];
  end;
  ConsumptionGridBandedTableView.ClearItems;
  for I := 0 to CFabConsHeaderCount - 1 do
  begin
    with ConsumptionGridBandedTableView.CreateColumn as TcxGridColumn do
    begin
      Caption := CFabConsHeaders[I];
      DataBinding.ValueTypeClass := CFabConsValueTypeClasses[I];
      PropertiesClass := CFabConsPropertyTypeClasses[I];
      if not(I in [8,9,11]) then
      begin
        Properties.ReadOnly := True;
        Options.Editing := False;
        Options.Focusing:= False;
      end else
      begin
        TcxSpinEditProperties(Properties).ValueType := vtFloat;
        Options.Editing := True;
        Options.Focusing:= True;
        Properties.ReadOnly := False;
      end;
      if I < 3 then
      begin
        Width := 100;
        Visible := False;
        SortOrder := soAscending;
        GroupIndex := I;
      end else
      if I = 3 then
      begin
        Width := 0;
        Visible := False;
      end 
      else
        Width := 75;
      // 'stname', 'fitname', 'mtcd', 'ftname', 
      // 'stpid', 'lrcnt', 'pcscnt', 'avgyy'
      if (I < 8) then Position.BandIndex := 0
      // 'mrkwidth', 'actwidth',      
      else if (I in [8, 9]) then Position.BandIndex := 1
      // 'mrklen', 'actlen', 
      else if (I in [10, 11]) then Position.BandIndex := 2
      // 'mrktot', 'acttot',
      else if (I in [12, 13]) then Position.BandIndex := 3
      // 'fballowance', 'fbalwlen', 'actfbalwlen', 
      else if (I in [14, 15, 16]) then Position.BandIndex := 4
      // 'totlen', 'acttotlen',
      else if (I in [17, 18]) then Position.BandIndex := 5
      // 'totpcs', 'acttotpcs', 
      else if (I = 19) then Position.BandIndex := 6
      // 'consumption', 'actconsumption'
      else if (I in [20, 21]) then Position.BandIndex := 7;              
    end;
  end;
  ConsumptionGridBandedTableView.DataController.Summary.SummaryGroups.Clear;
  with ConsumptionGridBandedTableView.DataController.Summary.SummaryGroups.Add do
  begin
    TcxGridTableSummaryGroupItemLink(Links.Add).Column := TcxGridColumn(ConsumptionGridBandedTableView.Items[2]);

    ASummary := SummaryItems.Add;
    ASummary.Kind := skSum;
    ASummary.Position := spFooter;
    ASummary.Format   := 'Len: 0.00';
    TcxGridTableSummaryItem(ASummary).Column := TcxGridColumn(ConsumptionGridBandedTableView.Items[17]);

    ASummary := SummaryItems.Add;
    ASummary.Kind := skSum;
    ASummary.Position := spFooter;
    ASummary.Format   := 'Len: 0.00';
    TcxGridTableSummaryItem(ASummary).Column := TcxGridColumn(ConsumptionGridBandedTableView.Items[18]);

    ASummary := SummaryItems.Add;
    ASummary.Kind := skSum;
    ASummary.Position := spFooter;
    ASummary.Format   := 'Total pcs: 0.00';
    TcxGridTableSummaryItem(ASummary).Column := TcxGridColumn(ConsumptionGridBandedTableView.Items[19]);

    ASummary := SummaryItems.Add;
    ASummary.Kind := skAverage;
    ASummary.Position := spFooter;
    ASummary.Format   := 'Avg cons: 0.00';
    TcxGridTableSummaryItem(ASummary).Column := TcxGridColumn(ConsumptionGridBandedTableView.Items[20]);

    ASummary := SummaryItems.Add;
    ASummary.Kind := skAverage;
    ASummary.Position := spFooter;
    ASummary.Format   := 'Avg cons: 0.00';
    TcxGridTableSummaryItem(ASummary).Column := TcxGridColumn(ConsumptionGridBandedTableView.Items[21]);
  end;
  ConsumptionGridBandedTableView.OptionsView.ColumnAutoWidth := False;
  ConsumptionGridBandedTableView.OptionsView.GroupFooters := gfVisibleWhenExpanded;
  ConsumptionGridBandedTableView.OptionsView.GroupSummaryLayout := gslAlignWithColumns;
  ConsumptionGridBandedTableView.OptionsView.GroupFooterMultiSummaries := False;
end;

destructor TfrmMarkerOptimizer.Destroy;
var
  I: integer;
begin
  for I := 0 to EngineTabControl.Tabs.Count-1 do
    if EngineTabControl.Tabs.Objects[I] <> nil then
      TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[I]).DeinitEngine;
  FreeAndNil(FOrder);
  FDatasource := nil;
  FDistGridDatasource.Free;
  FConsGridDatasource.Free;
  inherited Destroy;
end;

procedure TfrmMarkerOptimizer.DisplayDetailEngineProperties(
  AObject: TCSPBaseOptimizer);
var
  ARootNode, ANode: TXmlNode;
  ARowIndex: integer;
  ARow: TcxEditorRow;
  ACatRow: TcxCustomRow;
  I: integer;
  ACaption: string;
begin
  if AObject = nil then
    exit;
  PropertyGrid.ClearRows;
  ARowIndex := DistributionGridBandedTableView.DataController
                  .DataControllerInfo.FocusedRecordIndex;
  if (FDistGridDatasource.GetRecordCount > 0) and (ARowIndex > -1) and
     (ARowIndex < FDistGridDatasource.GetRecordCount) then
  begin
    ARootNode := AObject.GetProperties(AObject.DistributionRoot.Nodes[ARowIndex]);
    for I := 0 to ARootNode.NodeCount - 1 do
    begin
      ANode := ARootNode.Nodes[I];
      ACaption := ANode.ReadAttributeString('category');
      ACatRow := PropertyGrid.RowByCaption(ACaption);
      if ACatRow = nil then
      begin
        ACatRow := PropertyGrid.Add(TcxCategoryRow);
        TcxCategoryRow(ACatRow).Properties.Caption := ACaption;
      end;
      ARow := TcxEditorRow(PropertyGrid.AddChild(ACatRow, TcxEditorRow));
      ARow.Properties.Caption := ANode.ReadAttributeString('caption');
      ARow.Tag := ANode.ReadAttributeInteger('tag');
      case TFieldType(ANode.ReadAttributeInteger('datatype')) of
        ftFloat, ftBCD:
          begin
            ARow.Properties.DataBinding.ValueTypeClass := TcxFloatValueType;
            ARow.Properties.Value := ANode.ReadAttributeFloat('value');
            ARow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
          end;
        ftSmallInt, ftInteger, ftLargeInt:
          begin
            ARow.Properties.DataBinding.ValueTypeClass := TcxIntegerValueType;
            ARow.Properties.Value := ANode.ReadAttributeInteger('value');
            ARow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
          end;
        ftDateTime:
          begin
            ARow.Properties.DataBinding.ValueTypeClass := TcxDateTimeValueType;
            ARow.Properties.Value := ANode.ReadAttributeDateTime('value');
            ARow.Properties.EditPropertiesClassName := 'TcxDateEditProperties';
          end;
        ftBoolean:
          begin
            ARow.Properties.DataBinding.ValueTypeClass := TcxBooleanValueType;
            ARow.Properties.Value := ANode.ReadAttributeBool('value');
            ARow.Properties.EditPropertiesClassName := 'TcxCheckBoxProperties';
          end;
        ftString:
          begin
            ARow.Properties.DataBinding.ValueTypeClass := TcxStringValueType;
            ARow.Properties.Value := ANode.ReadAttributeString('value');
            ARow.Properties.EditPropertiesClassName := 'TcxTextEditProperties';
          end;
      end;
    end;
  end;
end;

procedure TfrmMarkerOptimizer.DisplayEngineProperties(
  AObject: TCSPBaseOptimizer);
var
  ARootNode, ANode: TXmlNode;
  ARow: TcxEditorRow;
  ACatRow: TcxCustomRow;
  I: integer;
  ACaption: string;
begin
  ARootNode := AObject.GetProperties;
  if ARootNode = nil then
    exit;
  PropertyGrid.ClearRows;
  for I := 0 to ARootNode.NodeCount - 1 do
  begin
    ANode := ARootNode.Nodes[I];
    ACaption := ANode.ReadAttributeString('category');
    ACatRow := PropertyGrid.RowByCaption(ACaption);
    if ACatRow = nil then
    begin
      ACatRow := PropertyGrid.Add(TcxCategoryRow);
      TcxCategoryRow(ACatRow).Properties.Caption := ACaption;
    end;
    ARow := TcxEditorRow(PropertyGrid.AddChild(ACatRow, TcxEditorRow));
    ARow.Properties.Caption := ANode.ReadAttributeString('caption');
    ARow.Tag := ANode.ReadAttributeInteger('tag');
    case TFieldType(ANode.ReadAttributeInteger('datatype')) of
      ftFloat, ftBCD:
        begin
          ARow.Properties.DataBinding.ValueTypeClass := TcxFloatValueType;
          ARow.Properties.Value := ANode.ReadAttributeFloat('value');
          ARow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
        end;
      ftSmallInt, ftInteger, ftLargeInt:
        begin
          ARow.Properties.DataBinding.ValueTypeClass := TcxIntegerValueType;
          ARow.Properties.Value := ANode.ReadAttributeInteger('value');
          ARow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
        end;
      ftDateTime:
        begin
          ARow.Properties.DataBinding.ValueTypeClass := TcxDateTimeValueType;
          ARow.Properties.Value := ANode.ReadAttributeDateTime('value');
          ARow.Properties.EditPropertiesClassName := 'TcxDateEditProperties';
        end;
      ftBoolean:
        begin
          ARow.Properties.DataBinding.ValueTypeClass := TcxBooleanValueType;
          ARow.Properties.Value := ANode.ReadAttributeBool('value');
          ARow.Properties.EditPropertiesClassName := 'TcxCheckBoxProperties';
        end;
      ftString:
        begin
          ARow.Properties.DataBinding.ValueTypeClass := TcxStringValueType;
          ARow.Properties.Value := ANode.ReadAttributeString('value');
          ARow.Properties.EditPropertiesClassName := 'TcxTextEditProperties';
        end;
    end;
  end;
end;

procedure TfrmMarkerOptimizer.DistributionGridBandedTableViewCanFocusRecord(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  var AAllow: Boolean);
begin
  HandlePropTabChanging(Sender, AAllow);
end;

procedure TfrmMarkerOptimizer.DistributionGridBandedTableViewCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  AView: TcxGridBandedTableView;
  AEngine: TCSPBaseOptimizer;
  ARecordIndex, AColIndex: integer;
  AValue: Boolean;
begin
  AView := DistributionGridBandedTableView;
  AEngine := TDistributionDatasource(AView.DataController.CustomDataSource).GetEngine;
  if AEngine <> nil then
  begin
    ARecordIndex := AViewInfo.GridRecord.RecordIndex;
    AColIndex := AViewInfo.Item.Index;
    AValue := AEngine.RowLockStatus[ARecordIndex];
    if not AValue then
      AValue := AEngine.LockStatus[ARecordIndex, AColIndex];
    if AValue = True then
    begin
      ARec := AViewInfo.Bounds;
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := $00E3CBB8;
      ACanvas.Font.Style  := ACanvas.Font.Style + [fsBold];
      ACanvas.Font.Color  := clWhite;
      ACanvas.Canvas.FillRect(ARec);
    end;
  end;
end;

procedure TfrmMarkerOptimizer.DistributionGridBandedTableViewDblClick(
  Sender: TObject);
begin
  actShowMarkerInfoExecute(Sender);
end;

procedure TfrmMarkerOptimizer.DistributionGridBandedTableViewFocusedRecordChanged(
  Sender: TcxCustomGridTableView; APrevFocusedRecord,
  AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  HandlePropTabChanged(Sender);
end;

procedure TfrmMarkerOptimizer.EngineTabControlChange(Sender: TObject);
var
  AObject: TCSPBaseOptimizer;
begin
  if (EngineTabControl.Tabs.Count > 0) and (EngineTabControl.TabIndex >= 0) then
  begin
    AObject := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[EngineTabControl.TabIndex]);
    if AObject <> nil then
    begin
      HandlePropTabChanged(Sender);
      FDistGridDatasource.DataChanged;
      FConsGridDatasource.DataChanged;
      DistributionGridBandedTableView.DataController.Groups.FullExpand;
      ConsumptionGridBandedTableView.DataController.Groups.FullExpand;
    end;
  end;
end;

procedure TfrmMarkerOptimizer.EngineTabControlChanging(Sender: TObject;
  var AllowChange: Boolean);
var
  AObject: TCSPBaseOptimizer;
begin
  if (EngineTabControl.Tabs.Count > 0) and (EngineTabControl.TabIndex >= 0) then
  begin
    AObject := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[EngineTabControl.TabIndex]);
    if AObject <> nil then
      HandlePropTabChanging(Sender, AllowChange);
  end;
end;

procedure TfrmMarkerOptimizer.HandlePropTabChanged(Sender: TObject);
var
  AObject: TCSPBaseOptimizer;
begin
  if (EngineTabControl.Tabs.Count > 0) and (EngineTabControl.TabIndex >= 0) then
  begin
    AObject := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[EngineTabControl.TabIndex]);
    if AObject <> nil then
    begin
      if TabProperties.TabIndex = 0 then
        DisplayEngineProperties(AObject)
      else
        DisplayDetailEngineProperties(AObject);
    end;
  end;
end;

procedure TfrmMarkerOptimizer.HandlePropTabChanging(Sender: TObject;
  var AllowChange: Boolean);
var
  AObject: TCSPBaseOptimizer;
begin
  if (EngineTabControl.Tabs.Count > 0) and (EngineTabControl.TabIndex >= 0) then
  begin
    AObject := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[EngineTabControl.TabIndex]);
    if AObject <> nil then
    begin
      if TabProperties.TabIndex = 0 then
        SaveEngineProperties(AObject)
      else
        SaveDetailEngineProperties(AObject);
    end;
  end;
end;

procedure TfrmMarkerOptimizer.InitOrder;
var
  ARoot, ANode: TXmlNode;
begin
  dmPackage.BuildXMLDatasource(FOrder, FDatasource.DataSet);
  dmPackage.ExtractXMLSizes(FOrder);
  dmPackage.ExtractXMLColors(FOrder);
  dmPackage.ExtractXMLMaterials(FOrder);
  dmPackage.RegisterXMLEngines(FOrder);
  CreateEngineTabs;
  CreateDistributionColumns;
  CreateFabricConsumptionColumns;
  ARoot := FOrder.Root.FindNode('orders', False); if ARoot = nil then exit;
  ANode := ARoot.FindNode('order', False); if ANode = nil then exit;
  edOrderNo.Text := ANode.ReadAttributeString('ordno');
  edOrderDate.Date := ANode.ReadAttributeDateTime('orddate');
  edOrderDesc.Text := ANode.ReadAttributeString('orddesc');
  FGridReportLink.ReportTitle.Text := Format('%s - %s'#13#10#13#10,[edOrderNo.Text, edOrderDesc.Text]);
  FGridReportLink.ReportTitle.Mode := tmOnFirstPage;
  FGridReportLink.ReportTitle.TextAlignX := taLeft;
  FGridReportLink.ReportTitle.Font.Name := 'Arial';
  FGridReportLink.ReportTitle.Font.Style := [fsBold];
  FGridReportLink.ReportTitle.Font.Color := clBlack;
  FGridReportLink.ReportTitle.Font.Size  := 18;
  LoadValues;
  if TabProperties.TabIndex = 0 then  
    DisplayEngineProperties(FDistGridDatasource.GetEngine)
  else
    DisplayDetailEngineProperties(FDistGridDatasource.GetEngine);
  FDistGridDatasource.DataChanged;
  FConsGridDatasource.DataChanged;
  DistributionGridBandedTableView.DataController.Groups.FullExpand;
  ConsumptionGridBandedTableView.DataController.Groups.FullExpand;
end;

procedure TfrmMarkerOptimizer.LoadValues;
var
  AEngine: TCSPBaseOptimizer;
  AMemStream: TMemoryStream;
  AXMLDataset: TNativeXml;
  I: integer;
begin
  AMemStream := TMemoryStream.Create;
  AXMLDataset := TNativeXml.Create;
  try
    TBlobField(FDatasource.DataSet.FieldByName('optresult')).SaveToStream(AMemStream);
    if AMemStream.Size > 0 then
    begin
      AMemStream.Position := 0;
      AXMLDataset.Root.Name := 'optresult';
      AXMLDataset.LoadFromStream(AMemStream);
      for I := 0 to EngineTabControl.Tabs.Count - 1 do
      begin
        AEngine := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[I]);
        if AEngine <> nil then
          AEngine.LoadXMLData(AXMLDataset);
      end;
    end;
  finally
    AXMLDataset.Free;
    AMemStream.Free;
  end;
end;

procedure TfrmMarkerOptimizer.PrepareOptimization;
var
  AEngine: TCSPBaseOptimizer;
begin
  AEngine := FDistGridDatasource.GetEngine;
  if AEngine <> nil then
  begin
    if TabProperties.TabIndex = 0 then    
      SaveEngineProperties(AEngine)
    else
      SaveDetailEngineProperties(AEngine);
  end;
end;

procedure TfrmMarkerOptimizer.SaveDetailEngineProperties(
  AObject: TCSPBaseOptimizer);

  function FindChildRow(const ACaption: string; const ATag: integer; AParent: TcxCustomRow): TcxCustomRow;
  var
    I: integer;
  begin
    Result := nil;
    for I := 0 to AParent.Count - 1 do
      if    (TcxEditorRow(AParent.Rows[I]).Properties.Caption = ACaption)
        and (TcxEditorRow(AParent.Rows[I]).Tag = ATag) then
      begin
        Result := AParent.Rows[I];
        break;
      end;      
  end;

var
  ARootNode, ANode: TXmlNode;
  ARowIndex: integer;  
  ARow: TcxEditorRow;
  ACatRow: TcxCustomRow;
  ATag, I: integer;
  ACaption: string;
begin
  if AObject = nil then
    exit;
  ARowIndex := DistributionGridBandedTableView.DataController
                  .DataControllerInfo.FocusedRecordIndex;
  if (FDistGridDatasource.GetRecordCount > 0) and (ARowIndex > -1) and
     (ARowIndex < FDistGridDatasource.GetRecordCount) then
  begin
    ARootNode := AObject.GetProperties(AObject.DistributionRoot.Nodes[ARowIndex]);
    if ARootNode = nil then
      exit;
    for I := 0 to ARootNode.NodeCount - 1 do
    begin
      ANode := ARootNode.Nodes[I];
      ACaption := ANode.ReadAttributeString('category');
      ACatRow := PropertyGrid.RowByCaption(ACaption);
      if ACatRow <> nil then
      begin
        ACaption := ANode.ReadAttributeString('caption');
        ATag := ANode.ReadAttributeInteger('tag');
        ARow := TcxEditorRow(FindChildRow(ACaption, ATag, ACatRow));
        if ARow <> nil then
        begin
          case TFieldType(ANode.ReadAttributeInteger('datatype')) of
            ftFloat, ftBCD:
                ANode.WriteAttributeFloat('value', ARow.Properties.Value);
            ftSmallInt, ftInteger, ftLargeInt:
              begin
                ANode.WriteAttributeInteger('value', ARow.Properties.Value);
              end;
            ftDateTime:
              begin
                ANode.WriteAttributeDateTime('value', ARow.Properties.Value);
              end;
            ftBoolean:
              begin
                ANode.WriteAttributeBool('value', ARow.Properties.Value);
              end;
            ftString:
              begin
                ANode.WriteAttributeString('value', ARow.Properties.Value);
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMarkerOptimizer.SaveEngineProperties(AObject: TCSPBaseOptimizer);
  function FindChildRow(const ACaption: string; const ATag: integer; AParent: TcxCustomRow): TcxCustomRow;
  var
    I: integer;
  begin
    Result := nil;
    for I := 0 to AParent.Count - 1 do
      if    (TcxEditorRow(AParent.Rows[I]).Properties.Caption = ACaption)
        and (TcxEditorRow(AParent.Rows[I]).Tag = ATag) then
      begin
        Result := AParent.Rows[I];
        break;
      end;      
  end;

var
  ARootNode, ANode: TXmlNode;
  ARow: TcxEditorRow;
  ACatRow: TcxCustomRow;
  ATag, I: integer;
  ACaption: string;
begin
  if AObject = nil then
    exit;
  ARootNode := AObject.GetProperties;
  if ARootNode = nil then
    exit;
  for I := 0 to ARootNode.NodeCount - 1 do
  begin
    ANode := ARootNode.Nodes[I];
    ACaption := ANode.ReadAttributeString('category');
    ACatRow := PropertyGrid.RowByCaption(ACaption);
    if ACatRow <> nil then
    begin
      ACaption := ANode.ReadAttributeString('caption');
      ATag := ANode.ReadAttributeInteger('tag');
      ARow := TcxEditorRow(FindChildRow(ACaption, ATag, ACatRow));
      if ARow <> nil then
      begin
        case TFieldType(ANode.ReadAttributeInteger('datatype')) of
          ftFloat, ftBCD:
              ANode.WriteAttributeFloat('value', ARow.Properties.Value);
          ftSmallInt, ftInteger, ftLargeInt:
            begin
              ANode.WriteAttributeInteger('value', ARow.Properties.Value);
            end;
          ftDateTime:
            begin
              ANode.WriteAttributeDateTime('value', ARow.Properties.Value);
            end;
          ftBoolean:
            begin
              ANode.WriteAttributeBool('value', ARow.Properties.Value);
            end;
          ftString:
            begin
              ANode.WriteAttributeString('value', ARow.Properties.Value);
            end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMarkerOptimizer.SaveValues;
var
  AEngine: TCSPBaseOptimizer;
  AMemStream: TMemoryStream;
  AXMLDataset: TNativeXml;
  I: integer;
begin
  if TabProperties.TabIndex = 0 then
    SaveEngineProperties(FDistGridDatasource.GetEngine)
  else
    SaveDetailEngineProperties(FDistGridDatasource.GetEngine);
  AMemStream := TMemoryStream.Create;
  AXMLDataset := TNativeXml.Create;
  try
    AXMLDataset.Root.Name := 'optresult';
    for I := 0 to EngineTabControl.Tabs.Count - 1 do
    begin
      AEngine := TCSPBaseOptimizer(EngineTabControl.Tabs.Objects[I]);
      if AEngine <> nil then
        AEngine.SaveXMLData(AXMLDataset);
    end;
    AXMLDataset.SaveToStream(AMemStream);
    AMemStream.Position := 0;
    FDatasource.DataSet.Edit;
    try
      TBlobField(FDatasource.DataSet.FieldByName('optresult'))
        .LoadFromStream(AMemStream);
      FDatasource.DataSet.Post;
    except
      FDatasource.DataSet.Cancel;
      raise;
    end;
  finally
    AXMLDataset.Free;
    AMemStream.Free;
  end;
end;

class procedure TfrmMarkerOptimizer.ShowOptimizer(AParent: TComponent;
  ADatasource: TDataSource);
begin
  with ADatasource.DataSet do
    if (not Active) or (IsEmpty) then
      exit;
  dmPackage.MaintainOrderStyle(ADatasource.DataSet.FieldByName('ordno').AsString);
  with TfrmMarkerOptimizer.Create(AParent, ADatasource) do
  try
    InitOrder;
    ShowModal;
    // for debugging purpose
    // FOrder.SaveToFile('c:\test.xml');
  finally
    Free;
  end;
end;

procedure TfrmMarkerOptimizer.TabPropertiesChange(Sender: TObject);
begin
  HandlePropTabChanged(Sender);
end;

procedure TfrmMarkerOptimizer.TabPropertiesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  HandlePropTabChanging(Sender, AllowChange);
end;

{ TDistributionDatasource }

constructor TDistributionDatasource.Create(AEngineTab: TcxTabControl);
begin
  FEngineTab := AEngineTab;
end;

procedure TDistributionDatasource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
var
  AEngine: TCSPBaseOptimizer;
  ADistRoot, ARowNode: TXmlNode;
begin
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistRoot := AEngine.DistributionRoot;
    if ADistRoot <> nil then
    begin
      ARowNode := ADistRoot.Nodes[Integer(ARecordHandle)];
      ADistRoot.NodeDelete(ARowNode.IndexInParent);
      DataChanged;
    end;
  end;
end;

function TDistributionDatasource.GetEngine: TCSPBaseOptimizer;
begin
  Result := nil;
  if (FEngineTab.TabIndex > -1) and (FEngineTab.Tabs.Count > 0) then
    Result := TCSPBaseOptimizer(FEngineTab.Tabs.Objects[FEngineTab.TabIndex]);
end;

function TDistributionDatasource.GetRecordCount: Integer;
var
  AEngine: TCSPBaseOptimizer;
  ADistNode: TXmlNode;
begin
  Result := 0;
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistNode := AEngine.DistributionRoot;
    if ADistNode <> nil then
      Result := ADistNode.NodeCount;
  end;
end;

function TDistributionDatasource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  AEngine: TCSPBaseOptimizer;
  AColumnRoot, ADistRoot, ARowNode, AColumnNode: TXmlNode;
  AColumnId: Integer;
begin
  Result := null;
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistRoot := AEngine.DistributionRoot;
    if ADistRoot <> nil then
    begin
      ARowNode := ADistRoot.Nodes[Integer(ARecordHandle)];
      AColumnId := GetDefaultItemID(Integer(AItemHandle));              
      AColumnRoot := AEngine.DistributionColumns;
      if (ARowNode <> nil) and (AColumnRoot.NodeCount > AColumnId) then
      begin
        AColumnNode := AColumnRoot.Nodes[AColumnID];
        case TFieldType(AColumnNode.ReadAttributeInteger('datatype')) of
          ftInteger: Result := ARowNode.ReadAttributeInteger(AColumnNode.Name);
          ftString, ftWideString: Result := ARowNode.ReadAttributeString(AColumnNode.Name);
        end;
      end;
    end;
  end;
end;

procedure TDistributionDatasource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  AEngine: TCSPBaseOptimizer;
  AColumnRoot, ADistRoot, ARowNode, AColumnNode: TXmlNode;
  AColumnId: Integer;
begin
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistRoot := AEngine.DistributionRoot;
    if ADistRoot <> nil then
    begin
      ARowNode := ADistRoot.Nodes[Integer(ARecordHandle)];
      AColumnId := GetDefaultItemID(Integer(AItemHandle));
      AColumnRoot := AEngine.DistributionColumns;
      if (ARowNode <> nil) and (AColumnRoot.NodeCount > AColumnId) then
      begin
        AColumnNode := AColumnRoot.Nodes[AColumnID];
        case TFieldType(AColumnNode.ReadAttributeInteger('datatype')) of
          ftInteger:
            begin
              if AValue = null then              
                ARowNode.WriteAttributeInteger(AColumnNode.Name, 0)
              else
                ARowNode.WriteAttributeInteger(AColumnNode.Name, AValue)
            end;
          ftString:
            begin
              if AValue = null then              
                ARowNode.WriteAttributeString(AColumnNode.Name, EmptyStr)
              else
                ARowNode.WriteAttributeString(AColumnNode.Name, AValue)
            end;          
        end;
      end;
    end;
  end;
end;

{ TConsumptionDatasource }

constructor TConsumptionDatasource.Create(AEngineTab: TcxTabControl);
begin
  FEngineTab := AEngineTab;
end;

procedure TConsumptionDatasource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
var
  AEngine: TCSPBaseOptimizer;
  ADistRoot, ARowNode: TXmlNode;
begin
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistRoot := AEngine.ConsumptionRoot;
    if ADistRoot <> nil then
    begin
      ARowNode := ADistRoot.Nodes[Integer(ARecordHandle)];
      ADistRoot.NodeDelete(ARowNode.IndexInParent);
    end;
    DataChanged;
  end;
end;

function TConsumptionDatasource.GetEngine: TCSPBaseOptimizer;
begin
  Result := nil;
  if (FEngineTab.TabIndex > -1) and (FEngineTab.Tabs.Count > 0) then
    Result := TCSPBaseOptimizer(FEngineTab.Tabs.Objects[FEngineTab.TabIndex]);
end;

function TConsumptionDatasource.GetRecordCount: Integer;
var
  AEngine: TCSPBaseOptimizer;
  ADistNode: TXmlNode;
begin
  Result := 0;
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistNode := AEngine.ConsumptionRoot;
    if ADistNode <> nil then
      Result := ADistNode.NodeCount;
  end;
end;

function TConsumptionDatasource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  AEngine: TCSPBaseOptimizer;
  AColumnRoot, ADistRoot, ARowNode, AColumnNode: TXmlNode;
  AColumnId: Integer;
begin
  Result := null;
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistRoot := AEngine.ConsumptionRoot;
    if ADistRoot <> nil then
    begin
      ARowNode := ADistRoot.Nodes[Integer(ARecordHandle)];
      AColumnId := GetDefaultItemID(Integer(AItemHandle));
      // Result := ARowNode.Read
      AColumnRoot := AEngine.ConsumptionColumns;
      if (ARowNode <> nil) and (AColumnRoot.NodeCount > AColumnId) then
      begin
        AColumnNode := AColumnRoot.Nodes[AColumnID];
        case TFieldType(AColumnNode.ReadAttributeInteger('datatype')) of
          ftInteger: Result := ARowNode.ReadAttributeInteger(AColumnNode.Name);
          ftFloat: Result := ARowNode.ReadAttributeFloat(AColumnNode.Name);
          ftString, ftWideString: Result := ARowNode.ReadAttributeString(AColumnNode.Name);
        end;
      end;
    end;
  end;
end;

procedure TConsumptionDatasource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  AEngine: TCSPBaseOptimizer;
  AColumnRoot, ADistRoot, ARowNode, AColumnNode: TXmlNode;
  AColumnId: Integer;
begin
  AEngine := GetEngine;
  if AEngine <> nil then
  begin
    ADistRoot := AEngine.ConsumptionRoot;
    if ADistRoot <> nil then
    begin
      ARowNode := ADistRoot.Nodes[Integer(ARecordHandle)];
      AColumnId := GetDefaultItemID(Integer(AItemHandle));
      AColumnRoot := AEngine.ConsumptionColumns;
      if (ARowNode <> nil) and (AColumnRoot.NodeCount > AColumnId) then
      begin
        AColumnNode := AColumnRoot.Nodes[AColumnID];
        case TFieldType(AColumnNode.ReadAttributeInteger('datatype')) of
          ftFloat:
            begin
              if AValue = null then
                ARowNode.WriteAttributeFloat(AColumnNode.Name, 0)
              else
                ARowNode.WriteAttributeFloat(AColumnNode.Name, AValue)            
            end;
          ftInteger:
            begin
              if AValue = null then              
                ARowNode.WriteAttributeInteger(AColumnNode.Name, 0)
              else
                ARowNode.WriteAttributeInteger(AColumnNode.Name, AValue)
            end;
          ftString:
            begin
              if AValue = null then              
                ARowNode.WriteAttributeString(AColumnNode.Name, EmptyStr)
              else
                ARowNode.WriteAttributeString(AColumnNode.Name, AValue)
            end;          
        end;
        AEngine.CalculateConsumption(ARowNode);
      end;
    end;
  end;
end;

end.
