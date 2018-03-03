unit frmMainU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CSPConsts, CSPCustomForm, ActnList, StdActns, dxBar,
  cxClasses, Menus, cxGraphics, dxSkinsCore, dxSkinsDefaultPainters,
  cxControls, dxStatusBar, dxDockControl, dxDockPanel, cxPC, ComCtrls,
  cxContainer, cxTreeView, cxListView, ehshelprouter, frmLogU,
  dxBarExtItems, cxEdit, cxTextEdit, cxStyles, cxMaskEdit, cxDropDownEdit,
  cxGrid, cxGridCustomView, cxLookAndFeelPainters,
  dxSkinsdxBarPainter, dxSkinsdxDockControlPainter, dxSkinscxPCPainter,
  dxSkinsdxStatusBarPainter, cxLookAndFeels, ecfextrntools,
  dxSkinsdxRibbonPainter, dxRibbon, dxRibbonStatusBar, ExtCtrls,
  dxNavBar, cxSplitter, dxNavBarCollns, cxCustomData, cxFilter, cxData,
  cxDataStorage, DB, cxDBData, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridCustomPopupMenu, cxGridPopupMenu,
  dxNavBarBase, xfrRptExplr, onguard, ogUtil, frxACRComponents,
  frxDBExporter, fs_dbexprtti, frxDBExpEditor, licmanager;

{$I CSPDefs.inc}  

type
  TFormClass = class of TForm;
  
  PPersistInfo = ^TPersistInfo;
  TPersistInfo = record
    WinControl: TWinControl;
    SpecInfo: Integer;
  end;

  TCSPDaysCode = class(TOgDaysCode)
  protected
    procedure DoOnChecked(Value : TCodeStatus); override;
    function DoOnGetCode : TCode; override;
    procedure DoOnGetKey(var Key : TKey); override;
    procedure DoOnChangeCode(Value : TCode); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TECFNavigationMode = (nvNavBar, nvExplorer);
  TECFModuleInstanceType = (itForm, itPanel);

  TECFNavBar = class(TdxNavBar)
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TOrderField = class(TCollectionItem)
  private
    FFieldName: string;
    FCaption: string;
  public
    property FieldName: string read FFieldName write FFieldName;
    property Caption: string read FCaption write FCaption;
  end;

  TOrderFields = class(TOwnedCollection)
  private
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;

    procedure SetFields(const AFields, ACaptions: string);
    procedure GetCaptionList(AList: TStrings);
    function AddFieldDef(const AField, ACaption: string): TOrderField;
  end;

  TfrmMain = class(TCSPForm)
    STDActions: TActionList;
    actPackageNew: TAction;
    actPackageOpen: TAction;
    actExit: TAction;
    actEditCut1: TAction;
    actEditCopy1: TAction;
    actEditPaste1: TAction;
    actEditSelectAll1: TAction;
    actEditUndo1: TAction;
    actRefresh: TAction;
    BarManager: TdxBarManager;
    stdBarMain: TdxBar;
    stdBtnPackageNew: TdxBarButton;
    stdBtnPackageOpen: TdxBarButton;
    stdBtnExit: TdxBarButton;
    Package1: TdxBarSubItem;
    Order1: TdxBarSubItem;
    stdBtnOrderRefresh: TdxBarButton;
    References1: TdxBarSubItem;
    View1: TdxBarSubItem;
    Options1: TdxBarButton;
    Tools1: TdxBarSubItem;
    About1: TdxBarButton;
    Help1: TdxBarSubItem;
    stdBarStandard: TdxBar;
    StatusBar: TdxStatusBar;
    dxDockingManager1: TdxDockingManager;
    DockSite: TdxDockSite;
    DockMainPanel: TdxDockPanel;
    DockLogPanel: TdxDockPanel;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxLayoutDockSite3: TdxLayoutDockSite;
    ViewLogPanel: TdxBarButton;
    itmToolbar: TdxBarToolbarsListItem;
    HelpAbout: TAction;
    HelpReadme: TAction;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarButton6: TdxBarButton;
    HelpContent: TAction;
    HelpSearch: TAction;
    HelpOnHelp: TAction;
    MainPopupMenu: TPopupMenu;
    actWelcome: TAction;
    actOptions: TAction;
    actRptExplorer: TAction;
    dxBarButton9: TdxBarButton;
    itmShowDesigner: TdxBarButton;
    actOrderPrint: TAction;
    stdBtnOrderPrint: TdxBarButton;
    itmTheme: TdxBarSubItem;
    itmLanguage: TdxBarSubItem;
    actExternTools: TAction;
    dxBarButton1: TdxBarButton;
    itmExternalTools: TdxBarSubItem;
    Ribbon: TdxRibbon;
    RibbonTabHelp: TdxRibbonTab;
    rbnBtnRefresh: TdxBarLargeButton;
    rbnBtnView: TdxBarSubItem;
    rbnBtnTools: TdxBarSubItem;
    rbnBarQuickAccess: TdxBar;
    rbnBarOnlineGuide: TdxBar;
    rbnBtnHelpContent: TdxBarLargeButton;
    rbnBtnReadme: TdxBarLargeButton;
    rbnBtnHelpSearch: TdxBarLargeButton;
    rbnBtnAbout: TdxBarLargeButton;
    rbnBtnReport: TdxBarLargeButton;
    actPackageClose: TAction;
    stdBtnPackageClose: TdxBarButton;
    actPackageProperties: TAction;
    stdBtnPackageProperties: TdxBarButton;
    dxRibbonQuickAccessGroupButton1: TdxRibbonQuickAccessGroupButton;
    RibbonTabPackage: TdxRibbonTab;
    rbnBarPackage: TdxBar;
    rbnBtnNewPackage: TdxBarLargeButton;
    rbnBtnOpenPackage: TdxBarButton;
    rbnBtnPackageProps: TdxBarButton;
    rbnBtnClosePackage: TdxBarButton;
    rbnBarOrders: TdxBar;
    actOrderNew: TAction;
    rbnBtnOrderNew: TdxBarLargeButton;
    actOrderEdit: TAction;
    actOrderDelete: TAction;
    rbnBtnOrderEdit: TdxBarButton;
    rbnBtnOrderDelete: TdxBarButton;
    actOrderFind: TAction;
    actOrderOptimize: TAction;
    rbnBtnOrderFind: TdxBarButton;
    rbnBtnOrderOptimize: TdxBarLargeButton;
    rbnBtnOrderPrint: TdxBarLargeButton;
    rbnBarMisceleanous: TdxBar;
    stdBtnOrderNew: TdxBarButton;
    stdBtnOrderEdit: TdxBarButton;
    stdBtnOrderDelete: TdxBarButton;
    stdBtnOrderFind: TdxBarButton;
    stdBtnOrderOptimize: TdxBarButton;
    stdBtnRefUoM: TdxBarButton;
    stdBtnRefFeatures: TdxBarButton;
    stdBtnRefCustomers: TdxBarButton;
    stdBtnRefLayingRule: TdxBarButton;
    stdBtnRefStyle: TdxBarButton;
    stdBtnRefItems: TdxBarButton;
    rbnBarReferences: TdxBar;
    actRefUoM: TAction;
    actRefFeatures: TAction;
    actRefLayingRule: TAction;
    actRefItems: TAction;
    actRefCustomers: TAction;
    actRefStyles: TAction;
    rbnBtnRefUoM: TdxBarLargeButton;
    rbnBtnFeatures: TdxBarLargeButton;
    rbnBtnLayingRule: TdxBarLargeButton;
    rbnBtnItems: TdxBarLargeButton;
    rbnBtnCustomers: TdxBarLargeButton;
    rbnBtnStyles: TdxBarLargeButton;
    OrderNavBar: TdxNavBar;
    OrderNavBarOrderGroup: TdxNavBarGroup;
    OrderNavBarOrderGroupControl: TdxNavBarGroupControl;
    GridOrder: TcxGrid;
    GridOrderDBTableView1: TcxGridDBTableView;
    GridOrderLevel1: TcxGridLevel;
    actRefMaterials: TAction;
    stdBtnRefMaterials: TdxBarButton;
    rbnBtnMaterials: TdxBarLargeButton;
    Neworder1: TMenuItem;
    Editorder1: TMenuItem;
    Deleteorder1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Find1: TMenuItem;
    Optimize1: TMenuItem;
    Print1: TMenuItem;
    N3: TMenuItem;
    References2: TMenuItem;
    Items1: TMenuItem;
    N4: TMenuItem;
    UnitofMeasurement1: TMenuItem;
    Features1: TMenuItem;
    LayingRule1: TMenuItem;
    Materials1: TMenuItem;
    Customers1: TMenuItem;
    N5: TMenuItem;
    Styles1: TMenuItem;
    ReportExplorer1: TMenuItem;
    RefreshView1: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    Print2: TMenuItem;
    BarAppMenu: TdxBarApplicationMenu;
    actRepairPackage: TAction;
    actCompactPackage: TAction;
    itmMaintenance: TdxBarSubItem;
    stdBtnRepairPackage: TdxBarButton;
    stdBtnCompactPackage: TdxBarButton;
    actRepairCompactExternPackage: TAction;
    itmRepairCompactExtrnPkg: TdxBarButton;
    procedure actExitExecute(Sender: TObject);
    procedure ViewLogPanelClick(Sender: TObject);
    procedure DockLogPanelVisibleChanged(Sender: TdxCustomDockControl);
    procedure HideDockControl(Sender: TdxDockSite;
      AControl: TdxCustomDockControl);
    procedure ShowDockControl(Sender: TdxDockSite;
      AControl: TdxCustomDockControl);
    procedure actRefreshExecute(Sender: TObject);
    procedure STDActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure HelpContents1Execute(Sender: TObject);
    procedure HelpTopicSearch1Execute(Sender: TObject);
    procedure HelpOnHelp1Execute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure HelpReadmeExecute(Sender: TObject);
    procedure actWelcomeExecute(Sender: TObject);
    procedure actEditCut1Execute(Sender: TObject);
    procedure actEditCopy1Execute(Sender: TObject);
    procedure actEditPaste1Execute(Sender: TObject);
    procedure actEditSelectAll1Execute(Sender: TObject);
    procedure actEditUndo1Execute(Sender: TObject);
    procedure actOrderPrintExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BarManagerBarAfterReset(Sender: TdxBarManager; ABar: TdxBar);
    procedure actExternToolsExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actPackageNewExecute(Sender: TObject);
    procedure actPackageOpenExecute(Sender: TObject);
    procedure actPackagePropertiesExecute(Sender: TObject);
    procedure actPackageCloseExecute(Sender: TObject);
    procedure actRefUoMExecute(Sender: TObject);
    procedure actRefFeaturesExecute(Sender: TObject);
    procedure actRefLayingRuleExecute(Sender: TObject);
    procedure actRefItemsExecute(Sender: TObject);
    procedure actRefCustomersExecute(Sender: TObject);
    procedure actRefStylesExecute(Sender: TObject);
    procedure actRefMaterialsExecute(Sender: TObject);
    procedure actOrderNewExecute(Sender: TObject);
    procedure actOrderEditExecute(Sender: TObject);
    procedure actOrderOptimizeExecute(Sender: TObject);
    procedure actOrderDeleteExecute(Sender: TObject);
    procedure actOrderFindExecute(Sender: TObject);
    procedure GridOrderDBTableView1DblClick(Sender: TObject);
    procedure GridOrderDBTableView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actRptExplorerExecute(Sender: TObject);
    procedure BarAppMenuExtraPaneItemClick(Sender: TObject; AIndex: Integer);
    procedure actRepairPackageExecute(Sender: TObject);
    procedure actCompactPackageExecute(Sender: TObject);
    procedure actRepairCompactExternPackageExecute(Sender: TObject);
  private
    FReporter: TxfrReportExplorer;
    FNavBar: TECFNavBar;
    FSplitter: TcxSplitter;
    FHelpRouter: THelpRouter;
    FLogForm: TfrmLog;
    FFirstRun: boolean;
    FDisableUpdateState: boolean;
    FExtrnTools: TExternalTools;
    FOrderFields: TOrderFields;
    FConstructed: Boolean;
    FDatasource: TDataSource;
    FOGDaysCode: TCSPDaysCode;
    FLicenseManager: TCSPLicenseManager;
    FACRComponent: TfrxACRComponents;

    procedure UpdatePackageStatus(Sender: TObject);

    function GridItemIndex(const AIndex: integer): integer;
    function GetLogPanelVisible: boolean;

    procedure SetLogPanelVisible(const Value: boolean);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure ReadMainForm;
    procedure WriteMainForm;

    procedure ResetMenubar;
    procedure ConstructModule;
    function GetShowDesigner: boolean;
    procedure SetShowDesigner(const Value: boolean);
    procedure SelectTheme(Sender: TObject);
    procedure SelectLanguage(Sender: TObject);
    procedure ExecuteTool(Sender: TObject);
    procedure ActiveControlChanged(Sender: TObject) ;
  protected
    procedure PerformAppMsgProc(var Message: TAppMessage); override;
    procedure PopulateAppMsgproc(var Message: TAppMessage); override;

    procedure Loaded; override;

    procedure DoFindOrder;

    procedure InternalPrepare; override;
    procedure InternalUnprepare; override;
    procedure InternalUpdateState; override;
    procedure InternalInitialize; override;
    procedure InternalFinalize; override;

    procedure ECFHandleException(Sender: TObject; E: Exception);

    procedure AutoHideChanged(Sender: TdxCustomDockControl);
    procedure AutoHideChanging(Sender: TdxCustomDockControl);
    procedure StartDock(Sender: TdxCustomDockControl; X, Y: Integer);
    procedure EndDock(Sender: TdxCustomDockControl; Zone: TdxZone; X, Y: Integer);
    procedure HookupEvents(Sender: TdxCustomDockControl);
    procedure CreateTabContainer(Sender: TdxCustomDockControl; ATabContainer: TdxTabContainerDockSite);
    procedure CreateSiteContainer(Sender: TdxCustomDockControl; ASideContainer: TdxSideContainerDockSite);
    procedure CreateFloatSite(Sender: TdxCustomDockControl; AFloatSite: TdxFloatDockSite);

    function CreateFrame(AFrameClass: TFormClass; AOwner: TdxDockPanel): TForm;

    procedure CreateThemeList;
    procedure CreateLangList;
    procedure CreateToolsList;

    procedure ConstructResources;
    procedure UpdateToolbar;

    procedure ConstructOrderFieldView;
    procedure ConstructOrderView;
    procedure DeconstructOrderview;

    procedure DoExecReferences(const AMsgID: Cardinal);
    procedure DoExecRefPkg(const AMsgID: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure AdjustTitle(const ATitle: string);
    procedure CheckAutoOpen;

    property HelpRouter: THelpRouter read FHelpRouter;

    property LicenseManager: TCSPLicenseManager read FLicenseManager;

  published
    property LogPanelVisible: boolean
      read GetLogPanelVisible write SetLogPanelVisible;

    property ShowDesigner: boolean
      read GetShowDesigner write SetShowDesigner;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  NativeRegXML, shlobj, CSPAppUtil, TPropertiesUnit,
  clipbrd, TypInfo, ecfnavres, shellapi, frmAboutU, dmMainU,
  dmReporterU, ecfutils, gnugettext, dmResU,
  languagecodes, frmOptionsU, ecflangutil, dxNavBarConsts,
  ecfnavexceptions, dmPackageU, dmReferencesU, frmBrowserU, frmUoMU,
  frmFeatureU, frmLayingRuleU, frmMaterialU, frmCustomerU, frmStylesU, 
  frmItemU, frmOrderU, frmFindU, frmPropU, frmMarkerOptimizerU;

const
  CKey : TKey = ($0E,$1F,$B4,$09,$CD,$21,$B8,$01,
                 $4C,$CD,$21,$90,$90,$54,$68,$73);

type
  TSetupActionType = (stVisibility, stEnability);
  TSetupActionTypes = set of TSetupActionType;
  THackedECFForm = class(TCSPForm);

{$R *.dfm}
{.$R ecfnav.tlb}

procedure SetupActions(Actions: TActionList; const AValue: boolean;
  const AType: TSetupActionTypes; const AInclue: string = '';
  const AExclude: string = '');
var
  i, APos: integer;
  AStr: string;
  BInclude, BExclude: boolean;
  SInclude, SExclude: TStrings;
  AAction: TAction;
begin
  SInclude := TStringList.Create;
  SExclude := TStringList.Create;
  try
    BInclude := (AInclue <> EmptyStr);
    BExclude := (AExclude <> EmptyStr);
    if BInclude then
    begin
      APos := 1;
      AStr := ExtractFieldName(AInclue, APos);
      while AStr <> EmptyStr do
      begin
        SInclude.Add(AStr);
        AStr := ExtractFieldName(AInclue, APos);
      end;
    end;
    if BExclude then
    begin
      APos := 1;
      AStr := ExtractFieldName(AExclude, APos);
      while AStr <> EmptyStr do
      begin
        SExclude.Add(AStr);
        AStr := ExtractFieldName(AExclude, APos);
      end;
    end;
    for I := 0 to Actions.ActionCount-1 do
    begin
      AAction := TAction(Actions.Actions[i]);
      if BInclude then
      begin
        APos := SInclude.IndexOf(AAction.Category);
        if APos >= 0 then
        begin
          if (stVisibility in AType) then AAction.Visible := AValue;
          if (stEnability in AType) then AAction.Enabled := AValue;
        end;
      end else
      if BExclude then
      begin
        APos := SExclude.IndexOf(AAction.Category);
        if APos = -1 then
        begin
          if (stVisibility in AType) then AAction.Visible := AValue;
          if (stEnability in AType) then AAction.Enabled := AValue;
        end;
      end else
      begin
        if (stVisibility in AType) then
          TAction(Actions.Actions[i]).Visible := AValue;
        if (stEnability in AType) then
          TAction(Actions.Actions[i]).Enabled := AValue;
      end;
    end;
  finally
    SInclude.Free;
    SExclude.Free;
  end;
end;

function GetFocusedControl(AWinControl: TWinControl): TWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to AWinControl.ControlCount - 1 do
    if AWinControl.Controls[i] is TWinControl then
    begin
      Result := GetFocusedControl(TWinControl(AWinControl.Controls[i]));
      if Result <> nil then Exit;
      if TWinControl(AWinControl.Controls[i]).Focused then
      begin
        Result := TWinControl(AWinControl.Controls[i]);
        Exit;
      end;
    end;
end;

procedure StorePersistentInfo(Sender: TdxCustomDockControl);
var
  AWinControl: TWinControl;
  p: PPersistInfo;
begin
  if Sender.Visible and (Sender.Tag = 0) then
  begin
    AWinControl := GetFocusedControl(Sender);
    if AWinControl <> nil then
    begin
      New(p);
      p^.WinControl := AWinControl;
      Sender.Tag := Integer(p);
    end;
  end;
end;

procedure RestorePersistentInfo(Sender: TdxCustomDockControl);
begin
  if Sender.Visible and (Sender.Tag <> 0) then
  begin
    if PPersistInfo(Sender.Tag)^.WinControl.CanFocus then
      PPersistInfo(Sender.Tag)^.WinControl.SetFocus;
    Dispose(PPersistInfo(Sender.Tag));
    Sender.Tag := 0;
  end;
end;

{ TOrderFields }

function TOrderFields.AddFieldDef(const AField, ACaption: string): TOrderField;
begin
  Result := TOrderField(Add);
  Result.FieldName := AField;
  Result.Caption := ACaption;
end;

constructor TOrderFields.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOrderField);
end;

destructor TOrderFields.Destroy;
begin
  inherited Destroy;
end;

procedure TOrderFields.GetCaptionList(AList: TStrings);
var
  I: integer;
begin
  AList.Clear;
  for I := 0 to Count - 1 do
    AList.Add(TOrderField(Items[i]).Caption);
end;

procedure TOrderFields.SetFields(const AFields, ACaptions: string);
var
  APos, BPos: integer;
  AField, ACaption: string;
begin
  APos := 1;
  BPos := 1;
  Clear;
  while APos <= Length(AFields) do
  begin
    AField := ExtractFieldName(AFields, APos);
    if BPos <= Length(ACaptions)  then
      ACaption := ExtractFieldName(ACaptions, BPos);
    AddFieldDef(AField, ACaption);
  end;  
end;

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOGDaysCode := TCSPDaysCode.Create(Self);
  FLicenseManager := TCSPLicenseManager.Create(Self);
  FConstructed := False;
  FDatasource  := nil;
  FOrderFields := TOrderFields.Create(Self);
  FReporter := TxfrReportExplorer.Create(Self);
  FReporter.frxReport := dmReporter.frxReport;
  FReporter.ReportFileName := GetAppIDClass.ApplicationReportPath+'.rpt';
  FReporter.DataSourceMode := dsFile;
  FNavBar := TECFNavBar.Create(Self);
  FNavBar.Parent := Self;
  FNavBar.Left := 2;
  FNavBar.Width := 200;
  FNavBar.Align := alLeft;
  FNavBar.Visible := True;
  FNavBar.LargeImages := dmResources.LargeImages;
  FNavBar.SmallImages := dmResources.SmallImages;
  FSplitter := TcxSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Left := 168;
  FSplitter.Width := 4;
  FSplitter.Control := FNavBar;
  FSplitter.Visible := True;
  FExtrnTools := TExternalTools.Create(Self);
  FDisableUpdateState := False;
  FHelpRouter := THelpRouter.Create(self);
  FHelpRouter.HelpType := htHTMLhelp;
  Application.HelpFile := ChangeFileExt(Application.ExeName,'.chm');
  FLogForm := TfrmLog(CreateFrame(TfrmLog, DockLogPanel));
  Application.OnException := ECFHandleException;
  Application.OnIdle := AppIdle;
  HookupEvents(DockLogPanel);
  FFirstRun := True;
  Ribbon.ColorSchemeName := dmMain.SkinController.SkinName;
  UpdateToolbar;
  // Screen.OnActiveControlChange := ActiveControlChanged;
  Ribbon.ActiveTab := RibbonTabPackage;
  ReadMainForm;
  OrderNavBar.View := dxNavBarSkinNavigatorPaneView;
  OrderNavBar.OptionsBehavior.NavigationPane.AdjustWidthByPopup := True;
  OrderNavBar.OptionsBehavior.NavigationPane.Collapsible := False;
  OrderNavBar.OptionsView.Common.ShowGroupCaptions := False;
  OrderNavBar.OptionsView.NavigationPane.ShowOverflowPanel := False;
  TabStop := False;
  dmReferences.EnsureRefExists;
  dmPackage.NotifyMainForm := UpdatePackageStatus;
  ConstructModule;
  UpdatePackageStatus(nil);
  {$ifdef COMMERCIAL}
  // FOGDaysCode.Loaded;
  FLicenseManager.Loaded;
  {$endif}
  FACRComponent := TfrxACRComponents.Create(Self);
  FACRComponent.DefaultSession := dmReferences.RefSession.SessionName;
  FACRComponent.DefaultDatabase:= dmReferences.RefConnection.DatabaseName;
  AdjustTitle(EmptyStr);
end;

procedure TfrmMain.DeconstructOrderview;
begin
  FConstructed := False;
  FDatasource.DataSet.Active := False;
  FDatasource.DataSet.Free;
  FreeAndNil(FDatasource);
  FOrderFields.Clear;
  ConstructOrderFieldView;
  GridOrderDBTableView1.FilterRow.Visible := False;
  GridOrderDBTableView1.OptionsView.GroupByBox := False;
end;

destructor TfrmMain.Destroy;
begin
  FACRComponent.Free;
  FOGDaysCode.Free;
  dmPackage.NotifyMainForm := Nil;
  FreeAndNil(FHelpRouter);
  FreeAndNil(FExtrnTools);
  FreeAndNil(FOrderFields);
  Screen.OnActiveControlChange := nil;
  inherited Destroy;
end;

procedure TfrmMain.InternalFinalize;
var
  I, ACount: integer;
begin
  inherited InternalFinalize;
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      WriteString('Language',GetAppIDClass.DefaultLanguangeUsed);
      WriteString('Version', GetAppVersionInfo);
      WriteInteger('navigation_width',FNavBar.Width);
      WriteInteger('navigation_lastgroup',FNavBar.ActiveGroupIndex);
      CloseKey;
    end;
    if OpenKey(GetAppIDClass.RegistryPath+'\Recent packages', False) then
    begin
      closeKey;
      if OpenKey(GetAppIDClass.RegistryPath, False) then
      begin      
        deleteKey('Recent packages');
        closeKey;
      end;
    end;
    if OpenKey(GetAppIDClass.RegistryPath+'\Recent packages', True) then
    begin
      ACount := BarAppMenu.ExtraPane.Items.Count;
      if ACount > 10 then
        ACount := 10;
      for I := 0 to ACount - 1 do
      begin
        writeString(BarAppMenu.ExtraPane.Items[i].Text,
          BarAppMenu.ExtraPane.Items[i].DisplayText);      
      end;
    end;
    CloseKey;    
    OpenKey(GetAppIDClass.RegistryPath+'\MainForm', True);
    Close;
  finally
    Free;
  end;
  WriteMainForm;
end;

procedure TfrmMain.InternalInitialize;
var
  AFile: string;
  I: integer;
  AStrList: TStringList;
  AItem: TdxBarExtraPaneItem;
begin
  inherited InternalInitialize;
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath, True) then
    begin
      if ValueExists('Language') then
        GetAppIDClass.DefaultLanguangeUsed := ReadString('Language')
      else
        WriteString('Language', GetAppIDClass.DefaultLanguangeUsed);
      if ValueExists('navigation_width') then
        FNavBar.Width  := ReadInteger('navigation_width');
      if ValueExists('navigation_lastgroup') then
        FNavBar.ActiveGroupIndex := ReadInteger('navigation_lastgroup');
      CloseKey;
    end;
    BarAppMenu.ExtraPane.Items.Clear;
    if OpenKey(GetAppIDClass.RegistryPath+'\Recent packages', False) then
    begin
      AStrList := TStringList.Create;
      try
        getValueNames(AStrList);
        for I := 0 to AStrList.Count - 1 do
        begin
          AItem := BarAppMenu.ExtraPane.Items.Add;
          AItem.Text := AStrList[i];
          AItem.DisplayText := readString(AStrList[i]);
        end;        
      finally
        AStrList.Free;
      end;          
    end;    
  finally
    Free;
  end;
  AFile := GetAppIDClass.UserDataPath+'\'+SCSPExtrnToolsDataFile;
  if FileExists(AFile) then
    FExtrnTools.LoadFromFile(AFile);
  UseLanguage(GetAppIDClass.DefaultLanguangeUsed);
  ConstructResources;
  CreateThemeList;
  ResetMenubar;
  CreateLangList;
  CreateToolsList;
end;

procedure TfrmMain.InternalPrepare;
begin
  if FFirstRun then
  begin
    FFirstRun := True;
    ExecCommand := 'IWELCOME';
  end;
end;

procedure TfrmMain.InternalUnprepare;
begin
end;

procedure TfrmMain.InternalUpdateState;
begin
  Application.ProcessMessages;
  if FDisableUpdateState then
    exit;
  BarManager.LockUpdate := True;
  try
    UpdatePackageStatus(Self);
    actOptions.Enabled := True;
    actExit.Enabled := True;
    actExternTools.Enabled := True;
  finally
    BarManager.LockUpdate := False;
  end;
end;

procedure TfrmMain.Loaded;
begin
  inherited Loaded;
end;

procedure TfrmMain.PerformAppMsgProc(var Message: TAppMessage);
const
  sEmptyStr: PChar = '';
var
  AStr: String;
  sHelpFile: string;
  rptValues: Variant;
begin
  case Message.Msg of
    CMD_PKG_NEW:
      begin
        dmMain.SaveDialog.FileName := '';
        dmMain.SaveDialog.Title := _(SCSPNewPkg);
        dmMain.SaveDialog.DefaultExt := SCSPPkgExt;
        dmMain.SaveDialog.Filter := SCSPPkgFilter;
        dmMain.SaveDialog.Options := [ofHideReadOnly, ofEnableSizing, ofOverwritePrompt];
        if dmMain.SaveDialog.Execute(Self.Handle) then
        begin
          dmPackage.CreatePackage(dmMain.SaveDialog.FileName);
          TfrmProperties.ShowProperties;          
        end;
      end;
    CMD_PKG_OPEN:
      begin
        dmMain.OpenDialog.FileName := '';
        dmMain.OpenDialog.Title := _(SCSPOpenPkg);
        dmMain.OpenDialog.DefaultExt := SCSPPkgExt;
        dmMain.OpenDialog.Filter := SCSPPkgFilter;
        dmMain.OpenDialog.Options := [ofHideReadOnly, ofEnableSizing];
        if dmMain.OpenDialog.Execute(Self. Handle) then
          dmPackage.OpenPackage(dmMain.OpenDialog.FileName);        
      end;
    CMD_PKG_CLOSE:
      begin
        if dmPackage.Opened then
          dmPackage.ClosePackage;
      end;
    CMD_PKG_PROP:
      begin
        TfrmProperties.ShowProperties;      
      end;
    CMD_ORD_NEW..CMD_ORD_DELETE:
      begin
        with TfrmOrder.Create(Self) do
        try
          DoAction(Message.Msg, Self.FDatasource.DataSet);
        finally
          Free;
        end;
      end;
    CMD_ORD_FIND:
      begin
        DoFindOrder;      
      end;
    CMD_ORD_OPTIMIZE:
      begin
        TfrmMarkerOptimizer.ShowOptimizer(Self, FDatasource);      
      end;
    CMD_REF_UOM .. CMD_REF_CUSTOMERS:
      begin
        if not dmReferences.Opened then
          raise Exception.Create(_(SERRRefDataNotExist));
        DoExecReferences(Message.Msg);
      end;
    CMD_REF_ITEMS .. CMD_REF_STYLES:
      begin
        if not dmPackage.Opened then
          raise Exception.Create(_(SERRPkgDataNotExist));
        DoExecRefPkg(Message.Msg);      
      end;
    CMD_RECONSTRUCTMODS:
      begin
        ConstructModule;
      end;
    CMD_HLP_CONTENTS:
        begin
          Application.HelpCommand(HELP_CONTENTS,0);
        end;
      CMD_HLP_SEARCH:
        begin
          Application.HelpCommand(HELP_PARTIALKEY, Longint(sEmptyStr));
        end;
      CMD_HLP_HOW:
        begin
          sHelpFile := Application.HelpFile;
          Application.HelpFile := 'winhelp.hlp';
          Application.HelpCommand(HELP_FINDER,0);
          Application.HelpFile := sHelpFile;
        end;
      CMD_HLP_README:
        begin
          {Open Notepad for readme}
          ShellExecute(0,'Open','wordpad.exe',
             PChar(APP_READMEFILE), PChar(GetAppIDClass.ApplicationPath),SW_SHOW);
        end;
      CMD_HLP_ABOUT :
        begin
          // Sample code you can use it on your application
          with TfrmAbout.Create(application) do
          try
            ShowModal;
          finally
            Free;
          end;
        end;
      CMD_EXECPRINT:
        begin
          rptValues := Variant(Ptr(Message.Param)^);
          FReporter.ExecuteReport(rptValues, ShowDesigner);
        end;
  else
    inherited PerformAppMsgProc(Message);
  end;
end;

procedure TfrmMain.PopulateAppMsgproc(var Message: TAppMessage);
begin
  inherited;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actExternToolsExecute(Sender: TObject);
var
  AFile: string;
begin
  FExtrnTools.ManageTools;
  AFile := GetAppIDClass.UserDataPath+'\'+SCSPExtrnToolsDataFile;
  FExtrnTools.SaveToFile(AFile);
  ResetMenubar;
end;

function TfrmMain.GetLogPanelVisible: boolean;
begin
  Result := DockLogPanel.Visible;
  ViewLogPanel.Down := Result;
end;

procedure TfrmMain.SelectLanguage(Sender: TObject);
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    DefaultInstance.GetListOfLanguages(DefaultTextDomain, AList);
    if (TdxBarButton(Sender).Tag < AList.Count) and
       (AList[TdxBarButton(Sender).Tag] <> GetAppIDClass.DefaultLanguangeUsed) then
    begin
      GetAppIDClass.DefaultLanguangeUsed := AList[TdxBarButton(Sender).Tag];
      UseLanguage(GetAppIDClass.DefaultLanguangeUsed);
      ConstructResources;
      UpdateToolbar;
      ShowMessage(_('You should restart this application to make your changes fully applied'));
    end;
  finally
    AList.Free;
  end;
end;

procedure TfrmMain.SelectTheme(Sender: TObject);
var
  ASkin: string;
begin
  if Sender is TdxBarButton then
  begin
    if TdxBarButton(Sender).Tag = 0 then
    begin
      if SameText(GetAppIDClass.Properties.GetProperty('ribbonbar', 'false'), 'true') then
      begin
        dmMain.SkinCollection.SkinName := 'Office 2007 Blue';
        dmMain.SkinCollection.UseSkin := True;
      end else
        dmMain.SkinCollection.UseSkin := False;
    end else
    begin
      if dmMain.SkinCollection.SkinNames.Count > (TdxBarButton(Sender).Tag-1) then
      begin
        ASkin := dmMain.SkinCollection.SkinNames[TdxBarButton(Sender).Tag-1];
        dmMain.SkinCollection.SkinName := ASkin;
        dmMain.SkinCollection.UseSkin := True;
      end else
        dmMain.SkinCollection.UseSkin := False;      
    end;
    if not dmMain.SkinCollection.UseSkin then
      GetAppIDClass.Properties.SetProperty('ribbonbar', 'false');
    UpdateToolbar;
  end;
end;

procedure TfrmMain.SetLogPanelVisible(const Value: boolean);
begin
  DockLogPanel.Visible := Value;
  ViewLogPanel.Down := Value;
  if (DockLogPanel.Parent is TdxTabContainerDockSite) and (DockLogPanel.Visible) then
    TdxTabContainerDockSite(DockLogPanel.Parent).ActiveChild := DockLogPanel;
  if DockLogPanel.Visible then
    DockLogPanel.Perform(WM_NCPAINT, 0, 0);
end;

procedure TfrmMain.ViewLogPanelClick(Sender: TObject);
begin
  if DockLogPanel.AutoHide and ViewLogPanel.Down then
    DockLogPanel.AutoHide := False;
  LogPanelVisible := ViewLogPanel.Down;
end;

procedure TfrmMain.DockLogPanelVisibleChanged(
  Sender: TdxCustomDockControl);
begin
  LogPanelVisible := DockLogPanel.Visible;
end;

procedure TfrmMain.DoExecReferences(const AMsgID: Cardinal);
begin
  case AMsgID of
    CMD_REF_UOM:
      begin
        TfrmUoM.DoBrowse;        
      end;
    CMD_REF_FEATURES:
      begin
        TfrmFeature.DoBrowse;
      end;
    CMD_REF_LAYINGRULES:
      begin
        TfrmLayingRule.DoBrowse;        
      end;
    CMD_REF_MATERIALS:
      begin
        TfrmMaterial.DoBrowse;
      end;
    CMD_REF_CUSTOMERS:
      begin
        TfrmCustomer.DoBrowse;
      end;
  end;
end;

procedure TfrmMain.DoExecRefPkg(const AMsgID: Cardinal);
begin
  case AMsgID of
    CMD_REF_ITEMS:
      begin
        TfrmItem.DoBrowse;
      end;
    CMD_REF_STYLES:
      begin
        TfrmStyle.DoBrowse;
      end;
  end;
end;

procedure TfrmMain.DoFindOrder;
var
  AList: TStringList;
  AText: string;
  AIndex, AFieldIndex: integer;
begin
  AList := TStringList.Create;
  try
    FOrderFields.GetCaptionList(AList);
    if TfrmFind.ShowFindDialog(Self, AText, AIndex, AList) then
    begin
      AFieldIndex := GridItemIndex(AIndex);
      GridOrderDBTableView1.DataController.Search.Locate(AFieldIndex, AText);           
    end;    
  finally
    AList.Free;
  end;
end;

procedure TfrmMain.ECFHandleException(Sender: TObject; E: Exception);
begin
  if not Application.Terminated then
  begin
    dmMain.Logger.Error(E);
    if not LogPanelVisible then
      LogPanelVisible := True;
    Application.ShowException(E);
  end;
end;

procedure TfrmMain.AdjustTitle(const ATitle: string);
begin
  if ATitle <> EmptyStr then
    Self.Caption := Format('%s - %s', [GetAppIDClass.Title, ATitle])
  else
    Self.Caption := GetAppIDClass.Title;
end;

procedure TfrmMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
end;

procedure TfrmMain.AutoHideChanged(Sender: TdxCustomDockControl);
begin
  RestorePersistentInfo(Sender);
end;

procedure TfrmMain.AutoHideChanging(Sender: TdxCustomDockControl);
begin
  StorePersistentInfo(Sender);
end;

procedure TfrmMain.BarAppMenuExtraPaneItemClick(Sender: TObject;
  AIndex: Integer);
begin
  dmPackage.OpenPackage(BarAppMenu.ExtraPane.Items[AIndex].Text);
end;

procedure TfrmMain.BarManagerBarAfterReset(Sender: TdxBarManager; ABar: TdxBar);
begin
  CreateThemeList;
  CreateLangList;
  CreateToolsList;
end;

procedure TfrmMain.CreateFloatSite(Sender: TdxCustomDockControl;
  AFloatSite: TdxFloatDockSite);
begin
  HookupEvents(AFloatSite);
end;

procedure TfrmMain.CreateSiteContainer(Sender: TdxCustomDockControl;
  ASideContainer: TdxSideContainerDockSite);
begin
  HookupEvents(ASideContainer);
end;

procedure TfrmMain.CreateTabContainer(Sender: TdxCustomDockControl;
  ATabContainer: TdxTabContainerDockSite);
begin
  HookupEvents(ATabContainer);
end;

procedure TfrmMain.CreateThemeList;
var
  I: integer;
  AItem: TdxBarButton;
begin
  while itmTheme.ItemLinks.Count > 0 do
    if itmTheme.ItemLinks[0].Item <> nil then
    begin
      AItem := TdxBarButton(itmTheme.ItemLinks[0].Item);
      itmTheme.ItemLinks[0].Free;
      AItem.Free;
    end;
  AItem := TdxBarButton(BarManager.AddItem(TdxBarButton));
  AItem.Category := itmTheme.Category;
  AItem.Caption  := 'Standard';
  AItem.Tag      := 0;
  AItem.OnClick  := SelectTheme;
  AItem.ButtonStyle := bsChecked;
  AItem.GroupIndex := 10;
  if not dmMain.SkinCollection.UseSkin then
    AItem.Down := True;
  itmTheme.ItemLinks.Add(AItem);
  for I := 0 to dmMain.SkinCollection.SkinCount - 1 do
  begin
    AItem := TdxBarButton(BarManager.AddItem(TdxBarButton));
    AItem.Category := itmTheme.Category;
    AItem.Caption  := dmMain.SkinCollection.SkinNames[I];
    AItem.Tag      := I + 1;
    AItem.OnClick  := SelectTheme;
    AItem.ButtonStyle := bsChecked;
    AItem.GroupIndex := 10;
    with dmMain.SkinCollection do
    begin
      if UseSkin and (SkinName = AItem.Caption) then
        AItem.Down := True;
    end;
    with itmTheme.ItemLinks.Add(AItem) do
      if I = 0 then BeginGroup := True;
  end;
  itmTheme.Enabled := itmTheme.ItemLinks.Count > 0;
end;

procedure TfrmMain.CreateToolsList;
var
  I: integer;
  AItem: TdxBarButton;
  AToolItem: TExternalToolItem;
begin
  while itmExternalTools.ItemLinks.Count > 0 do
    if itmExternalTools.ItemLinks[0].Item <> nil then
    begin
      AItem := TdxBarButton(itmExternalTools.ItemLinks[0].Item);
      itmExternalTools.ItemLinks[0].Free;
      AItem.Free;
    end;
  for I := 0 to FExtrnTools.Count - 1 do
  begin
    AToolItem := TExternalToolItem(FExtrnTools.Items[I]);
    AItem := TdxBarButton(BarManager.AddItem(TdxBarButton));
    AItem.Category := itmExternalTools.Category;
    AItem.Caption  := AToolItem.Title;
    AItem.Tag      := I;
    AItem.OnClick  := ExecuteTool;
    AItem.ButtonStyle := bsDefault;
    itmExternalTools.ItemLinks.Add(AItem);
  end;
  itmExternalTools.Enabled := itmExternalTools.ItemLinks.Count > 0;
end;

procedure TfrmMain.EndDock(Sender: TdxCustomDockControl; Zone: TdxZone; X,
  Y: Integer);
begin
  RestorePersistentInfo(Sender);
end;

procedure TfrmMain.ExecuteTool(Sender: TObject);
var
  AItem: TExternalToolItem;
begin
  if (Sender is TdxBarButton) and (TdxBarButton(Sender).Tag < FExtrnTools.Count) then
  begin
    AItem := TExternalToolItem(FExtrnTools.Items[TdxBarButton(Sender).Tag]);
    if AItem <> nil then
      ShellExecute(0, PChar('Open'), PChar(AItem.ProgramName), PChar(AItem.Parameters),
        PChar(AItem.WorkingDir), SW_NORMAL);
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//
end;

procedure TfrmMain.HookupEvents(Sender: TdxCustomDockControl);
begin
  if Sender is TdxDockSite then
  begin
    TdxDockSite(Sender).OnShowControl := ShowDockControl;
    TdxDockSite(Sender).OnHideControl := HideDockControl;
  end;
  Sender.OnAutoHideChanged := AutoHideChanged;
  Sender.OnAutoHideChanging := AutoHideChanging;
  Sender.OnEndDocking := EndDock;
  Sender.OnStartDocking := StartDock;
  Sender.OnCreateTabContainer := CreateTabContainer;
  Sender.OnCreateSideContainer := CreateSiteContainer;
  Sender.OnCreateFloatSite := CreateFloatSite;
end;

procedure TfrmMain.StartDock(Sender: TdxCustomDockControl; X, Y: Integer);
begin
  StorePersistentInfo(Sender);
end;

procedure TfrmMain.HideDockControl(Sender: TdxDockSite;
  AControl: TdxCustomDockControl);
begin
  StorePersistentInfo(AControl);
end;

procedure TfrmMain.ShowDockControl(Sender: TdxDockSite;
  AControl: TdxCustomDockControl);
begin
  RestorePersistentInfo(AControl);
end;

function TfrmMain.CreateFrame(AFrameClass: TFormClass;
  AOwner: TdxDockPanel): TForm;
var
  AFrame: TCustomForm;
begin
  AFrame := AFrameClass.Create(AOwner);
  AFrame.Parent := AOwner;
  AFrame.Align := alClient;
  AFrame.Visible := True;
  AFrame.BorderStyle := bsNone;
  AOwner.Caption := AFrame.Caption;
  Result := TForm(AFrame);
end;

procedure TfrmMain.CreateLangList;
var
  AList: TStringList;
  I: Integer;
  AItem: TdxBarButton;
begin
  while itmLanguage.ItemLinks.Count > 0 do
    if itmLanguage.ItemLinks[0].Item <> nil then
    begin
      AItem := TdxBarButton(itmLanguage.ItemLinks[0].Item);
      itmLanguage.ItemLinks[0].Free;
      AItem.Free;
    end;
  AList := TStringList.Create;
  try
    DefaultInstance.GetListOfLanguages(DefaultTextDomain, AList);
    for i := 0 to AList.Count - 1 do
    begin
      AItem := TdxBarButton(BarManager.AddItem(TdxBarButton));
      AItem.Category := itmLanguage.Category;
      AItem.Caption  := getlanguagename(AList[i]);
      AItem.Tag      := i;
      AItem.OnClick  := SelectLanguage;
      AItem.ButtonStyle := bsChecked;
      AItem.GroupIndex := 11;
      if SameText(GetAppIDClass.DefaultLanguangeUsed, AList[I]) then
        AItem.Down := True;
      itmLanguage.ItemLinks.Add(AItem);
    end;
  finally
    AList.Free;
  end;
  itmLanguage.Enabled := itmLanguage.ItemLinks.Count > 0;
end;

procedure TfrmMain.actRefCustomersExecute(Sender: TObject);
begin
  MessageID := CMD_REF_CUSTOMERS;
end;

procedure TfrmMain.actRefFeaturesExecute(Sender: TObject);
begin
  MessageID := CMD_REF_FEATURES;
end;

procedure TfrmMain.actRefItemsExecute(Sender: TObject);
begin
  MessageID := CMD_REF_ITEMS;
end;

procedure TfrmMain.actRefLayingRuleExecute(Sender: TObject);
begin
  MessageID := CMD_REF_LAYINGRULES;
end;

procedure TfrmMain.actRefMaterialsExecute(Sender: TObject);
begin
  MessageID := CMD_REF_MATERIALS;
end;

procedure TfrmMain.actRefreshExecute(Sender: TObject);
begin
  MessageID := CMD_UPDATESTATE;
end;

procedure TfrmMain.actRefStylesExecute(Sender: TObject);
begin
  MessageID := CMD_REF_STYLES;
end;

procedure TfrmMain.actRefUoMExecute(Sender: TObject);
begin
  MessageID := CMD_REF_UOM;
end;

procedure TfrmMain.actRepairCompactExternPackageExecute(Sender: TObject);
begin
  dmMain.OpenDialog.FileName := '';
  dmMain.OpenDialog.Title := _(SCSPOpenCorruptedPkg);
  dmMain.OpenDialog.DefaultExt := SCSPPkgExt;
  dmMain.OpenDialog.Filter := SCSPCorruptedPkgFilter;
  dmMain.OpenDialog.Options := [ofHideReadOnly, ofEnableSizing];
  if dmMain.OpenDialog.Execute(Self. Handle) then
  begin
    dmPackage.RepairExternPackage(dmMain.OpenDialog.FileName);
    ShowMessage(_('Package repaired successfully'));
  end;
end;

procedure TfrmMain.actRepairPackageExecute(Sender: TObject);
begin
  dmPackage.RepairPackage;
end;

procedure TfrmMain.actRptExplorerExecute(Sender: TObject);
begin
  FReporter.ShowExplorer;
end;

procedure TfrmMain.STDActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  AEmpty: boolean;
begin
  Handled := True;
  if (ActiveControl is TcxCustomTextEdit) then
    actEditCut1.Enabled := TcxCustomTextEdit(ActiveControl).SelLength > 0
  else
  if (ActiveControl is TcxGridSite) then
  begin
    actEditCut1.Enabled := True;
  end else
    actEditCut1.Enabled := False;
  // <end remove>
  actEditCopy1.Enabled := actEditCut1.Enabled;
  if (ActiveControl <> nil) and
     (ActiveControl is TWinControl) and
     (ActiveControl.HandleAllocated) then
  begin
    actEditPaste1.Enabled := Clipboard.HasFormat(CF_TEXT);
  end;
  if Assigned(FDatasource) and Assigned(FDatasource.DataSet) then
  begin
    AEmpty := (not FDatasource.DataSet.Active) or (FDatasource.DataSet.IsEmpty);
    actOrderEdit.Enabled := not AEmpty;
    actOrderDelete.Enabled := not AEmpty;
    actOrderPrint.Enabled := not AEmpty;
    actOrderOptimize.Enabled := not AEmpty;
    actOrderFind.Enabled := not AEmpty;    
  end;
end;

procedure TfrmMain.ReadMainForm;
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  PropType: PTypeInfo;
  AValue: Longint;
  AFileName: string;
begin
  with TRegXML.Create(Self) do
  try
    AFileName := GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile;
    if not FileExists(AFileName) then
      AFileName := GetAppIDClass.ApplicationPath+'\'+SCSPDefaultSettingFile;
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath+'\MainForm', False) then
    begin
      Count := GetTypeData(Self.ClassInfo)^.PropCount;
      if Count > 0 then
      begin
        GetMem(PropList, Count * SizeOf(Pointer));
        try
          GetPropInfos(Self.ClassInfo, PropList);
          for I := 0 to Count - 1 do
          begin
            PropInfo := PropList^[I];
            if PropInfo = nil then
              Break;
            if IsStoredProp(Self, PropInfo) then
            begin
                if (PropInfo^.GetProc <> nil) and
                   ((PropInfo^.SetProc <> nil) or
                   ((PropInfo^.PropType^.Kind = tkClass) and
                    (TObject(GetOrdProp(Self, PropInfo)) is TComponent) and
                    (csSubComponent in TComponent(GetOrdProp(Self, PropInfo)).ComponentStyle))) then
                begin
                  if (not SameText(PPropInfo(PropInfo)^.Name, 'Visible')) and
                     (ValueExists(PPropInfo(PropInfo)^.Name)) then
                  begin
                    PropType := PropInfo^.PropType^;
                    case PropType^.Kind of
                      tkInteger, tkChar, tkEnumeration:
                        begin
                          AValue := ReadInteger(PPropInfo(PropInfo)^.Name);
                          SetOrdProp(Self, PropInfo, AValue);
                        end;
                    end;
                  end;
                end;
            end;
          end;
        finally
          FreeMem(PropList, Count * SizeOf(Pointer));
        end;
      end;
    end;
  finally
    Free;
    FormStyle := fsNormal;
    BorderStyle := bsSizeable;
  end;
end;

procedure TfrmMain.ResetMenubar;
var
  i: integer;
begin
  for i := 0 to BarManager.Bars.Count - 1 do
    BarManager.Bars[i].Reset;
end;

procedure TfrmMain.WriteMainForm;
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  PropType: PTypeInfo;
begin
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath+'\MainForm', True) then
    begin
      Count := GetTypeData(Self.ClassInfo)^.PropCount;
      if Count > 0 then
      begin
        GetMem(PropList, Count * SizeOf(Pointer));
        try
          GetPropInfos(Self.ClassInfo, PropList);
          for I := 0 to Count - 1 do
          begin
            PropInfo := PropList^[I];
            if PropInfo = nil then
              Break;
            if IsStoredProp(Self, PropInfo) then
            begin
                if (PropInfo^.GetProc <> nil) and
                   ((PropInfo^.SetProc <> nil) or
                   ((PropInfo^.PropType^.Kind = tkClass) and
                    (TObject(GetOrdProp(Self, PropInfo)) is TComponent) and
                    (csSubComponent in TComponent(GetOrdProp(Self, PropInfo)).ComponentStyle))) then
                begin
                  PropType := PropInfo^.PropType^;
                  case PropType^.Kind of
                    tkChar, tkInteger, tkEnumeration: WriteInteger(PPropInfo(PropInfo)^.Name, GetOrdProp(Self, PropInfo));
                  end;
                end;
            end;
          end;
        finally
          FreeMem(PropList, Count * SizeOf(Pointer));
        end;
      end;
    end;
    Close;
  finally
    Free;
  end;
end;

procedure TfrmMain.CheckAutoOpen;
var
  AFileName: String;
begin
  if ParamCount > 0 then
  begin
    AFileName := ExpandFileName(ParamStr(1));
    if FileExists(AFileName) then
      dmPackage.OpenPackage(AFileName);               
  end;  
end;

procedure TfrmMain.ConstructModule;
var
  AGroup: TdxNavBarGroup;
  i: integer;
  AAction: TAction;
  AItem: TdxNavBarItem;
begin
  FNavBar.BeginUpdate;
  try
    FNavBar.Groups.Clear;
    AGroup := FNavBar.Groups.Add;
    AGroup.Caption := rbnBarReferences.Caption;
    AGroup.LargeImageIndex := 28;
    AGroup.SmallImageIndex := 28;
    for i := 0 to rbnBarReferences.ItemLinks.Count - 1 do
      if (rbnBarReferences.ItemLinks[i].Item <> nil) and
         (rbnBarReferences.ItemLinks[i].Item.Action <> nil) then
      begin
        AAction := TAction(rbnBarReferences.ItemLinks[i].Item.Action);
        AItem := FNavBar.Items.Add;
        AItem.Action := AAction;
        AGroup.CreateLink(AItem);
      end;
  finally
    FNavBar.EndUpdate;
  end;
end;

procedure TfrmMain.ConstructOrderFieldView;
var
  i: integer;
  AColumn: TcxGridDBColumn;
  AField: TField;
  AFieldView: TOrderField;
  ADataset: TDataSet;
begin
  GridOrderDBTableView1.DataController.Options :=
      [
        dcoAnsiSort, dcoCaseInsensitive, dcoAssignGroupingValues,
        dcoAssignMasterDetailKeys, dcoSaveExpanding
      ];
  GridOrderDBTableView1.OptionsBehavior.IncSearch := True;
  GridOrderDBTableView1.OptionsData.Deleting := False;
  GridOrderDBTableView1.OptionsData.Editing := False;
  GridOrderDBTableView1.OptionsData.Inserting := False;
  GridOrderDBTableView1.OptionsSelection.HideFocusRectOnExit := False;
  GridOrderDBTableView1.OptionsSelection.InvertSelect := False;
  GridOrderDBTableView1.OptionsSelection.MultiSelect := True;
  GridOrderDBTableView1.OptionsView.ColumnAutoWidth := True;
  GridOrderDBTableView1.OptionsView.GroupFooters := gfVisibleWhenExpanded;
  GridOrderDBTableView1.OptionsView.Navigator := True;
  GridOrderDBTableView1.NavigatorButtons.Insert.Enabled := False;
  GridOrderDBTableView1.NavigatorButtons.Insert.Visible := False;
  GridOrderDBTableView1.NavigatorButtons.Edit.Enabled := False;
  GridOrderDBTableView1.NavigatorButtons.Edit.Visible := False;
  GridOrderDBTableView1.NavigatorButtons.Append.Enabled := False;
  GridOrderDBTableView1.NavigatorButtons.Append.Visible := False;
  GridOrderDBTableView1.NavigatorButtons.Delete.Enabled := False;
  GridOrderDBTableView1.NavigatorButtons.Delete.Visible := False;
  GridOrderDBTableView1.NavigatorButtons.Post.Enabled := False;
  GridOrderDBTableView1.NavigatorButtons.Post.Visible := False;
  GridOrderDBTableView1.NavigatorButtons.Cancel.Enabled := False;
  GridOrderDBTableView1.NavigatorButtons.Cancel.Visible := False;
  GridOrderDBTableView1.DataController.Filter.Active := True;
  GridOrderDBTableView1.DataController.DataModeController.GridMode := False;
  GridOrderDBTableView1.OptionsCustomize.ColumnFiltering :=
      GridOrderDBTableView1.DataController.Filter.Active;
  GridOrderDBTableView1.OptionsCustomize.ColumnSorting :=
      GridOrderDBTableView1.DataController.Filter.Active;
  GridOrderDBTableView1.DataController.Filter.Options := [fcoCaseInsensitive];
  GridOrderDBTableView1.DataController.Filter.PercentWildcard := '*';
  GridOrderDBTableView1.DataController.Filter.UnderscoreWildcard := '?';
  GridOrderDBTableView1.DateTimeHandling.Filters :=
    [dtfRelativeDays, dtfRelativeDayPeriods, dtfRelativeWeeks,
     dtfRelativeMonths, dtfRelativeYears, dtfPastFuture, dtfMonths, dtfYears];
  GridOrderDBTableView1.DateTimeHandling.IgnoreTimeForFiltering := True;
  GridOrderDBTableView1.DateTimeHandling.Grouping := dtgRelativeToToday;
  GridOrderDBTableView1.Filtering.ColumnPopup.MaxDropDownItemCount := 30;
  GridOrderDBTableView1.OptionsView.GroupByBox := True;
  GridOrderDBTableView1.FilterRow.Visible := True;
  GridOrderDBTableView1.ClearItems;
  if (FDatasource = nil) or (FDatasource.DataSet = nil) then
    exit;
  ADataset := FDatasource.DataSet;
  for I := 0 to FOrderFields.Count - 1 do
  begin
    AFieldView := TOrderField(FOrderFields.Items[I]);
    AField := ADataset.FindField(AFieldView.FieldName);
    if AField <> nil then
    begin
      AColumn := GridOrderDBTableView1.CreateColumn;
      AColumn.DataBinding.FieldName := AField.FieldName;
      AColumn.Caption := _(AFieldView.Caption);
      AColumn.Visible := True;
    end;
  end;
end;

procedure TfrmMain.ConstructOrderView;
begin
  if FDatasource = nil then
  begin
    FDatasource := dmPackage.GetDatasource(
        dmPackage.GetACRTable('orders', 'idx_orders_1')
      );
    GridOrderDBTableView1.DataController.DataSource := FDatasource;
  end;
  if not dmPackage.TableExists('orders') then
    exit;
  FDatasource.DataSet.Open;
  FConstructed := True;
  FOrderFields.SetFields(
    'orddate;ordno;orddesc;cstname',
    'Date;Order No;Decription;Customer'
  );
  ConstructOrderFieldView;
end;

procedure TfrmMain.ConstructResources;
begin
  BarManager.BeginUpdate;
  try
    Caption := Application.Title;
    actPackageNew.Caption := 'New package';
    actPackageOpen.Caption:= 'Open';
    actPackageProperties.Caption := 'Properties';
    actPackageClose.Caption := 'Close package';
    actOrderNew.Caption   := 'New order';
    actOrderEdit.Caption  := 'Edit';
    actOrderDelete.Caption:= 'Delete';
    actOrderFind.Caption  := 'Find order';
    actOrderPrint.Caption := 'Print';
    actOrderOptimize.Caption := 'Optimize';
    actRefUoM.Caption     := 'Unit of Measurement';
    actRefFeatures.Caption:= 'Features';
    actRefLayingRule.Caption := 'Laying Rule';
    actRefItems.Caption     := 'Items';
    actRefCustomers.Caption     := 'Customers';
    actRefStyles.Caption     := 'Styles';
    actRefMaterials.Caption     := 'Materials'; 
    actExit.Caption       := 'Exit';
    actRefresh.Caption    := 'Refresh';
    actEditCut1.Caption   := 'Cut';
    actEditCopy1.Caption  := 'Copy';
    actEditPaste1.Caption := 'Paste';
    actEditSelectAll1.Caption := 'Select all';
    actEditUndo1.Caption  := 'Undo';
    ViewLogPanel.Caption  := 'Log panel';
    itmTheme.Caption := 'Themes';
    itmLanguage.Caption := 'Languages';
    itmToolbar.Caption    := 'Toolbars...';
    itmShowDesigner.Caption := 'Show report designer';
    actOptions.Caption    := 'Options...';
    actRptExplorer.Caption:= 'Report explorer';
    actWelcome.Caption    := 'Welcome';
    HelpContent.Caption   := 'Contents';
    HelpReadme.Caption    := 'Readme';
    HelpSearch.Caption    := 'Search help on...';
    HelpOnHelp.Caption    := 'How to use help';
    HelpAbout.Caption     := 'About';
    Package1.Caption      := 'Package';
    Order1.Caption        := 'Orders';
    References1.Caption    := 'Functions';
    View1.Caption         := 'View';
    Tools1.Caption        := 'Tools';
    Help1.Caption         := 'Help';
    DockLogPanel.Caption  := 'Log View';
    actExternTools.Caption := 'Manage external tools...';
    itmExternalTools.Caption := 'External tools';
    RibbonTabPackage.Caption := 'Package';
    RibbonTabHelp.Caption := 'Help';
    rbnBarPackage.Caption := 'Package';
    rbnBarOrders.Caption := 'Manage Orders';
    rbnBarReferences.Caption := 'Manage References';
    rbnBarMisceleanous.Caption := 'Miscellaneous';
    rbnBarQuickAccess.Caption := 'Quick Access';
    rbnBarOnlineGuide.Caption := 'Online Guide';
    rbnBtnView.Caption := View1.Caption;
    rbnBtnTools.Caption := Tools1.Caption;
    ReconstructLanguage(Self);
    FLogForm.ConstructResources;
  finally
    BarManager.EndUpdate;
  end;
end;

procedure TfrmMain.UpdatePackageStatus(Sender: TObject);
var
  AOpen: boolean;
  I: integer;
  AAction: TAction;
  AIndex: integer;
  AItem: TdxBarExtraPaneItem;  
begin
  AOpen := dmPackage.Opened;
  for i := 0 to STDActions.ActionCount - 1 do
  begin
    AAction := TAction(STDActions.Actions[i]);
    // action dgn tag 1 termasuk dalam kategori package dependant.
    if AAction.Tag = 1 then
      AAction.Enabled := AOpen;
  end;
  if AOpen and (not FConstructed) then
  begin
    ConstructOrderView;
    AdjustTitle(dmPackage.PackageInfo.Values['data_0']);
    AIndex := BarAppMenu.ExtraPane.Items.IndexOf(
      dmPackage.PkgConnection.DatabaseFileName);
    if AIndex >= 0 then
      BarAppMenu.ExtraPane.Items[AIndex].Index := 0
    else
    begin
      AItem := BarAppMenu.ExtraPane.Items.Add;
      AItem.Text := dmPackage.PkgConnection.DatabaseFileName;
      AItem.DisplayText := dmPackage.PackageInfo.Values['data_0'];
    end;
  end;
  if (not AOpen) and FConstructed then
  begin
    DeconstructOrderview;
    AdjustTitle(EmptyStr);
  end;
end;

procedure TfrmMain.UpdateToolbar;
var
  i: integer;
  AVisible: boolean;
begin
  BarManager.BeginUpdate;
  try
    AVisible := SameText(GetAppIDClass.Properties.GetProperty('ribbonbar', 'false'), 'true');
    if not AVisible then
    begin
      Ribbon.SupportNonClientDrawing := AVisible;
      Ribbon.Visible := AVisible;
    end;
    for I := 0 to BarManager.Bars.Count - 1 do
      if pos('rbnBar', BarManager.Bars[I].Name) >0 then
      begin
        BarManager.Bars[I].Visible := AVisible;
        BarManager.Bars[I].AllowClose := AVisible;
      end else
      if pos('stdBar', BarManager.Bars[I].Name) >0 then
      begin
        BarManager.Bars[I].Visible := not AVisible;
        BarManager.Bars[I].AllowClose := not AVisible;
      end;
    for I := 0 to BarManager.ItemCount - 1 do
      if BarManager.Items[I] is TdxBarLargeButton then
      begin
        if AVisible then
          BarManager.Items[I].Visible := ivAlways
        else
          BarManager.Items[I].Visible := ivNever;
      end;
    if AVisible then
    begin
      Ribbon.ColorSchemeName := dmMain.SkinController.SkinName;
      Ribbon.SupportNonClientDrawing := AVisible;
      Ribbon.Visible := AVisible;
    end;
  finally
    BarManager.EndUpdate;
  end;
end;

function TfrmMain.GetShowDesigner: boolean;
begin
  Result := itmShowDesigner.Down;
end;

function TfrmMain.GridItemIndex(const AIndex: integer): integer;
var
  AItem: TOrderField;
  I: integer;
begin
  Result := -1;
  AItem := TOrderField(FOrderFields.Items[AIndex]);
  for I := 0 to GridOrderDBTableView1.ItemCount - 1 do
    if SameText(AItem.FieldName,
          TcxGridDBColumn(
            GridOrderDBTableView1.Items[i]).DataBinding.FieldName
        ) then
    begin
      Result := I;
      break;    
    end;     
end;

procedure TfrmMain.GridOrderDBTableView1DblClick(Sender: TObject);
begin
  if (FDatasource.DataSet.Active) and (not FDatasource.DataSet.IsEmpty) then  
    MessageID := CMD_ORD_EDIT;
end;

procedure TfrmMain.GridOrderDBTableView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      GridOrderDBTableView1DblClick(Sender);
    VK_DELETE:
      actOrderDeleteExecute(Sender);
  end;
end;

procedure TfrmMain.SetShowDesigner(const Value: boolean);
begin
  itmShowDesigner.Down := Value;
end;

procedure TfrmMain.HelpContents1Execute(Sender: TObject);
begin
  MessageID := CMD_HLP_CONTENTS;
end;

procedure TfrmMain.HelpTopicSearch1Execute(Sender: TObject);
begin
  MessageID := CMD_HLP_SEARCH;
end;

procedure TfrmMain.HelpOnHelp1Execute(Sender: TObject);
begin
  MessageID := CMD_HLP_HOW;
end;

procedure TfrmMain.HelpAboutExecute(Sender: TObject);
begin
  MessageID := CMD_HLP_ABOUT;
end;

procedure TfrmMain.HelpReadmeExecute(Sender: TObject);
begin
  MessageID := CMD_HLP_README;
end;

procedure TfrmMain.actWelcomeExecute(Sender: TObject);
begin
  ExecCommand := 'IWELCOME';
end;

procedure TfrmMain.actEditCut1Execute(Sender: TObject);
begin
  MessageID := CMD_CUT;
end;

procedure TfrmMain.actPackageCloseExecute(Sender: TObject);
begin
  MessageID := CMD_PKG_CLOSE;
end;

procedure TfrmMain.actCompactPackageExecute(Sender: TObject);
begin
  dmPackage.CompactPackage;
end;

procedure TfrmMain.actEditCopy1Execute(Sender: TObject);
begin
  MessageID := CMD_COPY;
end;

procedure TfrmMain.actEditPaste1Execute(Sender: TObject);
begin
  MessageID := CMD_PASTE;
end;

procedure TfrmMain.actEditSelectAll1Execute(Sender: TObject);
begin
  MessageID := CMD_SELECTALL;
end;

procedure TfrmMain.actEditUndo1Execute(Sender: TObject);
begin
  MessageID := CMD_UNDO;
end;

procedure TfrmMain.actOrderDeleteExecute(Sender: TObject);
begin
  MessageID := CMD_ORD_DELETE;
end;

procedure TfrmMain.actOrderEditExecute(Sender: TObject);
begin
  MessageID := CMD_ORD_EDIT;
end;

procedure TfrmMain.actOrderFindExecute(Sender: TObject);
begin
  MessageID := CMD_ORD_FIND;
end;

procedure TfrmMain.actOrderNewExecute(Sender: TObject);
begin
  MessageID := CMD_ORD_NEW;
end;

procedure TfrmMain.actOrderOptimizeExecute(Sender: TObject);
begin
  MessageID := CMD_ORD_OPTIMIZE;
end;

procedure TfrmMain.actOrderPrintExecute(Sender: TObject);
var
  AValue : Variant;
begin
  AValue := VarArrayOf([
                  RPT_ORDER_ID,
                  VarArrayOf([
                      FDatasource.DataSet.FieldByName('ordno').AsString,
                      FDatasource.DataSet.FieldByName('orddate').AsDateTime
                    ])
              ]);
  Self.SendMessage(CMD_EXECPRINT, Integer(@AValue));
end;

procedure TfrmMain.actPackagePropertiesExecute(Sender: TObject);
begin
  MessageID := CMD_PKG_PROP;
end;

procedure TfrmMain.ActiveControlChanged(Sender: TObject);
var
  focusedHandle : HWND;
  focusedControl : TWinControl;
begin
  focusedHandle := Windows.GetFocus;
  if focusedHandle <> 0 then
  begin
    focusedControl := FindControl(focusedHandle);
    if focusedControl <> nil then
      StatusBar.Panels[2].Text := focusedControl.ClassName
    else
      StatusBar.Panels[2].Text := 'Unknown';;
  end;
end;

{ TECFNavBar }

constructor TECFNavBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DELPHI9}
  AlignWithMargins := True;
  Margins.Left := 2;
  Margins.Top := 2;
  Margins.Right := 0;
  Margins.Bottom := 0;  
  {$ENDIF}
  ActiveGroupIndex := -1;
  DragCopyCursor := -1119;
  DragCursor := -1120;
  DragDropFlags := [fAllowDragLink, fAllowDropLink, fAllowDragGroup, fAllowDropGroup];
  HotTrackedGroupCursor := crDefault;
  HotTrackedLinkCursor := -1118;
  View := dxNavBarSkinNavigatorPaneView;
  OptionsBehavior.NavigationPane.AdjustWidthByPopup := True;
  OptionsBehavior.NavigationPane.Collapsible := True;
  OptionsView.NavigationPane.MaxVisibleGroups := 5;
  TabStop := False;

end;

procedure TfrmMain.actPackageNewExecute(Sender: TObject);
begin
  MessageID := CMD_PKG_NEW;
end;

procedure TfrmMain.actPackageOpenExecute(Sender: TObject);
begin
  MessageID := CMD_PKG_OPEN;
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
begin
  with TfrmOptions.Create(Self) do
  try
    if ShowModal = mrOk then
    begin
      UseLanguage(GetAppIDClass.DefaultLanguangeUsed);
      UpdateToolbar;
      ConstructResources;
      ResetMenubar;
    end;
  finally
    Free;
  end;
end;

{ TCSPDaysCode }

constructor TCSPDaysCode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCSPDaysCode.Destroy;
begin
  inherited Destroy;
end;

procedure TCSPDaysCode.DoOnChangeCode(Value: TCode);
var
  AValue: string;
begin
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath+'\Registration', True) then
    begin
      AValue := BufferToHex(Value, SizeOf(Value));
      writeString('regcode', AValue);
    end;
    Close;
  finally
    Free;
  end;
end;

procedure TCSPDaysCode.DoOnChecked(Value: TCodeStatus);
var
  AMessage: string;
begin
  case Value of
    ogValidCode:
      begin
        TfrmMain(Owner).StatusBar.Panels[0].Text := Format('%d Days remaining ',
          [GetValue]);
          exit;      
      end;
    ogInvalidCode: AMessage := 'Invalid code';
    ogDayCountUsed : AMessage := 'Program used more than 30 days' + #13 +
                          'Please register NOW';
    ogCodeExpired  : AMessage := 'Evaluation period expired' + #13 +
                          'Please register NOW';
  end;
  ShowMessage(AMessage);
  Application.Terminate;
end;

function TCSPDaysCode.DoOnGetCode: TCode;
var
  AValue: string;
  Expires : TDateTime;
begin
  Expires := EncodeDate(2009, 08, 31);
  FillChar(Result, SizeOf(Result), 0);
  with TRegXML.Create(Self) do
  try
    Open(GetAppIDClass.UserEnvPath+'\'+SCSPInternalSettingFile);
    if OpenKey(GetAppIDClass.RegistryPath+'\Registration', True) then
    begin
      AValue := EmptyStr;
      if valueExists('regcode') then
        AValue := readString('regcode');
      if AValue = EmptyStr then
      begin
        InitDaysCode(CKey, 30, Expires, Result);
        AValue := BufferToHex(Result, SizeOf(Result));
        writeString('regcode', AValue);
      end else
      begin
        HexToBuffer(AValue, Result, SizeOf(Result));
        if GetDateCodeValue(CKey, Result) <> Expires then
        begin
          InitDaysCode(CKey, 30, Expires, Result);
          AValue := BufferToHex(Result, SizeOf(Result));
          writeString('regcode', AValue);
        end;        
      end;
    end;
    Close;
  finally
    Free;
  end;
end;

procedure TCSPDaysCode.DoOnGetKey(var Key: TKey);
begin
  Key := CKey;
end;

end.
