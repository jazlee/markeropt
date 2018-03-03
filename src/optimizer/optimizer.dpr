program optimizer;
{$I CSPDefs.inc}

uses
  SysUtils,
  Classes,
  Forms,
  Windows,
  cxLookAndFeelPainters,
  dxSkinsdxBarPainter,
  dxSkinsdxDockControlPainter,
  dxSkinscxPCPainter,
  dxSkinsdxStatusBarPainter,
  dxSkinsdxRibbonPainter,
  dxSkinsdxNavBar2Painter,
  dxSkinCollector in 'dxSkinCollector.pas',
  CSPAppUtil,
  CSPCustomEngine in '..\lib\engine\CSPCustomEngine.pas',
  dmMainU in 'dmMainU.pas' {dmMain: TDataModule},
  dmReporterU in 'dmReporterU.pas' {dmReporter: TDataModule},
  IApplication in 'IApplication.pas',
  frmAboutU in 'frmAboutU.pas' {frmAbout},
  ecfutils in 'ecfutils.pas',
  frmBannerU in 'frmBannerU.pas' {frmBanner},
  frmMainU in 'frmMainU.pas' {frmMain},
  dmResU in 'dmResU.pas' {dmResources: TDataModule},
  frmExtrnToolsU in 'frmExtrnToolsU.pas' {frmExtrnTools},
  frmExtrnToolsEditU in 'frmExtrnToolsEditU.pas' {frmExtrnToolsEdit},
  ecfextrntools in 'ecfextrntools.pas',
  ecflangutil in 'ecflangutil.pas',
  gettextutil in 'gettextutil.pas',
  ecfnavexceptions in 'ecfnavexceptions.pas',
  ecfreporter in 'ecfreporter.pas' {ecfrptexplorer},
  frmOptionsU in 'frmOptionsU.pas' {frmOptions},
  dmPackageU in 'dmPackageU.pas' {dmPackage: TDataModule},
  dmReferencesU in 'dmReferencesU.pas' {dmReferences: TDataModule},
  CSPNTopLeastEngine in '..\lib\engine\CSPNTopLeastEngine.pas',
  CSPNBottomLeastEngine in '..\lib\engine\CSPNBottomLeastEngine.pas',
  CSPTopFeaturePriorityEngine in '..\lib\engine\CSPTopFeaturePriorityEngine.pas',
  frmBrowserU in 'frmBrowserU.pas' {frmBrowser},
  frmUoMU in 'modules\frmUoMU.pas' {frmUoM},
  frmFeatureU in 'modules\frmFeatureU.pas' {frmFeature},
  frmLayingRuleU in 'modules\frmLayingRuleU.pas' {frmLayingRule},
  frmMaterialU in 'modules\frmMaterialU.pas' {frmMaterial},
  frmCustomerU in 'modules\frmCustomerU.pas' {frmCustomer},
  frmUoMConvU in 'modules\frmUoMConvU.pas' {frmUoMConv},
  frmFeatureItemsU in 'modules\frmFeatureItemsU.pas' {frmFeatureItems},
  frmLayingRuleItemsU in 'modules\frmLayingRuleItemsU.pas' {frmLayingRuleItems},
  frmStyleTypeU in 'modules\frmStyleTypeU.pas' {frmStyleType},
  frmStylesU in 'packages\frmStylesU.pas' {frmStyle},
  frmStFeatureU in 'packages\frmStFeatureU.pas' {frmStFeature},
  frmStFeatureItemsU in 'packages\frmStFeatureItemsU.pas' {frmStFeatureItems},
  frmStMaterialU in 'packages\frmStMaterialU.pas' {frmStMaterial},
  frmStLayingRuleItemsU in 'packages\frmStLayingRuleItemsU.pas' {frmStLayingRuleItems},
  frmItmCreationU in 'packages\frmItmCreationU.pas' {frmItmCreation},
  frmItemU in 'packages\frmItemU.pas' {frmItem},
  frmItmMaterialU in 'packages\frmItmMaterialU.pas' {frmItmMaterial},
  frmItmLayingRuleItemsU in 'packages\frmItmLayingRuleItemsU.pas' {frmItmLayingRuleItems},
  frmOrderU in 'packages\frmOrderU.pas' {frmOrder},
  NativeRegXML in '..\lib\xml\NativeRegXML.pas',
  frmOrdItemU in 'packages\frmOrdItemU.pas' {frmOrdItem},
  frmOrdMaterialU in 'packages\frmOrdMaterialU.pas' {frmOrdMaterial},
  frmOrdLayingRuleItemsU in 'packages\frmOrdLayingRuleItemsU.pas' {frmOrdLayingRuleItems},
  frmInputPasswdU in 'frmInputPasswdU.pas' {frmInputPassword},
  frmFindU in 'frmFindU.pas' {frmFind},
  frmPropU in 'frmPropU.pas' {frmProperties},
  CSPEditUtils in '..\lib\commons\CSPEditUtils.pas',
  frmMarkerOptimizerU in 'frmMarkerOptimizerU.pas' {frmMarkerOptimizer},
  frmOrderItmCreationU in 'packages\frmOrderItmCreationU.pas' {frmOrderItmCreation},
  frmProgressU in 'frmProgressU.pas' {frmProgress},
  frxACRComponents in '..\lib\commons\frxACRComponents.pas',
  frxACRRTTI in '..\lib\commons\frxACRRTTI.pas',
  fs_iacrrtti in '..\lib\commons\fs_iacrrtti.pas',
  frxACREditor in '..\lib\commons\frxACREditor.pas',
  frxDBExporter in '..\lib\rptexplorer\frxDBExporter.pas',
  fs_dbexprtti in '..\lib\rptexplorer\fs_dbexprtti.pas',
  frxDBExpEditor in '..\lib\rptexplorer\frxDBExpEditor.pas',
  OptitexUtils in 'OptitexUtils.pas',
  frmMaterialCatU in 'modules\frmMaterialCatU.pas' {frmMaterialCat},
  frmMarkerInfoU in 'frmMarkerInfoU.pas' {frmMarkerInfo},
  frmOrdStyleU in 'packages\frmOrdStyleU.pas' {frmOrdStyle};

{$R *.res}
{$R optimizer-res.res}

var
  FBannerShown: boolean;

type
  TBannerThread = class(TThread)
  private
    FStop: THandle;
    FBanner: TfrmBanner;
    procedure Initialize;
    procedure Finalize;
  protected
    procedure Execute; override;
  end;

procedure TBannerThread.Initialize;
begin
  FBanner := TfrmBanner.Create(nil);
  FBanner.Show;
  FBanner.BringToFront;
  FBanner.Update;
end;

procedure TBannerThread.Finalize;
begin
  FBannerShown := True;
  FBanner.Hide;
  FBanner.Free;
  Terminate;
end;

procedure TBannerThread.Execute;
begin
  FreeOnTerminate := True;
  FStop := CreateEvent(nil, False, False, nil);
  Synchronize(Initialize);
  while not Terminated do
  begin
    if WaitForSingleObject(FStop, 3000) = WAIT_TIMEOUT then
    begin
      SetEvent(FStop);
      Synchronize(Finalize);
    end;
  end;
end;

var
  sAppMutex: array[0..64] of char;
  AMode: string;
begin
  FBannerShown := False;
  IsMultiThread := True;
  Application.Initialize;
  {$IFDEF COMMERCIAL}
  Application.Title := 'PowerYY';
  {$ELSE}
  Application.Title := 'Marker Optimizer';
  {$ENDIF}
// <user_modification_block>
  with GetAppIDClass do
  begin
    {$IFDEF COMMERCIAL}
    ProjectCategory := 'CSP';
    ProjectName     := 'PowerYY'; // Application Name
    Title           := 'PowerYY';
    ApplicationID   := StringToGUID('{F08FD4D3-E551-4D1F-A83C-8D4FF56F7600}');
    ProjectLeader   := '';
    {$ELSE}
    ProjectCategory := 'CSP';
    ProjectName     := 'Marker Optimizer'; // Application Name
    Title           := 'Marker Optimizer';
    ApplicationID   := StringToGUID('{75E9C64A-A534-41AE-9186-352B30010F62}');
    ProjectLeader   := 'Jaimy Azle';
    {$ENDIF}
  end;
// </user_modification_block>
  GetAppIDClass.SetupAppDirectory;
  GetAppIDClass.ReadSettings;
  if not SameText(GetAppIDClass.Properties.GetProperty('multiinstance', 'true'), 'true') then
  begin
    StrPCopy(sAppMutex, TrimAll(GetAppIDClass.ProjectName)+'_mtx');
    CreateMutex (nil, false, sAppMutex);
    if GetLastError() = ERROR_ALREADY_EXISTS then
    begin
      { It's already running so Restore the other copy }
      SendMessage (HWND_BROADCAST,
                   RegisterWindowMessage(sAppMutex),
                   0,
                   0);
      Halt(0);
    end;
  end;
  if SameText(GetAppIDClass.Properties.GetProperty('splashscreen', 'true'), 'true') then
    TBannerThread.Create(False);

  Application.ProcessMessages;
  ShowWindow(Application.Handle, SW_RESTORE);
  
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TdmPackage, dmPackage);
  Application.CreateForm(TdmReferences, dmReferences);
  Application.CreateForm(TdmReporter, dmReporter);
  try
    Application.CreateForm(TfrmMain, frmMain);
    Application.CreateForm(TfrmProgress, frmProgress);
    Application.ShowMainForm := true;
    if not AppIDInitialized then
      AppIDErrorInit;

    Application.MainForm.BringToFront;
    frmMain.CheckAutoOpen;
    {$ifdef COMMERCIAL}
    Application.Title := 'PowerYY';
    if frmMain.LicenseManager.TrialMode then
      AMode := 'Trial'
    else
      AMode := 'Registered';
    Application.Title := Format('%s (%s)', [Application.Title, AMode]);
    GetAppIDClass.Title := Application.Title;
    {$endif}        
    Application.Run;
  finally
    GetAppIDClass.WriteSettings;
  end;
end.
