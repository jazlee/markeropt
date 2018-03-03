unit CSPCustomForm;

interface

uses
  SysUtils, Classes, Forms, Windows, Messages, Controls, CSPConsts, Consts,
  ActnList, Menus, dxRibbonForm, cxGraphics, ExtCtrls;

{$TYPEINFO ON}

type
  TCSPCustomModule = class;
  TCSPCustomModuleClass = class of TCSPCustomModule;
  TCSPCustomModulePanel = class;
  TCSPCustomModulePanelClass = class of TCSPCustomModulePanel;

  TCSPModuleInfo = class;
  TCSPModuleFolder = class;
  TCSPRootModuleFolder = class;

  TCSPIntegrationType = (itInternal, itPackage, itMvc);
  TCSPModuleType = (mtViewer, mtFullModule, mtProperties, mtPrint, mtSingleView, mtSingleEdit);
  TCSPFunctionType  = (ftSelect, ftNew, ftOpen, ftShow, ftCopy, ftDelete);
  TCSPFunctionTypes = set of TCSPFunctionType;

  TCSPModuleRec = packed record
    APIObjectAuthCheck: array[0..127] of char;
    ModuleType: TCSPModuleType;
    ShortcutName: array[0..127] of char;
    Description: array[0..127] of char;
    Category: array[0..127] of char;
    HideModule: boolean;
    SingleInstance: boolean;
    ScriptBased: boolean;
    IntegrationType: TCSPIntegrationType;
    ModuleClass: array[0..127] of char;
    InstanceCount: integer;
    PackageName: array[0..127] of char;
    ModuleHandle: HMODULE;
  end;

  TAppMessage = record
    Msg: Longint;
    case Integer of
      0: (
        Param: Longint;
        Result: Longint);
      1: (
        ParamLo: Word;
        ParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);
  end;
  TCSPMessageEvent = procedure(var Message: TAppMessage; var Handled: boolean) of object;

  TCSPForm = class(TdxRibbonForm)
  private
    FOnUnprepare: TNotifyEvent;
    FOnPrepare: TNotifyEvent;
    FOnUpdateState: TNotifyEvent;
    FOnEvaluateMessages: TCSPMessageEvent;
    FLastMessageID: integer;
    FInitialized: boolean;
    FPrepared: boolean;
    FLastCmd: string;

    procedure SetMessageID(const Value: integer);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure SetExecCmd(const Value: string);
  protected
    procedure DoPrepare;
    procedure DoUnprepare;
    procedure DoUpdatestate;

    procedure WndProc(var Message: TMessage); override;
    function InternalSendMessage(AForm: TCSPForm; const AMessageID, AParam: Longint): Longint; virtual;
    procedure PerformAppMsgProc(var Message: TAppMessage); virtual;
    procedure PopulateAppMsgproc(var Message: TAppMessage); virtual;

    procedure InternalPrepare; virtual;
    procedure InternalUnprepare; virtual;
    procedure InternalUpdateState; virtual;
    procedure InternalInitialize; virtual;
    procedure InternalFinalize; virtual;

    function GetModuleList: TList;
    function GetModuleFolder: TCSPRootModuleFolder;
    function GetModuleByShortcut(const AModName: string): TCSPModuleInfo;
    procedure SetupModule(AMod: TCSPCustomModule; APopupMenu: TPopupMenu); overload;
    procedure SetupModule(AMod: TCSPCustomModulePanel; APopupMenu: TPopupMenu); overload;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnlargeWindow;
    function CloseQuery: Boolean; override;
    procedure DoClose(var Action: TCloseAction); override;

    function SendMessage(AMessageID, AParam: Longint): Longint;
    function PostMessage(AMessageID, AParam: Longint): Longint;
    function ExecuteReport(rptValue: Variant): longint;
    property MessageID: integer read FLastMessageID write SetMessageID;
    property ExecCommand: string read FLastCmd write SetExecCmd;
    property Initialized: boolean read FInitialized;
    property Prepared: boolean read FPrepared;

  published
    property OnPrepare: TNotifyEvent
      read FOnPrepare write FOnPrepare;

    property OnUpdateState: TNotifyEvent
      read FOnUpdateState write FOnUpdateState;

    property OnUnprepare: TNotifyEvent
      read FOnUnprepare write FOnUnprepare;

    property OnEvaluateMessages: TCSPMessageEvent
      read FOnEvaluateMessages write FOnEvaluateMessages;
  end;

  TCSPCustomModule = class(TCSPForm)
  private
    FFuncSupported: TCSPFunctionTypes;
    FMainForm: TCSPForm;
    FMainPopupMenu: TPopupMenu;
    procedure SetFuncSupported(const Value: TCSPFunctionTypes);

  protected
    function GetExtendedFunctionList: TActionList; virtual;
    function GetOpenRelatedList: TActionList; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateModule(AOwner: TComponent; AParent: TWinControl);
    destructor Destroy; override;

    class function APIObjectAuthCheck: String; virtual;
    class function ModuleType: TCSPModuleType; virtual;
    class function ShortcutName: string; virtual;
    class function Description: string; virtual;
    class function Category: string; virtual;
    class function HideModule: boolean; virtual;
    class function SingleInstance: boolean; virtual;
    class function ScriptBased: boolean; virtual;

    class function GetExtData: Pointer; virtual;
    class procedure SetExtData(const Value: Pointer); virtual;

    property ExtData: Pointer read GetExtData write SetExtData;

    property FunctionSupported: TCSPFunctionTypes read FFuncSupported write SetFuncSupported;
    property MainForm: TCSPForm read FMainForm;
    property MainPopupMenu: TPopupMenu read FMainPopupMenu;
  end;

  TCSPCustomModulePanel = class(TCustomPanel)
  private
    FFuncSupported: TCSPFunctionTypes;
    FMainForm: TCSPForm;
    FMainPopupMenu: TPopupMenu;
    FPrepared: boolean;
    FInitialized: boolean;
    FLastCmd: string;
    FLastMessageID: integer;

    procedure SetFuncSupported(const Value: TCSPFunctionTypes);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure SetExecCmd(const Value: string);
    procedure SetMessageID(const Value: integer);
  protected
    procedure DoPrepare;
    procedure DoUnprepare;
    procedure DoUpdatestate;

    procedure WndProc(var Message: TMessage); override;
    procedure PerformAppMsgProc(var Message: TAppMessage); virtual;
    procedure PopulateAppMsgproc(var Message: TAppMessage); virtual;

    function GetExtendedFunctionList: TActionList; virtual;
    function GetOpenRelatedList: TActionList; virtual;

    function GetModuleFolder: TCSPRootModuleFolder;
    function GetModuleByShortcut(const AModName: string): TCSPModuleInfo;

    procedure InternalPrepare; virtual;
    procedure InternalUnprepare; virtual;
    procedure InternalUpdateState; virtual;
    procedure InternalInitialize; virtual;
    procedure InternalFinalize; virtual;
    
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateModule(AOwner: TComponent; AParent: TWinControl);
    destructor Destroy; override;

    function SendMessage(AMessageID, AParam: Longint): Longint;
    function PostMessage(AMessageID, AParam: Longint): Longint;
    function ExecuteReport(rptValue: Variant): longint;
    property MessageID: integer read FLastMessageID write SetMessageID;
    property ExecCommand: string read FLastCmd write SetExecCmd;

    class function APIObjectAuthCheck: String; virtual;
    class function ModuleType: TCSPModuleType; virtual;
    class function ShortcutName: string; virtual;
    class function Description: string; virtual;
    class function Category: string; virtual;
    class function HideModule: boolean; virtual;
    class function SingleInstance: boolean; virtual;
    class function ScriptBased: boolean; virtual;

    class function GetExtData: Pointer; virtual;
    class procedure SetExtData(const Value: Pointer); virtual;

    property ExtData: Pointer read GetExtData write SetExtData;

    property FunctionSupported: TCSPFunctionTypes read FFuncSupported write SetFuncSupported;
    property MainForm: TCSPForm read FMainForm;
    property MainPopupMenu: TPopupMenu read FMainPopupMenu;

    property Prepared: boolean read FPrepared;
    property Initialized: boolean read FInitialized;
  end;

  TCSPBaseModuleInfo = class(TComponent)
  protected
    procedure Changed; virtual;

    procedure DoChange; virtual;

    function GetParentFolder(const AFolder: string): TCSPModuleFolder;
    function GetProgamItem(const AProgram: string): TCSPModuleInfo;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    function GetChildOwner: TComponent; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCSPModuleInfo = class(TCSPBaseModuleInfo)
  private
    FHideModule: boolean;
    FInstanceCount: integer;
    FAPIObjectAuthCheck: string;
    FModuleClass: string;
    FPackageName: string;
    FScriptBased: boolean;
    FDescription: string;
    FShortcutName: string;
    FModuleHandle: HMODULE;
    FCategory: string;
    FIntegrationType: TCSPIntegrationType;
    FSingleInstance: boolean;
    FModuleType: TCSPModuleType;
    procedure SetAPIObjectAuthCheck(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetHideModule(const Value: boolean);
    procedure SetInstanceCount(const Value: integer);
    procedure SetIntegrationType(const Value: TCSPIntegrationType);
    procedure SetModuleClass(const Value: string);
    procedure SetModuleHandle(const Value: HMODULE);
    procedure SetModuleType(const Value: TCSPModuleType);
    procedure SetPackageName(const Value: string);
    procedure SetScriptBased(const Value: boolean);
    procedure SetShortcutName(const Value: string);
    procedure SetSingleInstance(const Value: boolean);
    function GetFormattedName: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property InstanceCount: integer read FInstanceCount write SetInstanceCount;
    property ModuleHandle: HMODULE read FModuleHandle write SetModuleHandle;
    property FormattedName: string read GetFormattedName;
  published
    property APIObjectAuthCheck: string read FAPIObjectAuthCheck write SetAPIObjectAuthCheck;
    property ModuleType: TCSPModuleType read FModuleType write SetModuleType;
    property ShortcutName: string read FShortcutName write SetShortcutName;
    property Description: string read FDescription write SetDescription;
    property Category: string read FCategory write SetCategory;
    property HideModule: boolean read FHideModule write SetHideModule;
    property SingleInstance: boolean read FSingleInstance write SetSingleInstance;
    property ScriptBased: boolean read FScriptBased write SetScriptBased;
    property IntegrationType: TCSPIntegrationType read FIntegrationType write SetIntegrationType;
    property ModuleClass: string read FModuleClass write SetModuleClass;
    property PackageName: string read FPackageName write SetPackageName;
  end;

  TCSPModuleFolder = class(TCSPBaseModuleInfo)
  private
    FFolderName: string;
    FLoading: Boolean;
    FModified: Boolean;
    
    procedure SetFolderName(const Value: string);

    function BindParentFolder(AStr: TStringList; APos: integer): TCSPModuleFolder;
    function GetFormattedName: string;
  protected
    procedure DoChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindParent(AItem: TCSPModuleFolder): boolean;

    property Modified: Boolean read FModified;
    property FormattedName: string read GetFormattedName;
  published
    property FolderName: string read FFolderName write SetFolderName;
  end;

  TCSPGroupModuleFolder = class(TCSPModuleFolder)
  private
    FSmallImageIndex: integer;
    FLargeImageIndex: integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LargeImageIndex: integer read FLargeImageIndex write FLargeImageIndex;
    property SmallImageIndex: integer read FSmallImageIndex write FSmallImageIndex;
  end;

  TCSPRootModuleFolder = class(TCSPModuleFolder)
  private
    FSmallImageList: TcxImageList;
    FLargeImageList: TcxImageList;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    
    procedure ReadLargeBitmap(Stream: TStream);
    procedure ReadSmallBitmap(Stream: TStream);
    procedure WriteLargeBitmap(Stream: TStream);
    procedure WriteSmallBitmap(Stream: TStream);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LargeImageList: TcxImageList read FLargeImageList;
    property SmallImageList: TcxImageList read FSmallImageList;
  end;

procedure RegisterECFInternalModule(AClass: TCSPCustomModuleClass);
procedure UnregisterECFInternalModule(AClass: TCSPCustomModuleClass);
procedure RegisterECFPackageModule(AClass: TCSPCustomModuleClass; const AHMod: HMODULE);
procedure UnregisterECFPackageModule(AClass: TCSPCustomModuleClass);
procedure RegisterECFMvcModule(ARec: TCSPModuleRec);
procedure UnregisterECFMvcModule(ARec: TCSPModuleRec);
procedure BringCachedHandleToTop(AHandle: HMODULE);
procedure BringCachedHandleToBottom(AHandle: HMODULE);
procedure CleanupCachedPackage;
function LoadPackageModule(const AName: string): HMODULE;

procedure SaveModuleIndex(const AFileName: string);
procedure LoadModuleIndex(const AFileName: string);

implementation
uses
  SysConst, gnugettext;

function CreateUniqueName(AComp: TComponent): string;
var
  I: Integer;

  function IsUnique(const AName: string): Boolean;
  begin
    Result := False;
    if (AComp.Owner <> nil) then
    begin
      if AComp.Owner.FindComponent(AName) = nil then
        Result := True
    end else
      Result := True;
  end;

begin
  for I := 1 to MaxInt do
  begin
    Result := AComp.ClassName + IntToStr(I);
    if IsUnique(Result) then Exit;
  end;
end;  

type
  THackedImageList = class(TcxImageList);
  
  TCSPInternalModuleManager = class(TObject)
  private
    FModuleList: TList;
    FRootFolder: TCSPRootModuleFolder;

    procedure Clear;
    function IndexOf(AShortName: string): integer;
    function FindByHModule(AHMod: HMODULE): TCSPModuleInfo;
  public
    constructor Create;
    destructor Destroy; override;

    function FindProperFolder(const ACategory: string): TCSPModuleFolder;

    procedure AddInternalModule(AClass: TCSPCustomModuleClass);
    procedure AddPackageModule(AClass: TCSPCustomModuleClass;
      const AHMod: HMODULE);
    procedure AddMvcModule(ARec: TCSPModuleRec);
    procedure RemoveInternalModule(AClass: TCSPCustomModuleClass);
    procedure RemovePackageModule(AClass: TCSPCustomModuleClass);
    procedure RemoveMvcModule(ARec: TCSPModuleRec);
    function FindModule(const AShortName: string): TCSPModuleInfo;

    property RootFolder: TCSPRootModuleFolder read FRootFolder;
    property ModuleList: TList read FModuleList;
  end;

const
  MaxPackageCached = 64;
  ufAllUnits = ufMainUnit or ufPackageUnit or ufWeakUnit or ufOrgWeakUnit or ufImplicitUnit;

var
  ModuleManager: TCSPInternalModuleManager;
  CachedPackageHandleList: TList;

procedure ParseKeys(AKeys: string; AStrings: TStrings);
var
  p: integer;
  s: string;
begin
  repeat
    p := Pos('\',AKeys);
    if p>0 then
    begin
      s := copy(AKeys,1,p-1);
      if ((pinteger(@pchar(s)[-4])^) > 0) then
        AStrings.Add(s);
      AKeys := copy(AKeys,p+1,(pinteger(@pchar(AKeys)[-4])^)-p);
    end;
  until p=0;
  if (pinteger(@pchar(AKeys)[-4])^ > 0) then
    AStrings.Add(AKeys);
end;  

{ TCSPInternalModuleManager }

procedure TCSPInternalModuleManager.AddInternalModule(
  AClass: TCSPCustomModuleClass);
var
  i: integer;
  AFound: Boolean;
  AFolder: TCSPModuleFolder;
begin
  AFound := False;
  for i := 0 to FModuleList.Count - 1 do
    if (TCSPModuleInfo(FModuleList[i]).ModuleClass = EmptyStr) and
       (SameText(TCSPModuleInfo(FModuleList[i]).ShortcutName,
            AClass.ShortcutName)) then
    begin
      AFound := True;
      break;
    end else
    if SameText(TCSPModuleInfo(FModuleList[i]).ModuleClass, AClass.ClassName) then
    begin
      AFound := True;
      break;
    end;
  if not AFound then
  begin
    if not AClass.HideModule then    
      AFolder := FindProperFolder(AClass.Category)
    else
      AFolder := FRootFolder;
    if AFolder <> nil then
      with TCSPModuleInfo.Create(AFolder) do
      begin
        APIObjectAuthCheck := AClass.APIObjectAuthCheck;
        ModuleType := AClass.ModuleType;
        ShortcutName := AClass.ShortcutName;
        Description := AClass.Description;
        Category := AClass.Category;
        HideModule := AClass.HideModule;
        SingleInstance := AClass.SingleInstance;
        ScriptBased := AClass.ScriptBased;
        IntegrationType := itInternal;
        ModuleClass := AClass.ClassName;
        PackageName := '';
      end;
    Classes.RegisterClass(AClass);
  end else
  begin
    with TCSPModuleInfo(FModuleList[I]) do
    begin
      APIObjectAuthCheck := AClass.APIObjectAuthCheck;
      ModuleType := AClass.ModuleType;
      ShortcutName := AClass.ShortcutName;
      Description := AClass.Description;
      Category := AClass.Category;
      HideModule := AClass.HideModule;
      SingleInstance := AClass.SingleInstance;
      ScriptBased := AClass.ScriptBased;
      IntegrationType := itInternal;
      ModuleClass := AClass.ClassName;
      PackageName := '';
    end;
    Classes.RegisterClass(AClass);  
  end;
end;

procedure TCSPInternalModuleManager.AddMvcModule(ARec: TCSPModuleRec);
var
  i: integer;
  AFound: Boolean;
  AFolder: TCSPModuleFolder;
begin
  AFound := False;
  for i := 0 to FModuleList.Count - 1 do
    if (SameText(TCSPModuleInfo(FModuleList[i]).ShortcutName,
            ARec.ShortcutName)) then
    begin
      AFound := True;
      break;
    end;
  if not AFound then
  begin
    if not ARec.HideModule then    
      AFolder := FindProperFolder(ARec.Category)
    else
      AFolder := FRootFolder;
    if AFolder <> nil then
      with TCSPModuleInfo.Create(AFolder) do
      begin
        APIObjectAuthCheck := ARec.APIObjectAuthCheck;
        ModuleType := ARec.ModuleType;
        ShortcutName := ARec.ShortcutName;
        Description := ARec.Description;
        Category := ARec.Category;
        HideModule := ARec.HideModule;
        SingleInstance := ARec.SingleInstance;
        ScriptBased := ARec.ScriptBased;
        IntegrationType := itMvc;
      end;    
  end;
end;

type
  PPkgName = ^TPkgName;
  TPkgName = packed record
    HashCode: Byte;
    Name: array[0..255] of Char;
  end;

  { Package flags:
    bit     meaning
    -----------------------------------------------------------------------------------------
    0     | 1: never-build                  0: always build
    1     | 1: design-time only             0: not design-time only      on => bit 2 = off
    2     | 1: run-time only                0: not run-time only         on => bit 1 = off
    3     | 1: do not check for dup units   0: perform normal dup unit check
    4..25 | reserved
    26..27| (producer) 0: pre-V4, 1: undefined, 2: c++, 3: Pascal
    28..29| reserved
    30..31| 0: EXE, 1: Package DLL, 2: Library DLL, 3: undefined
  }
  PPackageInfoHeader = ^TPackageInfoHeader;
  TPackageInfoHeader = packed record
    Flags: Cardinal;
    RequiresCount: Integer;
    {Requires: array[0..9999] of TPkgName;
    ContainsCount: Integer;
    Contains: array[0..9999] of TUnitName;}
  end;

function PackageInfoTable(Module: HMODULE): PPackageInfoHeader;
var
  ResInfo: HRSRC;
  Data: THandle;
begin
  Result := nil;
  ResInfo := FindResource(Module, 'PACKAGEINFO', RT_RCDATA);
  if ResInfo <> 0 then
  begin
    Data := LoadResource(Module, ResInfo);
    if Data <> 0 then
    try
      Result := LockResource(Data);
      UnlockResource(Data);
    finally
      FreeResource(Data);
    end;
  end;
end;

procedure TCSPInternalModuleManager.AddPackageModule(
  AClass: TCSPCustomModuleClass; const AHMod: HMODULE);
var
  InfoTable: PPackageInfoHeader;
  PkgName: PPkgName;
  i: integer;
  AFound: Boolean;
  AFolder: TCSPModuleFolder;
begin
  InfoTable := PackageInfoTable(AHMod);
  if not Assigned(InfoTable) then
  raise EPackageError.CreateFmt(SCannotReadPackageInfo,
    [ExtractFileName(GetModuleName(AHMod))]);
  PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
  for i := 0 to InfoTable^.RequiresCount - 1 do
    Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
  Inc(Integer(PkgName), StrLen(PkgName.Name) + 5);
  AFound := False;
  for i := 0 to FModuleList.Count - 1 do
    if (TCSPModuleInfo(FModuleList[i]).IntegrationType = itPackage) and
       (SameText(TCSPModuleInfo(FModuleList[i]).ShortcutName,
            AClass.ShortcutName)) then
    begin
      AFound := True;
      TCSPModuleInfo(FModuleList[i]).ModuleHandle:= AHMod;
      TCSPModuleInfo(FModuleList[i]).PackageName := PkgName.Name + '.bpl';
      Classes.RegisterClass(AClass);
      break;
    end else
    if SameText(TCSPModuleInfo(FModuleList[i]).ModuleClass, AClass.ClassName) then
    begin
      AFound := True;
      break;
    end;
  if not AFound then
  begin
    if not AClass.HideModule then    
      AFolder := FindProperFolder(AClass.Category)
    else
      AFolder := FRootFolder;
    if AFolder <> nil then
      with TCSPModuleInfo.Create(AFolder) do
      begin
        APIObjectAuthCheck := AClass.APIObjectAuthCheck;
        ModuleType := AClass.ModuleType;
        ShortcutName := AClass.ShortcutName;
        Description := AClass.Description;
        Category := AClass.Category;
        HideModule := AClass.HideModule;
        SingleInstance := AClass.SingleInstance;
        ScriptBased := AClass.ScriptBased;
        IntegrationType := itPackage;
        ModuleClass := AClass.ClassName;
        PackageName := PkgName.Name + '.bpl';
        ModuleHandle := AHMod;
      end;    
    Classes.RegisterClass(AClass);
  end else
  begin
    with TCSPModuleInfo(FModuleList[i]) do
    begin
      APIObjectAuthCheck := AClass.APIObjectAuthCheck;
      ModuleType := AClass.ModuleType;
      ShortcutName := AClass.ShortcutName;
      Description := AClass.Description;
      Category := AClass.Category;
      HideModule := AClass.HideModule;
      SingleInstance := AClass.SingleInstance;
      ScriptBased := AClass.ScriptBased;
      IntegrationType := itPackage;
      ModuleClass := AClass.ClassName;
      PackageName := PkgName.Name + '.bpl';
      ModuleHandle := AHMod;
    end;    
    Classes.RegisterClass(AClass);  
  end;
end;

procedure TCSPInternalModuleManager.Clear;
begin
  FRootFolder.DestroyComponents;
end;

constructor TCSPInternalModuleManager.Create;
begin
  FModuleList := TList.Create;
  FRootFolder := TCSPRootModuleFolder.Create(nil);
end;

destructor TCSPInternalModuleManager.Destroy;
begin
  Clear;
  FRootFolder.Free;
  FModuleList.Free;
  inherited Destroy;
end;

function TCSPInternalModuleManager.FindModule(
  const AShortName: string): TCSPModuleInfo;
var
  i: integer;
begin
  Result := nil;
  i := IndexOf(AShortName);
  if i > -1 then
    Result := TCSPModuleInfo(FModuleList[I]);
end;

function TCSPInternalModuleManager.FindByHModule(
  AHMod: HMODULE): TCSPModuleInfo;
var
  i: integer;
begin
  Result := nil;
  if AHMod <> 0 then
    for i := 0 to FModuleList.Count - 1 do
      if (TCSPModuleInfo(FModuleList[i]).ModuleHandle = AHMod) then
      begin
        Result := TCSPModuleInfo(FModuleList[i]);
        break;
      end;
end;

function TCSPInternalModuleManager.FindProperFolder(
  const ACategory: string): TCSPModuleFolder;
var
  AStr: TStringList;
begin
  AStr := TStringList.Create;
  try
    ParseKeys(ACategory, AStr);
    Result := FRootFolder.BindParentFolder(AStr, 0);
  finally
    AStr.Free;
  end;  
end;

function TCSPInternalModuleManager.IndexOf(AShortName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FModuleList.Count - 1 do
    if (SameText(TCSPModuleInfo(FModuleList[i]).ShortcutName, AShortName)) then
    begin
      Result := i;
      break;
    end;
end;

procedure TCSPInternalModuleManager.RemoveInternalModule(
  AClass: TCSPCustomModuleClass);
var
  i: integer;
  AInfo: TCSPModuleInfo;
begin
  i := IndexOf(AClass.ShortcutName);
  if i >= 0 then
  begin
    AInfo := TCSPModuleInfo(FModuleList[i]);
    if AInfo <> nil then
      AInfo.Free;
    Classes.UnRegisterClass(AClass);
  end;
end;

procedure TCSPInternalModuleManager.RemoveMvcModule(ARec: TCSPModuleRec);
var
  i: integer;
  AInfo: TCSPModuleInfo;
begin
  i := IndexOf(ARec.ShortcutName);
  if i >= 0 then
  begin
    AInfo := TCSPModuleInfo(FModuleList[i]);
    if AInfo <> nil then
      AInfo.Free;
  end;
end;

procedure TCSPInternalModuleManager.RemovePackageModule(
  AClass: TCSPCustomModuleClass);
var
  i: integer;
  AInfo: TCSPModuleInfo;
begin
  i := IndexOf(AClass.ShortcutName);
  if i >= 0 then
  begin
    AInfo := TCSPModuleInfo(FModuleList[i]);
    if AInfo <> nil then
      AInfo.Free;
    Classes.UnRegisterClass(AClass);
  end;
end;

procedure RegisterECFInternalModule(AClass: TCSPCustomModuleClass);
begin
  ModuleManager.AddInternalModule(AClass);
end;

procedure UnregisterECFInternalModule(AClass: TCSPCustomModuleClass);
begin
  ModuleManager.RemoveInternalModule(AClass);
end;

procedure RegisterECFPackageModule(AClass: TCSPCustomModuleClass; const AHMod: HMODULE);
begin
  ModuleManager.AddPackageModule(AClass, AHMod);
end;

procedure UnregisterECFPackageModule(AClass: TCSPCustomModuleClass);
begin
  ModuleManager.RemovePackageModule(AClass);
end;

procedure RegisterECFMvcModule(ARec: TCSPModuleRec);
begin
  ModuleManager.AddMvcModule(ARec);
end;

procedure UnregisterECFMvcModule(ARec: TCSPModuleRec);
begin
  ModuleManager.RemoveMvcModule(ARec);
end;

procedure BringCachedHandleToTop(AHandle: HMODULE);
var
  i: integer;
begin
  if CachedPackageHandleList <> nil then
  begin
    i := CachedPackageHandleList.IndexOf(Ptr(AHandle));
    if i > 0 then
      CachedPackageHandleList.Move(i, 0);
  end;
end;

procedure BringCachedHandleToBottom(AHandle: HMODULE);
var
  i: integer;
  PRec: Pointer;
begin
  if CachedPackageHandleList <> nil then
  begin
    i := CachedPackageHandleList.IndexOf(Ptr(AHandle));
    if (i >= 0) and (i < (CachedPackageHandleList.Count-1)) then
    begin
      PRec := CachedPackageHandleList[i];
      CachedPackageHandleList.Delete(i);
      CachedPackageHandleList.Add(PRec);
    end;
  end;
end;

procedure PackageIsUnLoadingProc(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);
type
  TUnRegisterProc = procedure;
var
  UnRegisterProc: TUnRegisterProc;
  LocalName: String;
begin
  if NameType = ntContainsUnit then
  begin
    LocalName := LowerCase(Name);
    if Length(LocalName) > 0 then
      LocalName[1] := UpCase(LocalName[1]);
    @UnRegisterProc := GetProcAddress(HModule(Param),
      PChar('@' + localName + '@UnRegisterViewModule$qqrv'));
    if @UnRegisterProc <> nil then
      UnRegisterProc;
  end;
end;

procedure UnLoadPackageModule(const AHMod: HMODULE);
var
  Index: integer;
  ASafe: Boolean;
  Flags             : Integer;
  AInfo: TCSPModuleInfo;
begin
  ASafe := True;
  if CachedPackageHandleList <> nil then
  begin
    Index := CachedPackageHandleList.IndexOf(Ptr(AHMod));
    if Index >= 0 then
    begin
      AInfo := ModuleManager.FindByHModule(AHMod);
      if (AInfo <> nil) and (AInfo.InstanceCount > 0) then
        ASafe := False;
      if ASafe then
        CachedPackageHandleList.Delete(Index);
    end;
  end;
  if ASafe then
  begin
    Flags := ufAllUnits;
    GetPackageInfo(AHMod, Pointer(AHMod),
         Flags, PackageIsUnLoadingProc);
    UnloadPackage(AHMod);
  end;
end;

procedure UnloadAllPackageModule;
begin
  if CachedPackageHandleList <> nil then
  begin
    while CachedPackageHandleList.Count > 0 do
      UnLoadPackageModule(Cardinal(CachedPackageHandleList[0]));
  end;
end;

procedure CleanupCachedPackage;
var
  AHMod: HMODULE;
  Index: integer;
  PRec: TCSPModuleInfo;
begin
  if CachedPackageHandleList = nil then
    exit;
  if CachedPackageHandleList.Count > MaxPackageCached then
  begin
    Index := CachedPackageHandleList.Count-1;
    while (Index >= 0) and
          (CachedPackageHandleList.Count > MaxPackageCached) do
    begin
      AHMod := Cardinal(CachedPackageHandleList.Items[Index]);
      PRec := ModuleManager.FindByHModule(AHMod);
      if (PRec <> nil) and (PRec.ModuleHandle <> 0) and
         (PRec.InstanceCount = 0) then
        BringCachedHandleToBottom(AHMod);
      UnLoadPackageModule(AHMod);
      if Index >= CachedPackageHandleList.Count then
        Index := CachedPackageHandleList.Count-1
      else
        Dec(Index);
    end;
  end;
end;

var
  ALocalPackageHandle: HMODULE;

procedure PackageIsLoadingProc(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);
type
  TRegisterProc = procedure(const PackageHandle: HMODULE);
var
  RegisterProc: TRegisterProc;
  LocalName: String;
begin
  if NameType = ntContainsUnit then
  begin
    localName := LowerCase(Name);
    if Length(localName) > 0 then
      localName[1] := UpCase(localName[1]);
    @RegisterProc := GetProcAddress(HModule(Param),
      PChar('@' + localName + '@RegisterViewModule$qqrxui'));
    if @RegisterProc <> nil then
      RegisterProc(ALocalPackageHandle);
  end;
end;

function LoadPackageModule(const AName: string): HMODULE;
var
  AHMod: HMODULE;
  AFlags: Integer;
begin
  if not FileExists(AName) then
    raise Exception.CreateFmt('Package %s is not found', [AName]);
  if CachedPackageHandleList = nil then
    CachedPackageHandleList := TList.Create;
  CleanupCachedPackage;
  AHMod := LoadPackage(AName);
  ALocalPackageHandle := AHMod;
  AFlags:= ufAllUnits;
  GetPackageInfo(AHMod, Pointer(AHMod), AFlags, PackageIsLoadingProc);
  if AHMod <> 0 then
    CachedPackageHandleList.Insert(0, Ptr(AHMod));
  Result := AHMod;
end;

procedure SaveModuleIndexToStream(AStream: TStream; const ABinary: Boolean = True);
var
  AMemStream: TMemoryStream;
begin
  if not ABinary then
  begin
    AMemStream := TMemoryStream.Create;
    try
      AMemStream.WriteComponent(ModuleManager.RootFolder);
      AMemStream.Position := 0;
      ObjectBinaryToText(AMemStream, AStream);
    finally
      AMemStream.Free;
    end;
  end else
    AStream.WriteComponent(ModuleManager.RootFolder);
end;

procedure SaveModuleIndex(const AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveModuleIndexToStream(AFileStream, False);
  finally
    AFileStream.Free;
  end;
end;

procedure LoadModuleIndexFromStream(AStream: TStream; const ABinary: boolean = True);
var
  AMemStream: TMemoryStream;
begin
  if not ABinary then
  begin
    AMemStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(AStream, AMemStream);
      AMemStream.Position := 0;
      ModuleManager.FRootFolder.DestroyComponents;
      AMemStream.ReadComponent(ModuleManager.RootFolder);
    finally
      AMemStream.Free;
    end;
  end else
  begin
    ModuleManager.FRootFolder.DestroyComponents;
    AStream.ReadComponent(ModuleManager.RootFolder);
  end;
end;

procedure LoadModuleIndex(const AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadModuleIndexFromStream(AFileStream, False);
  finally
    AFileStream.Free;
  end;
end;

{ TCSPForm }

function TCSPForm.CloseQuery: Boolean;
begin
  Result := Inherited CloseQuery;
  if Result then
    DoUnprepare;
end;

procedure TCSPForm.CMShowingChanged(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) and (fsShowing in FFormState) then
    raise EInvalidOperation.Create(SVisibleChanged);
  if (not (csDesigning in ComponentState)) and Showing then
  try
    if not Initialized then
      InternalInitialize;
    if not Prepared then
      DoPrepare;
    if Application.MainForm = Self then
      DoUpdatestate;
  except
    Application.HandleException(Self);
  end;
  Inherited;
end;

constructor TCSPForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialized := False;
  FPrepared := False;
end;

destructor TCSPForm.Destroy;
begin
  inherited Destroy;
end;

procedure TCSPForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  InternalFinalize;
end;

procedure TCSPForm.DoPrepare;
begin
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
  InternalPrepare;
  FPrepared := True;
end;

procedure TCSPForm.DoUnprepare;
begin
  if Assigned(FOnUnprepare) then
    FOnUnprepare(Self);
  InternalUnprepare;
  FPrepared := False;
end;

procedure TCSPForm.DoUpdatestate;
begin
  if Assigned(FOnUpdateState) then
    FOnUpdateState(Self);
  InternalUpdateState;
end;

procedure TCSPForm.EnlargeWindow;
begin
  left  := 0;
  top   := 0;
  width := GetSystemMetrics(SM_CXMAXIMIZED) - 4;
  height:= GetSystemMetrics(SM_CYMAXIMIZED) - 4;
end;

function TCSPForm.ExecuteReport(rptValue: Variant): longint;
begin
  Result := SendMessage(CMD_EXECPRINT, Integer(@rptValue))
end;

function TCSPForm.GetModuleByShortcut(
  const AModName: string): TCSPModuleInfo;
begin
  Result := ModuleManager.FindModule(AModName);
end;

function TCSPForm.GetModuleFolder: TCSPRootModuleFolder;
begin
  Result := ModuleManager.RootFolder;
end;

function TCSPForm.GetModuleList: TList;
begin
  Result := ModuleManager.ModuleList;
end;

procedure TCSPForm.InternalFinalize;
begin
  FInitialized := False;
end;

procedure TCSPForm.InternalInitialize;
begin
  FInitialized := True;
end;

procedure TCSPForm.InternalPrepare;
begin
end;

function TCSPForm.InternalSendMessage(AForm: TCSPForm; const AMessageID,
  AParam: Integer): Longint;
var
  AMsg: TAppMessage;
begin
  with AMsg do
  begin
    Msg   := AMessageID;
    Param := AParam;
  end;
  if Assigned(AForm) then  
    AForm.PerformAppMsgProc(AMsg);
  Result := AMsg.Result;
end;

procedure TCSPForm.InternalUnprepare;
begin
end;

procedure TCSPForm.InternalUpdateState;
begin
end;

procedure TCSPForm.PerformAppMsgProc(var Message: TAppMessage);
var
  AHandled: boolean;
begin
  case Message.Msg of
    CMD_PREPARE: DoPrepare;
    CMD_UNPREPARE: DoUnprepare;
    CMD_UPDATESTATE: DoUpdateState;
    CMD_CLOSE: Close;
  end;
  AHandled := False;
  if Assigned(FOnEvaluateMessages) then
    FOnEvaluateMessages(Message, AHandled);
  if not AHandled then
    PopulateAppMsgproc(Message);
end;

procedure TCSPForm.PopulateAppMsgproc(var Message: TAppMessage);
begin
end;

function TCSPForm.PostMessage(AMessageID, AParam: Integer): Longint;
begin
  if (Owner is TCSPForm) then
    Result := Longint(Windows.PostMessage(TCSPForm(Owner).Handle, WM_APPMSG,
      AMessageID, AParam))
  else if (Application.MainForm is TCSPForm) then
    Result := Longint(Windows.PostMessage(Application.MainForm.Handle, WM_APPMSG,
      AMessageID, AParam))
  else
    Result := Longint(Windows.PostMessage(Handle, WM_APPMSG,
      AMessageID, AParam));
end;

function TCSPForm.SendMessage(AMessageID, AParam: Integer): Longint;
begin
  if (Owner is TCSPForm) then
    {
    Result := Windows.SendMessage(TCSPForm(Owner).Handle, WM_APPMSG,
      AMessageID, AParam)
    }
    Result := InternalSendMessage(TCSPForm(Owner), AMessageID, AParam)
  else if (Application.MainForm is TCSPForm) then
    {
    Result := Windows.SendMessage(Application.MainForm.Handle, WM_APPMSG,
      AMessageID, AParam)
    }
    Result := InternalSendMessage(TCSPForm(Application.MainForm),
      AMessageID, AParam)
  else
    Result := Windows.SendMessage(Handle, WM_APPMSG,
      AMessageID, AParam);
end;

procedure TCSPForm.SetExecCmd(const Value: string);
begin
  FLastCmd := Value;
  SendMessage(CMD_EXECCMD, Integer(FLastCmd))
end;

procedure TCSPForm.SetMessageID(const Value: integer);
begin
  FLastMessageID := Value;
  SendMessage(MessageID, 0);
end;

procedure TCSPForm.SetupModule(AMod: TCSPCustomModulePanel;
  APopupMenu: TPopupMenu);
begin
  AMod.FMainForm := Self;
  AMod.FMainPopupMenu := APopupMenu;
end;

procedure TCSPForm.SetupModule(AMod: TCSPCustomModule; APopupMenu: TPopupMenu);
begin
  AMod.FMainForm := Self;
  AMod.FMainPopupMenu := APopupMenu;
end;

procedure TCSPForm.WndProc(var Message: TMessage);
var
  AMsg: TAppMessage;
begin
  if Message.Msg = WM_APPMSG then
  begin
    with AMsg do
    begin
      Msg   := Message.WParam;
      Param := Message.LParam;
    end;
    PerformAppMsgProc(AMsg);
    Message.Result := AMsg.Result;
  end else
  inherited WndProc(Message);
end;

{ TCSPCustomModule }

constructor TCSPCustomModule.Create(AOwner: TComponent);
var
  PRec: TCSPModuleInfo;
begin
  inherited Create(AOwner);
  FMainForm := nil;
  FFuncSupported := [];
  PRec := ModuleManager.FindModule(ShortcutName);
  if PRec <> nil then
  begin
    PRec.InstanceCount := PRec.InstanceCount + 1;
    if PRec.ModuleHandle <> 0 then
      BringCachedHandleToTop(PRec.ModuleHandle);
  end;
end;

class function TCSPCustomModule.APIObjectAuthCheck: String;
begin
  Result := '';
end;

constructor TCSPCustomModule.CreateModule(AOwner: TComponent;
  AParent: TWinControl);
begin
  Create(AOwner);
  Parent := AParent;
  BorderStyle := bsNone;
  Align := alClient;
  WindowState := wsMaximized;
end;

destructor TCSPCustomModule.Destroy;
var
  PRec: TCSPModuleInfo;
begin
  PRec := ModuleManager.FindModule(ShortcutName);
  if PRec <> nil then
  begin
    PRec.InstanceCount := PRec.InstanceCount - 1;
    if (PRec.InstanceCount = 0) and (PRec.ModuleHandle <> 0) then
      BringCachedHandleToBottom(PRec.ModuleHandle);
  end;
  inherited Destroy;
end;

function TCSPCustomModule.GetExtendedFunctionList: TActionList;
begin
  Result := nil;
end;

function TCSPCustomModule.GetOpenRelatedList: TActionList;
begin
  Result := nil;
end;

procedure TCSPCustomModule.SetFuncSupported(
  const Value: TCSPFunctionTypes);
begin
  if FFuncSupported <> Value then
  begin
    FFuncSupported := Value;
    if FMainForm <> nil then
      FMainForm.MessageID := CMD_UPDATESTATS;
  end;
end;

class function TCSPCustomModule.ModuleType: TCSPModuleType;
begin
  Result := mtFullModule;
end;

class function TCSPCustomModule.Category: string;
begin
  Result := '';
end;

class function TCSPCustomModule.Description: string;
begin
  Result := '';
end;

class function TCSPCustomModule.ShortcutName: string;
begin
  Result := '';
end;

class function TCSPCustomModule.GetExtData: Pointer;
begin
  Result := nil;
end;

class procedure TCSPCustomModule.SetExtData(const Value: Pointer);
begin
end;

class function TCSPCustomModule.HideModule: boolean;
begin
  Result := False;
end;

class function TCSPCustomModule.SingleInstance: boolean;
begin
  Result := False;
end;

class function TCSPCustomModule.ScriptBased: boolean;
begin
  Result := False;
end;

{ TBaseMVCObject }

procedure TCSPBaseModuleInfo.Changed;
begin
  if (Owner <> nil) and (Owner is TCSPBaseModuleInfo) then
    TCSPBaseModuleInfo(Owner).Changed
  else
    DoChange;
end;

constructor TCSPBaseModuleInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Changed;
end;

destructor TCSPBaseModuleInfo.Destroy;
begin
  Changed;
  inherited Destroy;
end;

procedure TCSPBaseModuleInfo.DoChange;
begin
end;

function TCSPBaseModuleInfo.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TCSPBaseModuleInfo.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Item: TCSPBaseModuleInfo;
begin
  for I := 0 to ComponentCount - 1 do
    if (Components[i] is TCSPBaseModuleInfo) then
    begin      
      Item := TCSPBaseModuleInfo(Components[i]);
      if (Item.Owner = Self) then
        Proc(Item);      
    end;
end;

function TCSPBaseModuleInfo.GetParentFolder(const AFolder: string): TCSPModuleFolder;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TCSPModuleFolder) and
       (SameText(TCSPModuleFolder(Components[I]).FolderName, AFolder)) then
    begin
      Result := TCSPModuleFolder(Components[I]);
      Break;
    end;
end;

function TCSPBaseModuleInfo.GetProgamItem(const AProgram: string): TCSPModuleInfo;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TCSPModuleInfo) and
       (SameText(TCSPModuleInfo(Components[I]).ShortcutName, AProgram)) then
    begin
      Result := TCSPModuleInfo(Components[I]);
      Break;
    end;
end;

procedure TCSPBaseModuleInfo.SetChildOrder(Child: TComponent; Order: Integer);
begin
  if FindComponent(Child.Name) = Child then
    Child.ComponentIndex := Order;
end;

{ TMVCProgramItem }

constructor TCSPModuleInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ModuleManager.FModuleList.Add(Self);
  Changed;
end;

destructor TCSPModuleInfo.Destroy;
begin
  ModuleManager.FModuleList.Delete(ModuleManager.FModuleList.IndexOf(Self));
  Changed;
  inherited Destroy;
end;

function TCSPModuleInfo.GetFormattedName: string;
begin
  Result := dgettext(ShortcutName, Description) + ' - ' + ShortcutName;
end;

procedure TCSPModuleInfo.SetAPIObjectAuthCheck(const Value: string);
begin
  if FAPIObjectAuthCheck <> Value then
  begin
    FAPIObjectAuthCheck := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetCategory(const Value: string);
begin
  if FCategory <> Value then
  begin
    FCategory := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetHideModule(const Value: boolean);
begin
  if FHideModule <> Value then
  begin
    FHideModule := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetInstanceCount(const Value: integer);
begin
  if FInstanceCount <> Value then
  begin
    FInstanceCount := Value;
  end;
end;

procedure TCSPModuleInfo.SetIntegrationType(const Value: TCSPIntegrationType);
begin
  if FIntegrationType <> Value then
  begin
    FIntegrationType := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetModuleClass(const Value: string);
begin
  if FModuleClass <> Value then
  begin
    FModuleClass := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetModuleHandle(const Value: HMODULE);
begin
  if FModuleHandle <> Value then
  begin
    FModuleHandle := Value;
  end;
end;

procedure TCSPModuleInfo.SetModuleType(const Value: TCSPModuleType);
begin
  if FModuleType <> Value then
  begin
    FModuleType := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetPackageName(const Value: string);
begin
  if FPackageName <> Value then
  begin
    FPackageName := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetScriptBased(const Value: boolean);
begin
  if FScriptBased <> Value then
  begin
    FScriptBased := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetShortcutName(const Value: string);
begin
  if FShortcutName <> Value then
  begin
    FShortcutName := Value;
    Name := Value;
    Changed;
  end;
end;

procedure TCSPModuleInfo.SetSingleInstance(const Value: boolean);
begin
  if FSingleInstance <> Value then
  begin
    FSingleInstance := Value;
    Changed;
  end;
end;

{ TMVCFolder }

constructor TCSPModuleFolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModified := False;
  FLoading := False;
  Self.Name := CreateUniqueName(Self);
end;

destructor TCSPModuleFolder.Destroy;
begin
  inherited Destroy;
end;

procedure TCSPModuleFolder.DoChange;
begin
  if not FLoading then
    FModified := True;
end;

function TCSPModuleFolder.FindParent(AItem: TCSPModuleFolder): boolean;
begin
  Result := False;
  if (Owner <> nil) and (Owner is TCSPModuleFolder) then
  begin
    if Owner = AItem then
      Result := True
    else
      Result := TCSPModuleFolder(Owner).FindParent(AItem);
  end;
end;

function TCSPModuleFolder.GetFormattedName: string;
begin
  Result := _(FolderName);
end;

function TCSPModuleFolder.BindParentFolder(AStr: TStringList;
  APos: integer): TCSPModuleFolder;
var
  AComp: TCSPModuleFolder;
begin
  if APos < AStr.Count then
  begin
    AComp := GetParentFolder(AStr[APos]);
    if AComp = nil then
    begin
      if Self is TCSPRootModuleFolder then
        AComp := TCSPGroupModuleFolder.Create(Self)
      else
        AComp := TCSPModuleFolder.Create(Self);
      AComp.FolderName := AStr[APos]; 
    end;
    Inc(APos);
    Result := AComp.BindParentFolder(AStr, APos);     
  end else
    Result := Self;
end;

procedure TCSPModuleFolder.SetFolderName(const Value: string);
begin
  if FFolderName <> Value then
  begin
    FFolderName := Value;
    Changed;
  end;
end;

{ TCSPRootModuleFolder }

constructor TCSPRootModuleFolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSmallImageList := TcxImageList.Create(nil);
  FLargeImageList := TcxImageList.Create(nil);
  FLargeImageList.Width := 24;
  FLargeImageList.Height:= 24;  
end;

procedure TCSPRootModuleFolder.DefineProperties(Filer: TFiler);

  function HasLargeBitmap: boolean;
  begin
    Result := FLargeImageList.Count > 0;  
  end;

  function HasSmallBitmap: boolean;
  begin
    Result := FSmallImageList.Count > 0;  
  end;
    
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('LargeBitmap', ReadLargeBitmap, WriteLargeBitmap, HasLargeBitmap);
  Filer.DefineBinaryProperty('SmallBitmap', ReadSmallBitmap, WriteSmallBitmap, HasSmallBitmap);
end;

destructor TCSPRootModuleFolder.Destroy;
begin
  inherited Destroy;
  FSmallImageList.Free;
  FLargeImageList.Free;
end;

procedure TCSPRootModuleFolder.ReadLargeBitmap(Stream: TStream);
begin
  Stream.ReadComponent(FLargeImageList);
end;

procedure TCSPRootModuleFolder.ReadSmallBitmap(Stream: TStream);
begin
  Stream.ReadComponent(FSmallImageList)
end;

procedure TCSPRootModuleFolder.WriteLargeBitmap(Stream: TStream);
begin
  Stream.WriteComponent(FLargeImageList);
end;

procedure TCSPRootModuleFolder.WriteSmallBitmap(Stream: TStream);
begin
  Stream.WriteComponent(FSmallImageList);
end;

{ TCSPGroupModuleFolder }

constructor TCSPGroupModuleFolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLargeImageIndex := -1;
  FSmallImageIndex := -1;
end;

destructor TCSPGroupModuleFolder.Destroy;
begin
  inherited Destroy;
end;

{ TCSPCustomModulePanel }

constructor TCSPCustomModulePanel.Create(AOwner: TComponent);
var
  PRec: TCSPModuleInfo;
begin
  inherited Create(AOwner);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FInitialized := False;
  FPrepared := False;  
  FMainForm := nil;
  FFuncSupported := [];
  PRec := ModuleManager.FindModule(ShortcutName);
  if PRec <> nil then
  begin
    PRec.InstanceCount := PRec.InstanceCount + 1;
    if PRec.ModuleHandle <> 0 then
      BringCachedHandleToTop(PRec.ModuleHandle);
  end;
end;

class function TCSPCustomModulePanel.APIObjectAuthCheck: String;
begin
  Result := '';
end;

constructor TCSPCustomModulePanel.CreateModule(AOwner: TComponent;
  AParent: TWinControl);
begin
  Create(AOwner);
  Parent := AParent;
  Align := alClient;
end;

destructor TCSPCustomModulePanel.Destroy;
var
  PRec: TCSPModuleInfo;
begin
  PRec := ModuleManager.FindModule(ShortcutName);
  if PRec <> nil then
  begin
    PRec.InstanceCount := PRec.InstanceCount - 1;
    if (PRec.InstanceCount = 0) and (PRec.ModuleHandle <> 0) then
      BringCachedHandleToBottom(PRec.ModuleHandle);
  end;
  InternalFinalize;
  inherited Destroy;
end;

procedure TCSPCustomModulePanel.DoPrepare;
begin
  InternalPrepare;
  FPrepared := True;
end;

procedure TCSPCustomModulePanel.DoUnprepare;
begin
  InternalUnprepare;
  FPrepared := False;
end;

procedure TCSPCustomModulePanel.DoUpdatestate;
begin
  InternalUpdateState;
end;

function TCSPCustomModulePanel.ExecuteReport(rptValue: Variant): longint;
begin
  Result := SendMessage(CMD_EXECPRINT, Integer(@rptValue))
end;

function TCSPCustomModulePanel.GetExtendedFunctionList: TActionList;
begin
  Result := nil;
end;

function TCSPCustomModulePanel.GetModuleByShortcut(
  const AModName: string): TCSPModuleInfo;
begin
  Result := ModuleManager.FindModule(AModName);
end;

function TCSPCustomModulePanel.GetModuleFolder: TCSPRootModuleFolder;
begin
  Result := ModuleManager.RootFolder;
end;

function TCSPCustomModulePanel.GetOpenRelatedList: TActionList;
begin
  Result := nil;
end;

procedure TCSPCustomModulePanel.SetFuncSupported(
  const Value: TCSPFunctionTypes);
begin
  if FFuncSupported <> Value then
  begin
    FFuncSupported := Value;
    if FMainForm <> nil then
      FMainForm.MessageID := CMD_UPDATESTATS;
  end;
end;

procedure TCSPCustomModulePanel.SetMessageID(const Value: integer);
begin
  FLastMessageID := Value;
  SendMessage(MessageID, 0);
end;

class function TCSPCustomModulePanel.ModuleType: TCSPModuleType;
begin
  Result := mtFullModule;
end;

procedure TCSPCustomModulePanel.PerformAppMsgProc(var Message: TAppMessage);
var
  AHandled: boolean;
begin
  case Message.Msg of
    CMD_PREPARE: DoPrepare;
    CMD_UNPREPARE: DoUnprepare;
    CMD_UPDATESTATE: DoUpdateState;
  end;
  AHandled := False;
  if not AHandled then
    PopulateAppMsgproc(Message);
end;

procedure TCSPCustomModulePanel.PopulateAppMsgproc(var Message: TAppMessage);
begin

end;

function TCSPCustomModulePanel.PostMessage(AMessageID,
  AParam: Integer): Longint;
begin
  if (Owner is TCSPForm) then
    Result := Longint(Windows.PostMessage(TCSPForm(Owner).Handle, WM_APPMSG,
      AMessageID, AParam))
  else if (Application.MainForm is TCSPForm) then
    Result := Longint(Windows.PostMessage(Application.MainForm.Handle, WM_APPMSG,
      AMessageID, AParam))
  else
    Result := Longint(Windows.PostMessage(Handle, WM_APPMSG,
      AMessageID, AParam));
end;

class function TCSPCustomModulePanel.Category: string;
begin
  Result := '';
end;

procedure TCSPCustomModulePanel.CMShowingChanged(var Message: TMessage);
begin
  if (not (csDesigning in ComponentState)) and Showing then
  try
    if not Initialized then
      InternalInitialize;
    if not Prepared then
      DoPrepare;
  except
    Application.HandleException(Self);
  end;
  Inherited;
end;

class function TCSPCustomModulePanel.Description: string;
begin
  Result := '';
end;

class function TCSPCustomModulePanel.ShortcutName: string;
begin
  Result := '';
end;

class function TCSPCustomModulePanel.GetExtData: Pointer;
begin
  Result := nil;
end;

function TCSPCustomModulePanel.SendMessage(AMessageID,
  AParam: Integer): Longint;
begin
  if (Owner is TCSPForm) then
    Result := Windows.SendMessage(TCSPForm(Owner).Handle, WM_APPMSG,
      AMessageID, AParam)
  else if (Application.MainForm is TCSPForm) then
    Result := Windows.SendMessage(Application.MainForm.Handle, WM_APPMSG,
      AMessageID, AParam)
  else
    Result := Windows.SendMessage(Handle, WM_APPMSG,
      AMessageID, AParam);
end;

procedure TCSPCustomModulePanel.SetExecCmd(const Value: string);
begin
  FLastCmd := Value;
  SendMessage(CMD_EXECCMD, Integer(FLastCmd))
end;

class procedure TCSPCustomModulePanel.SetExtData(const Value: Pointer);
begin
end;

class function TCSPCustomModulePanel.HideModule: boolean;
begin
  Result := False;
end;

procedure TCSPCustomModulePanel.InternalFinalize;
begin
  FInitialized := False;
end;

procedure TCSPCustomModulePanel.InternalInitialize;
begin
  FInitialized := True;
end;

procedure TCSPCustomModulePanel.InternalPrepare;
begin
end;

procedure TCSPCustomModulePanel.InternalUnprepare;
begin
end;

procedure TCSPCustomModulePanel.InternalUpdateState;
begin
end;

class function TCSPCustomModulePanel.SingleInstance: boolean;
begin
  Result := False;
end;

procedure TCSPCustomModulePanel.WndProc(var Message: TMessage);
var
  AMsg: TAppMessage;
begin
  if Message.Msg = WM_APPMSG then
  begin
    with AMsg do
    begin
      Msg   := Message.WParam;
      Param := Message.LParam;
    end;
    PerformAppMsgProc(AMsg);
    Message.Result := AMsg.Result;
  end else
  inherited WndProc(Message);
end;

class function TCSPCustomModulePanel.ScriptBased: boolean;
begin
  Result := False;
end;

initialization
  RegisterClasses([TCSPBaseModuleInfo, TCSPModuleInfo, TCSPModuleFolder,
    TCSPGroupModuleFolder, TCSPRootModuleFolder]);
  ModuleManager := TCSPInternalModuleManager.Create;
  CachedPackageHandleList := nil;

finalization
  ModuleManager.Free;
  if CachedPackageHandleList <> nil then
  begin
    UnloadAllPackageModule;
    CachedPackageHandleList.Free;
  end;
  UnRegisterClasses([TCSPBaseModuleInfo, TCSPModuleInfo, TCSPModuleFolder,
    TCSPGroupModuleFolder, TCSPRootModuleFolder]);
end.

