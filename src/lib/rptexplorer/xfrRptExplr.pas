unit xfrRptExplr;

interface
uses
  SysUtils, Classes, Windows, Messages, Forms, frxClass,
  frxVariables, frxDesgn, Graphics, Controls, dxRibbonForm;

type
  TxfrExplorerEventTypes = (etPreview, etDesign, etShowProperties, etTagAuth);
  TxfrDataSourceMode = (dsStream, dsFile);
  TxfrItemType = (itmRoot = 0, itmRecycleDir, itmReport, itmDirectory);
  TxfrReportItemAttr = (ftReadOnly = 0, ftHidden, ftSystem, ftPwdProtected, ftDirectory);
  TxfrReportItemAttrs = set of TxfrReportItemAttr;

  TxfrAbstractItem = class;
  TxfrReportItem = class;
  TxfrRecycleDir = class;
  TxfrCustomExplorer = class;
  TxfrDirectoryItem = class;
  TxfrReportExplorer = class;

  TxfrBeforeExecReport = procedure(var ARptID: integer; var ParamValues: variant) of object;
  TxfrValidateReport = procedure(AReport: TxfrReportItem; const AEvent: TxfrExplorerEventTypes) of object;

  TxfrAbstractItem = class(TComponent)
  private
    FReportName: string;
    FDesc: string;
    FDate: TDateTime;
    FAttrs: TxfrReportItemAttrs;
    FItemType: TxfrItemType;
    FDeleted: boolean;
    FOnSave: TNotifyEvent;
    FOldOwner: TxfrAbstractItem;
    FLinkedNode: TObject;
    function GetReportName: string;
    procedure SetItemType(const Value: TxfrItemType);
    procedure SetReportName(const Value: string);
    procedure SetDeleted(const Value: boolean);
    function GetSecureArchive: boolean;
    function GetPath: string;
    function GetTypeStr: string;
    function GetTimeStampStr: string;
    procedure SetDate(const Value: TDateTime);
    procedure AssignComponentName(AOwner: TComponent);
    function FindComponentName(AOwner: TComponent; const NewName: string): TComponent;
    procedure ReadOldOwner(Reader: TReader);
    procedure WriteOldOwner(Writer: TWriter);

  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetNextID: Cardinal; virtual;
    procedure Restore;
    procedure SaveMe;
    procedure AssignTo(Dest: TPersistent); override;

    procedure GetChildFolders(AList: TStrings);
    procedure GetParentFolders(AList: TStrings);    

    function InternalGetReportName: string; virtual;

    property SecureArchive: boolean read GetSecureArchive;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    function FindParent(AItem: TxfrAbstractItem): boolean;
    
    procedure MoveTo(NewOwner: TComponent); virtual;

    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property Path: string read GetPath;

    property LinkedNode: TObject read FLinkedNode write FLinkedNode;
    property TypeStr: string read GetTypeStr;
    property TimestampStr: string read GetTimeStampStr;
  published
    property ItemType: TxfrItemType read FItemType write SetItemType;
    property TimeStamp: TDateTime read FDate write SetDate;
    property ReportName: string read GetReportName write SetReportName;
    property ReportAttr: TxfrReportItemAttrs read FAttrs write FAttrs;
    property Description: string read FDesc write FDesc;
    property Deleted: boolean read FDeleted write SetDeleted;
    property Name;
    property Tag;
  end;

  TxfrDirectoryItem = class(TxfrAbstractItem)
  private
    FRecycleDir: TxfrRecycleDir;
    FLastID: Cardinal;
    FExplorer: TxfrReportExplorer;

    procedure ReorderRecycleDir; virtual;    
    procedure CreateRecycleDir;
    procedure ReadLastID(Reader: TReader);
    procedure WriteLastID(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    function GetChildOwner: TComponent; override;

    procedure ValidateInsert(AComponent: TComponent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
      
    procedure ReadComponent(Stream: TStream);
    procedure WriteComponent(Stream: TStream);

    function GetNextID: Cardinal; override;
  public
    constructor Create(AOwner: TComponent); override;
    
    function FindItemInternalID(ARptID: Integer): TxfrAbstractItem;
    function FindItemByID(ARptID: Integer): TxfrAbstractItem;
    function FindName(AName: String; AItem: TxfrAbstractItem): boolean;

    function AddDir: TxfrDirectoryItem;
    function AddReport: TxfrReportItem;

    procedure Clear;
    procedure Delete(AIndex: integer);
    property RecycleDir: TxfrRecycleDir read FRecycleDir;
  end;

  TxfrRecycleDir = class(TxfrDirectoryItem)
  private
    procedure ReorderRecycleDir; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Restore(AIndex: Integer);
    procedure Release(AIndex: Integer);
    procedure ReleaseAll;
    procedure RestoreAll;
  end;

  TxfrReportItem = class(TxfrAbstractItem)
  private
    FStream: TMemoryStream;
    FReportID: integer;
    FInternalID: integer;
    procedure SetReportID(const Value: integer);

    function GetSize: Longint;
    function GetSizeStr: string;
    function GetContent: string;
    procedure SetContent(const Value: string);
    
    procedure ReadInternalID(Reader: TReader);
    procedure WriteInternalID(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ValidateInsert(AComponent: TComponent); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);

    property Size: Longint read GetSize;
    property SizeStr: string read GetSizeStr;    
  published
    property ReportID: integer read FReportID write SetReportID;
    property FileContent: string read GetContent write SetContent;
  end;

  TxfrReportExplorer = class(TComponent)
  private
    FDesigner: TfrxDesigner;
    FRptVariables: TfrxVariables;
    FRoot: TxfrDirectoryItem;
    FRptFileName: string;
    FSourceMode: TxfrDataSourceMode;
    FReport: TfrxReport;
    FSecureArchive: boolean;
    FExplorer: TxfrCustomExplorer;
    FBeforeExecReport: TxfrBeforeExecReport;
    FModified: boolean;
    FTagAuthentication: string;
    FTagAuthenticated: boolean;
    FOnValidateReport: TxfrValidateReport;
    FCurrFolder: TxfrDirectoryItem;
    FCurrentReport: TxfrReportItem;
    FOnUpdating: boolean;
    FOnSaveData: TNotifyEvent;
    FOnLoadData: TNotifyEvent;
    function GetVariable(Index: String): Variant;
    procedure SetVariable(Index: String; const Value: Variant);
    procedure ApplyLocalVariables(const AHasParam, AParamIsArray: boolean;
      const AParamValues: variant);
    procedure InternalOnSave(Sender: TObject);
    function GetTagAuthenticated: boolean;
    function GetCurrFolder: TxfrDirectoryItem;
    procedure SetSecureArchive(const Value: boolean);
    procedure SetTagAuthentication(const Value: string);
  protected
    procedure LoadData;
    procedure SaveData;

    procedure ValidateReport(AReport: TxfrReportItem; const AEvent: TxfrExplorerEventTypes);    
    procedure AssignVariables(AReport: TxfrReportItem);
    procedure DesignReport(AReport: TxfrReportItem);
    procedure PreviewReport(AReport: TxfrReportItem);
    function Authenticate: boolean;
    property CurrentReport: TxfrReportItem read FCurrentReport;
    function frxDesignerSaveReport(Report: TfrxReport;
      SaveAs: Boolean): Boolean;
    function frxDesignerLoadReport(Report: TfrxReport): Boolean;
    procedure SetTagAuthenticated(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure ShowExplorer;
    procedure ExecuteReport(const rptValues: variant; const ShowDesigner: boolean = false);
    procedure FinalizeExplorer;

    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property Variables[Index: String]: Variant read GetVariable
      write SetVariable; default;
    property Root: TxfrDirectoryItem read FRoot;
    property CurrentFolder: TxfrDirectoryItem read GetCurrFolder write FCurrFolder;
    property Modified: boolean read FModified;
    property TagAuthenticated: boolean read GetTagAuthenticated;     
  published
    property SecureArchive: boolean read FSecureArchive write SetSecureArchive;
    property TagAuthentication: string read FTagAuthentication write SetTagAuthentication; 
    property ReportFileName: string read FRptFileName write FRptFileName;
    property DataSourceMode: TxfrDataSourceMode read FSourceMode write FSourceMode default dsFile;
    property frxReport: TfrxReport read FReport write FReport;
    property OnBeforeExecReport: TxfrBeforeExecReport read FBeforeExecReport write FBeforeExecReport;
    property OnValidateReport: TxfrValidateReport read FOnValidateReport write FOnValidateReport;
    property OnSaveData: TNotifyEvent read FOnSaveData write FOnSaveData;
    property OnLoadData: TNotifyEvent read FOnLoadData write FOnLoadData;
  end;

  TxfrExplorerResources = class(TObject)
  private
    FDisabledButtonImages: TImageList;
    FMainButtonImages: TImageList;
    function GetMainButtonImages: TImageList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetButtonImages(Images: TBitmap; Clear: Boolean = False);

    property MainButtonImages: TImageList read GetMainButtonImages;
    property DisabledButtonImages: TImageList read FDisabledButtonImages;
  end;

  TxfrCustomExplorer = class(TForm)
  private
    FReport: TfrxReport;
    FShowHiddenObj: boolean;
    FExplorer: TxfrReportExplorer;
    procedure SetShowHiddenObj(const Value: boolean);
  protected
    FModalFinished: Boolean;
    FSelectedObjects: TList;

    procedure SetReporter(AReport: TfrxReport);
    procedure SetExplorer(AExpl: TxfrReportExplorer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitExplorer; virtual; abstract;

    procedure Lock; virtual; abstract;
    procedure UpdateFolderTree; virtual; abstract;
    procedure UpdateReportList; virtual; abstract;
    procedure UpdateItems; virtual; abstract;
        
    property Report: TfrxReport read FReport;
    property Explorer: TxfrReportExplorer read FExplorer; 
    property SelectedObjects: TList read FSelectedObjects;
    property ShowHiddenObjects: boolean read FShowHiddenObj write SetShowHiddenObj;
  end;
      
  TxfrExplorerClass = class of TxfrCustomExplorer;

function xfrExplorerResources: TxfrExplorerResources; 

var
  frxExplorerClass: TxfrExplorerClass;

implementation
uses
  RTLConsts, Variants, frxGZip, frxUtils, frxPassw, frxNetUtils,
  xfrOpenSaveDlg;

const
  frxItemTypeStr: array[itmRoot..itmDirectory] of string =
   ('Root Folder', 'Recycle Bin', 'Report File', 'Folder');

  SecureSignature: array[1..4] of AnsiChar = 'FXZ'#7;

var
  FResources             : TxfrExplorerResources = nil;

resourcestring
  SInvalidInstance   = 'not a valid instance class';
  SNoInstanceAllowed = 'could not have an instance for this item';
  SReportIDExists    = 'Report ID has already exists';
  SInvalidOwnerRef   = 'Invalid owner references';
  SNotARootObject    = 'Current Object is not a root object';
  SUnAssignedRptFile = 'Report file name unspecified';
  SCanSaveFile       = 'Could not save report database, please make sure you have sufficient access right to report file';
  SMissLink          = 'Missing link to current designed report, cannot save';
  SReportTemplate    =
  '<?xml version="1.0" encoding="utf-8"?>'#13#10+
  '<TfrxReport Version="4.1" DotMatrixReport="False" '+
  'IniFile="\Software\Fast Reports" PreviewOptions.Buttons="4095" '+
  'PreviewOptions.Zoom="1" PrintOptions.Printer="Default" '+
  'PrintOptions.PrintOnSheet="0" ReportOptions.CreateDate="39115,6589597801" '+
  'ReportOptions.Description.Text="" ReportOptions.'+
  'LastChange="39115,6590641204" ScriptLanguage="PascalScript" ScriptText.'+
  'Text="&#13;&#10;begin&#13;&#10;&#13;&#10;end." PropData="'+
  '044C65667403230203546F70026908446174617365747301000956617269'+
  '61626C65730100055374796C650100">'#13#10+
  '  <TfrxDataPage Name="Data" Height="1000" Left="0" '+
  'Top="0" Width="1000"/>'#13#10+
  '  <TfrxReportPage Name="Page1" PaperWidth="210" PaperHeight="297" '+
  'PaperSize="9" LeftMargin="10" RightMargin="10" TopMargin="10" '+
  'BottomMargin="10" ColumnWidth="0" ColumnPositions.Text="" '+
  'HGuides.Text="" VGuides.Text="">'#13#10+
  '    <TfrxReportTitle Name="ReportTitle1" Height="22,67718" '+
  'Left="0" Top="18,89765" Width="718,1107"/>'#13#10+
  '    <TfrxMasterData Name="MasterData1" Height="22,67718" '+
  'Left="0" Top="102,04731" Width="718,1107" ColumnWidth="0" '+
  'ColumnGap="0" RowCount="0"/>'#13#10+
  '    <TfrxPageFooter Name="PageFooter1" Height="22,67718" Left="0" '+
  'Top="185,19697" Width="718,1107">'#13#10+
  '      <TfrxMemoView Name="Memo1" Left="642,5201" Top="0" '+
  'Width="75,5906" Height="18,89765" HAlign="haRight" Text="[Page#]"/>'#13#10+
  '    </TfrxPageFooter>'#13#10+
  '  </TfrxReportPage>'#13#10+
  '</TfrxReport>';

function TrimAll(const Str: string): string;
var
  I: integer;
begin
  Result := EmptyStr;
  for i := 1 to PCardinal(Cardinal(Str)-4)^ do
    if (Str[i] <> ' ') then
      Result := Result + Str[i];
end;

{ TxfrAbstractItem }

procedure TxfrAbstractItem.AssignTo(Dest: TPersistent);
begin
  if (Dest is TxfrAbstractItem) then
  begin
    TxfrAbstractItem(Dest).ItemType     := Self.ItemType;
    TxfrAbstractItem(Dest).TimeStamp    := Self.TimeStamp;
    TxfrAbstractItem(Dest).ReportName     := Self.ReportName;
    TxfrAbstractItem(Dest).ReportAttr   := Self.ReportAttr;
    TxfrAbstractItem(Dest).Description  := Self.Description;
    TxfrAbstractItem(Dest).Deleted      := Self.Deleted;
  end else
    inherited AssignTo(Dest);
  SaveMe;
end;

constructor TxfrAbstractItem.Create(AOwner: TComponent);
begin
  FOldOwner := nil;
  FDate := Now;
  FAttrs := [];
  if (AOwner = nil) or (not (AOwner is TxfrDirectoryItem)) then
    FItemType := itmRoot;
  AssignComponentName(AOwner);
  inherited Create(AOwner);
  if (Owner <> nil) and (Owner is TxfrDirectoryItem) and
     (TxfrDirectoryItem(Owner).ItemType = itmRoot) then
    TxfrDirectoryItem(Owner).ReorderRecycleDir;
  SetReportName('Untitled');
  SaveMe;
end;

destructor TxfrAbstractItem.Destroy;
begin
  inherited Destroy;
end;

function TxfrAbstractItem.FindParent(AItem: TxfrAbstractItem): boolean;
begin
  Result := False;
  if (Owner <> nil) then
  begin
    if Owner = AItem then
      Result := True
    else
      Result := TxfrAbstractItem(Owner).FindParent(AItem);
  end;
end;

function TxfrAbstractItem.GetSecureArchive: boolean;
begin
  if (Owner <> nil) then
  begin
    if (Owner is TxfrAbstractItem) and
       (TxfrAbstractItem(Owner).ItemType <> itmRoot) then
      Result := TxfrAbstractItem(Owner).GetSecureArchive
    else begin
      if (ItemType = itmRoot) and (TxfrDirectoryItem(Self).FExplorer <> nil) then
        Result := TxfrDirectoryItem(Self).FExplorer.SecureArchive
      else
        Result := False;
    end;
  end else
    Result := False;
end;

function TxfrAbstractItem.GetNextID: Cardinal;
begin
  if (ItemType <> itmRoot) and (Owner is TxfrAbstractItem) then
    Result := TxfrAbstractItem(Owner).GetNextID;
end;

function TxfrAbstractItem.GetParentComponent: TComponent;
begin
  if (Owner <> nil) and (Owner is TxfrDirectoryItem) then
    Result := Owner else
  Result := inherited GetParentComponent;
end;

function TxfrAbstractItem.GetPath: string;
var
  AStr: string;
begin
  if (ItemType = itmRoot) or (not(Owner is TxfrAbstractItem)) or (Owner = nil) then
    Result := '\'
  else
  begin
    AStr := TxfrAbstractItem(Owner).GetPath;
    if (ItemType in [itmRoot, itmDirectory, itmRecycleDir]) then
      Result := AStr + '\' + ReportName
    else
      Result := AStr;
  end;
end;

function TxfrAbstractItem.GetTimeStampStr: string;
begin
  Result := DateTimeToStr(TimeStamp);
end;

function TxfrAbstractItem.GetTypeStr: string;
begin
  Result := frxItemTypeStr[ItemType]; 
end;

function TxfrAbstractItem.GetReportName: string;
begin
  Result := InternalGetReportName;
end;

function TxfrAbstractItem.HasParent: Boolean;
begin
  if (Owner <> nil) and (Owner is TxfrDirectoryItem) then
    Result := True else
    Result := inherited HasParent;
end;

function TxfrAbstractItem.InternalGetReportName: string;
begin
  Result := FReportName;
end;

procedure TxfrAbstractItem.MoveTo(NewOwner: TComponent);
begin
  if Owner <> nil then
  begin
    FOldOwner := TxfrAbstractItem(Owner);
    Owner.RemoveComponent(Self);
  end;
  if NewOwner <> nil then
    NewOwner.InsertComponent(Self);
  if (NewOwner is TxfrRecycleDir) then
  begin
    Deleted := True
  end else
  begin
    FOldOwner := nil;
    Deleted := False;
  end;
  if (ItemType = itmDirectory) then
    TxfrDirectoryItem(Self).CreateRecycleDir
  else if (Owner <> nil) and (Owner is TxfrDirectoryItem) then
    TxfrDirectoryItem(Owner).CreateRecycleDir;
  SaveMe;
end;

procedure TxfrAbstractItem.Restore;
begin
  if FOldOwner <> nil then
  begin
    Owner.RemoveComponent(Self);
    if (not (FOldOwner is TxfrRecycleDir)) and (Deleted) then
      Deleted := False;
    FOldOwner.InsertComponent(Self);
    FOldOwner := nil;
    SaveMe;
  end else
    raise Exception.Create(SInvalidOwnerRef);
end;

procedure TxfrAbstractItem.SaveMe;
begin
  if (Owner <> nil) and (Owner is TxfrAbstractItem) then
    TxfrAbstractItem(Owner).SaveMe
  else
    if Assigned(FOnSave) then
      FOnSave(Self);
end;

procedure TxfrAbstractItem.SetDate(const Value: TDateTime);
begin
  if csReading in ComponentState then
    FDate := Value
  else
  if Value <> FDate then
  begin
    FDate := Value;
    SaveMe;
  end;
end;

procedure TxfrAbstractItem.SetDeleted(const Value: boolean);
var
  i: integer;
begin
  if csReading in ComponentState then
    FDeleted := Value
  else
  if FDeleted <> Value then
  begin
    for i := 0 to ComponentCount-1 do
      if (Components[i] is TxfrAbstractItem) then
        TxfrAbstractItem(Components[i]).Deleted := Value;
    FDeleted := Value;
    SaveMe;
  end;
end;

procedure TxfrAbstractItem.SetItemType(const Value: TxfrItemType);
begin
  if (csReading in ComponentState) then
    FItemType := Value
  else
  if FItemType <> Value then
  begin
    FItemType := Value;
    SaveMe;
  end;
end;

procedure TxfrAbstractItem.SetReportName(const Value: string);
var
  AIndex: integer;
  ARet: boolean;
  AName: String;
begin
  if csReading in ComponentState then
    FReportName := Value
  else
  begin
    if (ftReadOnly in FAttrs) or (FReportName = Value) then
      exit;
    AName := Value;
    if (Owner <> nil) and (Owner is TxfrDirectoryItem) then
    begin
      AName := Value;
      AIndex := 0;
      ARet := True;
      while ARet do
      begin
        ARet := TxfrDirectoryItem(Owner).FindName(AName, Self);
        if ARet then
        begin
          Inc(AIndex);
          AName := Format('%s (%d)', [Value, AIndex]);
        end;
      end;
    end;
    FReportName := AName;
    SaveMe;
  end;
end;

function TxfrAbstractItem.FindComponentName(AOwner: TComponent; const NewName: string): TComponent;
  function InternalFindComponent(AComponent: TComponent; const AName: String): TComponent;
  var
    i: integer;
  begin
    Result := nil;
    if (AComponent <> nil) then
      for i := 0 to AComponent.ComponentCount-1 do
      begin
        if SameText(AComponent.Components[i].Name, AName) then
          Result := AComponent.Components[i]
        else if (AComponent.Components[i].ComponentCount > 0) then
          Result := InternalFindComponent(AComponent.Components[i], AName);
        if Result <> nil then
          break;
      end; 
  end;
begin
  if (ItemType <> itmRoot) and (AOwner is TxfrAbstractItem) then
    Result := TxfrAbstractItem(AOwner).FindComponentName(AOwner.Owner, NewName)
  else
    Result := InternalFindComponent(Self, NewName);
end;

procedure TxfrAbstractItem.AssignComponentName(AOwner: TComponent);
var
  i: integer;
  AFound: boolean;
  ANewName: string;
begin
  i := 1;
  while Name = EmptyStr do
  begin
    ANewName := Copy(ClassName, 2, Length(ClassName)-1)+IntToStr(i);
    AFound := (FindComponentName(AOwner, ANewName) <> nil);
    if AFound then
      Inc(i)
    else
      Name := ANewName;
  end;
end;

procedure TxfrAbstractItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('OldOwner', ReadOldOwner, WriteOldOwner,
    (FOldOwner <> nil));
end;

procedure TxfrAbstractItem.ReadOldOwner(Reader: TReader);
var
  ACompName: string;
  AComp: TComponent;
begin
  ACompName := Reader.ReadString;
  if ACompName <> EmptyStr then
  begin
    AComp     := FindComponentName(Owner, ACompName);
    if (AComp <> nil) and (AComp is TxfrAbstractItem) then
      FOldOwner := TxfrAbstractItem(AComp);
  end;
end;

procedure TxfrAbstractItem.WriteOldOwner(Writer: TWriter);
begin
  if FOldOwner <> nil then
    Writer.WriteString(FOldOwner.Name);
end;

procedure TxfrAbstractItem.GetChildFolders(AList: TStrings);
var
  i: integer;
begin
  AList.Clear;
  if (ItemType in [itmRoot, itmDirectory]) then
  begin
    for i := 0 to ComponentCount-1 do
      if (Components[i] is TxfrDirectoryItem) and
         (TxfrDirectoryItem(Components[i]).ItemType in [itmRoot, itmDirectory]) then
        AList.AddObject(TxfrDirectoryItem(Components[i]).ReportName, Components[i]);
  end;
end;

procedure TxfrAbstractItem.GetParentFolders(AList: TStrings);
begin
  if (ItemType in [itmRoot, itmDirectory]) then
  begin
    AList.AddObject(ReportName, Self);
    if (Owner <> nil) and (Owner is TxfrDirectoryItem) then
      TxfrAbstractItem(Owner).GetParentFolders(AList);
  end;
end;

{ TxfrDirectoryItem }

function TxfrDirectoryItem.AddDir: TxfrDirectoryItem;
begin
  Result := TxfrDirectoryItem.Create(Self);
end;

function TxfrDirectoryItem.AddReport: TxfrReportItem;
begin
  Result := TxfrReportItem.Create(Self);
end;

procedure TxfrDirectoryItem.AssignTo(Dest: TPersistent);
var
  i: integer;
  AInstance,
  AChild,
  AComp: TxfrAbstractItem;
begin
  inherited AssignTo(Dest);
  if (Dest is TxfrDirectoryItem) then
  begin
    AComp := TxfrDirectoryItem(Dest);
    for i := 0 to ComponentCount-1 do
    begin
      if (Components[i] is TxfrAbstractItem) then
      begin
        AChild := TxfrAbstractItem(Components[i]);
        case AChild.ItemType of
          itmReport:
            begin
              AInstance := TxfrReportItem.Create(AComp);
              AInstance.Assign(AChild);              
            end;
          itmDirectory:
            begin
              AInstance := TxfrDirectoryItem.Create(AComp);
              AInstance.Assign(AChild);              
            end;
        end;
      end;
    end;
  end;
end;

procedure TxfrDirectoryItem.Clear;
begin
  while ComponentCount > 0 do Delete(0);
end;

constructor TxfrDirectoryItem.Create(AOwner: TComponent);
begin
  if (FItemType = itmRoot) then
  begin
    if (AOwner <> nil) and (AOwner is TxfrDirectoryItem) then
      FItemType := itmDirectory
    else
      FItemType := itmRoot
  end;
  FAttrs := [ftDirectory];
  inherited Create(AOwner);
  FExplorer := nil;
  if ItemType = itmRoot then
  begin
    ReportName := 'Folder List';
    FAttrs := FAttrs + [ftReadOnly];
  end else
    ReportName := 'New Folder';
end;

procedure TxfrDirectoryItem.CreateRecycleDir;
begin
  if (ItemType <> itmRoot) and (Owner <> nil) and (Owner is TxfrDirectoryItem) then
    TxfrDirectoryItem(Owner).CreateRecycleDir
  else
  begin
    if (FRecycleDir = nil) and (ItemType = itmRoot) then
      FRecycleDir := TxfrRecycleDir.Create(Self);
  end;
end;

procedure TxfrDirectoryItem.DefineProperties(Filer: TFiler);  
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('NextID', ReadLastID, WriteLastID,
    (ItemType = itmRoot) and
    (FLastID > 0));
end;

procedure TxfrDirectoryItem.Delete(AIndex: integer);
var
  AItem: TComponent;
begin
  AItem := Components[AIndex];
  if (AItem <> nil) then
    FreeAndNil(AItem);
end;

function TxfrDirectoryItem.FindItemByID(ARptID: Integer): TxfrAbstractItem;
  function InternalFindID(AFrom: TxfrAbstractItem): TxfrAbstractItem;
  var
    i: integer;
  begin
    Result := nil;
    case AFrom.ItemType of
      itmReport:
        begin
          if TxfrReportItem(AFrom).ReportID = ARptID then
            Result := AFrom;
        end;
      itmRoot, itmDirectory, itmRecycleDir:
          for i := 0 to TxfrDirectoryItem(AFrom).ComponentCount-1 do
          begin
            if (TxfrDirectoryItem(AFrom).Components[i] is TxfrAbstractItem) then
              Result := InternalFindID(
                  TxfrAbstractItem(TxfrDirectoryItem(AFrom).Components[i])
                );
            if Result <> nil then
              Break;
          end;
    end; 
  end; 

begin
  if (ItemType <> itmRoot) and (Owner <> nil) and
     (Owner is TxfrDirectoryItem) then
    Result := TxfrDirectoryItem(Owner).FindItemByID(ARptID)
  else
    Result := InternalFindID(Self);
end;

function TxfrDirectoryItem.FindItemInternalID(
  ARptID: Integer): TxfrAbstractItem;
  function InternalFindID(AFrom: TxfrAbstractItem): TxfrAbstractItem;
  var
    i: integer;
  begin
    Result := nil;
    case AFrom.ItemType of
      itmReport:
        begin
          if TxfrReportItem(AFrom).FInternalID = ARptID then
            Result := AFrom;
        end;
      itmRoot, itmDirectory, itmRecycleDir:
          for i := 0 to TxfrDirectoryItem(AFrom).ComponentCount-1 do
          begin
            if (TxfrDirectoryItem(AFrom).Components[i] is TxfrAbstractItem) then
              Result := InternalFindID(
                  TxfrAbstractItem(TxfrDirectoryItem(AFrom).Components[i])
                );
            if Result <> nil then
              Break;
          end;
    end; 
  end; 

begin
  if (ItemType <> itmRoot) and (Owner <> nil) and
     (Owner is TxfrDirectoryItem) then
    Result := TxfrDirectoryItem(Owner).FindItemInternalID(ARptID)
  else
    Result := InternalFindID(Self);
end;

function TxfrDirectoryItem.FindName(AName: String;
  AItem: TxfrAbstractItem): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to ComponentCount-1 do
    if (Components[i] is TxfrAbstractItem) and
       (TxfrAbstractItem(Components[i]).ItemType = AItem.ItemType) and
       (SameText(TxfrAbstractItem(Components[i]).ReportName, AName)) then
      exit;
  Result := False;
end;

function TxfrDirectoryItem.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TxfrDirectoryItem.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  Item: TxfrAbstractItem;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[i] is TxfrAbstractItem then
    begin
      Item := TxfrAbstractItem(Components[i]);
      if Item.Owner = Self then
        Proc(Item);
    end;
end;

function TxfrDirectoryItem.GetNextID: Cardinal;
begin
  if ItemType = itmRoot then
  begin
    if (csReading in ComponentState) then
      Result := 0
    else
    begin
      Result := FLastID;
      Inc(FLastID);
    end;
  end else
    Result := inherited GetNextID;
end;

procedure TxfrDirectoryItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (ItemType = itmRoot) and (not (AComponent is TxfrRecycleDir)) and
     (FRecycleDir <> nil) and
     (FRecycleDir.ComponentIndex <> (ComponentCount-1)) then
    FRecycleDir.ComponentIndex := ComponentCount-1;
end;

procedure TxfrDirectoryItem.ReadComponent(Stream: TStream);
begin
  if (ItemType <> itmRoot) then
    raise Exception.Create(SNotARootObject);
  Clear;
  Stream.ReadComponent(Self);
  if FRecycleDir = nil then
    CreateRecycleDir;  
end;

procedure TxfrDirectoryItem.ReadLastID(Reader: TReader);
begin
  FLastID := Reader.ReadInteger;
end;

procedure TxfrDirectoryItem.ReorderRecycleDir;
begin
  if (FRecycleDir <> nil) then
    FRecycleDir.ReorderRecycleDir; 
end;

procedure TxfrDirectoryItem.SetChildOrder(Child: TComponent;
  Order: Integer);
begin
  if FindComponent(Child.Name) = Child then
    Child.ComponentIndex := Order;
end;

procedure TxfrDirectoryItem.ValidateInsert(AComponent: TComponent);
begin
  if not (AComponent is TxfrAbstractItem) then
    raise Exception.Create(SInvalidInstance)
  else 
    AComponent.SetSubComponent(True);
end;

procedure TxfrDirectoryItem.WriteComponent(Stream: TStream);
begin
  if (ItemType <> itmRoot) then
    raise Exception.Create(SNotARootObject);  
  Stream.WriteComponent(Self);
end;

procedure TxfrDirectoryItem.WriteLastID(Writer: TWriter);
begin
  Writer.WriteInteger(FLastID);
end;

{ TxfrReportItem }

procedure TxfrReportItem.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if (Dest is TxfrReportItem) then
  begin
    TxfrReportItem(Dest).FStream.Clear;
    TxfrReportItem(Dest).FStream.LoadFromStream(FStream);
  end;
end;

constructor TxfrReportItem.Create(AOwner: TComponent);
var
  AStr: PChar;
  ALen: integer;
begin
  FItemType := itmReport;
  inherited Create(AOwner);
  FStream := TMemoryStream.Create;
  AStr := PChar(SReportTemplate);
  ALen := PCardinal(Cardinal(AStr)-4)^;
  FStream.WriteBuffer(AStr^, ALen);
  FStream.Seek(0, soFromBeginning);
  ReportName := 'Untitled Report';
  FReportID := 0;
  FInternalID := GetNextID;
end;

procedure TxfrReportItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('InternalID', ReadInternalID, WriteInternalID,
    (FInternalID > 0));
end;

destructor TxfrReportItem.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TxfrReportItem.GetContent: string;
begin
  if (FStream.Size > 0) and (not GetSecureArchive) then
  begin
    SetString(Result, nil, FStream.Size);
    FStream.Seek(0, soBeginning);
    FStream.Read(Pointer(Result)^, FStream.Size);
  end;
end;

function TxfrReportItem.GetSize: Longint;
begin
  Result := FStream.Size;
end;

function TxfrReportItem.GetSizeStr: string;
begin
  Result := Format('%f kb',[Size / 1024]);
end;

procedure TxfrReportItem.LoadFromStream(AStream: TStream);
begin
  try
    FStream.Clear;
    FStream.LoadFromStream(AStream);
  finally
    FStream.Position := 0;
    TimeStamp := now;
    if not(csReading in ComponentState) then
      SaveMe;
  end;
end;

procedure TxfrReportItem.ReadInternalID(Reader: TReader);
begin
  FInternalID := Reader.ReadInteger;
end;

procedure TxfrReportItem.SaveToStream(AStream: TStream);
begin
  FStream.SaveToStream(AStream)
end;

procedure TxfrReportItem.SetContent(const Value: string);
begin
  FStream.Clear;
  FStream.Write(Pointer(Value)^, PCardinal(Cardinal(Value)-4)^);
end;

procedure TxfrReportItem.SetReportID(const Value: integer);
begin
  if csReading in ComponentState then
    FReportID := Value
  else
  begin
    if (Value > 0) and (FReportID <> Value) and
       (FReportID > 0) and
       (not(csReading in ComponentState)) and
       (Owner <> nil) and (Owner is TxfrDirectoryItem) and
       (TxfrDirectoryItem(Owner).FindItemByID(Value) <> nil) then
      raise Exception.Create(SReportIDExists);
    FReportID := Value;
    SaveMe;
  end;
end;

procedure TxfrReportItem.ValidateInsert(AComponent: TComponent);
begin
  raise Exception.Create(SNoInstanceAllowed);
end;

procedure TxfrReportItem.WriteInternalID(Writer: TWriter);
begin
  Writer.WriteInteger(FInternalID);
end;

{ TxfrRecycleDir }

constructor TxfrRecycleDir.Create(AOwner: TComponent);
begin
  FItemType := itmRecycleDir;
  inherited Create(AOwner);
  if (Owner <> nil) and (Owner is TxfrDirectoryItem) and
     (TxfrDirectoryItem(Owner).ItemType = itmRoot) and
     (TxfrDirectoryItem(Owner).FRecycleDir = nil) then
    TxfrDirectoryItem(Owner).FRecycleDir := Self;
  ReportName := 'Recycle Bin';
  FAttrs := FAttrs + [ftReadOnly];
end;

destructor TxfrRecycleDir.Destroy;
begin
  if (Owner is TxfrDirectoryItem) and
     (TxfrDirectoryItem(Owner).FRecycleDir = Self) then
    TxfrDirectoryItem(Owner).FRecycleDir := nil;
  inherited Destroy;
end;

procedure TxfrRecycleDir.Release(AIndex: Integer);
begin
  Delete(AIndex);
  SaveMe;
end;

procedure TxfrRecycleDir.ReleaseAll;
begin
  Clear;
  SaveMe;
end;

procedure TxfrRecycleDir.ReorderRecycleDir;
begin
  if (Owner <> nil) and (ComponentIndex <> (Owner.ComponentCount-1)) then
    ComponentIndex := Owner.ComponentCount-1;
end;

procedure TxfrRecycleDir.Restore(AIndex: Integer);
begin
  if (Components[AIndex] is TxfrAbstractItem) then
    TxfrAbstractItem(Components[AIndex]).Restore;
  SaveMe;
end;

procedure TxfrRecycleDir.RestoreAll;
var
  AIndex: integer;
begin
  for AIndex := 0 to ComponentCount-1 do
    Restore(AIndex);
  SaveMe;
end;

{ TxfrReportExplorer }

procedure TxfrReportExplorer.ApplyLocalVariables(const AHasParam,
  AParamIsArray: boolean; const AParamValues: variant);
begin
  Variables['AppParamExists']  := AHasParam;
  Variables['AppParamIsArray'] := AParamIsArray;
  Variables['AppParamValues']  := AParamValues;
end;

procedure TxfrReportExplorer.AssignVariables(AReport: TxfrReportItem);
var
  i: integer;
begin
  frxReport.Script.Variables['AppRptID']   := AReport.ReportID;
  frxReport.Script.Variables['AppRptName'] := AReport.ReportName;
  frxReport.Script.Variables['AppRptDesc'] := AReport.Description;
  for i := 0 to FRptVariables.Count-1 do
    frxReport.Script.Variables[FRptVariables.Items[i].Name] :=
      FRptVariables.Items[i].Value;
end;

function TxfrReportExplorer.Authenticate: boolean;
begin
  if (not TagAuthenticated) then
    if (FTagAuthentication <> EmptyStr) then
    begin
      with TfrxPasswordForm.Create(Application) do
      try
        FTagAuthenticated := (ShowModal = mrOk) and
          (FTagAuthentication = PasswordE.Text);
      finally
        Free;
      end;      
    end else
      FTagAuthenticated := True;
  Result := TagAuthenticated;
end;

constructor TxfrReportExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoot := TxfrDirectoryItem.Create(nil);
  FRoot.FExplorer := Self;
  FRoot.OnSave := InternalOnSave;
  FRptVariables := TfrxVariables.Create;
  ApplyLocalVariables(False, False, null);
  FTagAuthenticated := False;
  FDesigner := TfrxDesigner.Create(Self);
  FDesigner.OnLoadReport := frxDesignerLoadReport;
  FDesigner.OnSaveReport := frxDesignerSaveReport;
  FOnUpdating := False;
end;

procedure TxfrReportExplorer.DesignReport(AReport: TxfrReportItem);
var
  AStream: TMemoryStream;
begin
  frxReport.Clear;
  ValidateReport(AReport, etDesign);
  FCurrentReport := AReport;
  if AReport.Size > 0 then
  begin
    AStream := TMemoryStream.Create;
    try
      AReport.SaveToStream(AStream);
      AStream.Seek(0, soFromBeginning);
      frxReport.LoadFromStream(AStream);
      frxReport.FileName := AReport.ReportName;
    finally
      AStream.Free;
    end;
  end;
  AssignVariables(AReport);
  frxReport.DesignReport;
end;

destructor TxfrReportExplorer.Destroy;
begin
  if FExplorer <> nil then
    FExplorer.Free;
  FDesigner.Free;
  FRptVariables.Free;
  FRoot.Free;
  inherited Destroy;
end;

procedure TxfrReportExplorer.ExecuteReport(const rptValues: variant;
  const ShowDesigner: boolean);
var
  nRptID: integer;
  ParamValues: Variant;
  ParamExist, ParamIsArray: Boolean;
  AItem: TxfrAbstractItem;
begin
  if VarIsArray(rptValues) then
  begin
    nRptID := rptValues[0];
    ParamValues := rptValues[1];
  end else
  begin
    nRptID := rptValues;
    ParamValues := null;
  end;
  if Assigned(FBeforeExecReport) then
    FBeforeExecReport(nRptID, ParamValues);
  ParamIsArray := VarIsArray(ParamValues);
  ParamExist   := ParamIsArray;
  if not ParamExist then
    ParamExist := ParamValues <> null;
  if nRptID = 0 then
    exit;
  LoadData;
  try
    AItem := Root.FindItemByID(nRptID);
    if (AItem = nil) or (AItem.ItemType <> itmReport) then
      exit;      
    ApplyLocalVariables(ParamExist, ParamIsArray, ParamValues);
    if ShowDesigner then
      DesignReport(TxfrReportItem(AItem))
    else
      PreviewReport(TxfrReportItem(AItem));    
    if FModified then
      SaveData;
  finally
    FRoot.Clear;
  end;
end;

procedure TxfrReportExplorer.FinalizeExplorer;
begin
  FRoot.Clear;
end;

function TxfrReportExplorer.frxDesignerLoadReport(
  Report: TfrxReport): Boolean;
var
  AItem: TxfrReportItem;
  AStream: TMemoryStream;
begin
  Result := True;
  AStream := TMemoryStream.Create;
  try
    with TxfrOpenSaveDialog.Create(Self) do
    try
      DialogType := dtOpenDialog;
      ReportExplorer := Self;
      CurrentFolder := Self.CurrentFolder;
      if ShowModal = mrOK then
      begin
        AItem := SelectedItem;
        AItem.SaveToStream(AStream);
        AStream.Seek(0, soFromBeginning);
        Self.frxReport.Clear;
        Self.ValidateReport(AItem, etDesign);
        Self.frxReport.LoadFromStream(AStream);
        Self.frxReport.FileName := AItem.ReportName;
        Self.AssignVariables(AItem);
      end else
        Result := False; 
    finally
      Free;
    end;    
  finally
    AStream.Free;
  end;
end;

function TxfrReportExplorer.frxDesignerSaveReport(Report: TfrxReport;
  SaveAs: Boolean): Boolean;
var
  AItem: TxfrReportItem;
  AStream: TMemoryStream;
begin
  Result := True;
  if (CurrentReport = nil) and (SaveAs = False) then
    raise Exception.Create(SMissLink);
  AItem := CurrentReport;
  AStream := TMemoryStream.Create;
  try 
    if (not SaveAs) and (Report.FileName <> EmptyStr) then
    begin
      frxReport.SaveToStream(AStream);
      AStream.Seek(0, soFromBeginning);
      AItem.LoadFromStream(AStream);
      SaveData;
    end else
    begin
      with TxfrOpenSaveDialog.Create(Self) do
      try
        DialogType := dtSaveDialog;
        ReportExplorer := Self;
        CurrentFolder := Self.CurrentFolder;
        if ShowModal = mrOK then
        begin
          AItem := SelectedItem;
          if (AItem = nil) and (ItemName <> EmptyStr) then
          begin
            if CurrentFolder <> nil then
              AItem := TxfrReportItem.Create(CurrentFolder)
            else if Self.CurrentFolder <> nil then
              AItem := TxfrReportItem.Create(
                Self.CurrentFolder
              );
            if AItem <> nil then
              AItem.ReportName := ItemName;
          end;
          if AItem = nil then
            raise Exception.Create(SMissLink);
          Self.frxReport.SaveToStream(AStream);
          Self.frxReport.FileName := AItem.ReportName;
          AStream.Seek(0, soFromBeginning);
          AItem.LoadFromStream(AStream);
          SaveData;
        end else
          Result := False;
      finally
        Free;
      end;      
    end;
  finally
    AStream.Free;
  end;
end;

function TxfrReportExplorer.GetCurrFolder: TxfrDirectoryItem;
begin
  if FCurrFolder = nil then
    Result := FRoot
  else
    Result := FCurrFolder;
end;

function TxfrReportExplorer.GetTagAuthenticated: boolean;
begin
  Result := FTagAuthenticated;
end;

function TxfrReportExplorer.GetVariable(Index: String): Variant;
begin
  Result := FRptVariables.Variables[Index];
end;

procedure TxfrReportExplorer.InternalOnSave(Sender: TObject);
begin
  if not FOnUpdating then
    FModified := True;
end;

procedure TxfrReportExplorer.LoadData;
begin
  if Assigned(FOnLoadData) then
    FOnLoadData(Self);
  if (DataSourceMode = dsFile) then
  begin
    if (ReportFileName = EmptyStr) then
      raise Exception.Create(SUnAssignedRptFile);
    if FileExists(ReportFileName) then
      LoadFromFile(ReportFileName);
  end;
  FRoot.CreateRecycleDir;
end;

procedure TxfrReportExplorer.LoadFromFile(const AFileName: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    AStream.Seek(0, soFromBeginning);
    if AStream.Size > 0 then
      LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TxfrReportExplorer.LoadFromStream(Stream: TStream);
var
  ALastPos, ASignature: Longint;
  ASecured: boolean;
  AStream, BStream: TMemoryStream;
begin
  FRoot.Clear;
  ALastPos := Stream.Position;
  AStream := TMemoryStream.Create;
  BStream := TMemoryStream.Create;
  FOnUpdating := True;
  try
    Stream.Read(ASignature, SizeOf(ASignature));
    ASecured := ASignature = Longint(SecureSignature);
    if not ASecured then
    begin
      Stream.Position := ALastPos;
      ObjectTextToBinary(Stream, AStream);
      AStream.Seek(0, soFromBeginning);
      FRoot.ReadComponent(AStream);
    end else
    begin
      BStream.CopyFrom(Stream, Stream.Size-Stream.Position);
      BStream.Seek(0, soFromBeginning);
      frxDecompressStream(BStream, AStream);
      BStream.Clear;
      AStream.Seek(0, soFromBeginning);
      FRoot.ReadComponent(AStream);
    end;
  finally
    FOnUpdating := False;
    BStream.Free;
    AStream.Free;
  end;
end;

type
  TfrxReportHack = class(TfrxReport);
  
procedure TxfrReportExplorer.PreviewReport(AReport: TxfrReportItem);
  function HasReportPage(AFrxReport: TfrxReport): boolean;
  var
    AList: TList;
    i: integer;
  begin
    Result := False;
    AList := AFrxReport.AllObjects;
    for i := 0 to AList.Count-1 do
      if TfrxComponent(AList[i]).InheritsFrom(TfrxReportPage) then
      begin
        Result := True;
        break;
      end;
  end;
var
  AStream: TMemoryStream;
begin
  frxReport.Clear;
  ValidateReport(AReport, etPreview);
  if AReport.Size > 0 then
  begin
    AStream := TMemoryStream.Create;
    try
      AReport.SaveToStream(AStream);
      AStream.Seek(0, soFromBeginning);
      frxReport.LoadFromStream(AStream);
      frxReport.FileName := AReport.ReportName;
    finally
      AStream.Free;
    end;
  end;
  AssignVariables(AReport);
  frxReport.PreviewOptions.Modal := True;

  frxReport.PreviewPages.Clear;
  if frxReport.PrepareReport(False) then
    if HasReportPage(frxReport) then
      frxReport.ShowPreparedReport;
end;

procedure TxfrReportExplorer.SaveData;
begin
  if Assigned(FOnSaveData) then
    FOnSaveData(Self);
  if (DataSourceMode = dsFile) then
  begin
    if (ReportFileName = EmptyStr) then
      raise Exception.Create(SUnAssignedRptFile);
    SaveToFile(ReportFileName);
  end;
end;

procedure TxfrReportExplorer.SaveToFile(const AFileName: string);
var
  AStream: TMemoryStream;
  AFile: string;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(AStream);
    AStream.Seek(0, soFromBeginning);
    if AFileName <> EmptyStr then
      AFile := AFileName
    else
      AFile := ReportFileName;
    if FileExists(AFile) then
    begin
      if SysUtils.DeleteFile(AFile) then
        AStream.SaveToFile(AFile)
      else
        raise Exception.Create(SCanSaveFile);
    end else
      AStream.SaveToFile(AFile)
  finally
    AStream.Free;
  end;
end;

procedure TxfrReportExplorer.SaveToStream(Stream: TStream);
var
  ASecured: boolean;
  AStream, BStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  BStream := TMemoryStream.Create;
  FOnUpdating := True;
  try
    FRoot.WriteComponent(AStream);
    AStream.Seek(0, soFromBeginning);
    ASecured := SecureArchive;
    if not ASecured then
    begin
      ObjectBinaryToText(AStream, Stream);
      AStream.Clear;
    end else
    begin
      frxCompressStream(AStream, BStream);
      AStream.Clear;
      BStream.Seek(0, soFromBeginning);
      Stream.Write(SecureSignature, SizeOf(SecureSignature));
      BStream.SaveToStream(Stream);
    end;
  finally
    FOnUpdating := False;
    BStream.Free;
    AStream.Free;
  end;
end;

procedure TxfrReportExplorer.SetSecureArchive(const Value: boolean);
begin
  if FTagAuthentication = EmptyStr then
    FSecureArchive := Value
  else
    FSecureArchive := True;
end;

procedure TxfrReportExplorer.SetTagAuthenticated(const Value: boolean);
begin
  FTagAuthenticated := Value;
end;

procedure TxfrReportExplorer.SetTagAuthentication(const Value: string);
begin
  FTagAuthentication := Value;
  if FTagAuthentication <> EmptyStr then
    FSecureArchive := True;
end;

procedure TxfrReportExplorer.SetVariable(Index: String;
  const Value: Variant);
begin
  FRptVariables.Variables[Index] := Value;
end;

procedure TxfrReportExplorer.ShowExplorer;
var
  f: TForm;
begin
  LoadData;
  FTagAuthenticated := False;
  if FExplorer <> nil then
  begin
    FExplorer.UpdateItems;
    FExplorer.Activate;
    Exit;
  end;  
  try
    if FExplorer <> nil then Exit;
    if frxExplorerClass <> nil then
    begin
      ApplyLocalVariables(False, False, Null);
      try
        f := Screen.ActiveForm;
        if f <> nil then
          f.Enabled := False;
        FExplorer := TxfrCustomExplorer(frxExplorerClass.NewInstance);
        FExplorer.Create(Self);
        FExplorer.FExplorer := Self;
        FExplorer.FReport   := Self.FReport;
        FExplorer.InitExplorer;
        FExplorer.Show;        
        while not FExplorer.FModalFinished do
          Application.HandleMessage;
      finally
        FreeAndNil(FExplorer);
      end;
      if f <> nil then
      begin
        f.Enabled := True;
        if f.Visible then
          f.SetFocus;
      end;
    end;
  finally
    if FModified then
      SaveData;  
    if (DataSourceMode = dsFile) then
      FinalizeExplorer;
  end;
end;

procedure TxfrReportExplorer.ValidateReport(AReport: TxfrReportItem; const AEvent: TxfrExplorerEventTypes);
begin
  if AReport.Deleted then
    raise EAbort.Create('');
  if Assigned(FOnValidateReport) then
    FOnValidateReport(AReport, AEvent);
end;

{ TxfrCustomExplorer }

constructor TxfrCustomExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReport := nil;
  FSelectedObjects := TList.Create;
end;

destructor TxfrCustomExplorer.Destroy;
begin
  FSelectedObjects.Free;
  inherited Destroy;
end;

procedure TxfrCustomExplorer.SetExplorer(AExpl: TxfrReportExplorer);
begin
  FExplorer := AExpl;
end;

procedure TxfrCustomExplorer.SetReporter(AReport: TfrxReport);
begin
  FReport := AReport;
end;

procedure TxfrCustomExplorer.SetShowHiddenObj(const Value: boolean);
begin
  if FShowHiddenObj <> Value then
  begin
    FShowHiddenObj := Value;
    UpdateItems;
  end;
end;

{ TxfrExplorerResources }

constructor TxfrExplorerResources.Create;
begin
  FDisabledButtonImages := TImageList.Create(nil);
  FDisabledButtonImages.Width := 16;
  FDisabledButtonImages.Height := 16;
  FMainButtonImages := TImageList.Create(nil);
  FMainButtonImages.Width := 16;
  FMainButtonImages.Height := 16;
end;

destructor TxfrExplorerResources.Destroy;
begin
  FDisabledButtonImages.Free;
  FMainButtonImages.Free;
  inherited Destroy;
end;

function TxfrExplorerResources.GetMainButtonImages: TImageList;
var
  Images: TBitmap;
begin
  if FMainButtonImages.Count = 0 then
  begin
    Images := TBitmap.Create;
    try
      Images.LoadFromResourceName(hInstance, 'FRXEXPBUTTONS');
      SetButtonImages(Images);
    finally
      Images.Free;
    end;
  end;
  Result := FMainButtonImages;
end;

procedure TxfrExplorerResources.SetButtonImages(Images: TBitmap;
  Clear: Boolean);
begin
  if Clear then
  begin
    FMainButtonImages.Clear;
    FDisabledButtonImages.Clear;
  end;
  frxAssignImages(Images, 16, 16, FMainButtonImages, FDisabledButtonImages);
end;

function xfrExplorerResources: TxfrExplorerResources;
begin
  if FResources = nil then
    FResources := TxfrExplorerResources.Create;
  Result := FResources;
end;

initialization
  RegisterClasses([TxfrDirectoryItem, TxfrRecycleDir, TxfrReportItem]); 

finalization
  if FResources <> nil then
    FreeAndNil(FResources);
  UnRegisterClasses([TxfrDirectoryItem, TxfrRecycleDir, TxfrReportItem]);
  
end.
