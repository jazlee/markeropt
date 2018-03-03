unit CSPCustomEngine;

interface

uses
  SysUtils, Classes, Windows, DB, NativeXML, ecfutils;

const
  CMaxStep: integer = 100;

  CDistHeaderCount = 7;
  CDistHeaders: array[0..(CDistHeaderCount-1)] of string =
  (
    'Style', 'Color', 'Material', 'Feature', 'Step', 'Laying', 'Pieces'
  );
  CDistFields: array[0..(CDistHeaderCount-1)] of string =
  (
    'stname', 'fitname', 'mtcd', 'ftname', 'stpid', 'lrcnt', 'pcscnt'
  );
  CDistColumnSuffixCount = 3;
  CDistColumnSuffixes: array[0..(CDistColumnSuffixCount-1)] of string =
  (
    '', 'Dist', 'Outst'
  );
  CDistColumnFieldSuffixes: array[0..(CDistColumnSuffixCount-1)] of string =
  (
    'fld', 'dst', 'out'
  );

  CFabConsHeaderCount = 22;
  CFabConsHeaders: array[0..(CFabConsHeaderCount-1)] of string =
  (
    'Style', 'Color', 'Material', 'Feature', 'Step', 'Laying', 'Pieces', 'Avg YY',
//  -- Marker Width -- -- Marker Length -- -- Total Marker --
    'Est',   'Realize','Est',   'Realize', 'Est',   'Realize',
//  ------- Allowance Length ------- -- Total Length -- -- Total Pieces --
    'Alowance', 'Est',    'Realize', 'Est',   'Realize', 'Total Pcs',
//  -- Fab. Consumption --    
    'Est',        'Realize'
  );
  
  CFabConsFields: array[0..(CFabConsHeaderCount-1)] of string =
  (
    'stname', 'fitname', 'mtcd', 'ftname', 'stpid', 'lrcnt', 'pcscnt', 'avgyy',
    'mrkwidth', 'actwidth', 'mrklen', 'actlen', 'mrktot', 'acttot',
    'fballowance', 'fbalwlen', 'actfbalwlen', 'totlen', 'acttotlen',
    'totpcs', 'consumption', 'actconsumption'
  );

  {
  CFabConsHeaderCount = 16;
  CFabConsHeaders: array[0..(CFabConsHeaderCount-1)] of string =
  (
    'Style', 'Color', 'Material', 'Feature', 'Step', 'Laying', 'Pieces', 'Marker Width', 'Marker Len',
    'Avg YY', 'Total Marker', 'Allowance', 'Allowance Len', 'Total Len', 'Total Pcs',
    'Fab. Consumption'
  );
  
  CFabConsFields: array[0..(CFabConsHeaderCount-1)] of string =
  (
    'stname', 'fitname', 'mtcd', 'ftname', 'stpid', 'lrcnt', 'pcscnt', 'mrkwidth', 'mrklen',
    'avgyy', 'mrktot', 'fballowance', 'fbalwlen', 'totlen', 'totpcs', 'consumption'
  );
  }

type
  TCSPBaseOptimizerClass = class of TCSPBaseOptimizer;
  TCSPBaseOptimizer = class(TComponent)
  private
    FDocument: TNativeXml;
    FEngineRootNode: TXmlNode;
    FProperties: TECFParams;
    FRowProperties: TECFParams;
    FCurrentStep: integer;

    procedure PackageDistColumns;
    procedure PackageDistValues;
    procedure PackageFieldValues;
    procedure PackageConsumptionColumns;
    procedure PackageConsumptionValues;
    function GetDistributionRoot: TXmlNode;
    function GetDistributionColumns: TXmlNode;
    function GetDistributionSheet: TXmlNode;
    function GetConsumptionColumns: TXmlNode;
    function GetConsumptionRoot: TXmlNode;

    procedure GetFieldValues;
    procedure DeleteUndistributedRows;
    procedure ClearLock;

    procedure SortDistribution;
    procedure SortConsumption;

    function GetDistributionLock: TXmlNode;
    function FindLockedMaterials(const AStyle, AFtName, AFitName, AMtCD: string): TXmlNode;
  protected
    procedure LoadRowProperties(ARootNode: TXmlNode); overload;
    procedure LoadRowProperties(ARootNode: TXmlNode; ARowProperties: TECFParams); overload;
    procedure SaveRowProperties(ARootNode: TXmlNode; ARowProperties: TECFParams);
    
    procedure InternalInitDistribution; virtual;
    procedure InternalInitConsumption; virtual;
    procedure InitWorksheet;

    function GetLockStatus(RowIndex, ColIndex: integer): Boolean; virtual;
    procedure SetLockStatus(RowIndex, ColIndex: integer; const Value: Boolean); virtual;
    function GetRowLockStatus(RowIndex: integer): Boolean; virtual;
    procedure SetRowLockStatus(RowIndex: integer; const Value: Boolean); virtual;

    procedure CreateSheetItem(ARoot, AItem: TXmlNode);
    procedure InternalDeinitDistribution; virtual;
    procedure InternalDeinitConsumption; virtual;
    procedure DeinitWorksheet;

    procedure UpdateDistribution;

    procedure GetSizeNames(const AStName, AFtName, AFitName, AMtCD: string; ASzList: TStringList); overload;
    procedure GetSizeNames(ANode: TXmlNode; ASzList: TStringList); overload;
    procedure InternalOptimize; virtual; abstract;

    procedure SetProperties(const AFields: string;
      ACategories, ACaptions, ADefaultValues, AInheritable: Variant);

    function GetLoadedData(const AStyle, AFtName, AFitName, AMaterial: string; const AStepID: integer): TXmlNode;
    function GetLoadedConsumption(const AStyle, AFtName, AFitName, AMaterial: string; const AStepID: integer): TXmlNode;
    function FindLockItem(const AStyle, AFtName, AFitName, AMaterial: string): TXmlNode;

    function FindBreakdownStyle(const AStyle: string): TXmlNode;
    function FindBreakdownMaterials(const AStyle, AFtName, AFitName, AMtCD: string): TXmlNode;

    property CurrentStep: Integer read FCurrentStep write FCurrentStep;

    property EngineRootNode: TXmlNode read FEngineRootNode;
    property Document: TNativeXml read FDocument;
    property EngineProperties: TECFParams read FProperties;
    property RowProperties: TECFParams read FRowProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitEngine; virtual;
    procedure DeinitEngine; virtual;

    procedure PackageValues;

    procedure SetDocument(ADocument: TNativeXml); virtual;
    procedure DoOptimize; virtual;

    procedure CalculateConsumption(ANode: TXmlNode);

    function GetProperties(ARootNode: TXmlNode): TXmlNode; overload;
    function GetProperties: TXmlNode; overload;


    procedure LoadXMLData(ADocument: TNativeXml);
    procedure SaveXMLData(ADocument: TNativeXml);

    property DistributionRoot: TXmlNode read GetDistributionRoot;
    property DistributionColumns: TXmlNode read GetDistributionColumns;
    property DistributionSheet: TXmlNode read GetDistributionSheet;
    property DistributionLock: TXmlNode read GetDistributionLock;
    property ConsumptionRoot: TXmlNode read GetConsumptionRoot;
    property ConsumptionColumns: TXmlNode read GetConsumptionColumns;
    property LockStatus[RowIndex, ColIndex: integer]: Boolean read GetLockStatus write SetLockStatus;
    property RowLockStatus[RowIndex: integer]: Boolean read GetRowLockStatus write SetRowLockStatus;

    class function EngineUniqueID: string; virtual;
    class function EngineName: string; virtual;
    class function EngineDescription: string; virtual;
  end;

procedure RegisterOptimizer(AClass: TCSPBaseOptimizerClass);
procedure UnregisterOptimizer(AClass: TCSPBaseOptimizerClass);
procedure GetEngineList(AList: TStrings);
function GetEngineClass(const AName: string): TCSPBaseOptimizerClass;

implementation
uses
  Math, CSPAppUtil;

type
  TCSPInternalEngineManager = class(TObject)
  private
    FEngineList: TList;
    FLock: TRTLCriticalSection;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
    procedure InternalAddEngine(AClass: TCSPBaseOptimizerClass);
    procedure InternalRemoveEngine(AClass: TCSPBaseOptimizerClass);
    procedure GetEngineList(AList: TStrings);
    
    function GetEngineClass(const AName: string): TCSPBaseOptimizerClass;
  end;

var
  InternalEnginesManager: TCSPInternalEngineManager;

procedure RegisterOptimizer(AClass: TCSPBaseOptimizerClass);
begin
  InternalEnginesManager.Lock;
  try
    InternalEnginesManager.InternalAddEngine(AClass);
  finally
    InternalEnginesManager.Unlock;
  end;
end;

procedure UnregisterOptimizer(AClass: TCSPBaseOptimizerClass);
begin
  InternalEnginesManager.Lock;
  try
    InternalEnginesManager.InternalRemoveEngine(AClass);
  finally
    InternalEnginesManager.Unlock;
  end;
end;

procedure GetEngineList(AList: TStrings);
begin
  InternalEnginesManager.Lock;
  try
    InternalEnginesManager.GetEngineList(AList);
  finally
    InternalEnginesManager.Unlock;
  end;
end;

function GetEngineClass(const AName: string): TCSPBaseOptimizerClass;
begin
  InternalEnginesManager.Lock;
  try
    Result := InternalEnginesManager.GetEngineClass(AName);
  finally
    InternalEnginesManager.Unlock;
  end;
end;

{ TCSPInternalEngineManager }

constructor TCSPInternalEngineManager.Create;
begin
  FEngineList := TList.Create;
  InitializeCriticalSection(FLock);
end;

destructor TCSPInternalEngineManager.Destroy;
begin
  DeleteCriticalSection(FLock);
  FEngineList.Free;
  inherited;
end;

function TCSPInternalEngineManager.GetEngineClass(
  const AName: string): TCSPBaseOptimizerClass;
var
  I: integer;
  AEngine: TCSPBaseOptimizerClass;
begin
  Result := nil;
  for I := 0 to FEngineList.Count - 1 do
  begin
    AEngine := TCSPBaseOptimizerClass(FEngineList[i]);
    if AEngine.EngineName = AName then
    begin
      Result := AEngine;
      break;
    end;       
  end;
end;

procedure TCSPInternalEngineManager.GetEngineList(AList: TStrings);
var
  I: integer;
  AEngine: TCSPBaseOptimizerClass;
begin
  AList.Clear;
  for I := 0 to FEngineList.Count - 1 do
  begin
    AEngine := TCSPBaseOptimizerClass(FEngineList[i]);
    AList.Add(AEngine.EngineName);    
  end;
end;

procedure TCSPInternalEngineManager.InternalAddEngine(
  AClass: TCSPBaseOptimizerClass);
var
  i: integer;
  AFound: Boolean;
  AExistingClass: TCSPBaseOptimizerClass;  
begin
  AFound := False;
  for I := 0 to FEngineList.Count - 1 do
  begin
    AExistingClass := TCSPBaseOptimizerClass(FEngineList[i]);
    if AExistingClass.EngineName = AClass.EngineName then
    begin
      AFound := True;
      break;
    end;
  end;
  if not AFound then
  begin
    FEngineList.Add(AClass);
  end;
end;

procedure TCSPInternalEngineManager.InternalRemoveEngine(
  AClass: TCSPBaseOptimizerClass);
begin

end;

procedure TCSPInternalEngineManager.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TCSPInternalEngineManager.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

{ TCSPBaseOptimizer }

procedure TCSPBaseOptimizer.CalculateConsumption(ANode: TXmlNode);
var
  AMaterial: TXmlNode;
  AValA, AValB, ATot: Double;
begin
  if ANode = nil then
    exit;
  AMaterial := FindBreakdownMaterials(ANode.ReadAttributeString('stname'),
    ANode.ReadAttributeString('ftname'), ANode.ReadAttributeString('fitname'),
    ANode.ReadAttributeString('mtcd'));
  // Calculate Total Marker
  AValA := ANode.ReadAttributeInteger('lrcnt');
  AValB := ANode.ReadAttributeFloat('mrklen');
  ATot  := AValA * AValB;
  ANode.WriteAttributeFloat('mrktot', ATot);
  AValB := ANode.ReadAttributeFloat('actlen');
  ATot  := AValA * AValB;
  ANode.WriteAttributeFloat('acttot', ATot);
  
  // Calculata Fabric Allowance
  if AMaterial <> nil then
  begin  
    ANode.WriteAttributeFloat('fbalval', AMaterial.ReadAttributeFloat('fballowance'));
    ANode.WriteAttributeFloat('fbaltp', AMaterial.ReadAttributeFloat('fbaltype'));
  end;
  AValB := ANode.ReadAttributeFloat('fbalval');
  if ANode.ReadAttributeInteger('fbaltp', 0) = 0 then
  begin
    ANode.WriteAttributeString('fballowance',
      Format('%3.4f %%',
        [
          AValB
        ]
      )
    );
    AValA := ANode.ReadAttributeFloat('mrktot');
    ATot  := (AValA * AValB) / 100;
    ANode.WriteAttributeFloat('fbalwlen', ATot);
    AValA := ANode.ReadAttributeFloat('acttot');
    ATot  := (AValA * AValB) / 100;
    ANode.WriteAttributeFloat('actfbalwlen', ATot);
  end else
  begin
    ANode.WriteAttributeString('fballowance',
      Format('%3.4f',
        [
          AValB
        ]
      )
    );
    AValA := ANode.ReadAttributeInteger('lrcnt');
    ATot  := AValA * AValB;
    ANode.WriteAttributeFloat('fbalwlen', ATot);
    ANode.WriteAttributeFloat('actfbalwlen', ATot);
  end;
  // calculate totlen
  AValA := ANode.ReadAttributeFloat('mrktot');
  AValB := ANode.ReadAttributeFloat('fbalwlen');
  ATot  := AValA + AValB;
  ANode.WriteAttributeFloat('totlen', ATot);

  AValA := ANode.ReadAttributeFloat('acttot');
  AValB := ANode.ReadAttributeFloat('actfbalwlen');
  ATot  := AValA + AValB;
  ANode.WriteAttributeFloat('acttotlen', ATot);
  // calculate totpcs
  AValA := ANode.ReadAttributeInteger('lrcnt');
  AValB := ANode.ReadAttributeInteger('pcscnt');
  ATot  := AValA * AValB;
  ANode.WriteAttributeFloat('totpcs', ATot);
  // calculate consumption
  if ATot > 0 then
  begin
    AValB := ATot; // totpcs
    AValA := ANode.ReadAttributeFloat('totlen');
    ATot  := AValA / AValB;
    ANode.WriteAttributeFloat('consumption', ATot);
    AValA := ANode.ReadAttributeFloat('acttotlen');
    ATot  := AValA / AValB;
    ANode.WriteAttributeFloat('actconsumption', ATot);
  end else
  begin
    ANode.WriteAttributeFloat('consumption', 0);
    ANode.WriteAttributeFloat('actconsumption', 0);
  end;  
end;

procedure TCSPBaseOptimizer.ClearLock;
var
  AName: string;
  ALock: TXmlNode;
begin
  ALock := DistributionLock;
  AName := ALock.Name;
  ALock.NodesClear;
end;

constructor TCSPBaseOptimizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TECFParams.Create(Self);
  FRowProperties := TECFParams.Create(Self);
  FDocument := nil;
  FEngineRootNode := nil;
end;

procedure TCSPBaseOptimizer.CreateSheetItem(ARoot, AItem: TXmlNode);
var
  ANode, APrevData, ACurData, ASizes, ASize, ALock: TXmlNode;
  I: integer;
  AFlag, AFieldName: string;
  AQtyVal, ADistVal, ALaying, AOutVal: integer;
  ARowLock, AHasValue: Boolean;
begin
  AHasValue := False;
  ANode := ARoot.NodeNew('material');
  ANode.AttributeCopy(AItem);
  ANode.WriteAttributeString('stname', ARoot.ReadAttributeString('stname'));
  ANode.WriteAttributeInteger('breakdownlinkindex', AItem.IndexInParent);
  // copy from existing step
  ACurData := GetLoadedData(ARoot.ReadAttributeString('stname'),
                           AItem.ReadAttributeString('ftname'),
                           AItem.ReadAttributeString('fitname'),
                           AItem.ReadAttributeString('mtcd'),
                           CurrentStep);
                           
  // initalize row props
  FRowProperties.AssignInheritable(FProperties);
  if ACurData <> nil then
  begin
    ANode.AttributeCopy(ACurData);
    LoadRowProperties(ACurData, FRowProperties);
  end;
  APrevData := nil;
  if (CurrentStep > 1) then
    APrevData := GetLoadedData(ARoot.ReadAttributeString('stname'),
                           AItem.ReadAttributeString('ftname'),
                           AItem.ReadAttributeString('fitname'),
                           AItem.ReadAttributeString('mtcd'),
                           CurrentStep-1);
  ANode.WriteAttributeInteger('stpid', CurrentStep);
  ARowLock := False;
  if ANode.HasAttribute('rowlock') then
    ARowLock := ANode.ReadAttributeBool('rowlock');
  if (not ARowLock) and
     (
      (ANode.HasAttribute('lrcnt-flag') = false) or
      (ANode.ReadAttributeBool('lrcnt-flag') <> True)
     ) then
    ANode.WriteAttributeInteger('lrcnt', 0);
  if (not ARowLock) and
     (
      (ANode.HasAttribute('pcscnt-flag') = false) or
      (ANode.ReadAttributeBool('pcscnt-flag') <> True)
     ) then
    ANode.WriteAttributeInteger('pcscnt', 0);
  ASizes := AItem.FindNode('sizes', False);
  for I := 0 to ASizes.NodeCount - 1 do
  begin
    ASize := ASizes.Nodes[I];
    AFieldName := TrimAll(Format('%s_%s',
                              [
                                ASize.ReadAttributeString('ftname'),
                                ASize.ReadAttributeString('fitname')
                              ]));
    // check quantity
    if APrevData <> nil then
      // curr qty = last outstanding
      ANode.WriteAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[0]])),
          APrevData.ReadAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[2]])))
        )
    else
      // copy from current size
      ANode.WriteAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[0]])), ASize.ReadAttributeInteger('orqty'));

    if ANode.ReadAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[0]]))) > 0 then
      AHasValue := True;

    // zerofy dist flag if not locked

    AFlag := TrimAll(Format('%s_%s-flag', [AFieldName, CDistColumnFieldSuffixes[1]]));
    if (not ARowLock) and
      (    (ANode.HasAttribute(AFlag) = False)
        or (ANode.ReadAttributeBool(AFlag) = False)
      ) then
      ANode.WriteAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[1]])), 0);

    AQtyVal := ANode.ReadAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[0]])));
    ADistVal:= ANode.ReadAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[1]])));
    ALaying := ANode.ReadAttributeInteger('lrcnt');
    AOutVal := AQtyVal - (ADistVal * ALaying);
    ANode.WriteAttributeInteger(TrimAll(Format('%s_%s', [AFieldName, CDistColumnFieldSuffixes[2]])), AOutVal);
  end;
  // if zero then lock it
  if not AHasValue then
  begin
    ALock := DistributionLock.NodeNew(ANode.Name);
    ALock.NodeCopy(ANode);
  end;
end;

procedure TCSPBaseOptimizer.DeinitEngine;
begin

end;

procedure TCSPBaseOptimizer.DeinitWorksheet;
begin
  InternalDeinitDistribution;
  InternalDeinitConsumption;
  ClearLock;
end;

procedure TCSPBaseOptimizer.DeleteUndistributedRows;
var
  ARoot, ADistRoot, ATmpRoot, AStyle, ANode, AItem, ATmp,
  ALock, ATConsRoot, AConsRoot : TXmlNode;
  I, J, K, AStep: integer;
begin
  ATmpRoot := EngineRootNode.NodeNew('tmpdist');
  ATConsRoot := EngineRootNode.NodeNew('tmpcons');
  ARoot := Document.Root.FindNode('breakdown', False);
  ADistRoot := DistributionRoot;
  AConsRoot := ConsumptionRoot;
  if (ARoot = nil) or (ADistRoot = nil) then
    exit;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    AStyle := ARoot.Nodes[I];
    for J := 0 to AStyle.NodeCount - 1 do
    begin
      ANode := AStyle.Nodes[J];
      for K := 0 to ADistRoot.NodeCount - 1 do
      begin
        AItem := ADistRoot.Nodes[K];
        ALock := FindLockedMaterials(AItem.ReadAttributeString('stname'),
                                     AItem.ReadAttributeString('ftname'),
                                     AItem.ReadAttributeString('fitname'),
                                     AItem.ReadAttributeString('mtcd'));
        if ALock <> nil then
          AStep := ALock.ReadAttributeInteger('stpid')
        else
          AStep := CurrentStep;
        if    (AItem.ReadAttributeString('stname') = AStyle.ReadAttributeString('stname'))
          and (AItem.ReadAttributeString('ftname') = ANode.ReadAttributeString('ftname'))
          and (AItem.ReadAttributeString('fitname') = ANode.ReadAttributeString('fitname'))
          and (AItem.ReadAttributeString('mtcd') = ANode.ReadAttributeString('mtcd'))
          and (AItem.ReadAttributeInteger('stpid') < AStep)
        then
        begin
          ATmp := ATmpRoot.NodeNew('material');
          ATmp.Assign(AItem);
        end;
      end;
      for K := 0 to AConsRoot.NodeCount - 1 do
      begin
        AItem := AConsRoot.Nodes[K];
        ALock := FindLockedMaterials(AItem.ReadAttributeString('stname'),
                                     AItem.ReadAttributeString('ftname'),
                                     AItem.ReadAttributeString('fitname'), 
                                     AItem.ReadAttributeString('mtcd'));
        if ALock <> nil then
          AStep := ALock.ReadAttributeInteger('stpid')
        else
          AStep := CurrentStep;
        if    (AItem.ReadAttributeString('stname') = AStyle.ReadAttributeString('stname'))
          and (AItem.ReadAttributeString('ftname') = ANode.ReadAttributeString('ftname'))
          and (AItem.ReadAttributeString('fitname') = ANode.ReadAttributeString('fitname'))
          and (AItem.ReadAttributeString('mtcd') = ANode.ReadAttributeString('mtcd'))
          and (AItem.ReadAttributeInteger('stpid') < AStep)
        then
        begin
          ATmp := ATConsRoot.NodeNew('material');
          ATmp.Assign(AItem);
        end;
      end;
    end;
  end;
  ADistRoot.NodesClear;
  for I := 0 to ATmpRoot.NodeCount - 1 do
  begin
    ATmp := ATmpRoot.Nodes[I];
    AItem := ADistRoot.NodeNew('material');
    AItem.Assign(ATmp);
  end;
  AConsRoot.NodesClear;
  for I := 0 to ATConsRoot.NodeCount - 1 do
  begin
    ATmp := ATConsRoot.Nodes[I];
    AItem := AConsRoot.NodeNew('material');
    AItem.Assign(ATmp);
  end;
  EngineRootNode.NodeDelete(ATmpRoot.IndexInParent);
  EngineRootNode.NodeDelete(ATConsRoot.IndexInParent);
end;

destructor TCSPBaseOptimizer.Destroy;
begin
  FEngineRootNode := nil;
  FDocument := nil;
  FProperties.Free;
  FRowProperties.Free;
  inherited Destroy;
end;

procedure TCSPBaseOptimizer.DoOptimize;
begin
  try
    GetFieldValues;
    InitWorksheet;
    try
      InternalOptimize;
    finally
      DeinitWorksheet;
    end;
  except on E:Exception do
    raise Exception.Create(Format('%s caught exception: %s',
      [EngineName, E.Message]));
  end;
end;

class function TCSPBaseOptimizer.EngineDescription: string;
begin
  Result := 'TCSPBaseOptimizer is an abstract engine which is not '+
    'purposed for operational usage';
end;

class function TCSPBaseOptimizer.EngineName: string;
begin
  Result := 'Abstract Engine';
end;


class function TCSPBaseOptimizer.EngineUniqueID: string;
begin
  Result := 'MUST_UNIQUE';
end;

function TCSPBaseOptimizer.FindBreakdownMaterials(const AStyle,
  AFtName, AFitName, AMtCD: string): TXmlNode;
var
  ARoot, ANode: TXmlNode;
  I: integer;
begin
  Result := nil;
  ARoot := FindBreakdownStyle(AStyle);
  if ARoot = nil then
    Exit;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    ANode := ARoot.Nodes[I];
    if   (ANode.Name = 'material')
     and (ANode.ReadAttributeString('ftname') = AFtName)
     and (ANode.ReadAttributeString('fitname') = AFitName)
     and (ANode.ReadAttributeString('mtcd') = AMtCD) then
    begin
      Result := ANode;
      Break;
    end;
  end;
end;

function TCSPBaseOptimizer.FindBreakdownStyle(const AStyle: string): TXmlNode;
var
  ARoot, ANode: TXmlNode;
  I: integer;
begin
  Result := nil;
  ARoot := Document.Root.FindNode('breakdown', False);
  if ARoot = nil then
    exit;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    ANode := ARoot.Nodes[I];
    if (ANode.Name = 'style') and
       (ANode.ReadAttributeString('stname') =  AStyle) then
    begin
      Result := ANode;
      break;
    end;    
  end;    
end;

function TCSPBaseOptimizer.FindLockedMaterials(const AStyle,
  AFtName, AFitName, AMtCD: string): TXmlNode;
var
  ARoot, ANode: TXmlNode;
  I: integer;
begin
  Result := nil;
  ARoot := DistributionLock;
  if ARoot = nil then
    exit;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    ANode := ARoot.Nodes[I];
    if (ANode.Name = 'material') and
       (ANode.ReadAttributeString('stname') =  AStyle) and
       (ANode.ReadAttributeString('ftname') =  AFtName) and
       (ANode.ReadAttributeString('fitname') =  AFitName) and
       (ANode.ReadAttributeString('mtcd') =  AMtCD) then
    begin
      Result := ANode;
      break;
    end;    
  end;    
end;

function TCSPBaseOptimizer.FindLockItem(const AStyle,
  AFtName, AFitName, AMaterial: string): TXmlNode;
var
  ANode: TXmlNode;
  I: integer;
begin
  Result := nil;
  for I := 0 to DistributionLock.NodeCount - 1 do
  begin
    ANode := DistributionLock.Nodes[I];
    if (ANode.Name = 'material') and
       (ANode.ReadAttributeString('stname') = AStyle) and
       (ANode.ReadAttributeString('ftname') = AFtName) and
       (ANode.ReadAttributeString('fitname') = AFitName) and       
       (ANode.ReadAttributeString('mtcd') = AMaterial) then
    begin
      Result := ANode;
      Break;
    end;
  end;
end;

function TCSPBaseOptimizer.GetConsumptionColumns: TXmlNode;
begin
  Result := FEngineRootNode.FindNode('consumcolumns', False);
  if Result = nil then
    Result := FEngineRootNode.NodeNew('consumcolumns');
end;

function TCSPBaseOptimizer.GetConsumptionRoot: TXmlNode;
var
  ARootProp: TXmlNode;
begin
  Result := nil;
  if FEngineRootNode <> nil then
  begin
    ARootProp := FEngineRootNode.FindNode('consumptions', False);
    if ARootProp = nil then
      ARootProp := FEngineRootNode.NodeNew('consumptions');
    Result := ARootProp;
  end;
end;

function TCSPBaseOptimizer.GetDistributionColumns: TXmlNode;
begin
  Result := FEngineRootNode.FindNode('distcolumns', False);
  if Result = nil then
    Result := FEngineRootNode.NodeNew('distcolumns');
end;

function TCSPBaseOptimizer.GetDistributionLock: TXmlNode;
var
  ARootProp: TXmlNode;
begin
  Result := nil;
  if FEngineRootNode <> nil then
  begin
    ARootProp := FEngineRootNode.FindNode('distlock', False);
    if ARootProp = nil then
      ARootProp := FEngineRootNode.NodeNew('distlock');
    Result := ARootProp;
  end;
end;

function TCSPBaseOptimizer.GetDistributionRoot: TXmlNode;
var
  ARootProp: TXmlNode;
begin
  Result := nil;
  if FEngineRootNode <> nil then
  begin
    ARootProp := FEngineRootNode.FindNode('distributions', False);
    if ARootProp = nil then
      ARootProp := FEngineRootNode.NodeNew('distributions');
    Result := ARootProp;
  end;
end;

function TCSPBaseOptimizer.GetDistributionSheet: TXmlNode;
var
  ARoot: TXmlNode;
begin
  ARoot := EngineRootNode.FindNode('distsheet', False);
  if ARoot = nil then
    ARoot := EngineRootNode.NodeNew('distsheet');
  Result := ARoot;
end;

procedure TCSPBaseOptimizer.GetFieldValues;
var
  ARootProp, ANode: TXmlNode;
  I: integer;
begin
  if FEngineRootNode <> nil then
  begin
    ARootProp := GetProperties;
    for I := 0 to ARootProp.NodeCount - 1 do
    begin
      ANode := ARootProp.Nodes[I];
      if EngineProperties.FindParam(ANode.Name) <> nil then
      case TFieldType(ANode.ReadAttributeInteger('datatype')) of
        ftFloat, ftBCD:
            EngineProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeFloat('value');
        ftSmallInt, ftInteger, ftLargeInt:
            EngineProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeInteger('value');
        ftDateTime:
            EngineProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeDateTime('value');
        ftBoolean:
            EngineProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeBool('value');
        ftString:
            EngineProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeString('value');
      end;
    end;
  end;
end;

function TCSPBaseOptimizer.GetLoadedConsumption(const AStyle, AFtName, AFitName, AMaterial: string;
  const AStepID: integer): TXmlNode;
var
  ANode: TXmlNode;
  I: integer;
begin
  Result := nil;
  for I := 0 to ConsumptionRoot.NodeCount - 1 do
  begin
    ANode := ConsumptionRoot.Nodes[I];
    if (ANode.Name = 'material') and
       (ANode.ReadAttributeString('stname') = AStyle) and
       (ANode.ReadAttributeString('ftname') = AFtName) and
       (ANode.ReadAttributeString('fitname') = AFitName) and       
       (ANode.ReadAttributeString('mtcd') = AMaterial) and
       (ANode.ReadAttributeInteger('stpid') = AStepID) then
    begin
      Result := ANode;
      Break;
    end;
  end;
end;

function TCSPBaseOptimizer.GetLoadedData(const AStyle, AFtName, AFitName, AMaterial: string;
  const AStepID: integer): TXmlNode;
var
  ANode: TXmlNode;
  I: integer;
begin
  Result := nil;
  for I := 0 to DistributionRoot.NodeCount - 1 do
  begin
    ANode := DistributionRoot.Nodes[I];
    if (ANode.Name = 'material') and
       (ANode.ReadAttributeString('stname') = AStyle) and
       (ANode.ReadAttributeString('ftname') = AFtName) and
       (ANode.ReadAttributeString('fitname') = AFitName) and
       (ANode.ReadAttributeString('mtcd') = AMaterial) and
       (ANode.ReadAttributeInteger('stpid') = AStepID) then
    begin
      Result := ANode;
      Break;
    end;
  end;
end;

function TCSPBaseOptimizer.GetLockStatus(RowIndex, ColIndex: integer): Boolean;
var
  ADistCols, ADistRows, ANode, ACol: TXmlNode;
  AColName: string;
begin
  Result := False;
  ACol := nil;
  ANode := nil;
  ADistCols := DistributionColumns;
  ADistRows := DistributionRoot;
  if (ADistCols.NodeCount <= 0) or (ADistRows.NodeCount <= 0) then
    exit;
  if (ColIndex > 2) and (ColIndex < ADistCols.NodeCount) then
    ACol := ADistCols.Nodes[ColIndex];
  if (RowIndex < ADistRows.NodeCount) then
    ANode := ADistRows.Nodes[RowIndex];
  if (ACol = nil) or (ANode = nil) then
    exit;
  AColName := TrimAll(Format('%s-flag', [ACol.Name]));
  if ANode.HasAttribute(AColName) then
    Result := ANode.ReadAttributeBool(AColName);
end;

function TCSPBaseOptimizer.GetProperties(ARootNode: TXmlNode): TXmlNode;
begin
  Result := ARootNode.FindNode('engineprops', False);
  if Result = nil then
    Result := ARootNode.NodeNew('engineprops');
end;

function TCSPBaseOptimizer.GetProperties: TXmlNode;
begin
  Result := GetProperties(EngineRootNode)
end;

function TCSPBaseOptimizer.GetRowLockStatus(RowIndex: integer): Boolean;
var
  ADistRows, ANode: TXmlNode;
begin
  Result := False;
  ANode := nil;
  ADistRows := DistributionRoot;
  if (ADistRows.NodeCount <= 0) then
    exit;
  if (RowIndex < ADistRows.NodeCount) then
    ANode := ADistRows.Nodes[RowIndex];
  if (ANode = nil) then
    exit;
  if ANode.HasAttribute('rowlock') then
    Result := ANode.ReadAttributeBool('rowlock');
end;

procedure TCSPBaseOptimizer.GetSizeNames(ANode: TXmlNode; ASzList: TStringList);
begin
  GetSizeNames(ANode.ReadAttributeString('stname'),
               ANode.ReadAttributeString('ftname'),
               ANode.ReadAttributeString('fitname'),
               ANode.ReadAttributeString('mtcd'),
               ASzList);
end;

procedure TCSPBaseOptimizer.GetSizeNames(const AStName, AFtName, AFitName, AMtCD: string;
  ASzList: TStringList);

  function GetRoot: TXmlNode;
  var
    I: Integer;
    ASheet: TXmlNode;
  begin
    Result := nil;
    ASheet := DistributionSheet;
    for I := 0 to ASheet.NodeCount - 1 do
      if (ASheet.Nodes[I].ReadAttributeString('stname') = AStName) then
      begin
        Result := ASheet.Nodes[I];
        Break;
      end;
  end;

  function GetMtcd: TXmlNode;
  var
    I: Integer;
    AMaterials: TXmlNode;
  begin
    Result := nil;
    AMaterials := GetRoot;
    if AMaterials = nil then
      exit;    
    for I := 0 to AMaterials.NodeCount - 1 do
      if  (AMaterials.Nodes[I].ReadAttributeString('ftname') = AFtName) and
          (AMaterials.Nodes[I].ReadAttributeString('fitname') = AFitName) and
          (AMaterials.Nodes[I].ReadAttributeString('mtcd') = AMtCD) then
      begin
        Result := AMaterials.Nodes[I];
        Break;
      end;  
  end;

var
  ANode: TXmlNode;
  I, APos: integer;
  AName: string;
begin
  ASzList.Clear;
  ANode := GetMtcd;
  if ANode = nil then
    exit;
  for I := 0 to ANode.AttributeCount - 1 do
  begin
    AName := ANode.AttributeName[I];
    APos := Pos(CDistColumnFieldSuffixes[0], AName);
    if APos > 0 then
    begin
      AName := Copy(AName, 1, APos-2);
      ASzList.Add(AName);
    end;       
  end;
end;

procedure TCSPBaseOptimizer.PackageConsumptionColumns;
var
  ARootColumns, ANode: TXmlNode;
  I: integer;
begin
  if FEngineRootNode <> nil then
  begin
    ARootColumns := FEngineRootNode.FindNode('consumcolumns', False);
    if ARootColumns = nil then
      ARootColumns := FEngineRootNode.NodeNew('');
    ARootColumns.NodesClear;
    ARootColumns.Name := 'consumcolumns';
    for I := 0 to CFabConsHeaderCount - 1 do
    begin
      ANode := ARootColumns.NodeNew(CFabConsFields[I]);
      ANode.WriteAttributeString('caption', CFabConsHeaders[I]);
      if I < 4 then
        ANode.WriteAttributeInteger('datatype',Integer(ftWideString))
      else if I < 6 then
        ANode.WriteAttributeInteger('datatype', Integer(ftInteger))
      else if I = 10 then
        ANode.WriteAttributeInteger('datatype', Integer(ftString))
      else
        ANode.WriteAttributeInteger('datatype', Integer(ftFloat));
    end;
  end;
end;

procedure TCSPBaseOptimizer.PackageConsumptionValues;
var
  ARootProp: TXmlNode;
begin
  if FEngineRootNode <> nil then
  begin
    ARootProp := FEngineRootNode.FindNode('consumptions', False);
    if ARootProp = nil then
      ARootProp := FEngineRootNode.NodeNew('consumptions');
    ARootProp.NodesClear;
  end;
end;

procedure TCSPBaseOptimizer.PackageDistColumns;
var
  ARootColumns, ASizeRoot, ASize, ANode: TXmlNode;
  I, J: integer;
  AFeature, AFit, ACap: String;
begin
  ASizeRoot := FDocument.Root.FindNode('allsizes', False);
  if ASizeRoot = nil then
    exit;
  if FEngineRootNode <> nil then
  begin
    ARootColumns := FEngineRootNode.FindNode('distcolumns', False);
    if ARootColumns = nil then
      ARootColumns := FEngineRootNode.NodeNew('');
    ARootColumns.Clear;
    ARootColumns.Name := 'distcolumns';
    for I := 0 to CDistHeaderCount - 1 do
    begin
      ANode := ARootColumns.NodeNew(CDistFields[I]);
      ANode.WriteAttributeString('caption', CDistHeaders[I]);
      if I < 4 then
        ANode.WriteAttributeInteger('datatype',Integer(ftWideString))
      else
        ANode.WriteAttributeInteger('datatype', Integer(ftInteger));
    end;
    for I := 0 to ASizeRoot.NodeCount-1 do
    begin
      ASize := ASizeRoot.Nodes[I];
      AFeature := ASize.AttributeByName['ftname'];
      AFit := ASize.AttributeByName['fitname'];
      for J := 0 to CDistColumnSuffixCount - 1 do
      begin
        ACap := TrimAll(Format('%s_%s_%s',[AFeature, AFit, CDistColumnFieldSuffixes[J]]));
        ANode := ARootColumns.NodeNew(ACap);
        if (CDistColumnSuffixes[J] = EmptyStr) then
          ANode.WriteAttributeString('caption', AFit)
        else
          ANode.WriteAttributeString('caption',
            Format('%s %s', [AFit, CDistColumnSuffixes[J]]));
        ANode.WriteAttributeInteger('datatype', Integer(ftInteger));
      end;
    end;    
  end;
end;

procedure TCSPBaseOptimizer.PackageDistValues;
var
  ARootProp: TXmlNode;
begin
  if FEngineRootNode <> nil then
  begin
    ARootProp := FEngineRootNode.FindNode('distributions', False);
    if ARootProp = nil then
      ARootProp := FEngineRootNode.NodeNew('distributions');
    ARootProp.NodesClear;
  end;
end;

procedure TCSPBaseOptimizer.PackageFieldValues;
var
  ARootProp, ANode: TXmlNode;
  I: integer;
  AProp: TECFParam;
begin
  if FEngineRootNode <> nil then
  begin
    ARootProp := GetProperties;
    ARootProp.Clear;
    ARootProp.Name := 'engineprops';
    for I := 0 to FProperties.Count - 1 do
    begin
      AProp := TECFParam(FProperties.Items[I]);
      ANode := ARootProp.FindNode(AProp.Name, False);
      if ANode = nil then
        ANode := ARootProp.NodeNew(AProp.Name);
      ANode.WriteAttributeString('category', AProp.Category);
      ANode.WriteAttributeString('caption', AProp.Caption);
      ANode.WriteAttributeInteger('tag', AProp.Index);
      ANode.WriteAttributeInteger('datatype', Integer(AProp.DataType));
      case AProp.DataType of
        ftFloat, ftBCD:
            ANode.WriteAttributeFloat('value', AProp.AsFloat);
        ftSmallInt, ftInteger, ftLargeInt:
            ANode.WriteAttributeInteger('value', AProp.AsInteger);
        ftDateTime:
            ANode.WriteAttributeDateTime('value', AProp.AsDateTime);
        ftBoolean:
            ANode.WriteAttributeBool('value', AProp.AsBoolean);
        ftString:
            ANode.WriteAttributeString('value', AProp.AsString);
      end;
    end;
  end;
end;

procedure TCSPBaseOptimizer.PackageValues;
begin
  PackageFieldValues;
  PackageDistColumns;
  PackageDistValues;
  PackageConsumptionColumns;
  PackageConsumptionValues;
end;

procedure TCSPBaseOptimizer.InitEngine;
begin
end;

procedure TCSPBaseOptimizer.InitWorksheet;
begin
  CurrentStep := 1;
  ClearLock;
  InternalInitDistribution;
  InternalInitConsumption;
end;

procedure TCSPBaseOptimizer.InternalDeinitConsumption;
begin
  SortConsumption;
end;

procedure TCSPBaseOptimizer.InternalDeinitDistribution;
var
  ARoot: TXmlNode;
begin
  ARoot := DistributionSheet;
  ARoot.NodesClear;
  DeleteUndistributedRows;
  SortDistribution;
end;

procedure TCSPBaseOptimizer.InternalInitConsumption;
begin

end;

procedure TCSPBaseOptimizer.InternalInitDistribution;

  procedure ExpandMaterials(ARoot, AStyle: TXmlNode);
  var
    AItem: TXmlNode;
    I: integer;
  begin
    for I := 0 to AStyle.NodeCount - 1 do
    begin
      AItem := AStyle.Nodes[I];
      if (AItem.Name = 'material') then
        CreateSheetItem(ARoot, AItem);
    end;
  end;
  
var
  ABreakdownRoot, AStyle, ARoot, ANode: TXmlNode;
  I : integer;
begin
  ARoot := DistributionSheet;
  ARoot.Clear;
  ARoot.Name := 'distsheet';
  ABreakdownRoot := Document.Root.FindNode('breakdown', False);
  if ABreakdownRoot = nil then
    exit;
  for I := 0 to ABreakdownRoot.NodeCount - 1 do
  begin
    AStyle := ABreakdownRoot.Nodes[I];
    if (AStyle.Name = 'style') then
    begin
      ANode := ARoot.NodeNew('style');
      ANode.NodeCopy(AStyle);
      ExpandMaterials(ANode, AStyle);
    end;
  end;
end;

procedure TCSPBaseOptimizer.LoadRowProperties(ARootNode: TXmlNode;
  ARowProperties: TECFParams);
var
  ARootProp, ANode: TXmlNode;
  I: integer;
begin
  if ARootNode <> nil then
  begin
    ARootProp := GetProperties(ARootNode);
    for I := 0 to ARootProp.NodeCount - 1 do
    begin
      ANode := ARootProp.Nodes[I];
      if ARowProperties.FindParam(ANode.Name) <> nil then
      case TFieldType(ANode.ReadAttributeInteger('datatype')) of
        ftFloat, ftBCD:
            ARowProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeFloat('value');
        ftSmallInt, ftInteger, ftLargeInt:
            ARowProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeInteger('value');
        ftDateTime:
            ARowProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeDateTime('value');
        ftBoolean:
            ARowProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeBool('value');
        ftString:
            ARowProperties.ParamValues[ANode.Name] :=
              ANode.ReadAttributeString('value');
      end;
    end;
  end;
end;

procedure TCSPBaseOptimizer.LoadRowProperties(ARootNode: TXmlNode);
var
  ACurData: TXmlNode;
begin
  // copy from existing step
  ACurData := GetLoadedData(ARootNode.ReadAttributeString('stname'),
                           ARootNode.ReadAttributeString('ftname'),
                           ARootNode.ReadAttributeString('fitname'),
                           ARootNode.ReadAttributeString('mtcd'),
                           CurrentStep);
                           
  // initalize row props
  FRowProperties.AssignInheritable(FProperties);
  if ACurData <> nil then
    LoadRowProperties(ACurData, FRowProperties);
end;

procedure TCSPBaseOptimizer.LoadXMLData(ADocument: TNativeXml);
var
  ARoot, ASrc, ADest: TXmlNode;
begin
  ARoot := ADocument.Root.FindNode(Self.EngineUniqueID, False);
  if ARoot <> nil then
  begin
    ASrc := ARoot.FindNode('engineprops', False);
    ADest := FEngineRootNode.FindNode('engineprops');
    if (ASrc <> nil) and (ADest <> nil) then
      ADest.Assign(ASrc);
    ASrc := ARoot.FindNode('distributions', False);
    ADest := FEngineRootNode.FindNode('distributions', False);
    if (ASrc <> nil) and (ADest <> nil) then
      ADest.Assign(ASrc);
    ASrc := ARoot.FindNode('consumptions', False);
    ADest := FEngineRootNode.FindNode('consumptions', False);
    if (ASrc <> nil) and (ADest <> nil) then
      ADest.Assign(ASrc);
  end;
end;

procedure TCSPBaseOptimizer.SaveRowProperties(ARootNode: TXmlNode;
  ARowProperties: TECFParams);
var
  ARootProp, ANode: TXmlNode;
  I: integer;
  AProp: TECFParam;
begin
  if ARootNode <> nil then
  begin
    ARootProp := GetProperties(ARootNode);
    ARootProp.Clear;
    ARootProp.Name := 'engineprops';
    for I := 0 to ARowProperties.Count - 1 do
    begin
      AProp := TECFParam(ARowProperties.Items[I]);
      ANode := ARootProp.FindNode(AProp.Name, False);
      if ANode = nil then
        ANode := ARootProp.NodeNew(AProp.Name);
      ANode.WriteAttributeString('category', AProp.Category);
      ANode.WriteAttributeString('caption', AProp.Caption);
      ANode.WriteAttributeInteger('tag', AProp.Index);
      ANode.WriteAttributeInteger('datatype', Integer(AProp.DataType));
      case AProp.DataType of
        ftFloat, ftBCD:
            ANode.WriteAttributeFloat('value', AProp.AsFloat);
        ftSmallInt, ftInteger, ftLargeInt:
            ANode.WriteAttributeInteger('value', AProp.AsInteger);
        ftDateTime:
            ANode.WriteAttributeDateTime('value', AProp.AsDateTime);
        ftBoolean:
            ANode.WriteAttributeBool('value', AProp.AsBoolean);
        ftString:
            ANode.WriteAttributeString('value', AProp.AsString);
      end;
    end;
  end;
end;

procedure TCSPBaseOptimizer.SaveXMLData(ADocument: TNativeXml);
var
  ARoot, ASrc, ADest: TXmlNode;
begin
  ARoot := ADocument.Root.FindNode(Self.EngineUniqueID, False);
  if ARoot = nil then
  begin
    ARoot := ADocument.Root.NodeNew(Self.EngineUniqueID);
    ARoot.NodeCopy(FEngineRootNode);
  end;
  ASrc := FEngineRootNode.FindNode('engineprops', False);
  if (ASrc <> nil) then
  begin
    ADest := ARoot.FindNode('engineprops', False);
    if ADest = nil then
      ADest := ARoot.NodeNew('engineprops');
    ADest.Assign(ASrc);
  end;
  ASrc := FEngineRootNode.FindNode('distributions', False);
  if (ASrc <> nil) then
  begin
    ADest := ARoot.FindNode('distributions', False);
    if ADest = nil then
      ADest := ARoot.NodeNew('distributions');
    ADest.Assign(ASrc);
  end;
  ASrc := FEngineRootNode.FindNode('consumptions', False);
  if (ASrc <> nil) then
  begin
    ADest := ARoot.FindNode('consumptions', False);
    if ADest = nil then
      ADest := ARoot.NodeNew('consumptions');
    ADest.Assign(ASrc);
  end;
end;

procedure TCSPBaseOptimizer.SetDocument(ADocument: TNativeXml);
var
  ARoot: TXmlNode;
begin
  FDocument := ADocument;
  ARoot := ADocument.Root.FindNode('engines', False);
  if ARoot <> nil then
  begin
    FEngineRootNode := ARoot.FindNode(EngineUniqueID, False);
    if FEngineRootNode = nil then
    begin
      FEngineRootNode := ARoot.NodeNew(EngineUniqueID);
      FEngineRootNode.WriteAttributeString('name', EngineName);
      FEngineRootNode.WriteAttributeString('desc', EngineDescription);
      FEngineRootNode.WriteAttributeString('class', self.ClassName);
    end;
  end;
end;

procedure TCSPBaseOptimizer.SetLockStatus(RowIndex, ColIndex: integer;
  const Value: Boolean);
var
  ADistCols, ADistRows, ANode, ACol: TXmlNode;
  AColName: string;
begin
  ACol := nil;
  ANode := nil;
  ADistCols := DistributionColumns;
  ADistRows := DistributionRoot;
  if (ADistCols.NodeCount <= 0) or (ADistRows.NodeCount <= 0) then
    exit;
  if (ColIndex > 2) and (ColIndex < ADistCols.NodeCount) then
    ACol := ADistCols.Nodes[ColIndex];
  if (RowIndex < ADistRows.NodeCount) then
    ANode := ADistRows.Nodes[RowIndex];
  if (ACol = nil) or (ANode = nil) then
    exit;
  AColName := TrimAll(Format('%s-flag', [ACol.Name]));
  ANode.WriteAttributeBool(AColName, Value);
end;

procedure TCSPBaseOptimizer.SetProperties(const AFields: string;
  ACategories, ACaptions, ADefaultValues, AInheritable: Variant);
begin
  FProperties.Clear;
  FProperties.ParseParams(AFields);
  FProperties.ParamCaptions[AFields] := ACaptions;
  FProperties.ParamCategories[AFields] := ACategories;
  FProperties.ParamValues[AFields] := ADefaultValues;
  FProperties.ParamInheritables[AFields] := AInheritable;
end;

procedure TCSPBaseOptimizer.SetRowLockStatus(RowIndex: integer;
  const Value: Boolean);
var
  ADistRows, ANode: TXmlNode;
begin
  ANode := nil;
  ADistRows := DistributionRoot;
  if (ADistRows.NodeCount <= 0) then
    exit;
  if (RowIndex < ADistRows.NodeCount) then
    ANode := ADistRows.Nodes[RowIndex];
  if (ANode = nil) then
    exit;
  ANode.WriteAttributeBool('rowlock', Value);
end;

function SortNodeCompare(Node1, Node2: TXmlNode; Info: TPointer): integer;
var
  AValue, BValue: String;
  XValue, YValue: Integer;
begin
  AValue := Node1.ReadAttributeString('stname');
  BValue := Node2.ReadAttributeString('stname');
  Result := CompareStr(AValue, BValue);
  if Result = 0 then
  begin
    AValue := Node1.ReadAttributeString('ftname');
    BValue := Node2.ReadAttributeString('ftname');
    Result := CompareStr(AValue, BValue);
    if Result = 0 then
    begin
      AValue := Node1.ReadAttributeString('fitname');
      BValue := Node2.ReadAttributeString('fitname');
      Result := CompareStr(AValue, BValue);
      if Result = 0 then
      begin
        AValue := Node1.ReadAttributeString('mtcd');
        BValue := Node2.ReadAttributeString('mtcd');
        Result := CompareStr(AValue, BValue);
        if Result = 0 then
        begin
          XValue := Node1.ReadAttributeInteger('stpid');
          YValue := Node2.ReadAttributeInteger('stpid');
          if XValue = YValue then
            Result := 0
          else if XValue > YValue then
            Result := 1
          else Result := -1;
        end;
      end;
    end;
  end;
end;

procedure TCSPBaseOptimizer.SortConsumption;
begin
  ConsumptionRoot.SortChildNodes(SortNodeCompare, nil);
end;

procedure TCSPBaseOptimizer.SortDistribution;
begin
  DistributionRoot.SortChildNodes(SortNodeCompare, nil);
end;

procedure TCSPBaseOptimizer.UpdateDistribution;
var
  ADistRoot, ADistSheet, ANode, ADist, ACols, ACol, ALock,
  AConsRoot, AConsCols, ACons: TXmlNode;
  I, J, K: integer;
begin
  ADistRoot := DistributionRoot;
  ADistSheet:= DistributionSheet;
  AConsRoot := ConsumptionRoot;
  ACols     := DistributionColumns;
  AConsCols := ConsumptionColumns;
  for I := 0 to ADistSheet.NodeCount - 1 do
    for J := 0 to ADistSheet.Nodes[I].NodeCount - 1 do
    begin
      ANode := ADistSheet.Nodes[I].Nodes[J];
      ALock := FindLockItem(ANode.ReadAttributeString('stname'),
                            ANode.ReadAttributeString('ftname'),
                            ANode.ReadAttributeString('fitname'),
                            ANode.ReadAttributeString('mtcd'));
      if (ALock = nil) then
      begin
        ANode.WriteAttributeInteger('stpid', CurrentStep);
        ADist := GetLoadedData(ADistSheet.Nodes[I].ReadAttributeString('stname'),
                      ANode.ReadAttributeString('ftname'),
                      ANode.ReadAttributeString('fitname'),
                      ANode.ReadAttributeString('mtcd'),
                      CurrentStep);
        if ADist = nil then
        begin
          ADist := ADistRoot.NodeNew('material');
          SaveRowProperties(ADist, FRowProperties);
        end;
        for K := 0 to ACols.NodeCount - 1 do
        begin
          ACol := ACols.Nodes[K];
          ADist.WriteAttributeString(ACol.Name, ANode.ReadAttributeString(ACol.Name));
        end;
        ACons := GetLoadedConsumption(ADistSheet.Nodes[I].ReadAttributeString('stname'),
                      ANode.ReadAttributeString('ftname'),
                      ANode.ReadAttributeString('fitname'),
                      ANode.ReadAttributeString('mtcd'),
                      CurrentStep);
        LoadRowProperties(ANode);
        if ACons = nil then
          ACons := AConsRoot.NodeNew('material');
        for K := 0 to AConsCols.NodeCount - 1 do
        begin
          ACol := AConsCols.Nodes[K];
          if FRowProperties.FindParam(ACol.Name) <> nil then
            ACons.WriteAttributeFloat(ACol.Name, FRowProperties.ParamValues[ACol.Name])
          else if ANode.HasAttribute(ACol.Name) then
            ACons.WriteAttributeString(ACol.Name, ANode.ReadAttributeString(ACol.Name));
        end;
        CalculateConsumption(ACons);          
      end;
    end;
end;

initialization
  InternalEnginesManager := TCSPInternalEngineManager.Create;

finalization
  InternalEnginesManager.Free;

end.
