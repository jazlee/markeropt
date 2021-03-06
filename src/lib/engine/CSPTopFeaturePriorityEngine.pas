unit CSPTopFeaturePriorityEngine;

interface
uses
  SysUtils, Classes, CSPCustomEngine, NativeXml, ecfutils;

type
  TSizeItem = class(TCollectionItem)
  private
    FNode: TXmlNode;
    FDistribute: integer;
    procedure SetDistribute(const Value: integer);

    procedure Reset;
    function GetQuantity: Integer;
    function GetSizeName: string;
    function GetOutstanding: integer;
  public
    procedure SetupSize(ARoot: TXmlNode);

    property SizeName: string read GetSizeName;
    property Quantity: Integer read GetQuantity;
    property Distribute: integer read FDistribute write SetDistribute;
    property Outstanding: integer read GetOutstanding;
    property Node: TXmlNode read FNode;
  end;

  TSizeItems = class(TOwnedCollection)
  public
    constructor Create(AOwner: TPersistent);
    procedure SetupSizes(ARoot: TXmlNode);

    function SumOfOutstanding: integer;
  end;

  TMaterialItem = class(TCollectionItem)
  private
    FSizeItems: TSizeItems;
    FPieces: integer;
    FLaying: integer;
    FNode: TXmlNode;
    FMaxLayer: integer;
    FMinLayer: integer;
    FMaxPcs: Integer;
    FMaxLen: Double;
    FDefMaxLen: Double;
    FDefMaxPcs: Integer;
    FPrevTotal: Integer;
    function GetMaterialCode: string;
    function GetAvgYY: Double;

    function ReadRatio: Integer;
    procedure Reinitialize;
    procedure Persist;
    procedure DoOptimize;

    function TryDistribute: boolean;
    function RowLocked: boolean;
    function ColLock(ASize: TSizeItem): boolean;
    function LayingLocked: boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function IsOptimum: boolean;

    procedure SetupMaterial(ARoot: TXmlNode);
    function OptimizeLine: boolean;

    procedure OptimizeMaterial;

    property MaterialCode: string read GetMaterialCode;
    property Laying: integer read FLaying;
    property Pieces: integer read FPieces;
    property Node: TXmlNode read FNode;
    property MinLayer: integer read FMinLayer;
    property MaxLayer: integer read FMaxLayer;
    property AvgYY: Double read GetAvgYY;
    property MaxLen: Double read FMaxLen;
    property MaxPcs: Integer read FMaxPcs;
    property DefaultMaxPcs: Integer read FDefMaxPcs;
    property DefaultMaxLen: Double read FDefMaxLen;

    property Sizes: TSizeItems read FSizeItems;
  end;

  TMaterialItems = class(TOwnedCollection)
  public
    constructor Create(AOwner: TPersistent);

    procedure OptimizeMaterials;
    procedure Persist;
    procedure SetupMaterials(ARoot: TXmlNode);
  end;
                
  TStyleItem = class(TCollectionItem)
  private
    FMaterials: TMaterialItems;
    FNode: TXmlNode;
    function GetStyle: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure SetupStyle(ARoot: TXmlNode);
    procedure OptimizeStyle;
    procedure Persist;

    property Style: string read GetStyle;
    property Materials: TMaterialItems read FMaterials;
    property Node: TXmlNode read FNode;
  end;

  TStyleItems = class(TOwnedCollection)
  private
    FDocument: TNativeXml;
    FPersisted: Boolean;
  public
    constructor Create(AOwner: TPersistent);

    function optimized: boolean;

    procedure Optimize;
    procedure Persist;

    procedure SetupDistribution(ARoot: TXmlNode);
    property Document: TNativeXml read FDocument;
  end;

  TCSPTopFeaturePriorityOptimizer = class(TCSPBaseOptimizer)
  private
    FStyle: TStyleItems;
    procedure PrepareFeaturePriority;
  protected
    procedure InternalInitDistribution; override;
    procedure InternalDeinitDistribution; override;

    procedure InternalOptimize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitEngine; override;
    procedure DeinitEngine; override;

    class function EngineUniqueID: string; override;
    class function EngineName: string; override;
    class function EngineDescription: string; override;
  end;    

implementation
uses
  CSPAppUtil, Math, Variants;

{ TCSPFeaturePriorityOptimizer }

constructor TCSPTopFeaturePriorityOptimizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStyle := TStyleItems.Create(Self);  
end;

procedure TCSPTopFeaturePriorityOptimizer.DeinitEngine;
begin
  inherited DeinitEngine;
end;

destructor TCSPTopFeaturePriorityOptimizer.Destroy;
begin
  FStyle.Free;
  inherited Destroy;
end;

class function TCSPTopFeaturePriorityOptimizer.EngineDescription: string;
begin
  Result := 'Optimizer based on feature priority';
end;

class function TCSPTopFeaturePriorityOptimizer.EngineName: string;
begin
  Result := 'Top Feature Priority';
end;

class function TCSPTopFeaturePriorityOptimizer.EngineUniqueID: string;
begin
  Result := 'TopFeaturePriority';
end;


procedure TCSPTopFeaturePriorityOptimizer.InitEngine;
var
  AMarkerLen: Double;
  ARootNode: TXmlNode;
begin
  inherited InitEngine;
  AMarkerLen := 0;
  ARootNode := Document.Root.FindNode('orders');
  if ARootNode <> nil then
    ARootNode := ARootNode.FindNode('order');
  if ARootNode <> nil then
    AMarkerLen := ARootNode.ReadAttributeFloat('mrklen');
  ARootNode := Document.Root.FindNode('orders').FindNode('order');
  SetProperties('reverse;mrklen;maxstep;deepopt',
                VarArrayOf(
                  [
                    'General',
                    'General',
                    'Distribution',
                    'Distribution'
                  ]),
                VarArrayOf(
                  [
                    'Reverse',
                    'Marker Length',
                    'Max Step',
                    'Smart Optimize'
                  ]),
                VarArrayOf(
                  [
                    False,
                    AMarkerLen,
                    CMaxStep,
                    False
                  ]),
                VarArrayOf(
                  [
                    True,
                    True,
                    False,
                    False
                  ]));

end;

procedure TCSPTopFeaturePriorityOptimizer.InternalDeinitDistribution;
begin
  inherited InternalDeinitDistribution;
end;

procedure TCSPTopFeaturePriorityOptimizer.InternalInitDistribution;
begin
  inherited InternalInitDistribution;
end;

procedure TCSPTopFeaturePriorityOptimizer.InternalOptimize;
var
  AMaxStep: integer;
  ALoop: boolean;
begin
  PrepareFeaturePriority;
  FStyle.Clear;
  FStyle.SetupDistribution(DistributionSheet);
  AMaxStep := CMaxStep;
  if EngineProperties.ParamValues['maxstep'] <> null then
    AMaxStep := EngineProperties.ParamValues['maxstep'];
  ALoop := True;
  while ALoop do
  begin
    if CurrentStep > AMaxStep then
      raise Exception.Create('WARNING: Possibly could not find the most optimum value');
    ALoop := not FStyle.optimized;
    FStyle.Optimize;
    FStyle.Persist;
    UpdateDistribution;
    CurrentStep := CurrentStep + 1;
    InternalInitDistribution;
    PrepareFeaturePriority;
    FStyle.Clear;
    FStyle.SetupDistribution(DistributionSheet);
  end;
end;

function SortNodeCompare(Node1, Node2: TXmlNode; Info: TPointer): integer;
var
  AValue, BValue: Integer;
begin
  AValue := Node1.ReadAttributeInteger('xfitprio2');
  BValue := Node2.ReadAttributeInteger('xfitprio2');

  if AValue = BValue then
    Result := 0
  else if AValue > BValue then
    Result := 1
  else Result := -1;
end;

procedure TCSPTopFeaturePriorityOptimizer.PrepareFeaturePriority;

  procedure CollectSizes(ANode: TXmlNode);
  var
    ASizeList: TStringList;
    I: integer;
    ALock, AItem: TXmlNode;
  begin
    LoadRowProperties(ANode);
    ALock := FindLockItem(ANode.ReadAttributeString('stname'),
                            ANode.ReadAttributeString('ftname'),
                            ANode.ReadAttributeString('fitname'),
                            ANode.ReadAttributeString('mtcd'));
    if ALock <> nil then
      exit;
    ASizeList := TStringList.Create;
    try
      GetSizeNames(ANode, ASizeList);
      ANode.NodesClear;
      for I := 0 to ASizeList.Count - 1 do
      begin
        ANode.WriteAttributeInteger(TrimAll(Format('%s_out',[ASizeList[I]])),
            ANode.ReadAttributeInteger(TrimAll(Format('%s_fld',[ASizeList[I]]))) -
            (
              ANode.ReadAttributeInteger('lrcnt') *
              ANode.ReadAttributeInteger(TrimAll(Format('%s_dst',[ASizeList[I]])))
            )
          );
        if (ANode.ReadAttributeInteger(TrimAll(Format('%s_fld',[ASizeList[I]]))) > 0) then
        begin
          AItem := ANode.FindNode(ASizeList[I], False);
          if AItem = nil then
            AItem := ANode.NodeNew(ASizeList[I]);
          AItem.WriteAttributeInteger('orqty',
            ANode.ReadAttributeInteger(TrimAll(Format('%s_fld',[ASizeList[I]]))));
        end;
      end;
    finally
      ASizeList.Free;
    end;
    ANode.SortChildNodes(SortNodeCompare, RowProperties);
  end;
var
  I, J: integer;
  AStyleRoot, ANode: TXmlNode;
begin
  AStyleRoot := DistributionSheet;
  for I := 0 to AStyleRoot.NodeCount - 1 do
    for J := 0 to AStyleRoot.Nodes[I].NodeCount - 1 do
    begin
      ANode := AStyleRoot.Nodes[I].Nodes[J];
      CollectSizes(ANode);
    end;
end;

{ TSizeItem }

function TSizeItem.GetOutstanding: integer;
begin
  Result := Quantity - (FDistribute *  TMaterialItem(Collection.Owner).FLaying);
end;

function TSizeItem.GetQuantity: Integer;
begin
  Result := Node.ReadAttributeInteger('orqty')
end;

function TSizeItem.GetSizeName: string;
begin
  Result  := Node.Name;
end;

procedure TSizeItem.Reset;
begin
  FDistribute := 0;
end;

procedure TSizeItem.SetDistribute(const Value: integer);
begin
  if (FDistribute <> 0) then
    TMaterialItem(Collection.Owner).FPieces := TMaterialItem(Collection.Owner).FPieces -
      FDistribute;
  FDistribute := Value;
  TMaterialItem(Collection.Owner).FPieces := TMaterialItem(Collection.Owner).FPieces +
      FDistribute;
end;

procedure TSizeItem.SetupSize(ARoot: TXmlNode);
begin
  FNode := ARoot;
  Reset;
end;

{ TSizeItems }

constructor TSizeItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TSizeItem);
end;

procedure TSizeItems.SetupSizes(ARoot: TXmlNode);
var
  I: integer;
  AItem: TSizeItem;
begin
  Self.Clear;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    AItem := TSizeItem(Add);
    AItem.SetupSize(ARoot.Nodes[I]);
  end;
end;

function TSizeItems.SumOfOutstanding: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + TSizeItem(Items[I]).Outstanding;
end;

{ TMaterialItem }
function TMaterialItem.ColLock(ASize: TSizeItem): boolean;
var
  ALockName: string;
begin
  Result := False;
  ALockName := TrimAll(format('%s_dst-flag', [ASize.SizeName]));
  if (Node.HasAttribute(ALockName)) then
    Result := Node.ReadAttributeBool(ALockName);
end;

constructor TMaterialItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSizeItems := TSizeItems.Create(Self);
  FLaying := 0;
  FPieces := 0;
  FMaxLayer := 0;
  FMinLayer := 0;
  FMaxPcs := 0;
  FDefMaxLen := 0;
  FDefMaxPcs := 0;
end;

destructor TMaterialItem.Destroy;
begin
  FSizeItems.Free;
  inherited Destroy;
end;

procedure TMaterialItem.DoOptimize;
begin
  while not OptimizeLine do
  begin
    Reinitialize;
    if FMaxPcs > 0 then
      FMaxPcs := FMaxPcs - 1;
    if FMaxPcs = 0 then
      break;
  end;
end;

function TMaterialItem.GetAvgYY: Double;
begin
  Result := Node.ReadAttributeFloat('avgyy');
end;

function TMaterialItem.GetMaterialCode: string;
begin
  Result := Node.ReadAttributeString('mtcd');
end;

function TMaterialItem.IsOptimum: boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to Sizes.Count - 1 do
  begin
    Result := TSizeItem(Sizes.Items[I]).Outstanding <= 0;
    if not Result then
      break;
  end;
end;

function TMaterialItem.LayingLocked: boolean;
begin
  Result := False;
  if (Node.HasAttribute('lrcnt-flag')) then
    Result := Node.ReadAttributeBool('lrcnt-flag');
end;

function TMaterialItem.OptimizeLine: Boolean;
var
  AResult: Boolean;
  AProp: TECFParams;
begin
  AProp := TCSPTopFeaturePriorityOptimizer(TStyleItem(Collection.Owner).
    Collection.Owner).EngineProperties;
  if RowLocked then
  begin
    Result := True;
    exit;
  end;
  while (FMinLayer < FLaying)
    and (FMaxLayer >= FLaying) do
  begin
    AResult := TryDistribute;
    if LayingLocked then
      FMinLayer := FLaying
    else
    begin
      if AResult then
        FMinLayer := FLaying
      else
        FMaxLayer :=  FLaying;
      FLaying := (FMinLayer + FMaxLayer) div 2;
    end;
  end;
  Result := TryDistribute and (FMinLayer = FLaying);
end;

procedure TMaterialItem.OptimizeMaterial;
var
  ASum,
  AOldSum,
  AOldMaxPcs,
  AOldLayer,
  AOldPcs: integer;
  AProp: TECFParams;
begin
  AProp := TCSPTopFeaturePriorityOptimizer(TStyleItem(Collection.Owner).
    Collection.Owner).EngineProperties;
  Reinitialize;
  if AProp.ParamValues['deepopt'] then
  begin
    DoOptimize;
    AOldSum := Sizes.SumOfOutstanding;
    AOldMaxPcs := FMaxPcs;
    AOldLayer := FLaying;
    AOldPcs := FPieces;  
    FMaxPcs := FPieces;
    while True do
    begin
      if FMaxPcs > 0 then
        Dec(FMaxPcs);
      if FMaxPcs <= 0 then
        break;
      Reinitialize;
      DoOptimize;
      ASum := Sizes.SumOfOutstanding;
      if ASum < AOldSum then
      begin
        AOldSum := ASum;
        AOldMaxPcs := FMaxPcs;
        AOldLayer := FLaying;
        AOldPcs := FPieces;
      end else
      if (ASum = AOldSum) and
         (AOldPcs > FPieces) and
         (AOldLayer < FLaying) then
      begin
        AOldSum := ASum;
        AOldMaxPcs := FMaxPcs;
        AOldLayer := FLaying;
        AOldPcs := FPieces;    
      end;    
    end;
    Reinitialize;
    FMaxPcs   := AOldMaxPcs;
  end;
  DoOptimize;
end;

procedure TMaterialItem.Persist;
var
  I: integer;
  AItem: TSizeItem;
begin
  for I := 0 to Sizes.Count - 1 do
  begin
    AItem := TSizeItem(Sizes.Items[I]);
    Node.WriteAttributeInteger(TrimAll(Format('%s_fld',[AItem.SizeName])), AItem.Quantity);
    Node.WriteAttributeInteger(TrimAll(Format('%s_dst',[AItem.SizeName])), AItem.Distribute);
    Node.WriteAttributeInteger(TrimAll(Format('%s_out',[AItem.SizeName])), AItem.Outstanding);
  end;
  Node.WriteAttributeInteger('lrcnt', Laying);
  Node.WriteAttributeInteger('pcscnt', Pieces);
end;

function TMaterialItem.ReadRatio: Integer;
var
  I: integer;
  AItem: TSizeItem;
  AFieldName: string;
  AValue: Boolean;
begin
  AValue := False;
  if Node.HasAttribute('rowlock') then
    AValue := Node.ReadAttributeBool('rowlock');
  for I := 0 to Sizes.Count - 1 do
  begin
    AItem := TSizeItem(Sizes.Items[I]);
    AItem.Distribute := 0;
    AFieldName := TrimAll(Format('%s_dst-flag',[AItem.SizeName]));
    if (not AValue) and (Node.HasAttribute(AFieldName) = True) then
      AValue := Node.ReadAttributeBool(AFieldName);
    if AValue then
    begin
      AFieldName := TrimAll(Format('%s_dst',[AItem.SizeName]));
      AItem.Distribute := Node.ReadAttributeInteger(AFieldName);
    end;
  end;
  Result := Pieces;
end;

procedure TMaterialItem.Reinitialize;
var
  AProp: TECFParams;
  AReversed: Boolean;
  AValue: Boolean;
begin
  AValue := RowLocked;
  if not AValue then
    AValue := LayingLocked;
  TCSPTopFeaturePriorityOptimizer(TStyleItem(Collection.Owner).
    Collection.Owner).LoadRowProperties(Node);
  AProp := TCSPTopFeaturePriorityOptimizer(TStyleItem(Collection.Owner).
    Collection.Owner).RowProperties;
  AReversed := AProp.ParamByName('reverse').AsBoolean;
  FLaying := 0;
  FMinLayer := 1;
  FMaxLayer := 1;
  if Sizes.Count > 0 then
  begin
    if AReversed then
      FMaxLayer := TSizeItem(Sizes.Items[Sizes.Count-1]).Quantity
    else
      FMaxLayer := TSizeItem(Sizes.Items[0]).Quantity;
  end;
  // FDefMaxLen := Node.ReadAttributeFloat('mrklen');
  FDefMaxLen := AProp.ParamByName('mrklen').AsFloat;
  FMaxLen := FDefMaxLen;
  if AValue then
    FLaying := Node.ReadAttributeInteger('lrcnt')
  else
    FLaying := FMaxLayer;
  // FDefMaxPcs := Round(FMaxLen/AvgYY);
  FDefMaxPcs := Floor(FMaxLen/AvgYY);
  if FMaxPcs <= 0 then
  begin
    if AvgYY > 0 then
      FMaxPcs := FDefMaxPcs
    else
      FMaxPcs := 0;
  end;
  ReadRatio;
end;

function TMaterialItem.RowLocked: boolean;
begin
  Result := False;
  if (Node.HasAttribute('rowlock')) then
    Result := Node.ReadAttributeBool('rowlock');
end;

procedure TMaterialItem.SetupMaterial(ARoot: TXmlNode);
begin
  FNode := ARoot;
  FSizeItems.SetupSizes(Node);
  Reinitialize;
end;

function TMaterialItem.TryDistribute: boolean;
var
  AConsumed, I: integer;
  AFound, ALooped: boolean;
  AItem: TSizeItem;
  AProp: TECFParams;
  AEngineProp: TECFParams;  
  AReversed: Boolean;
begin
  AEngineProp := TCSPTopFeaturePriorityOptimizer(TStyleItem(Collection.Owner).
    Collection.Owner).EngineProperties;
  AProp := TCSPTopFeaturePriorityOptimizer(TStyleItem(Collection.Owner).
    Collection.Owner).RowProperties;
  AReversed := AProp.ParamByName('reverse').AsBoolean;
  AConsumed := ReadRatio;
  ALooped := False;
  AFound := True;
  while (AConsumed < FMaxPcs) and (not ALooped) do
  begin
    if not AFound then
      ALooped := True;
    AFound := False;
{
    NTop Priority    NTop Priority reversed
    1  start here    1
    2                2
    3                3   start here
}
    if not AReversed then       
      for i := 0 to FSizeItems.Count-1 do
      begin
        AItem := TSizeItem(FSizeItems.Items[I]);
        if    (AItem.Quantity >= ((AItem.Distribute + 1) * FLaying))
          and (AConsumed < FMaxPcs)
          and (not ColLock(AItem)) then
        begin
          Inc(AConsumed);
          AItem.Distribute := AItem.Distribute + 1;
          AFound := True;
        end;
        if AConsumed = FMaxPcs then
          break;
      end
    else
      for i := (FSizeItems.Count-1) downto 0 do
      begin
        AItem := TSizeItem(FSizeItems.Items[I]);
        if    (AItem.Quantity >= ((AItem.Distribute + 1) * FLaying))
          and (AConsumed < FMaxPcs)
          and (not ColLock(AItem)) then          
        begin
          Inc(AConsumed);
          AItem.Distribute := AItem.Distribute + 1;
          AFound := True;
        end;
        if AConsumed = FMaxPcs then
          break;
      end
  end;
  Result := (AConsumed = FMaxPcs);
end;

{ MaterialItems }

constructor TMaterialItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TMaterialItem);
end;

{ TStyleItem }

constructor TStyleItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FMaterials := TMaterialItems.Create(Self);
end;

destructor TStyleItem.Destroy;
begin
  FMaterials.Free;
  inherited Destroy;
end;

function TStyleItem.GetStyle: string;
begin
  Result := Node.ReadAttributeString('stname');
end;

procedure TStyleItem.OptimizeStyle;
begin
  Materials.OptimizeMaterials;
end;

procedure TStyleItem.Persist;
begin
  Materials.Persist;
end;

procedure TStyleItem.SetupStyle(ARoot: TXmlNode);
begin
  FNode := ARoot;
  Materials.SetupMaterials(Node);
end;

{ TStyleItems }

constructor TStyleItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TStyleItem);
  FDocument := nil;
end;

procedure TStyleItems.Optimize;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    TStyleItem(Items[I]).OptimizeStyle;
end;

function TStyleItems.optimized: boolean;
var
  I, J, K: integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    for J := 0 to TStyleItem(Items[I]).Materials.Count - 1 do
      for K := 0 to TMaterialItem(TStyleItem(Items[I]).Materials.Items[J]).Sizes.Count - 1 do
      begin
        Result := TSizeItem(TMaterialItem(TStyleItem(Items[I]).Materials.Items[J]).Sizes.Items[K]).Outstanding <= 0;
        if not Result then
          break;
      end;
end;

procedure TStyleItems.Persist;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    TStyleItem(Items[I]).Persist;
  FPersisted := True;
end;

procedure TStyleItems.SetupDistribution(ARoot: TXmlNode);
var
  I: integer;
  AStyle: TStyleItem;
begin
  FDocument := ARoot.Document;
  Self.Clear;
  FPersisted := False;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    AStyle := TStyleItem(Add);
    AStyle.SetupStyle(ARoot.Nodes[I]);
    if AStyle.Materials.Count = 0 then
      Delete(AStyle.Index);
  end;
end;

procedure TMaterialItems.OptimizeMaterials;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    TMaterialItem(Items[I]).OptimizeMaterial;
end;

procedure TMaterialItems.Persist;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    TMaterialItem(Items[I]).Persist;
end;

procedure TMaterialItems.SetupMaterials(ARoot: TXmlNode);
var
  I: integer;
  AItem: TMaterialItem;
  ANode, ALock: TXmlNode;
begin
  Self.Clear;
  for I := 0 to ARoot.NodeCount - 1 do
  begin
    ANode := ARoot.Nodes[I];
    ALock := TCSPTopFeaturePriorityOptimizer(TStyleItem(Owner).Collection.Owner).
                FindLockItem(ANode.ReadAttributeString('stname'),
                             ANode.ReadAttributeString('ftname'),
                             ANode.ReadAttributeString('fitname'),
                             ANode.ReadAttributeString('mtcd'));
    if ALock = nil then
    begin
      AItem := TMaterialItem(Add);
      AItem.SetupMaterial(ANode);
    end;
  end;
end;

initialization
  RegisterOptimizer(TCSPTopFeaturePriorityOptimizer);

finalization
  UnregisterOptimizer(TCSPTopFeaturePriorityOptimizer);

end.
