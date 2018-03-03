unit OptitexUtils;

interface
uses
  SysUtils, Classes, CSPCustomEngine, NativeXML, Windows, Dialogs;

type
  TCSPOptitexExportBatch = class(TComponent)
  private
    FEngine: TCSPBaseOptimizer;
    FBuffer: TStringList;
    FFileName: string;
    FDSNFileBuffer: TStringStream;
    FOldDSNFileName: string;
    FOldCRC: integer;
    FDSNFileName: string;

    procedure ExtractSizes(ARoot, AMaterial: TXmlNode);
    procedure SortSizeQuantity(ARoot: TXmlNode);
    procedure RetrieveDSNFileData; overload;
    procedure RetrieveDSNFileData(const AStName: string); overload;
    
    function GetOptitexUMCode(const AStName, AFtName, AFitName, AMtCD: string): string;
    procedure GetMarkerXY(const AStName, AFtName, AFitName, AMtCD: string; out ALen, AWidth: Double);
    function GetAlgorithmName(const AStName, AFtName, AFitName, AMtCD: string): string;
    function GetLayoutName(const AStName, AFtName, AFitName, AMtCD: string): string;
    function GetRootNode: TXmlNode;
  protected
    procedure InternalParseResult;
    procedure CallNestApp;
    procedure CallNestAppUsingBatch;

    property RootNode: TXmlNode read GetRootNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoExportBatch;

    property Engine: TCSPBaseOptimizer read FEngine write FEngine;
  end;

  TCSPOptitexImportXML = class(TComponent)
  private
    FEngine: TCSPBaseOptimizer;
    FFileName: string;
    FXMLDocument: TNativeXml;
  protected
    procedure InternalImportXML;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoImportXML;

    property Engine: TCSPBaseOptimizer read FEngine write FEngine;
  end;

implementation
uses
  Variants, CSPAppUtil, TPropertiesUnit, dmMainU, CSPConsts, GNUGettext,
  dmReferencesU, ShellAPI, ecfutils, HyperStr;

type
  THackedCSPBaseOptimizer = class(TCSPBaseOptimizer);

const
  Base32Str = '0123456789ABCDEFGHIJKLMNOPQRSTUV';
  Base32 : array [0..31] of char = Base32Str;
  Base32Max = 13; //max size req for Int64 Base32 is 13

function IntToBase32(X: Int64; Length: integer = 0): AnsiString ;
var
  buffer : array[0..Base32Max] of AnsiChar;
  n, len : integer;
begin
  Length := Abs(Length);
  if Length < 1 then
    result := Base32[0]
  else
    result := StringOfChar(Base32[0], Length);
    
  X := Abs(X);
  if X = 0 then
    exit;

  FillChar(buffer, SizeOf(buffer), Base32[0]);
  n := Base32Max +1;
  repeat
    Dec(n);
    buffer[n] := Base32[X mod 32];
    X := X div 32;
  until (X = 0) or (n < 0);
  if n >= 0 then
  begin
    len := Base32Max +1 -n;
    if Length > len then
    begin
      result := StringOfChar(Base32[0], Length);
      Move(buffer[n], result[Length -len +1], len);
    end else
    begin
      SetLength(result, len);
      Move(buffer[n], result[1], len);
    end;
  end;
end;

{ TCSPOptitexExportBatch }

procedure TCSPOptitexExportBatch.CallNestApp;
var
  ANestApp, ANestExecutable: string;
begin
  ANestApp := GetAppIDClass.Properties.GetProperty('nestapp');
  if ANestApp <> EmptyStr then
  begin
    ANestExecutable := ExpandFileName(ANestApp);
    if FileExists(ANestExecutable) then
      ShellExecute(0, 'open', PChar('"'+Trim(ANestExecutable)+'"'), PChar('"'+Trim(FFileName)+'"'), nil, SW_SHOW)
    else
      raise Exception.Create(Format(_(SERRNestAppNotFound), [ANestApp]));
  end;
end;

procedure TCSPOptitexExportBatch.CallNestAppUsingBatch;
var
  ANestApp, ANestExecutable, AFileName, ABatchFile: string;
  ABatchStr: TStringList;
begin
  ANestApp := GetAppIDClass.Properties.GetProperty('nestapp');
  if ANestApp <> EmptyStr then
  begin
    ANestExecutable := ExpandFileName(ANestApp);
    AFileName := ExpandFileName(FFileName);
    ABatchFile := ChangeFileExt(FFileName, SCSPBatExt);
    if FileExists(ANestExecutable) then
    begin
      ABatchStr  := TStringList.Create;
      try
        ABatchStr.Add('@echo off');
        ABatchStr.Add('');
        ABatchStr.Add(Format('"%s" "%s"',[ANestExecutable, AFileName]));
        ABatchStr.SaveToFile(ABatchFile);
      finally
        ABatchStr.Free;
      end;      
      // ShellExecute(0, 'open', PChar('"'+ANestExecutable+'"'), PChar('"'+FFileName+'"'), nil, SW_SHOW)
      ShellExecute(0, 'open', PChar(ABatchFile), nil, nil, SW_HIDE);
    end
    else
      raise Exception.Create(Format(_(SERRNestAppNotFound), [ANestApp]));
  end;
end;

constructor TCSPOptitexExportBatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TStringList.Create;
  FEngine := nil;
  FDSNFileBuffer := TStringStream.Create(EmptyStr);
  FOldCRC := -1;
  FDSNFileName := EmptyStr;
end;

destructor TCSPOptitexExportBatch.Destroy;
begin
  FDSNFileBuffer.Free;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TCSPOptitexExportBatch.DoExportBatch;
var
  ACaption: string;
  AProp: TProperties;
  AMemBuffer: TMemoryStream;
begin
  if not Assigned(FEngine) then
    raise Exception.Create(_(SERREngineHasNotSetupProperly));
  FBuffer.Clear;
  AProp := GetAppIDClass.Properties;  
  ACaption := Format('.%s', [StripDot(AProp.GetProperty('btfext', BTFFileExt))]);
  dmMain.SaveDialog.DefaultExt := ACaption;
  dmMain.SaveDialog.Filter := Format(BTFRemarkFormat, [ACaption, ACaption]);;
  dmMain.SaveDialog.Title := BTFSaveDialogTitle;
  if dmMain.SaveDialog.Execute then
  begin
    FFileName := dmMain.SaveDialog.FileName;
    InternalParseResult;
    FBuffer.SaveToFile(FFileName);
    CallNestApp;
    // CallNestAppUsingBatch;
    ShowMessage(_(SInfoBTFSaved));
  end;
end;

procedure TCSPOptitexExportBatch.ExtractSizes(ARoot, AMaterial: TXmlNode);
var
  I: integer;
  ASizeName,
  AAttrName: string;
  ANode: TXmlNode;
  AAllSizes, ASize: TXmlNode;
begin
  AAllSizes := THackedCSPBaseOptimizer(FEngine).Document.Root.FindNode('allsizes', False);
  if AAllSizes = nil then
    exit;
  for I := 0 to AAllSizes.NodeCount - 1 do
  begin
    ASize := AAllSizes.Nodes[i];
    ASizeName := TrimAll(Format('%s_%s', [
        ASize.ReadAttributeString('ftname'),
        ASize.ReadAttributeString('fitname')
      ])); 
    AAttrName := Format('%s_dst', [ASizeName]);
    if (AMaterial.HasAttribute(AAttrName)) and
       (AMaterial.ReadAttributeInteger(AAttrName) > 0) then
    begin
      ANode := ARoot.FindNode(ASizeName, False);
      if ANode = nil then
        ANode := ARoot.NodeNew(ASizeName);
      ANode.WriteAttributeInteger('orqty',
        AMaterial.ReadAttributeInteger(Format('%s_fld', [ASizeName])));      
      ANode.WriteAttributeInteger('orpcs',
        AMaterial.ReadAttributeInteger(AAttrName));
      ANode.WriteAttributeString('ftname',
        ASize.ReadAttributeString('ftname'));
      ANode.WriteAttributeString('fitname',
        ASize.ReadAttributeString('fitname'));
      ANode.WriteAttributeInteger('fitprio',
        ASize.ReadAttributeInteger('fitprio'));
      ANode.WriteAttributeInteger('fitprio2',
        ASize.ReadAttributeInteger('fitprio2'));      
    end; 
  end;
end;

function TCSPOptitexExportBatch.GetAlgorithmName(const AStName,
  AFtName, AFitName, AMtCD: string): string;
var
  ANode: TXmlNode;  
begin
  Result := EmptyStr;
  ANode := THackedCSPBaseOptimizer(FEngine).FindBreakdownMaterials(AStName,
                AFtName, AFitName, AMtCD);
  if (ANode <> nil) then
  begin
    Result := ANode.ReadAttributeString('algorithm');
    ANode := THackedCSPBaseOptimizer(FEngine).Document.Root.FindNode('orders', False);
    if  (ANode <> nil)
      and (ANode.FindNode('order', False) <> nil) then
        ANode := ANode.FindNode('order', False)
    else
      ANode := nil;
    if (Result = EmptyStr) then
      Result := ANode.ReadAttributeString('algorithm');    
  end;
end;

function TCSPOptitexExportBatch.GetLayoutName(const AStName,
  AFtName, AFitName, AMtCD: string): string;
var
  ANode: TXmlNode;  
begin
  Result := EmptyStr;
  ANode := THackedCSPBaseOptimizer(FEngine).FindBreakdownMaterials(AStName,
                AFtName, AFitName, AMtCD);
  if (ANode <> nil) then
  begin
    Result := ANode.ReadAttributeString('mtlayout');
    ANode := THackedCSPBaseOptimizer(FEngine).Document.Root.FindNode('orders', False);
    if  (ANode <> nil)
      and (ANode.FindNode('order', False) <> nil) then
        ANode := ANode.FindNode('order', False)
    else
      ANode := nil;
    if (Result = EmptyStr) then
      Result := ANode.ReadAttributeString('mtlayout');
  end;
end;

procedure TCSPOptitexExportBatch.GetMarkerXY(const AStName, AFtName, AFitName, AMtCD: string;
  out ALen, AWidth: Double);
var
  ANode: TXmlNode;  
begin
  ANode := THackedCSPBaseOptimizer(FEngine).FindBreakdownMaterials(AStName,
                AFtName, AFitName, AMtCD);
  if (ANode <> nil) then
  begin
    ALen := ANode.ReadAttributeFloat('mrklen');
    AWidth := ANode.ReadAttributeFloat('mrkwidth');
    ANode := THackedCSPBaseOptimizer(FEngine).Document.Root.FindNode('orders', False);
    if  (ANode <> nil)
      and (ANode.FindNode('order', False) <> nil) then
        ANode := ANode.FindNode('order', False)
    else
      ANode := nil;
    if (ALen  = 0) then
      ALen := ANode.ReadAttributeFloat('mrklen');
    if (AWidth  = 0) then
      AWidth := ANode.ReadAttributeFloat('mrkwidth');      
  end;
end;

function TCSPOptitexExportBatch.GetOptitexUMCode(const AStName,
  AFtName, AFitName, AMtCD: string): string;
var
  ANode: TXmlNode;
  AUMCode: string;
  AResult: variant;
begin
  Result := EmptyStr;
  ANode := THackedCSPBaseOptimizer(FEngine).FindBreakdownMaterials(AStName,
              AFtName, AFitName, AMtCD);
  if (ANode <> nil) and (ANode.HasAttribute('umcode')) then
  begin
    AUMCode := ANode.ReadAttributeString('umcode');
    if AUMCode <> EmptyStr then
    begin
      AResult := dmReferences.SelectValue('umcodes', 'umcode', 'umoptx', AUMCode);
      if AResult <> Null then
        Result := AResult;
    end;
  end;   
end;

function TCSPOptitexExportBatch.GetRootNode: TXmlNode;
begin
  Result := THackedCSPBaseOptimizer(FEngine).Document.Root;
end;

procedure TCSPOptitexExportBatch.InternalParseResult;
var
  ARootNode, ANode, ASizes, ASZNode, AMtNode: TXmlNode;
  I, J, AIndex, AMIndex, ANIndex, AOIndex, AOldVal, AVal: integer;
  AUMCode, AStName, AMtCd, AName, AStr, ATName, AMtCat,
  AFtName, AFitName: string;
  AUnchecked: boolean;
  ADefaultPath, AAlgorithm, ALayout: string;
  ALen, AWidth: Double;
  AHash, AOldCRC, ARQ, AMod, ARes, AResMod: Integer;
  AMemBuffer: TMemoryStream;
begin
  ARootNode := THackedCSPBaseOptimizer(FEngine).DistributionRoot;
  ADefaultPath := ExtractFilePath(FFileName);
  AOldCRC := -1;
  for I := 0 to ARootNode.NodeCount - 1 do
  begin
    ANode := ARootNode.Nodes[I];
    ASizes := ANode.FindNode('sizes', False);
    if ASizes = nil then
      ASizes := ANode.NodeNew('sizes');
    try
      ExtractSizes(ASizes, ANode);
      SortSizeQuantity(ASizes);
      AStName := ANode.ReadAttributeString('stname');
      AMtCd   := ANode.ReadAttributeString('mtcd');
      AFtName := ANode.ReadAttributeString('ftname');
      AFitName := ANode.ReadAttributeString('fitname');
      RetrieveDSNFileData(AStName);
      AMtNode := THackedCSPBaseOptimizer(FEngine).FindBreakdownMaterials(
                    AStName, AFtName, AFitName, AMtCD);
      AMtCat  := EmptyStr;
      if AMtNode <> nil then
        AMtCat  := AMtNode.ReadAttributeString('mtcatcd');
      if AMtCat = EmptyStr then
        AMtCat := AMtCd;
      AName := TrimAll(Format('%s%s%s%s%d',[
          AStName,
          AFtName,
          AFitName,
          AMtCd,
          ANode.ReadAttributeInteger('stpid')
        ]));
      AHash := CRC32(AOldCRC, AName);
      AOldCRC := AHash;
      // AName := Format('CSP%s',[IntToBase32(AHash)]);
      AUMCode := GetOptitexUMCode(AStName, AFtName, AFitName, AMtCd);
      if AUMCode = EmptyStr then
        raise Exception.Create(_(SERROptitexUMCodeUndefined));
      GetMarkerXY(AStName, AFtName, AFitName, AMtCd, ALen, AWidth);
      if (ALen = 0) or (AWidth = 0) then
        raise Exception.Create(_(SERRMarkerDimensionNotDefined));
      AAlgorithm := GetAlgorithmName(AStName, AFtName, AFitName, AMtCd);
      if AAlgorithm = EmptyStr then
        AAlgorithm := 'ANT';
      ALayout := GetLayoutName(AStName, AFtName, AFitName, AMtCd);
      if ALayout = EmptyStr then
        ALayout := 'SINGLE';
      AStr := EmptyStr;
      if ASizes.NodeCount > 0 then
      begin
        AIndex := 0;
        AOldVal:= 0;
        AUnchecked := False;
        AVal := 0;
        ARes := 0;
        while true do
        begin
          if AIndex >= ASizes.NodeCount then
          begin
            AMod      := (AVal mod 2);
            ARes      := (ARes + AMod);
            AResMod   := (ARes mod 2);
            ARQ       := ((AVal - AMod) div 2) + ((ARes - AResMod) div 2);
            ARes      := AResMod;
            if (AUnchecked) and (ARQ > 0) then
              AStr := Format('%s /RQ=%d',[AStr, ARQ]);
            break;
          end;
          ASZNode := ASizes.Nodes[AIndex];
          AVal := ASZNode.ReadAttributeInteger('orpcs');
          if (AOldVal = AVal) and (AOldVal > 0) then
          begin
            // Decrease ARes from OldValue
            AUnchecked := False;
            ANIndex := AIndex;
            while (AOldVal = AVal) do
            begin
              Inc(ANIndex);
              if (ANIndex >= ASizes.NodeCount) then
                break;
              ASZNode := ASizes.Nodes[ANIndex];
              AVal := ASZNode.ReadAttributeInteger('orpcs');
            end;
            Dec(AIndex);
            Dec(ANIndex);
            AOIndex := (AIndex + ANIndex) div 2;
            for J := AIndex to AOIndex do
            begin
              if J > AIndex then
              begin
                ASZNode := ASizes.Nodes[J];
                AVal := ASZNode.ReadAttributeInteger('orpcs');
                AStr := Format('%s /SIZE=%s /QUANTITY=%d',[
                  AStr,
                  UpperCase(ASZNode.ReadAttributeString('fitname')),
                  AVal
                ]);
              end;
              if J <= AIndex then
                AVal      := AOldVal;
              AMod      := (AVal mod 2);
              ARes      := (ARes + AMod);
              AResMod   := (ARes mod 2);
              ARQ       := ((AVal - AMod) div 2) + ((ARes - AResMod) div 2);
              ARes      := AResMod;
              if (ARQ > 0) then
                AStr := Format('%s /RQ=%d', [AStr, ARQ]);
                
              AMIndex := ANIndex - (J - AIndex);
              if (AMIndex <> J) then
              begin
                ASZNode := ASizes.Nodes[AMIndex];
                AVal := ASZNode.ReadAttributeInteger('orpcs');
                AStr := Format('%s /SIZE=%s /QUANTITY=%d',[
                  AStr,
                  UpperCase(ASZNode.ReadAttributeString('fitname')),
                  AVal
                ]);
                AMod      := (AVal mod 2);
                ARes      := (ARes + AMod);
                AResMod   := (ARes mod 2);
                ARQ       := ((AVal - AMod) div 2) + ((ARes - AResMod) div 2);
                ARes      := AResMod;
                if (ARQ > 0) then
                  AStr := Format('%s /RQ=%d', [AStr, ARQ]);
              end else
              begin
                AMod := (AVal mod 2);
                ARes := ARes + AMod;
              end;
            end;
            AIndex := ANIndex;
          end else
          begin
            if (AUnchecked) and (AOldVal > 0) then
            begin
              AMod      := (AOldVal mod 2);
              ARes      := (ARes + AMod);
              AResMod   := (ARes mod 2);
              ARQ       := ((AOldVal - AMod) div 2) + ((ARes - AResMod) div 2);
              ARes      := AResMod;
              if (ARQ > 0) then
                AStr := Format('%s /RQ=%d', [AStr, ARQ]);
            end;
            AOldVal := AVal;
            AStr := Format('%s /SIZE=%s /QUANTITY=%d',[
                AStr,
                UpperCase(ASZNode.ReadAttributeString('fitname')),
                AVal
              ]);
            AUnchecked := True;
          end;
          Inc(AIndex);
        end;
      end;
      if AStr <> EmptyStr then
      begin
        FBuffer.Add(Format('@NEW  /NAME=%s',[UpperCase(AName)]));
        FBuffer.Add(Format('@UNIT /%s',[UpperCase(AUMCode)]));
        FBuffer.Add(Format('@DESIGN  /FILE=%s /SEAM=BOTH /MATERIAL=%s %s',[
            FDSNFileName,
            AMtCat,
            AStr
          ]));
        FBuffer.Add(Format('@MARKER  /NAME=%s /LENGTH=%g /HEIGHT=%g /PLY=1 /LA=%s', [
            UpperCase(AName),
            ALen,
            AWidth,
            ALayout
          ]));
        FBuffer.Add(Format('@NEST  /ALGORITHM=%s /TIME=%s /UNLIMIT=YES',[AAlgorithm,
          GetAppIDClass.Properties.GetProperty('nesttime', IntToStr(DEF_NESTING_TIME))]));
        ATName := Format('%s%s%s',[
            ADefaultPath,
            AName,
            ExtractFileExt(Format('.%s', [
                GetAppIDClass.Properties.GetProperty('dspext', DSPFileExt)
              ]))
          ]);
        FBuffer.Add(Format('@SAVE  /FILE=%s',[ATName]));
        ATName := Format('%s%s%s',[
            ADefaultPath,
            AName,
            ExtractFileExt(Format('.%s', [
                GetAppIDClass.Properties.GetProperty('xmlext', XMLFileExt)
              ]))
          ]);
        FBuffer.Add(Format('@EXPORT  /FILE=%s /FORMAT=XML',[ATName]));
        // !! this is tend to be bug which causing nest only execute the 1st step
        // FBuffer.Add('');
      end;
    finally
      ANode.NodeDelete(ASizes.IndexInParent);
    end;
  end;
end;

procedure TCSPOptitexExportBatch.RetrieveDSNFileData(const AStName: string);
var
  ARoot, ANode, AItem, AAttachment: TXmlNode;
  AUseStyle: Byte;
  AHash: Integer;
  AName: string;
  AValid: boolean;
  AMemBuffer: TMemoryStream;
begin
  AValid := False;
  AUseStyle := 1;
  ANode := nil;
  AItem := nil;
  AAttachment := nil;
  ARoot := RootNode.FindNode('styles', False);
  if (AUseStyle = 1) and (ARoot = nil) then AUseStyle := 2;
  if (AUseStyle = 1) then ANode := ARoot.NodeByAttributeValue('style', 'stname', AStName);
  if (AUseStyle = 1) and (ANode = nil) then AUseStyle := 2;
  if (AUseStyle = 1) then AItem := ANode.FindNode('attachments', false);
  if (AUseStyle = 1) and (AItem = nil) then AUseStyle := 2;
  if (AUseStyle = 1) then AAttachment := AItem.FindNode('dsndata', false);
  if (AUseStyle = 1) and (AAttachment = nil) then AUseStyle := 2;
  
  if (AUseStyle = 2) then ARoot := RootNode.FindNode('orders', False);
  if (AUseStyle = 2) and (ARoot = nil) then AUseStyle := 0;
  if (AUseStyle = 2) then ANode := ARoot.FindNode('order', False);
  if (AUseStyle = 2) and (ANode = nil) then AUseStyle := 0;
  if (AUseStyle = 2) then AItem := ANode.FindNode('attachments', False);
  if (AUseStyle = 2) and (AItem = nil) then AUseStyle := 0;
  if (AUseStyle = 2) then AAttachment := AItem.FindNode('dsndata', false);
  if (AUseStyle = 2) and (AAttachment = nil) then AUseStyle := 0;
  
  if (AAttachment <> nil) then
  begin
    FOldDSNFileName := AAttachment.ReadAttributeString('name');
    AHash := CRC32(FOldCRC, TrimAll(FOldDSNFileName+AStName));
    AName := Format('CSP%s',[IntToBase32(AHash)]);
    FDSNFileName := Format('%s%s%s',
      [
        ExtractFilePath(FFileName),
        AName,
        ExtractFileExt(FOldDSNFileName)
      ]);
    FDSNFileBuffer.Size := 0;
    FDSNFileBuffer.WriteString(AAttachment.BinaryString);
    if FDSNFileBuffer.Size > 0 then
      AValid := True;
  end;
  if not AValid then
    raise Exception.Create(_(SERRDSNFileNotSupplied));
  FDSNFileBuffer.Position := 0;
  AMemBuffer := TMemoryStream.Create;
  try
    AMemBuffer.LoadFromStream(FDSNFileBuffer);
    AMemBuffer.Position := 0;
    AMemBuffer.SaveToFile(FDSNFileName);
  finally
    AMemBuffer.Free;
    FDSNFileBuffer.Size := 0;
  end;
end;

procedure TCSPOptitexExportBatch.RetrieveDSNFileData;
var
  ARootNode, ANode, AOrderNode: TXmlNode;
  AValid: boolean;
begin
  AValid := False;
  ARootNode := THackedCSPBaseOptimizer(FEngine).Document.Root.FindNode('orders', False);
  if    (ARootNode <> nil)
    and (ARootNode.FindNode('order', False) <> nil)
    and (ARootNode.FindNode('order', False).FindNode('attachments', False) <> nil)
    and (ARootNode.FindNode('order', False).FindNode('attachments', False)
            .FindNode('dsndata', False) <> nil)
      then
  begin
    AOrderNode := ARootNode.FindNode('order', False);
    ANode := AOrderNode
              .FindNode('attachments', False)
              .FindNode('dsndata', False);
    FDSNFileBuffer.Size := 0;
    FDSNFileBuffer.WriteString(ANode.BinaryString);
    FOldDSNFileName := ANode.ReadAttributeString('name');
    FDSNFileName := ExtractFilePath(FFileName) +
      Format('%s-%s', [
          AOrderNode.ReadAttributeString('ordno'),
          AOrderNode.ReadAttributeString('cstcode')
      ]) +
      ExtractFileExt(FOldDSNFileName);

    if FDSNFileBuffer.Size > 0 then
      AValid := True;
  end;
  if not AValid then
    raise Exception.Create(_(SERRDSNFileNotSupplied));
end;

function SortNodeCompare(Node1, Node2: TXmlNode; Info: TPointer): integer;
var
  AValue, BValue: Integer;
begin
  AValue := Node1.ReadAttributeInteger('orpcs');
  BValue := Node2.ReadAttributeInteger('orpcs');
  if AValue = BValue then
  begin
    AValue := Node1.ReadAttributeInteger('fitprio');
    BValue := Node2.ReadAttributeInteger('fitprio');
    if AValue = BValue then
      Result := 0
    else
    if AValue > BValue then
      Result := 1
    else
      Result := -1;
  end
  else if AValue > BValue then
    Result := 1
  else Result := -1;
end;

procedure TCSPOptitexExportBatch.SortSizeQuantity(ARoot: TXmlNode);
begin
  ARoot.SortChildNodes(SortNodeCompare, nil);
end;

{ TCSPOptitexImportXML }

constructor TCSPOptitexImportXML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLDocument := TNativeXml.Create;
end;

destructor TCSPOptitexImportXML.Destroy;
begin
  FXMLDocument.Free;
  inherited Destroy;
end;

procedure TCSPOptitexImportXML.DoImportXML;
var
  ACaption: string;
  AProp: TProperties;
begin
  if not Assigned(FEngine) then
    raise Exception.Create(_(SERREngineHasNotSetupProperly));
  AProp := GetAppIDClass.Properties;  
  ACaption := ExtractFileExt(Format('.%s', [AProp.GetProperty('btfext', BTFFileExt)]));
  dmMain.OpenDialog.DefaultExt := ACaption;
  dmMain.OpenDialog.Filter := Format(BTFRemarkFormat, [ACaption, ACaption]);;
  dmMain.OpenDialog.Title := BTFSaveDialogTitle;
  if dmMain.OpenDialog.Execute then
  begin
    FFileName := dmMain.OpenDialog.FileName;
    InternalImportXML;
  end;
end;

procedure TCSPOptitexImportXML.InternalImportXML;
var
  ARootNode, ANode, AItem: TXmlNode;
  AStName, AFtName, AFitName, AMtCD, AName, ADefaultPath, AXMLFileName: string;
  I, AStpId, AHash, AOldCrc: integer;
  AError, AMsg: String;
begin
  ARootNode := THackedCSPBaseOptimizer(FEngine).ConsumptionRoot;
  ADefaultPath := ExtractFilePath(FFileName);
  AOldCrc := -1;
  AError := EmptyStr;
  for I := 0 to ARootNode.NodeCount - 1 do
  begin
    ANode := ARootNode.Nodes[I];
    if (ANode.Name = 'material') then
    begin
      AStName := ANode.ReadAttributeString('stname');
      AFtName := ANode.ReadAttributeString('ftname');
      AFitName:= ANode.ReadAttributeString('fitname'); 
      AMtCD   := ANode.ReadAttributeString('mtcd');
      AStpId  := ANode.ReadAttributeInteger('stpid');
      AName   := TrimAll(Format('%s%s%s%s%d',[AStName, AFtName, AFitName, AMtCd, AStpId]));
      AHash   := CRC32(AOldCrc, AName);
      // AName   := Format('CSP%s',[IntToBase32(AHash)]);
      AOldCrc := AHash;
      AXMLFileName := Format('%s%s%s',[
            ADefaultPath,
            AName,
            Format('.%s', [
              StripDot(GetAppIDClass.Properties.GetProperty('xmlext', XMLFileExt))
              ])
          ]);
      try
        if FileExists(AXMLFileName) then
        begin
          FXMLDocument.Clear;
          FXMLDocument.LoadFromFile(AXMLFileName);
          if Assigned(FXMLDocument.Root) then
          begin
            AItem := FXMLDocument.Root.FindNode('WIDTH', False);
            if Assigned(AItem) then
              ANode.WriteAttributeFloat('actwidth', AItem.ValueAsFloat)
            else
              raise Exception.Create('file does not have WIDTH tag');
            AItem := FXMLDocument.Root.FindNode('LENGTH', False);
            if Assigned(AItem) then
              ANode.WriteAttributeFloat('actlen', AItem.ValueAsFloat)
            else
              raise Exception.Create('file does not have LENGTH tag');
            if ANode.NodeCount > 0 then
              ANode.NodeDelete(0);
            AItem := ANode.NodeNew('');
            AItem.Assign(FXMLDocument.Root);
            THackedCSPBaseOptimizer(FEngine).CalculateConsumption(ANode);
          end;
        end;
      except
        on E:Exception do
          begin
            AMsg := Format('%s caught an exception: %s',
                      [
                        ExtractFileName(AXMLFileName),
                        E.Message
                      ]);
            if AError = EmptyStr then
              AError := AMsg
            else
              AError := #13#10+AMsg;
          end;
      end;
    end;
  end;
  if AError <> EmptyStr then
    raise Exception.Create(AError);
end;

end.
