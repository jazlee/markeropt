unit ecfextrntools;

interface

uses
  SysUtils, Classes;

type
  TExternalToolItem = class(TCollectionItem)
  private
    FProgName: string;
    FWorkingDir: string;
    FTitle: string;
    FParameters: string;
    procedure SetProgName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Title: string read FTitle write FTitle;
    property ProgramName: string read FProgName write SetProgName;
    property WorkingDir: string read FWorkingDir write FWorkingDir;
    property Parameters: string read FParameters write FParameters;
  end;

  TExternalTools = class(TOwnedCollection)
  private
  protected
    function PropsToXML(AItem: TExternalToolItem): string;
    procedure XMLToProps(const AProps: string; AItem: TExternalToolItem);
  public
    constructor Create(AOwner: TPersistent);

    function AddItem: TExternalToolItem;

    procedure ManageTools;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
  end;

implementation

uses frmExtrnToolsU, TypInfo, frxXML, ecfutils, frxUtils, Variants;

{ TExternalTools }

function TExternalTools.AddItem: TExternalToolItem;
begin
  Result := TExternalToolItem(Add);
end;

constructor TExternalTools.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TExternalToolItem);
end;

procedure TExternalTools.LoadFromFile(const AFileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(f);
  finally
    f.Free;
  end;
end;

procedure TExternalTools.LoadFromStream(Stream: TStream);
var
  XMLDoc: TfrxXMLDocument;
  XMLItem: TfrxXMLItem;
  AItem: TExternalToolItem;
  I: Integer;
begin
  XMLDoc := TfrxXMLDocument.Create;
  XMLItem := XMLDoc.Root;
  try
    XMLDoc.LoadFromStream(Stream);
  except
    XMLDoc.Free;
    raise;
  end;
  Clear;
  if XMLItem.Name = Self.ClassName then
  begin
    for I := 0 to XMLItem.Count - 1 do
    begin
      if XMLItem[I].Name = TExternalToolItem.ClassName then
      begin        
        AItem := TExternalToolItem(Add);
        XMLToProps(XMLItem[I].Text, AItem);
      end;
    end;
  end;
end;

procedure TExternalTools.ManageTools;
begin
  with TfrmExtrnTools.Create(nil) do
  try
    AssignCollection(Self);
    ShowModal;
  finally
    Free;
  end;
end;

function TExternalTools.PropsToXML(AItem: TExternalToolItem): string;
var
  TypeInfo: PTypeInfo;
  PropCount: Integer;
  PropList: PPropList;
  i: Integer;
  s: String;
  Flag: Boolean;

  procedure DoOrdProp;
  var
    Value: Integer;

    function IsDefault: Boolean;
    begin
      Result := Value = PropList[i].Default;
    end;

  begin
    Value := GetOrdProp(AItem, PropList[i]);
    if not IsDefault then
      if PropList[i].PropType^.Kind = tkEnumeration then
        s := GetEnumName(PropList[i].PropType^, Value)
      else
        s := IntToStr(Value);
  end;

  procedure DoFloatProp;
  var
    Value: Extended;

    function IsDefault: Boolean;
    begin
      Result := False;
    end;

  begin
    Value := GetFloatProp(AItem, PropList[i]);
    if not IsDefault then
      s := FloatToStr(Value);
  end;

  procedure DoStrProp;
  var
    Value: String;

    function IsDefault: Boolean;
    begin
      Result := Value = '';
    end;

  begin
    Value := GetStrProp(AItem, PropList[i]);
    if not IsDefault then
      s := FrxStrToXML(Value);
  end;

  procedure DoVariantProp;
  var
    Value: Variant;

    function IsDefault: Boolean;
    begin
      Result := False;
    end;

  begin
    Value := GetVariantProp(AItem, PropList[i]);
    if not IsDefault then
      s := FrxStrToXML(VarToStr(Value));
  end;
  
begin
  Result := '';
  if AItem = nil then Exit;

  TypeInfo := AItem.ClassInfo;
  PropCount := GetTypeData(TypeInfo).PropCount;
  GetMem(PropList, PropCount * SizeOf(PPropInfo));
  GetPropInfos(TypeInfo, PropList);

  try
    for i := 0 to PropCount - 1 do
    begin
      s := '';
      Flag := False;

      if IsStoredProp(AItem, PropList[i]) then
        case PropList[i].PropType^.Kind of
          tkInteger, tkSet, tkChar, tkWChar, tkEnumeration:
            DoOrdProp;

          tkFloat:
            DoFloatProp;

          tkString, tkLString, tkWString:
            DoStrProp;

          tkVariant:
            DoVariantProp;
        end;

      if s <> '' then
        if Flag then
          Result := Result + s
        else
          Result := Result + ' ' + PropList[i].Name + '="' + s + '"';
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(PPropInfo));
  end;  
end;

procedure TExternalTools.SaveToFile(const AFileName: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

procedure TExternalTools.SaveToStream(Stream: TStream);

  procedure DoWrite(AItem: TfrxXMLItem; AToolItem: TExternalToolItem);
  begin
    AItem.Name := AToolItem.ClassName;
    AItem.Text := PropsToXML(AToolItem);
  end;
  
var
  XMLDoc: TfrxXMLDocument;
  XMLItem: TfrxXMLItem;
  I: Integer;
begin
  XMLDoc := TfrxXMLDocument.Create;
  XMLItem := XMLDoc.Root;
  XMLDoc.AutoIndent := True;
  XMLItem.Name := Self.ClassName;
  for I := 0 to Count - 1 do
    DoWrite(XMLItem.Add, TExternalToolItem(Items[I]));
  XMLDoc.SaveToStream(Stream);
end;

procedure TExternalTools.XMLToProps(const AProps: string;
  AItem: TExternalToolItem);
var
  APos, J, Code: Integer;
  AName, AValue: String;
  p: PPropInfo;
begin
  APos := 1;
  ExtractXMLProps(AProps, APos, AName, AValue);
  while AName <> EmptyStr do
  begin
    p := GetPropInfo(AItem.ClassInfo, AName);
    if (p <> nil) and (p.SetProc <> nil) then
    begin
      case p.PropType^.Kind of
        tkInteger, tkSet, tkChar, tkWChar:
          SetOrdProp(AItem, p, StrToInt(AValue));
        tkEnumeration:
          begin
            Val(AValue, J, code);
            if code = 0 then
              SetOrdProp(Aitem, p, j)
            else
              SetOrdProp(AItem, p, GetEnumValue(p.PropType^, AValue));
          end;
        tkFloat:
          SetFloatProp(AItem, p, frxStrToFloat(AValue));

        tkString, tkLString, tkWString:
          SetStrProp(AItem, p, frxXMLToStr(AValue));

        tkVariant:
          SetVariantProp(AItem, p, frxXMLToStr(AValue));
      end;
    end;
    ExtractXMLProps(AProps, APos, AName, AValue);      
  end;
end;

{ TExternalToolItem }

procedure TExternalToolItem.Assign(Source: TPersistent);
var
  AItem: TExternalToolItem;
begin
  if Source is TExternalToolItem then
  begin
    AItem := TExternalToolItem(Source);
    FProgName := AItem.ProgramName;
    FWorkingDir := AItem.WorkingDir;
    FTitle := AItem.Title;
    FParameters := AItem.Parameters;
  end else
    inherited Assign(Source);
end;

procedure TExternalToolItem.SetProgName(const Value: string);
begin
  if Value <> FProgName then
  begin
    FProgName := Value;
    FWorkingDir := ExtractFilePath(FProgName);
  end;
end;

end.
