unit NativeRegXML;

interface

uses
  Classes,
  SysUtils,
  NativeXML;

const
  (*
   * Uncomment or add the encoding you need...
   *)
  ENCODING       = 'utf-8';
  //ENCODING       = 'iso-8859-1';

  (*
   * RegXML uses this as root element for new XML files...
   *)
  DEFAULTDOCROOT = 'rootxml';

type
  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);

  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;

type
  TRegXML = class(TComponent)
  private
    FStream:                    TStream;
    FNativeXML:                 TNativeXml;              
    FActive:                    Boolean;
    FCurrKey:                   String;
    FCurrNode:                  TXmlNode;
    FDocRoot:                   String;
    FRootKey:                   String;
    FFilename:                  TFilename;

    function parseKey(key: string): TStringList;
    function findKey(ACurrNode: TXmlNode; AKeyname: string;
      CanCreate: boolean): TXmlNode;
    function findValue(ACurrNode: TXmlNode; AValueName: string): TXmlNode;
    procedure getNames(elementname: string; strings: TStrings);
    function readAttribute(const name, attribute: string): string;
    procedure writeValue(const name, value: string; datatype: TRegDataType);
    
    function DataTypeToStr(datatype: TRegDataType): string;
    function StrToDataType(const str: string): TRegDataType;

    procedure setActive(const Value: boolean);
    procedure setDocRoot(const Value: string);
    procedure setFilename(const Value: TFilename);
    procedure setRootKey(const Value: string);
  protected
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;

    function Open(filename: TFilename): boolean;
    procedure Close;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    function createKey(const key: string): boolean;
    function deleteKey(const key: string): boolean;
    function deleteValue(const name: string): boolean;
    function openKey(const key: string; CanCreate: boolean): boolean;
    procedure closeKey;
    function hasSubKeys: boolean;
    function keyExists(const key: string): boolean;
    function valueExists(const name: string): boolean;
    procedure getKeyNames(strings: TStrings);
    procedure getValueNames(strings: TStrings);
    function getDataInfo(const name: string; var value: TRegDataInfo): boolean;
    function getDataSize(const name: string): integer;
    function getDataType(const name: string): TRegDataType;
    function readBinaryData(const name: string; var buffer;
      BufSize: integer): integer;
    procedure writeBinaryData(const name: string; var buffer; BufSize: integer);
    function readInteger(const name: string): integer;
    procedure writeInteger(const name: string; value: integer);
    function readCardinal(const name: string): Cardinal;
    procedure writeCardinal(const name: string; value: Cardinal);
    function readFloat(const name: string): double;
    procedure writeFloat(const name: string; value: double);
    function readString(const name: string): string;
    procedure writeString(const name,value: string);
    procedure writeExpandString(const name,value: string);
    function readBool(const name: string): Boolean;
    procedure writeBool(const name: string; value: boolean);
    function readCurrency(const name: string): Currency;
    procedure writeCurrency(const name: string; value: Currency);
    function readDate(const name: string): TDateTime;
    procedure writeDate(const name: string; value: TDateTime);
    function readDateTime(const name: string): TDateTime;
    procedure writeDateTime(const name: string; value: TDateTime);
    function readTime(const name: string): TDateTime;
    procedure writeTime(const name: string; value: TDateTime);
    
  published
    property Active: boolean read FActive write setActive;
    property DocRoot: string read FDocRoot write setDocRoot;
    property Filename: TFilename read FFilename write setFilename;
    property RootKey: string read FRootKey write setRootKey;    
  end;

implementation


{ TRegXML }

procedure TRegXML.Close;
begin
  if (Length(FFilename)>0) then
    FNativeXML.SaveToFile(FFilename);
  self.Active := false;
end;

procedure TRegXML.closeKey;
begin
  FCurrKey := '';
  FCurrNode := nil;
end;

constructor TRegXML.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FStream := nil;
  FActive := false;
  FCurrKey := '';
  FCurrNode := nil;
  FDocRoot := DEFAULTDOCROOT;
  FRootKey := '';
  FNativeXML := TNativeXml.Create;
  FNativeXML.Utf8Encoded := True;
  FNativeXML.XmlFormat := xfReadable;
  FNativeXML.Root.Name := FDocRoot;

end;

function TRegXML.createKey(const key: string): boolean;
begin
  result := self.openKey(key, true);
end;

function TRegXML.DataTypeToStr(datatype: TRegDataType): string;
begin
  case datatype of
    rdUnknown:      result := 'REG_NONE';
    rdString:       result := 'REG_SZ';
    rdExpandString: result := 'REG_EXPAND_SZ';
    rdInteger:      result := 'REG_DWORD';
    rdBinary:       result := 'REG_BINARY';
  else
    result := 'REG_NONE';
  end;
end;

function TRegXML.deleteKey(const key: string): boolean;
var
  currelem: TXmlNode;
  i:        integer;
  keylist:  TStringList;
begin
  result := false;
  keylist := parseKey(key);
  if keylist.Count>0 then begin
    currelem := FCurrNode;
    if currelem<>nil then begin
      for i:=0 to keylist.Count-1 do begin
        currelem := findKey(currelem, keylist[i], false);
        if currelem=nil then begin
          keylist.Free;
          result := false;
          exit;
          end;
        end;
      currelem.Parent.NodeDelete(currelem.IndexInParent);
      result := true;
      end;
    end;
  keylist.Free;
end;

function TRegXML.deleteValue(const name: string): boolean;
var
  elem:  TXmlNode;
begin
  result := false;
  elem := findValue(FCurrNode,name);
  if elem<>nil then begin
    FCurrNode.NodeDelete(elem.IndexInParent);
    result := true;
    end;
end;

destructor TRegXML.Destroy;
begin
  inherited Destroy;
end;

function TRegXML.findKey(ACurrNode: TXmlNode; AKeyname: string;
      CanCreate: boolean): TXmlNode;
var
  I: integer;
  ANode: TXmlNode;
begin
  Result := nil;
  for I := 0 to ACurrNode.NodeCount - 1 do
  begin
    ANode := ACurrNode.Nodes[i];
    if (ANode.Name = 'key') and (ANode.AttributeByName['name'] = AKeyname) then
    begin
      Result := ANode;
      exit;
    end;
  end;
  if CanCreate then
  begin
    Result := ACurrNode.NodeNew('key');
    Result.WriteAttributeString('name', AKeyname);
  end;
end;

function TRegXML.findValue(ACurrNode: TXmlNode; AValueName: string): TXmlNode;
var
  I: integer;
  ANode: TXmlNode;
begin
  Result := nil;
  for I := 0 to ACurrNode.NodeCount - 1 do
  begin
    ANode := ACurrNode.Nodes[i];
    if (ANode.Name = 'value') and (ANode.AttributeByName['name'] = AValueName) then
    begin
      Result := ANode;
      exit;
    end;
  end;
end;

function TRegXML.getDataInfo(const name: string;
  var value: TRegDataInfo): boolean;
begin
  try
    value.RegData := self.getDataType(name);
    value.DataSize := self.getDataSize(name);
    result := true;
  except
    value.RegData := rdUnknown;
    value.DataSize := 0;
    result := false;
  end;
end;

function TRegXML.getDataSize(const name: string): integer;
begin
  try
    case self.getDataType(name) of
      rdUnknown:      result := Length(self.readAttribute(name,'value'));
      rdString,
      rdExpandString: result := Length(self.readAttribute(name,'value')) + 1;
      rdInteger:      result := sizeof(integer);
      rdBinary:       result := (Length(self.readAttribute(name,'value')) div 2);
    else
      result := -1;
    end;
  except
    result := -1;
  end;
end;

function TRegXML.getDataType(const name: string): TRegDataType;
begin
  result := StrToDataType(self.readAttribute(name, 'type'));
end;

procedure TRegXML.getKeyNames(strings: TStrings);
begin
  self.getNames('key', strings);
end;

procedure TRegXML.getNames(elementname: string; strings: TStrings);
var
  i: integer;
begin
  strings.Clear;
  for I := 0 to FCurrNode.NodeCount - 1 do
    if (FCurrNode.Nodes[i].Name = elementname) and
       (FCurrNode.Nodes[i].HasAttribute('name')) then
      strings.Add(FCurrNode.Nodes[i].AttributeByName['name']);  
end;

procedure TRegXML.getValueNames(strings: TStrings);
begin
  self.getNames('value', strings);
end;

function TRegXML.hasSubKeys: boolean;
var
  node: TXmlNode;
  I: integer;
begin
  result := false;
  for I := 0 to FCurrNode.NodeCount - 1 do
  begin
    node := FCurrNode.Nodes[i];
    if (node.Name = 'key') then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function TRegXML.keyExists(const key: string): boolean;
var
  currelem: TXmlNode;
  i:        integer;
  keylist:  TStringList;
begin
  result := false;
  keylist := parseKey(key);
  if keylist.Count>0 then begin
    currelem := FCurrNode;
    if currelem<>nil then begin
      for i:=0 to keylist.Count-1 do begin
        currelem := findKey(currelem, keylist[i], false);
        if currelem=nil then begin
          keylist.Free;
          result := false;
          exit;
          end;
        end;
      result := true;
      end;
    end;
  keylist.Free;
end;

procedure TRegXML.LoadFromStream(AStream: TStream);
begin
  FFilename := EmptyStr;
  FStream := AStream;
  self.Active := true;
end;

function TRegXML.Open(filename: TFilename): boolean;
begin
  FFilename := filename;
  self.Active := true;
  result := self.Active;
end;

function TRegXML.openKey(const key: string; CanCreate: boolean): boolean;
var
  keylist: TStringList;
  i:       integer;
begin
  if CompareText(FCurrKey,key)<>0 then begin
    keylist := parseKey(key);
    if (FCurrNode=nil) or (pos('\',key)=1) then begin
      if Length(FRootKey)>0 then
        FCurrNode := findKey(FNativeXML.Root, FRootKey, true)
      else
        FCurrNode := FNativeXML.Root;
      end;

    if FCurrNode<>nil then
      for i:=0 to keylist.Count-1 do begin
        FCurrNode := findKey(FCurrNode, keylist[i], CanCreate);
        if FCurrNode=nil then begin
          keylist.Free;
          result := false;
          exit;
          end;
        end;
    FCurrKey := key;
    keylist.Free;
    end;
  result := (FCurrNode<>nil);
end;

function TRegXML.parseKey(key: string): TStringList;
var
  p: integer;
  s: string;
begin
  result := TStringList.Create;
  repeat
    p := pos('\',key);
    if p>0 then begin
      s := copy(key,1,p-1);
      if Length(s)>0 then
        result.Add(s);
      key := copy(key,p+1,Length(key)-p);
      end;
  until p=0;
  if Length(key)>0 then
    result.Add(key);
end;

function TRegXML.readAttribute(const name, attribute: string): string;
var
  ANode: TXmlNode;
begin
  Result := '';
  ANode := findValue(FCurrNode, name);
  if ANode <> nil then
    Result := ANode.AttributeByName[attribute];
end;

function TRegXML.readBinaryData(const name: string; var buffer;
  BufSize: integer): integer;
var
  s: string;
  i: integer;
begin
  try
    s := self.readAttribute(name, 'value');
    for i:=0 to BufSize-1 do
      if i<Length(s)-1 then
        PByteArray(@buffer)[i] := StrToInt('$' + copy(s, i*2+1, 2));
    result := BufSize;
  except
    result := -1;
  end;
end;

function TRegXML.readBool(const name: string): Boolean;
begin
  result := (self.readAttribute(name, 'value')='1');
end;

function TRegXML.readCardinal(const name: string): Cardinal;
var
  s: string;
begin
  s := self.readAttribute(name, 'value');
  try
    result := StrToInt64(s);
  except
    result := 0;
  end;
end;

function TRegXML.readCurrency(const name: string): Currency;
begin
  try
    result := StrToCurr(self.readAttribute(name, 'value'));
  except
    result := 0.0;
  end;
end;

function TRegXML.readDate(const name: string): TDateTime;
begin
  try
    result := StrToDate(self.readAttribute(name, 'value'));
  except
    result := now;
  end;
end;

function TRegXML.readDateTime(const name: string): TDateTime;
begin
  try
    result := StrToDateTime(self.readAttribute(name, 'value'));
  except
    result := now;
  end;
end;

function TRegXML.readFloat(const name: string): double;
var
  s: string;
begin
  s := self.readAttribute(name, 'value');
  try
    result := StrToFloat(s);
  except
    result := 0.0;
  end;
end;

function TRegXML.readInteger(const name: string): integer;
var
  s: string;
begin
  s := self.readAttribute(name, 'value');
  try
    result := StrToInt(s);
  except
    result := 0;
  end;
end;

function TRegXML.readString(const name: string): string;
begin
  result := self.readAttribute(name, 'value');
end;

function TRegXML.readTime(const name: string): TDateTime;
begin
  try
    result := StrToTime(self.readAttribute(name, 'value'));
  except
    result := now;
  end;
end;

procedure TRegXML.SaveToStream(AStream: TStream);
begin
  FNativeXML.saveToStream(AStream);
  self.Active := false;
end;

procedure TRegXML.setActive(const Value: boolean);
begin
  if value<>FActive then
  begin
    if (Value) then
    begin
      if (FFilename <> EmptyStr) then
      begin
        if FileExists(FFilename) then
        begin
          FNativeXML.LoadFromFile(FFilename);
          FDocRoot := FNativeXML.Root.Name;
        end else
          FNativeXML.Root.Name := FDocRoot;
      end else
      if (FStream <> nil) then
      begin
        FNativeXML.loadFromStream(FStream);
        FDocRoot := FNativeXML.Root.Name;
      end;
      setRootKey(FRootKey);
      FActive := value;
    end
    else
    begin
    end;
  end;
end;

procedure TRegXML.setDocRoot(const Value: string);
begin
  if CompareText(value,FDocRoot)<>0 then
    FDocRoot := value;
end;

procedure TRegXML.setFilename(const Value: TFilename);
begin
  FFilename := Value;
end;

procedure TRegXML.setRootKey(const Value: string);
begin
  FRootKey := value;
  if Length(FRootKey)>0 then
    FCurrNode := findKey(FNativeXML.Root, FRootKey, true)
  else
    FCurrNode := FNativeXML.Root;
end;

function TRegXML.StrToDataType(const str: string): TRegDataType;
begin
  if CompareText(str,'REG_NONE')=0 then begin
    result := rdUnknown;
    exit;
    end;
  if CompareText(str,'REG_SZ')=0 then begin
    result := rdString;
    exit;
    end;
  if CompareText(str,'REG_EXPAND_SZ')=0 then begin
    result := rdExpandString;
    exit;
    end;
  if CompareText(str,'REG_DWORD')=0 then begin
    result := rdInteger;
    exit;
    end;
  if CompareText(str,'REG_BINARY')=0 then begin
    result := rdBinary;
    exit;
    end;
  result := rdUnknown;
end;

function TRegXML.valueExists(const name: string): boolean;
var
  node: TXmlNode;
  I: integer;
begin
  if FCurrNode<>nil then
  begin
    for I := 0 to FCurrNode.NodeCount - 1 do
    begin
      node := FCurrNode.Nodes[i];
      if (node.AttributeByName['name'] = name) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
  result := false;
end;

procedure TRegXML.writeBinaryData(const name: string; var buffer;
  BufSize: integer);
var
  s:  string;
  i:  integer;
begin
  s := '';
  for i:=0 to BufSize-1 do
    s := s + IntToHex(PByteArray(@buffer)[i], 2);
  self.writeValue(name, s, rdBinary);
end;

procedure TRegXML.writeBool(const name: string; value: boolean);
begin
  if value then
    self.writeValue(name, '1', rdInteger)
  else
    self.writeValue(name, '0', rdInteger);
end;

procedure TRegXML.writeCardinal(const name: string; value: Cardinal);
begin
  self.writeValue(name, IntToStr(value), rdInteger);
end;

procedure TRegXML.writeCurrency(const name: string; value: Currency);
begin
  self.writeValue(name, CurrToStr(value), rdString);
end;

procedure TRegXML.writeDate(const name: string; value: TDateTime);
begin
  self.writeValue(name, DateToStr(value), rdString);
end;

procedure TRegXML.writeDateTime(const name: string; value: TDateTime);
begin
  self.writeValue(name, DateTimeToStr(value), rdString);
end;

procedure TRegXML.writeExpandString(const name, value: string);
begin
  self.writeValue(name, value, rdExpandString);
end;

procedure TRegXML.writeFloat(const name: string; value: double);
begin
  self.writeValue(name, FloatToStr(value), rdString);
end;

procedure TRegXML.writeInteger(const name: string; value: integer);
begin
  self.writeValue(name, IntToStr(value), rdInteger);
end;

procedure TRegXML.writeString(const name, value: string);
begin
  self.writeValue(name, value, rdString);
end;

procedure TRegXML.writeTime(const name: string; value: TDateTime);
begin
  self.writeValue(name, TimeToStr(value), rdString);
end;

procedure TRegXML.writeValue(const name, value: string; datatype: TRegDataType);
var
  ANode: TXmlNode;
begin
  ANode := findValue(FCurrNode, name);
  if ANode <> nil then
    ANode.WriteAttributeString('value', value)
  else
  begin
    ANode := FCurrNode.NodeNew('value');
    ANode.WriteAttributeString('name', name);
    ANode.WriteAttributeString('type', DataTypeToStr(datatype));
    ANode.WriteAttributeString('value', value);
  end;
  
end;

end.
