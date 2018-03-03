{------------------------------------------------------------------------------

  Unit:          RegXML
  Version:       0.1.0
  Author:        Reinhard Kessler

  Copyright (c) 2003 by Reinhard Kessler (reinhard@ibkessler.de)

  Last modified: 01/13/2003

 ------------------------------------------------------------------------------

  This file is part of the RegXML project.

  RegXML is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  RegXML is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with RegXML; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 ------------------------------------------------------------------------------}

unit RegXML;

interface

uses
  Classes,
  SysUtils,
{$IFDEF WIN32}
  Registry,
  //msxml_impl, { Uncomment this line for MSXML... }
{$ENDIF}
  idom2,
  idom2_ext,
  libxmldom;

const
  (*
   * Here you can choose your prefered XML parser...
   *)
  DOMVENDOR      = SLIBXML;
  //DOMVENDOR      = 'MSXML2_RENTAL_MODEL';

  (*
   * Uncomment or add the encoding you need...
   *)
  ENCODING       = 'utf-8';
  //ENCODING       = 'iso-8859-1';

  (*
   * RegXML uses this as root element for new XML files...
   *)
  DEFAULTDOCROOT = 'rootxml';

{$IFDEF LINUX}
type
  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);

  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;
{$ENDIF}

type
  TRegXML = class(TComponent)
  private
    FStream:      TStream;
    FAccess:      LongWord;
    FActive:      boolean;
    FCurrKey:     string;
    FCurrElement: IDomElement;
    FCurrNode:    IDomNode;
    FDocRoot:     string;
    FDOM:         IDomImplementation;
    FDoc:         IDomDocument;
    FPersist:     IDomPersist;
    FFilename:    TFilename;
    FRootKey:     string;
    procedure setActive(value: boolean);
    procedure setDocRoot(value: string);
    procedure setRootKey(value: string);
    procedure setFilename(value: TFilename);
    function parseKey(key: string): TStringList;
    function findKey(currelement: IDomElement; keyname: string;
      CanCreate: boolean): IDomElement;
    function findValue(currelement: IDomElement; valuename: string): IDomElement;
    procedure getNames(elementname: string; strings: TStrings);
    function readAttribute(const name, attribute: string): string;
    procedure writeValue(const name, value: string; datatype: TRegDataType);
    function DataTypeToStr(datatype: TRegDataType): string;
    function StrToDataType(const str: string): TRegDataType;
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
    property Access: LongWord read FAccess write FAccess;
    property Active: boolean read FActive write setActive;
    property DocRoot: string read FDocRoot write setDocRoot;
    property Filename: TFilename read FFilename write setFilename;
    property RootKey: string read FRootKey write setRootKey;
  end;

implementation


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.setActive(value: boolean);
begin
  if value<>FActive then begin
    FPersist := nil;
    FDoc := nil;
    FActive := value;
    if (FActive) then begin
      FDoc := FDOM.createDocument('','',nil);
      (FDoc as IDomParseOptions).preserveWhiteSpace := false;
      (FDoc as IDomOutputOptions).prettyPrint := true;
      //(FDoc as IDomParseOptions).validate := True;
      FPersist := FDoc as IDomPersist;
      if (FFilename <> EmptyStr) then
      begin
        if FileExists(FFilename) then
        begin
          if not FPersist.load(FFilename) then
          begin
            self.Active := false;
            exit;
          end;
          FDocRoot := FDoc.documentElement.tagName;
        end else
        begin
          FPersist.loadxml('<?xml version="1.0" encoding="' + ENCODING + '"?><' + FDocRoot + '/>');
        end;
      end else
      if (FStream <> nil) then
      begin
        if not FPersist.loadFromStream(FStream) then
        begin
          Self.Active := False;
          exit;
        end;
        FDocRoot := FDoc.documentElement.tagName;
      end;
      setRootKey(FRootKey);
    end
    else begin
      end;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.setDocRoot(value: string);
{
var
  node:    IDomNode;
  element: IDomElement;
}
begin
  if CompareText(value,FDocRoot)<>0 then begin
    FDocRoot := value;
{
    if FDoc<>nil then begin
      element := FDoc.createElement(FDocRoot);
      node := FDoc.documentElement.firstChild;
      while node<>nil do begin
        element.appendChild(node.cloneNode(true));
        node := node.nextSibling;
        end;
      FDoc.replaceChild(element as IDomNode,FDoc.documentElement as IDomNode);
      end;
}
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.setRootKey(value: string);
begin
  FRootKey := value;
  if FDoc<>nil then begin
    if Length(FRootKey)>0 then
      FCurrElement := findKey(FDoc.documentElement, FRootKey, true)
    else
      FCurrElement := FDoc.documentElement;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.setFilename(value: TFilename);
begin
  FFilename := value;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.findKey(currelement: IDomElement; keyname: string;
  CanCreate: boolean): IDomElement;
var
  attr: IDomAttr;
  node: IDomNode;
begin
  result := nil;
  if currelement.hasChildNodes then begin
    node := currelement.firstChild;
    while node<>nil do begin
      if (node.nodeType=ELEMENT_NODE) and (node.nodeName='key') then begin
        attr := (node as IDomElement).getAttributeNode('name');
        if (attr<>nil) and (CompareText(attr.value,keyname)=0) then begin
          result := node as IDomElement;
          exit;
          end;
        end;
      node := node.nextSibling;
      end;
    end;
  //
  // Create new key
  //
  if CanCreate then begin
    result := currelement.appendChild(FDoc.createElement('key')) as IDomElement;
    result.setAttribute('name',keyname);
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.findValue(currelement: IDomElement; valuename: string): IDomElement;
var
  attr: IDomAttr;
  node: IDomNode;
begin
  result := nil;
  if currelement.hasChildNodes then begin
    node := currelement.firstChild;
    while node<>nil do begin
      if (node.nodeType=ELEMENT_NODE) and (node.nodeName='value') then begin
        attr := (node as IDomElement).getAttributeNode('name');
        if (attr<>nil) and (CompareText(attr.value,valuename)=0) then begin
          result := node as IDomElement;
          exit;
          end;
        end;
      node := node.nextSibling;
      end;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.getNames(elementname: string; strings: TStrings);
var
  attr: IDomAttr;
  node: IDomNode;
begin
  strings.Clear;
  node := FCurrElement.firstChild;
  while node<>nil do begin
    if (node.nodeType=ELEMENT_NODE) and (CompareText(elementname,node.nodeName)=0) then begin
      attr := (node as IDomElement).getAttributeNode('name');
      if attr<>nil then
        strings.Add(attr.value);
      end;
    node := node.nextSibling;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.readAttribute(const name, attribute: string): string;
var
  attr:  IDomAttr;
  elem:  IDomElement;
begin
  result := '';
  elem := findValue(FCurrElement,name);
  if elem<>nil then begin
    attr := elem.getAttributeNode(attribute);
    if attr<>nil then begin
      result := attr.value;
      exit;
      end;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeValue(const name, value: string; datatype: TRegDataType);
var
  attr:  IDomAttr;
  elem:  IDomElement;
begin
  elem := findValue(FCurrElement,name);
  if elem<>nil then begin
    attr := elem.getAttributeNode('value');
    if attr<>nil then begin
      attr.value := value;
      exit;
      end
    else begin
      elem.setAttribute('value',value);
      exit;
      end;
    end;
  //
  // Create new value
  //
  elem := FCurrElement.appendChild(FDoc.createElement('value')) as IDomElement;
  elem.setAttribute('name',name);
  elem.setAttribute('type',DataTypeToStr(datatype));
  elem.setAttribute('value',value);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

constructor TRegXML.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FActive := false;
  FCurrKey := '';
  FCurrNode := nil;
  FDocRoot := DEFAULTDOCROOT;
  FDOM := getDOM(DOMVENDOR);
  FRootKey := '';
end;


/////////////////////////////////////////////////////////////////////
//
//
//

destructor TRegXML.Destroy;
begin
  inherited Destroy;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.Open(filename: TFilename): boolean;
begin
  FFilename := filename;
  self.Active := true;
  result := self.Active;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.Close;
begin
  if (FPersist<>nil) then begin
    if (Length(FFilename)>0) then
      FPersist.save(FFilename);
    self.Active := false;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.createKey(const key: string): boolean;
begin
  result := self.openKey(key, true);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.deleteKey(const key: string): boolean;
var
  currelem: IDomElement;
  i:        integer;
  keylist:  TStringList;
begin
  result := false;
  keylist := parseKey(key);
  if keylist.Count>0 then begin
    currelem := FCurrElement;
    if currelem<>nil then begin
      for i:=0 to keylist.Count-1 do begin
        currelem := findKey(currelem, keylist[i], false);
        if currelem=nil then begin
          keylist.Free;
          result := false;
          exit;
          end;
        end;
      (currelem.parentNode as IDomElement).removeChild(currelem as IDomNode);
      result := true;
      end;
    end;
  keylist.Free;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.deleteValue(const name: string): boolean;
var
  elem:  IDomElement;
begin
  result := false;
  elem := findValue(FCurrElement,name);
  if elem<>nil then begin
    FCurrElement.removeChild(elem as IDomNode);
    result := true;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.openKey(const key: string; CanCreate: boolean): boolean;
var
  keylist: TStringList;
  i:       integer;
begin
  if CompareText(FCurrKey,key)<>0 then begin
    keylist := parseKey(key);
    if (FCurrElement=nil) or (pos('\',key)=1) then begin
      if Length(FRootKey)>0 then
        FCurrElement := findKey(FDoc.documentElement, FRootKey, true)
      else
        FCurrElement := FDoc.documentElement;
      end;

    if FCurrElement<>nil then
      for i:=0 to keylist.Count-1 do begin
        FCurrElement := findKey(FCurrElement, keylist[i], CanCreate);
        if FCurrElement=nil then begin
          keylist.Free;
          result := false;
          exit;
          end;
        end;
    FCurrKey := key;
    keylist.Free;
    end;
  result := (FCurrElement<>nil);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.closeKey;
begin
  FCurrKey := '';
  FCurrElement := nil;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.hasSubKeys: boolean;
var
  node: IDomNode;
begin
  result := false;
  node := FCurrNode.firstChild;
  while node<>nil do begin
    if (node.nodeType=ELEMENT_NODE) and (CompareText('key',node.nodeName)=0) then begin
      result := true;
      exit;
      end;
    node := node.nextSibling;
    end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.keyExists(const key: string): boolean;
var
  currelem: IDomElement;
  i:        integer;
  keylist:  TStringList;
begin
  result := false;
  keylist := parseKey(key);
  if keylist.Count>0 then begin
    currelem := FCurrElement;
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


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.valueExists(const name: string): boolean;
var
  attr: IDomAttr;
  node: IDomNode;
begin
  if FCurrElement<>nil then begin
    node := FCurrElement.firstChild;
    while node<>nil do begin
      attr := (node as IDomElement).getAttributeNode('name');
      if (attr<>nil) and (CompareText(name,attr.nodeValue)=0) then begin
        result := true;
        exit;
        end;
      node := node.nextSibling;
      end;
    end;
  result := false;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.getKeyNames(strings: TStrings);
begin
  self.getNames('key', strings);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.getValueNames(strings: TStrings);
begin
  self.getNames('value', strings);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.getDataType(const name: string): TRegDataType;
begin
  result := StrToDataType(self.readAttribute(name, 'type'));
end;


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeInteger(const name: string; value: integer);
begin
  self.writeValue(name, IntToStr(value), rdInteger);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

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


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeFloat(const name: string; value: double);
begin
  self.writeValue(name, FloatToStr(value), rdString);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.readString(const name: string): string;
begin
  result := self.readAttribute(name, 'value');
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeString(const name,value: string);
begin
  self.writeValue(name, value, rdString);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeExpandString(const name,value: string);
begin
  self.writeValue(name, value, rdExpandString);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.readBool(const name: string): Boolean;
begin
  result := (self.readAttribute(name, 'value')='1');
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeBool(const name: string; value: boolean);
begin
  if value then
    self.writeValue(name, '1', rdInteger)
  else
    self.writeValue(name, '0', rdInteger);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.readCurrency(const name: string): Currency;
begin
  try
    result := StrToCurr(self.readAttribute(name, 'value'));
  except
    result := 0.0;
  end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeCurrency(const name: string; value: Currency);
begin
  self.writeValue(name, CurrToStr(value), rdString);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.readDate(const name: string): TDateTime;
begin
  try
    result := StrToDate(self.readAttribute(name, 'value'));
  except
    result := now;
  end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeDate(const name: string; value: TDateTime);
begin
  self.writeValue(name, DateToStr(value), rdString);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.readDateTime(const name: string): TDateTime;
begin
  try
    result := StrToDateTime(self.readAttribute(name, 'value'));
  except
    result := now;
  end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeDateTime(const name: string; value: TDateTime);
begin
  self.writeValue(name, DateTimeToStr(value), rdString);
end;


/////////////////////////////////////////////////////////////////////
//
//
//

function TRegXML.readTime(const name: string): TDateTime;
begin
  try
    result := StrToTime(self.readAttribute(name, 'value'));
  except
    result := now;
  end;
end;


/////////////////////////////////////////////////////////////////////
//
//
//

procedure TRegXML.writeTime(const name: string; value: TDateTime);
begin
  self.writeValue(name, TimeToStr(value), rdString);
end;


/////////////////////////////////////////////////////////////////////


procedure TRegXML.LoadFromStream(AStream: TStream);
begin
  FFilename := EmptyStr;
  FStream := AStream;
  self.Active := true;
end;

procedure TRegXML.SaveToStream(AStream: TStream);
begin
  if (FPersist<>nil) then
  begin
    FPersist.saveToStream(AStream);
    self.Active := false;
  end;
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

procedure TRegXML.writeCardinal(const name: string; value: Cardinal);
begin
  self.writeValue(name, IntToStr(value), rdInteger);
end;

end.
