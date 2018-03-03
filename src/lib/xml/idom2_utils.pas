unit idom2_utils;

interface

uses
  idom2,
  idom2_ext;

type
  TDomCharEncoding = (ceUnknown, ceUTF8, ceUTF16LE, ceUTF16BE, ceISO88591);

const
  cDomCharEncodingIdentifier: array [TDomCharEncoding] of string =
   ( '', 'utf-8', 'utf-16', 'utf-16', 'iso-8859-1' );

function  idom_findElement(ANode: IDomNode; sName : DOMString): IDomElement;
function  idom_nodeList_get_xml(ANodeList: IDomNodeList): DOMstring;
procedure idom_optimize_EmptyTextNodes(ADOM: IDomDocument);

implementation

function idom_findElement(ANode: IDomNode; sName : DOMString): IDomElement;
begin
  result := nil;
  // cycle throug tree
  while Assigned(ANode) do begin

    if (ANode.NodeType = ELEMENT_NODE) and ((ANode as IDomElement).nodeName = sName)
       then begin
         result := (ANode as IDomElement);
         Exit;
       end;

    if ANode.HasChildNodes
       then begin
         result := idom_findElement(ANode.FirstChild, sName);
         if Assigned(result) then Exit;
       end;

    ANode := ANode.NextSibling;
  end;
end;

function idom_nodeList_get_xml(ANodeList: IDomNodeList): DOMstring;
var i: integer;
begin
  result := '';

  // build xmls
  for i := 0 to ANodeList.length - 1 do
    // concat their xml
    result := result + (ANodeList.item[i] as IDomNodeExt).xml;

end;

procedure idom_optimize_EmptyTextNodes(ADOM: IDomDocument);
var i: integer; iNodes: IDomNodeList;
begin
  // check for something to do
  if (not Assigned(ADOM)) or (not Assigned(ADOM.documentElement)) then exit;

  // search empty textnodes
  iNodes := (ADOM.documentElement as IDomNodeSelect).SelectNodes('//text()["" = normalize-space(.)]');
  try
    // remove them
    for i := 0 to iNodes.length - 1 do
        iNodes.item[i].parentNode.removeChild(iNodes.Item[i]);

  finally
    iNodes := nil;
  end;

end;

end.
