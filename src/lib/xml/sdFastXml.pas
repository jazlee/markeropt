{ unit sdFastXml

  This is a small-footprint implementation to read and write XML documents
  natively from Delpi code. sdFastXml has very fast parsing speeds.

  You can use this code to read XML documents from files, streams or strings.
  The load routine generates events that can be used to display load progress
  on the fly.

  Note: any external encoding (ANSI, UTF16, etc) is converted to an internal
  encoding that is UTF8. All "string" types used represent UTF8 strings.

  Author: Nils Haeck M.Sc.
  Copyright (c) 2007 Simdesign B.V.

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit sdFastXml;

// Uncomment to save memory space for large documents if you don't need tags.
// Tags are an additional integer field that can be used by the application.
{$DEFINE USETAGS}

interface

uses
  Classes, Contnrs, SysUtils, sdStringTable;

type

  // Note on TsdFastXml.OutputFormatting:
  // - xfReadable to be able to read the xml file with a standard editor.
  // - xfCompact (default) to save the xml fully compliant and at smallest size
  TsdFormattingType = (
    xfCompact,  // Save without any control chars except LF after declarations
    xfReadable  // Save in readable format with CR-LF and indents
  );

  // TsdElementType enumerates the different kinds of elements that can be found
  // in the XML document.
  TsdElementType = (
    xeElement,     // Normal element <name {attr}>[value][sub-elements]</name>
    xeAttribute,   // Attribute ( name='value' or name="value")
    xeComment,     // Comment <!--{comment}-->
    xeCData,       // literal data <![CDATA[{data}]]>
    xeDeclaration, // XML declaration <?xml{declaration}?>
    xeStylesheet,  // Stylesheet <?xml-stylesheet{stylesheet}?>
    xeDocType,     // DOCTYPE DTD declaration <!DOCTYPE{spec}>
    xeDtdElement,  // <!ELEMENT >
    xeDtdAttList,  // <!ATTLIST >
    xeDtdEntity,   // <!ENTITY >
    xeDtdNotation, // <!NOTATION >
    xeInstruction, // <?...?> processing instruction
    xeCharData,    // Character data in a node
    xeQuotedText,  // "bla" or 'bla'
    xeUnknown,     // Any <data>
    xeEndTag,      // </...>
    xeError
  );

  TsdElementTypes = set of TsdElementType;

  // Definition of different methods of string encoding.
  TsdStringEncoding = (
    se8Bit,      // General 8 bit encoding, encoding must be determined from encoding declaration
    seUCS4BE,    // UCS-4 Big Endian
    seUCS4LE,    // UCS-4 Little Endian
    seUCS4_2143, // UCS-4 unusual octet order (2143)
    seUCS4_3412, // UCS-4 unusual octet order (3412)
    seUTF8,      // UTF-8
    seUTF16BE,   // UTF-16 Big Endian
    seUTF16LE,   // UTF-16 Little Endian
    seEBCDIC     // EBCDIC flavour
  );

  TsdBufferReader = class;
  TsdFastXml = class;
  TsdAttribute = class;

  // TXmlNode is the ancestor for all nodes in the xml document. See TsdElement
  // for the elements, TsdAttribute for the attributes.
  TXmlNode = class(TPersistent)
  private
    FOwner: TsdFastXml;
    FParent: TXmlNode;
    {$IFDEF USETAGS}
    FTag: integer;
    {$ENDIF}
    function GetAttributeByName(const AName: string): TsdAttribute;
    function GetAttributeValueByName(const AName: string): string;
    procedure SetAttributeValueByName(const AName, Value: string);
    function GetValueWide: widestring;
    procedure SetValueWide(const Value: widestring);
    function GetAttributes(Index: integer): TsdAttribute;
    function GetAttributeName(Index: integer): string;
    function GetAttributeValue(Index: integer): string;
    procedure SetAttributeName(Index: integer; const Value: string);
    procedure SetAttributeValue(Index: integer; const Value: string);
    function GetAttributeValueAsInteger(Index: integer): integer;
    procedure SetAttributeValueAsInteger(Index: integer; const Value: integer);
  protected
    // string table lookup methods
    function TableGetString(AID: integer): string;
    procedure TableSetString(var AID: integer; const S: string);
    function GetName: string; virtual;
    function GetValue: string; virtual;
    procedure SetName(const Value: string); virtual;
    procedure SetValue(const Value: string); virtual;
    function GetNodes(Index: integer): TXmlNode; virtual;
    class function EscapeString(const S: string): string;
    class function UnescapeString(const S: string): string;
    procedure ParseStream(R: TsdBufferReader); virtual;
  public
    // Create a new node object. AOwner must be the TsdFastXml that is
    // going to hold this new node. Make sure to use the correct class when
    // creating, e.g. TsdElement.Create(Owner) for an element.
    constructor Create(AOwner: TsdFastXml); virtual;
    // Convert the UTF8 string S to a widestring
    class function ToWide(const S: string): widestring;
    // Convert the widestring W to an UTF8 string
    class function FromWide(const W: widestring): string;
    // The element type of this node.
    function ElementType: TsdElementType; virtual;
    {$IFDEF USETAGS}
    // Tag is an integer value the developer can use in any way. Tag does not get
    // saved to the XML. Tag is often used to point to a GUI element (and is then
    // cast to a pointer).
    property Tag: integer read FTag write FTag;
    {$ENDIF}
    // Parent points to the parent node of the current XML node.
    property Parent: TXmlNode read FParent;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node Name and value are empty.
    function IsClear: boolean;
    // This function returns True if the node has no subnodes and no attributes,
    // and if the node value is empty.
    function IsEmpty: boolean;
    // Delete this node. It will be removed from its parent list and removed
    // from memory completely.
    procedure Delete;
    // Get the number of attributes in this node
    function AttributeCount: integer; virtual;
    // Use this method to add an attribute with name AName and string value AValue
    // to the node. AName and AValue must be UTF8 encoded.
    procedure AttributeAdd(const AName, AValue: string);
    // Use this method to delete the attribute at Index in the list. Index must be
    // equal or greater than 0, and smaller than AttributeCount. Using an index
    // outside of that range has no effect.
    procedure AttributeDelete(Index: integer);
    // Number of subnodes present in this node (this includes attributes,
    // cdata, char-data, sub-elements, etcetera).
    function NodeCount: integer; virtual;
    // Add the node ANode to the nodelist. It will be added at the end, unless
    // it is an attribute, in that case it will be added at the end of the current
    // list of attributes. NodeAdd will set the parent of ANode to itself.
    function NodeAdd(ANode: TXmlNode): integer; virtual;
    // Return a reference to the first subnode in the nodelist that has name AName.
    // If no subnodes with AName are found, the function returns nil.
    function NodeByName(const AName: string): TXmlNode;
    // \Delete the subnode at Index. The node will also be freed, so do not free the
    // node in the application.
    procedure NodeDelete(Index: integer); virtual;
    // Call NodeIndexOf to get the index for ANode in the Nodes list. The first
    // node in the list has index 0, the second item has index 1, and so on. If
    // a node is not in the list, NodeIndexOf returns -1.
    function NodeIndexOf(ANode: TXmlNode): integer; virtual;
    // Insert the node ANode at location Index in the list. Make sure to honour
    // the fact that attributes are also nodes, and should always be first in
    // the list. You can find the number of attributes with AttributeCount.
    procedure NodeInsert(Index: integer; ANode: TXmlNode); virtual;
    // Switch position of the nodes at Index1 and Index2.
    procedure NodeExchange(Index1, Index2: integer); virtual;
    // Return the first subnode with AType, or nil if none
    function FirstNodeByType(AType: TsdElementType): TXmlNode; virtual;
    // Read TreeDepth to find out many nested levels there are for the current XML
    // node. Root has a TreeDepth of zero.
    function TreeDepth: integer;
    // The name of the node. For elements this is the element name. The string
    // is encoded as UTF8.
    property Name: string read GetName write SetName;
    // The value of the node. For elements this is the element value (based on
    // first chardata fragment), for attributes this is the attribute value. The
    // string is encoded as UTF8. Use ToWide(Node.Value) or Node.ValueWide
    // to get a widestring compatible with "wide" windows methods.
    property Value: string read GetValue write SetValue;
    // ValueWide returns the value of the node as a widestring.
    property ValueWide: widestring read GetValueWide write SetValueWide;
    // List of attributes present in this element. Use AttributeCount to iterate.
    property Attributes[Index: integer]: TsdAttribute read GetAttributes;
    // Get or set the name of the attribute at Index (as UTF8).
    property AttributeName[Index: integer]: string read GetAttributeName write SetAttributeName;
    // Get or set the value of the attribute at Index (as UTF8).
    property AttributeValue[Index: integer]: string read GetAttributeValue write SetAttributeValue;
    // Read this property to get the integer value of the attribute at index Index.
    // If the value cannot be converted, 0 will be returned. Write to it to set the
    // integer value.
    property AttributeValueAsInteger[Index: integer]: integer read GetAttributeValueAsInteger write SetAttributeValueAsInteger;
    // Get a reference to an attribute node by its name. If there is no attribute
    // with that name, nil will be returned.
    property AttributeByName[const AName: string]: TsdAttribute read GetAttributeByName;
    // Get the value of an attribute with name AName. If no attribute is present,
    // an empty string is returned. When setting this value, an attribute is
    // created if it does not yet exist.
    property AttributeValueByName[const AName: string]: string read
      GetAttributeValueByName write SetAttributeValueByName;
    // List of subnodes, by index. Iterate through the list using NodeCount
    // and this property. The attributes are listed first, then followed by
    // all other node types, in the order as found in the XML document.
    property Nodes[Index: integer]: TXmlNode read GetNodes; default;
  end;

  // TXmlNode metaclass
  TsdNodeClass = class of TXmlNode;

  // List of nodes
  TsdNodeList = class(TObjectList)
  private
    function GetItems(Index: integer): TXmlNode;
  public
    // ByType returns the first item in the list that has element type AType.
    // If no item is found, the function returns nil.
    function ByType(AType: TsdElementType): TXmlNode;
    property Items[Index: integer]: TXmlNode read GetItems; default;
  end;

  // Node representing a xml char-data fragment
  TsdCharData = class(TXmlNode)
  private
    FValueID: integer;
  protected
    function MustEscape: boolean; virtual;
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  public
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing quoted text ('bla' or "bla")
  TsdQuotedText = class(TsdCharData)
  private
    FQuoteChar: char;
  protected
    function GetName: string; override;
    function MustEscape: boolean; override;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // Node representing an xml attribute.
  TsdAttribute = class(TsdQuotedText)
  private
    FNameID: integer;
  protected
    function MustEscape: boolean; override;
    function GetName: string; override;
    procedure SetName(const Value: string); override;
  protected
    procedure ParseStream(R: TsdBufferReader); override;
  public
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
  end;

  // TsdContainerNode is the base class for all element types that can have
  // sub-nodes.
  TsdContainerNode = class(TXmlNode)
  private
    FNodes: TsdNodeList;
  protected
    function ParseAttributeList(R: TsdBufferReader): char; virtual;
    function ParseQuotedTextList(R: TsdBufferReader): char; virtual;
    function GetNodes(Index: integer): TXmlNode; override;
    property NodeList: TsdNodeList read FNodes;
  public
    constructor Create(AOwner: TsdFastXml); override;
    function AttributeCount: integer; override;
    function NodeCount: integer; override;
    function NodeAdd(ANode: TXmlNode): integer; override;
    procedure NodeDelete(Index: integer); override;
    function NodeIndexOf(ANode: TXmlNode): integer; override;
    procedure NodeInsert(Index: integer; ANode: TXmlNode); override;
    procedure NodeExchange(Index1, Index2: integer); override;
    function FirstNodeByType(AType: TsdElementType): TXmlNode; override;
    destructor Destroy; override;
  end;

  // Node representing an xml element.
  TsdElement = class(TsdContainerNode)
  private
    FNameID: integer;
  protected
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetName(const Value: string); override;
    procedure SetValue(const Value: string); override;
    procedure ParseIntermediateData(R: TsdBufferReader); virtual;
    procedure ParseElementList(R: TsdBufferReader; const SupportedTags: TsdElementTypes); virtual;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
  end;

  // Node representing an xml declaration, e.g. <?xml version="1.0"?>
  TsdDeclaration = class(TsdContainerNode)
  private
    function GetEncoding: string;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetVersion(const Value: string);
  protected
    function GetName: string; override;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    function ElementType: TsdElementType; override;
    property Version: string read GetVersion write SetVersion;
    property Encoding: string read GetEncoding write SetEncoding;
  end;

  // Node representing an xml comment. Get/set Value for the comment.
  TsdComment = class(TsdCharData)
  protected
    function GetName: string; override;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // Node representing a CData element. Get/Set value for the data in CDATA.
  TsdCData = class(TsdComment)
  protected
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // DocType declaration element. It can have sub-nodes with dtd elements,
  // entities, notations, etc.
  TsdDocType = class(TsdElement)
  private
    FExternalID: string;
    FSystemLiteral: TsdQuotedText;
    FPubIDLiteral: TsdQuotedText;
  protected
    procedure ParseIntermediateData(R: TsdBufferReader); override;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    constructor Create(AOwner: TsdFastXml); override;
    destructor Destroy; override;
    function ElementType: TsdElementType; override;
    // External ID: either SYSTEM or PUBLIC
    property ExternalID: string read FExternalID write FExternalID;
    // The system literal without quotes
    property SystemLiteral:  TsdQuotedText read FSystemLiteral;
    // The PubID literal without quotes
    property PubIDLiteral: TsdQuotedText read FPubIDLiteral;
  end;

  // DTD Element declaration
  TsdDtdElement = class(TsdElement)
  protected
    procedure ParseStream(R: TsdBufferReader); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD AttList declaration
  TsdDtdAttList = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Entity declaration
  TsdDtdEntity = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  // DTD Notation declaration
  TsdDtdNotation = class(TsdDtdElement)
  public
    function ElementType: TsdElementType; override;
  end;

  TsdInstruction = class(TsdCharData)
  protected
    function GetName: string; override;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    function ElementType: TsdElementType; override;
  end;

  TsdStyleSheet = class(TsdInstruction)
  protected
    function GetName: string; override;
    procedure ParseStream(R: TsdBufferReader); override;
  public
    function ElementType: TsdElementType; override;
  end;

  // todo
  TsdExclam = class(TXmlNode);

  TsdXmlNodeEvent = procedure (Sender: TObject; ANode: TXmlNode) of object;

  // TsdFastXml is a very fast XML reader (15 Mb per second). Use Create to
  // create a new instance, use LoadFromFile/LoadFromStream to load the XML
  // document from a file or stream.
  TsdFastXml = class(TComponent)
  private
    FExternalEncoding: TsdStringEncoding;
    FRootNodes: TsdNodeList;
    FPreserveWhiteSpace: boolean;
    FOnNodeNew: TsdXmlNodeEvent;
    FOnNodeLoaded: TsdXmlNodeEvent;
    FReader: TsdBufferReader;
    FOutputFormatting: TsdFormattingType;
    FAbortParsing: boolean;
    procedure DoNodeNew(ANode: TXmlNode);
    procedure DoNodeLoaded(ANode: TXmlNode);
    function GetReaderPosition: int64;
    function GetCommentString: string;
    procedure SetCommentString(const Value: string);
    function GetStyleSheet: TsdStyleSheet;
    function GetEncodingString: string;
    procedure SetEncodingString(const Value: string);
    function GetRoot: TsdElement;
    function GetVersionString: string;
    procedure SetVersionString(const Value: string);
    function GetReaderLineNumber: int64;
  protected
    procedure ParseStream(R: TsdBufferReader);
  public
    FStrings: TsdStringTable;
    // Create a new TsdFastXML component.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Clear all the nodes in the xml document
    procedure Clear;
    // Function IsEmpty returns true if the root is clear, or in other words, the
    // root contains no value, no name, no subnodes and no attributes.
    function IsEmpty: boolean;
    // Call procedure LoadFromFile to load an XML document from the filename
    // specified. See Create for an example. The LoadFromFile procedure will raise
    // an exception when it encounters non-wellformed XML.
    procedure LoadFromFile(const AFileName: string);
    // Load an XML document from the TStream object in AStream. The LoadFromStream
    // procedure will raise an exception when it encounters non-wellformed XML.
    // This method can be used with any TStream descendant. The stream is read
    // from chunk-wise (using 64K chunks). See also LoadFromFile and ReadFromString.
    procedure LoadFromStream(AStream: TStream);
    // Call SaveToFile to save the XML document to a file with FileName. If the
    // filename exists, it will be overwritten without warning. If the file cannot
    // be created, a standard I/O exception will be generated. Set XmlFormat to
    // xfReadable if you want the file to contain indentations to make the XML
    // more human-readable. This is not the default and also not compliant with
    // the XML specification.<p>
    // Saving to special encoding types can be achieved by setting two properties
    // before saving:
    // * ExternalEncoding
    // * EncodingString
    // ExternalEncoding can be se8bit (for plain ascii), seUtf8 (UTF-8), seUtf16LE
    // (for unicode) or seUtf16BE (unicode big endian).<p> Do not forget to also
    // set the EncodingString (e.g. "UTF-8" or "UTF-16") which matches with your
    // ExternalEncoding.
    procedure SaveToFile(const AFileName: string);
    // Call SaveToString to save the XML document to an UTF8 string. No BOM marker
    // will be written; use SaveToStream or SaveToFile if you want BOM markers.
    function SaveToString: string;
    // Root is the topmost element in the XML document. Access Root to read any
    // child elements. When creating a new XML document, you can automatically
    // include a Root element, by creating using CreateName.
    property Root: TsdElement read GetRoot;
    // RootNodes can be used to directly access the nodes in the root of the
    // XML document. Usually this list consists of one declaration node followed
    // by an element node which is the Root. You can use this property to add or
    // delete comments, stylesheets, dtd's etc.
    property RootNodes: TsdNodeList read FRootNodes;
    // A comment string above the root element \<!--{comment}--\> can be accessed with
    // this property. \Assign a comment to this property to add it to the XML document.
    // Use property RootNodeList to add/insert/extract multiple comments.
    property CommentString: string read GetCommentString write SetCommentString;
    // After reading, this property contains the XML version (usually "1.0").
    property VersionString: string read GetVersionString write SetVersionString;
    // Encoding string (e.g. "UTF-8" or "UTF-16"). This encoding string is stored in
    // the header.
    // Example: In order to get this header:
    // <?xml version="1.0" encoding="UTF-16" ?>
    // enter this code:
    // <CODE>MyXmlDocument.EncodingString := 'UTF-16';</CODE>
    // When reading a file, EncodingString will contain the encoding used.
    property EncodingString: string read GetEncodingString write SetEncodingString;
    // Get the stylesheet used for this XML document. If the node does not
    // exist yet, it will be created (thus if you use this property, and don't
    // set any of the attributes, an empty stylesheet node will be the result).
    property StyleSheet: TsdStyleSheet read GetStyleSheet;
    // External encoding is valid after loading, and indicates the encoding
    // detected in the file/stream. Internally, all string values are always
    // encoded in UTF8, so if the external stream is Ascii or UTF16, a conversion
    // is done. When writing to a file/stream, a BOM is generated for the encoding
    // and a conversion is done from UTF8 to this encoding if necessary.
    property ExternalEncoding: TsdStringEncoding read FExternalEncoding write FExternalEncoding;
    property OutputFormatting: TsdFormattingType read FOutputFormatting write FOutputFormatting;
    // Set PreserveWhiteSpace to True to preserve all whitespace present in the
    // file when reading. The blocks of whitespace are stored as CharData nodes.
    property PreserveWhiteSpace: boolean read FPreserveWhiteSpace write FPreserveWhiteSpace;
    // ReaderPosition gives the reader's current position in the stream when
    // loading.
    property ReaderPosition: int64 read GetReaderPosition;
    // ReaderLineNumber gives the readers current line number in the steram
    // when loading.
    property ReaderLineNumber: int64 read GetReaderLineNumber;
    // Set AbortParsing to True if you use the OnNodeNew and OnNodeLoaded events in
    // a SAX-like manner, and you want to abort the parsing process halfway. Example:
    // <code>
    // procedure MyForm.NativeXmlNodeLoaded(Sender: TObject; Node: TXmlNode);
    // begin
    //   if (Node.Name = 'LastNode') and (Sender is TNativeXml) then
    //     TNativeXml(Sender).AbortParsing := True;
    // end;
    // </code>
    property AbortParsing: boolean read FAbortParsing write FAbortParsing;
    // Connect to OnNodeNew to get informed of new nodes being added while loading.
    property OnNodeNew: TsdXmlNodeEvent read FOnNodeNew write FOnNodeNew;
    // Connect to OnNodeLoaded to get informed of nodes being finished loading.
    property OnNodeLoaded: TsdXmlNodeEvent read FOnNodeLoaded write FOnNodeLoaded;
  end;

  // Special reader class to read XML content. It buffers the source stream into
  // a memory buffer of limited size (64K) and reads from the stream chunk-wise.
  // This way, it can do string comparisons in memory, directly on the buffer.
  TsdBufferReader = class(TPersistent)
  private
    FSource: TStream;
    FBuffer: array of byte;
    FEncoding: TsdStringEncoding;
    FHasBom: boolean;
    FCurr: Pbyte;
    FLast: Pbyte;
    FClose: integer;
    FCapacity: integer;
    FConvertAscii: boolean;
    FBasePosition: int64;
    FBaseLineNumber: int64;
    FUtf16Content: array of byte;
    FEndOfStream: boolean;
    procedure ReadChunk;
    procedure SetCapacity(ACapacity: integer);
    procedure SetConvertAscii(const Value: boolean);
    function GetPosition: int64;
    function GetLineNumber: int64;
  public
    constructor Create(ASource: TStream);
    // Clear data used in the reader, after it has been used.
    procedure ClearData;
    // Read BOM (begin of file marker) from the file to detect which encoding
    // is used.
    procedure ReadBOM;
    // Call flush once in a while, to check if data can be flushed out. Flushing
    // means that the part before the current pointer is removed and the bytes
    // following are moved to 0 position. It is only actually done when current
    // pointer is more than halfway the chunk size.
    procedure Flush(Force: boolean = False);
    // Make at least ACount bytes of data available from Current position.
    // Raises an exception if not successful.
    function MakeDataAvailable(ACount: integer): integer;
    // Check if at least ACount bytes of data are available. If available, the
    // data is made available. If not, the available count is returned.
    function HasDataAvailable(ACount: integer): integer;
    // Read the next character, skip any blanks inbetween. Blanks are
    // #9 (tab), #10 (lf), #13 (cr) and #32 (space)
    function NextCharSkipBlanks: char;
    // Get the next character from the stream
    function NextChar: char;
    // Skip over any blank characters
    procedure SkipBlanks;
    // Read an new tag from the stream (from the position afer "<")
    function ReadOpenTag: TsdElementType;
    // Check if the stream at this position contains string S. If so, the stream
    // will be positioned after, if not, it will remain where it is.
    function CheckString(const S: string): boolean;
    // Move one position back in the stream
    procedure MoveBack;
    // Read a string from the stream until ATerm is found. The string returned
    // will be the part before ATerm, the stream is positioned after ATerm
    function ReadStringUntil(const ATerm: string): string;
    // Read a quoted string from the stream, return the unquoted string
    function ReadQuotedString(AQuote: char): string;
    // Read a string from the stream until a blank char, or a "/" or a ">" is
    // encountered.
    function ReadStringUntilBlankOrEndTag: string;
    // Read a string from the stream until character AChar is encountered. If
    // AllowEOF, the function returns gracefully if AChar is not found, but the
    // end of the stream instead.
    function ReadStringUntilChar(AChar: char; AllowEOF: boolean): string;
    // Set ConvertAscii to True if the stream read is an ASCII stream. Usually
    // ConvertAscii is only set to true after reading the xml declaration. If the
    // declaration doesn't state encoding='UTF-8', the stream is indeed ASCII and
    // ConvertAscii must be set to True.
    property ConvertAscii: boolean read FConvertAscii write SetConvertAscii;
    // The encoding detected in the source stream (valid after ReadBOM). Sometimes
    // the encoding is not detected correctly for UTF8, see remark with ConvertAscii.
    property Encoding: TsdStringEncoding read FEncoding write FEncoding;
    // Position in the stream in bytes from the start.
    property Position: int64 read GetPosition;
    // Line number in the stream. Lines are detected by analysing the stream
    // for occurances of #13 (CR). The line number is *calculated* when this
    // property is read, so it should not be read very regularly.
    property LineNumber: int64 read GetLineNumber;
    // Is the end of the stream detected?
    property EndOfStream: boolean read FEndOfStream;
  end;

var

  // XML Defaults

  cDefaultEncodingString:          string              = 'UTF-8';
  cDefaultExternalEncoding:        TsdStringEncoding   = seUTF8;
  cDefaultVersionString:           string              = '1.0';
  cDefaultXmlFormat:               TsdFormattingType   = xfCompact;
  cDefaultWriteOnDefault:          boolean             = True;
  cDefaultIndentString:            string              = '  ';
  cDefaultDropCommentsOnParse:     boolean             = False;
  cDefaultUseFullNodes:            boolean             = False;
  cDefaultSortAttributes:          boolean             = False;
  cDefaultFloatAllowScientific:    boolean             = True;
  cDefaultFloatSignificantDigits:  integer             = 6;

const

  cElementTypeNames: array[TsdElementType] of string =
    ('Element', 'Attribute', 'Comment', 'CData', 'Declaration', 'Stylesheet',
     'DocType', 'DtdElement', 'DtdAttList', 'DtdEntity', 'DtdNotation',
     'Question', 'CharData', 'QuotedText', 'Unknown', 'EndTag', 'Error');


resourcestring

  sPrematureEnd        = 'stream terminated prematurely';
  sInvalidStream       = 'invalid stream';
  sUnsupportedEncoding = 'Unsupported encoding in stream';
  sNotSupported        = 'Feature is not supported yet';
  sIllegalTag          = 'Illegal tag';
  sUnsupportedTag      = 'Unsupported tag';
  sIllegalEndTag       = 'Illegal end tag';
  sQuoteCharExpected   = 'Quote char expected';
  sCannotAddNode       = 'Cannot add node to this type of element';
  sCannotSetName       = 'Cannot set name on this type of element';
  sCannotSetValue      = 'Cannot set value on this type of element';
  sCannotManipulate    = 'Cannot manipulate nodes in this type of element';
  sBeginEndMismatch    = 'Begin and end tag mismatch';

{ Utility functions }

// Convert unicode widestring to UTF8 string
function sdUnicodeToUtf8(const W: widestring): string;

// Convert UTF8 string to unicode widestring
function sdUtf8ToUnicode(const S: string): widestring;

// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
function sdUnicodeToUtf8Mem(Src: Pword; Dst: Pbyte; Count: integer): integer;

function UnEscapeStringUTF8(const AValue: string): string;
function EscapeStringUTF8(const AValue: string): string;

implementation

type

  TBomInfo = packed record
    BOM: array[0..3] of byte;
    Len: integer;
    Enc: TsdStringEncoding;
    HasBOM: boolean;
  end;

const

  cBomInfoCount = 15;
  cBomInfo: array[0..cBomInfoCount - 1] of TBomInfo =
  ( (BOM: ($00,$00,$FE,$FF); Len: 4; Enc: seUCS4BE;    HasBOM: true),
    (BOM: ($FF,$FE,$00,$00); Len: 4; Enc: seUCS4LE;    HasBOM: true),
    (BOM: ($00,$00,$FF,$FE); Len: 4; Enc: seUCS4_2143; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 4; Enc: seUCS4_3412; HasBOM: true),
    (BOM: ($FE,$FF,$00,$00); Len: 2; Enc: seUTF16BE;   HasBOM: true),
    (BOM: ($FF,$FE,$00,$00); Len: 2; Enc: seUTF16LE;   HasBOM: true),
    (BOM: ($EF,$BB,$BF,$00); Len: 3; Enc: seUTF8;      HasBOM: true),
    (BOM: ($00,$00,$00,$3C); Len: 4; Enc: seUCS4BE;    HasBOM: false),
    (BOM: ($3C,$00,$00,$00); Len: 4; Enc: seUCS4LE;    HasBOM: false),
    (BOM: ($00,$00,$3C,$00); Len: 4; Enc: seUCS4_2143; HasBOM: false),
    (BOM: ($00,$3C,$00,$00); Len: 4; Enc: seUCS4_3412; HasBOM: false),
    (BOM: ($00,$3C,$00,$3F); Len: 4; Enc: seUTF16BE;   HasBOM: false),
    (BOM: ($3C,$00,$3F,$00); Len: 4; Enc: seUTF16LE;   HasBOM: false),
    (BOM: ($3C,$3F,$78,$6D); Len: 4; Enc: se8Bit;      HasBOM: false),
    (BOM: ($4C,$6F,$A7,$94); Len: 4; Enc: seEBCDIC;    HasBOM: false)
  );

const

  cNodeClass: array[TsdElementType] of TsdNodeClass =
    (TsdElement, TsdAttribute, TsdComment, TsdCData, TsdDeclaration, TsdStyleSheet,
     TsdDocType, TsdDtdElement, TsdDtdAttList, TsdDtdEntity, TsdDtdNotation,
     TsdInstruction, TsdCharData, TsdQuotedText, nil, nil, nil);

  cBlankChars = [#9, #10, #13, #32];
  cBlankCharsOrEndTag = cBlankChars + ['[', '/', '>'];

{ TXmlNode }

procedure TXmlNode.AttributeAdd(const AName, AValue: string);
var
  A: TsdAttribute;
begin
  A := TsdAttribute.Create(FOwner);
  A.Name := AName;
  A.Value := AValue;
  NodeAdd(A);
end;

function TXmlNode.AttributeCount: integer;
begin
  Result := 0;
end;

procedure TXmlNode.AttributeDelete(Index: integer);
begin
  if Index < AttributeCount then
    NodeDelete(Index);
end;

constructor TXmlNode.Create(AOwner: TsdFastXml);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TXmlNode.Delete;
begin
  // Free by removing from owning list:
  // Remove from our parent
  if FParent is TsdContainerNode then
    TsdContainerNode(FParent).NodeList.Remove(Self)
  else
    // Remove from owner root list.
    FOwner.FRootNodes.Remove(Self);
end;

function TXmlNode.ElementType: TsdElementType;
begin
  Result := xeUnknown;
end;

class function TXmlNode.EscapeString(const S: string): string;
begin
  Result := EscapeStringUTF8(S);
end;

function TXmlNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := nil;
end;

class function TXmlNode.FromWide(const W: widestring): string;
begin
  Result := sdUnicodeToUtf8(W);
end;

function TXmlNode.GetAttributeByName(const AName: string): TsdAttribute;
var
  i: integer;
begin
  for i := 0 to AttributeCount - 1 do
    if (Nodes[i].Name = AName) then
    begin
      Result := TsdAttribute(Nodes[i]);
      exit;
    end;
  Result := nil;
end;

function TXmlNode.GetAttributeName(Index: integer): string;
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if assigned(A) then
    Result := A.Name
  else
    Result := '';
end;

function TXmlNode.GetAttributes(Index: integer): TsdAttribute;
var
  Node: TXmlNode;
begin
  Node := GetNodes(Index);
  if Node is TsdAttribute then
    Result := TsdAttribute(Node)
  else
    Result := nil;
end;

function TXmlNode.GetAttributeValue(Index: integer): string;
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetAttributeValueAsInteger(Index: integer): integer;
begin
  Result := StrToIntDef(GetAttributeValue(Index), 0);
end;

function TXmlNode.GetAttributeValueByName(const AName: string): string;
var
  A: TsdAttribute;
begin
  A := AttributeByName[AName];
  if assigned(A) then
    Result := A.Value
  else
    Result := '';
end;

function TXmlNode.GetName: string;
begin
  Result := '';
end;

function TXmlNode.GetNodes(Index: integer): TXmlNode;
begin
  Result := nil;
end;

function TXmlNode.GetValue: string;
begin
  Result := '';
end;

function TXmlNode.GetValueWide: widestring;
begin
  Result := sdUtf8ToUnicode(GetValue);
end;

function TXmlNode.IsClear: boolean;
begin
  Result := IsEmpty and (length(Name) = 0);
end;

function TXmlNode.IsEmpty: boolean;
begin
  Result := (NodeCount = 0) and (length(Value) = 0)
end;

function TXmlNode.NodeAdd(ANode: TXmlNode): integer;
begin
  raise Exception.Create(sCannotAddNode);
end;

function TXmlNode.NodeByName(const AName: string): TXmlNode;
var
  i: integer;
begin
  for i := 0 to NodeCount - 1 do
    if Nodes[i].Name = AName then
    begin
      Result := Nodes[i];
      exit;
    end;
  Result := nil;
end;

function TXmlNode.NodeCount: integer;
begin
  Result := 0;
end;

procedure TXmlNode.NodeDelete(Index: integer);
begin
  raise Exception.Create(sCannotManipulate);
end;

procedure TXmlNode.NodeExchange(Index1, Index2: integer);
begin
  raise Exception.Create(sCannotManipulate);
end;

function TXmlNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  Result := -1;
end;

procedure TXmlNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  raise Exception.Create(sCannotAddNode);
end;

procedure TXmlNode.ParseStream(R: TsdBufferReader);
begin
// does nothing
end;

procedure TXmlNode.SetAttributeName(Index: integer; const Value: string);
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if not assigned(A) then exit;
  A.Name := Value;
end;

procedure TXmlNode.SetAttributeValue(Index: integer; const Value: string);
var
  A: TsdAttribute;
begin
  A := Attributes[Index];
  if not assigned(A) then exit;
  A.Value := Value;
end;

procedure TXmlNode.SetAttributeValueAsInteger(Index: integer; const Value: integer);
begin
  SetAttributeValue(Index, IntToStr(Value));
end;

procedure TXmlNode.SetAttributeValueByName(const AName, Value: string);
var
  A: TsdAttribute;
begin
  A := GetAttributeByName(AName);
  if not assigned(A) then
  begin
    A := TsdAttribute.Create(FOwner);
    A.Name := AName;
    NodeAdd(A);
  end;
  A.Value := Value;
end;

procedure TXmlNode.SetName(const Value: string);
begin
  raise Exception.Create(sCannotSetName);
end;

procedure TXmlNode.SetValue(const Value: string);
begin
  raise Exception.Create(sCannotSetValue);
end;

procedure TXmlNode.SetValueWide(const Value: widestring);
begin
  SetValue(sdUnicodeToUtf8(Value));
end;

function TXmlNode.TableGetString(AID: integer): string;
begin
  Result := FOwner.FStrings.GetString(AID);
end;

procedure TXmlNode.TableSetString(var AID: integer; const S: string);
begin
  FOwner.FStrings.SetString(AID, S);
end;

class function TXmlNode.ToWide(const S: string): widestring;
begin
  Result := sdUtf8ToUnicode(S);
end;

function TXmlNode.TreeDepth: integer;
begin
  if assigned(FParent) then
    Result := FParent.TreeDepth + 1
  else
    Result := 0;
end;

class function TXmlNode.UnescapeString(const S: string): string;
begin
  Result := UnescapeStringUTF8(S);
end;

{ TsdCharData }

destructor TsdCharData.Destroy;
begin
  TableSetString(FValueID, '');
  inherited;
end;

function TsdCharData.ElementType: TsdElementType;
begin
  Result := xeCharData;
end;

function TsdCharData.GetName: string;
begin
  Result := 'CharData';
end;

function TsdCharData.GetValue: string;
begin
  if MustEscape then
    Result := UnescapeString(TableGetString(FValueID))
  else
    Result := TableGetString(FValueID);
end;

function TsdCharData.MustEscape: boolean;
begin
  Result := True;
end;

procedure TsdCharData.SetValue(const Value: string);
begin
  if MustEscape then
    TableSetString(FValueID, EscapeString(Value))
  else
    TableSetString(FValueID, Value);
end;

{ TsdAttribute }

destructor TsdAttribute.Destroy;
begin
  TableSetString(FNameID, '');
  inherited;
end;

function TsdAttribute.ElementType: TsdElementType;
begin
  Result := xeAttribute;
end;

function TsdAttribute.GetName: string;
begin
  Result := TableGetString(FNameID);
end;

function TsdAttribute.MustEscape: boolean;
begin
  Result := True;
end;

procedure TsdAttribute.ParseStream(R: TsdBufferReader);
begin
  // Get the attribute name
  TableSetString(FNameID, Trim(R.ReadStringUntilChar('=', False)));
  // value
  inherited;
end;

procedure TsdAttribute.SetName(const Value: string);
begin
  TableSetString(FNameID, Value);
end;

{ TsdQuotedText }

function TsdQuotedText.ElementType: TsdElementType;
begin
  Result := xeQuotedText;
end;

function TsdQuotedText.GetName: string;
begin
  Result := 'QuotedText';
end;

function TsdQuotedText.MustEscape: boolean;
begin
  Result := False;
end;

procedure TsdQuotedText.ParseStream(R: TsdBufferReader);
begin
  // Get the quoted value
  FQuoteChar := R.NextCharSkipBlanks;
  if not (FQuoteChar in ['''', '"']) then
    raise Exception.Create(sQuoteCharExpected);
  TableSetString(FValueID, R.ReadQuotedString(FQuoteChar));
end;

{ TsdContainerNode }

function TsdContainerNode.AttributeCount: integer;
begin
  Result := 0;
  while FNodes[Result] is TsdAttribute do
    inc(Result);
end;

constructor TsdContainerNode.Create(AOwner: TsdFastXml);
begin
  inherited;
  FNodes := TsdNodeList.Create(True);
end;

destructor TsdContainerNode.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

function TsdContainerNode.FirstNodeByType(AType: TsdElementType): TXmlNode;
begin
  Result := FNodes.ByType(AType);
end;

function TsdContainerNode.GetNodes(Index: integer): TXmlNode;
begin
  Result := FNodes[Index];
end;

function TsdContainerNode.NodeAdd(ANode: TXmlNode): integer;
begin
  if ANode.ElementType = xeAttribute then
  begin
    Result := AttributeCount;
    FNodes.Insert(Result, ANode);
  end else
    Result := FNodes.Add(ANode);
  ANode.FParent := Self;
end;

function TsdContainerNode.NodeCount: integer;
begin
  Result := FNodes.Count;
end;

procedure TsdContainerNode.NodeDelete(Index: integer);
begin
  FNodes.Delete(Index);
end;

procedure TsdContainerNode.NodeExchange(Index1, Index2: integer);
begin
  FNodes.Exchange(Index1, Index2);
end;

function TsdContainerNode.NodeIndexOf(ANode: TXmlNode): integer;
begin
  Result := FNodes.IndexOf(ANode);
end;

procedure TsdContainerNode.NodeInsert(Index: integer; ANode: TXmlNode);
begin
  FNodes.Insert(Index, ANode);
  ANode.FParent := Self;
end;

function TsdContainerNode.ParseAttributeList(R: TsdBufferReader): char;
var
  A: TsdAttribute;
begin
  repeat
    Result := R.NextCharSkipBlanks;
    // Are any of the characters determining the end?
    if Result in ['!', '/', '>' ,'?'] then
      exit;
    R.MoveBack;
    A := TsdAttribute.Create(FOwner);
    NodeAdd(A);
    A.ParseStream(R);
    FOwner.DoNodeNew(A);
    FOwner.DoNodeLoaded(A);
  until False;
end;

function TsdContainerNode.ParseQuotedTextList(R: TsdBufferReader): char;
var
  T: TsdQuotedText;
begin
  repeat
    Result := R.NextCharSkipBlanks;
    // Are any of the characters determining the end?
    if Result in ['!', '/', '>' ,'?'] then
      exit;
    R.MoveBack;
    T := TsdQuotedText.Create(FOwner);
    T.ParseStream(R);
    NodeAdd(T);
    FOwner.DoNodeNew(T);
    FOwner.DoNodeLoaded(T);
  until False;
end;

{ TsdElement }

destructor TsdElement.Destroy;
begin
  TableSetString(FNameID, '');
  inherited;
end;

function TsdElement.ElementType: TsdElementType;
begin
  Result := xeElement;
end;

function TsdElement.GetName: string;
begin
  Result := TableGetString(FNameID);
end;

function TsdElement.GetValue: string;
var
  Node: TXmlNode;
begin
  // Return the value of the first subnode after the attributes if it is CharData
  Node := FNodes[AttributeCount];
  if Node is TsdCharData then
    Result := Node.Value
  else
    Result := '';
end;

procedure TsdElement.ParseElementList(R: TsdBufferReader;
  const SupportedTags: TsdElementTypes);
var
  B: char;
  N: string;
  ET: TsdElementType;
  NC: TsdNodeClass;
  Node: TXmlNode;
begin
  repeat
    // Process char data
    ParseIntermediateData(R);
    // Process subtags and end tag
    if R.EndOfStream then
      raise Exception.Create(sPrematureEnd);
    R.MoveBack;
    B := R.NextChar;
    if B = '<' then
    begin
      // Determine tag type
      ET := R.ReadOpenTag;
      if not (ET in SupportedTags) then
        raise Exception.Create(sIllegalTag);
      // End tag?
      if ET = xeEndTag then
      begin
        // Read end tag
        N := R.ReadStringUntilChar('>', False);
        // Check if begin and end tag match
        if TableGetString(FNameID) <> N then
          raise Exception.Create(Format(sBeginEndMismatch + ': %s %s', [TableGetString(FNameID), N]));
        // We're done reading this element, so we will set the capacity of the
        // nodelist to just the amount of items to avoid having overhead.
        FNodes.SetCapacity(FNodes.Count);
        exit;
      end;
      // Determine node class
      NC := cNodeClass[ET];
      // for now..
      if not assigned(NC) then
        raise Exception.Create(sUnsupportedTag);
      // Create new node and add
      Node := NC.Create(FOwner);
      NodeAdd(Node);
      if ET <> xeElement then
        FOwner.DoNodeNew(Node);
      // The node will parse itself
      Node.ParseStream(R);
      FOwner.DoNodeLoaded(Node);
    end else
      // Since this virtual proc is also used for doctype parsing.. check
      // end char here
      if (B = ']') and (ElementType = xeDocType) then
        break;
  until FOwner.FAbortParsing;
end;

procedure TsdElement.ParseIntermediateData(R: TsdBufferReader);
var
  ChData: string;
  CD: TsdCharData;
begin
  ChData := R.ReadStringUntilChar('<', True);
  if not FOwner.PreserveWhiteSpace then
    ChData := Trim(ChData);
  if length(ChData) > 0 then
  begin
    // Insert CharData node
    CD := TsdCharData.Create(FOwner);
    TablesetString(CD.FValueID, ChData);
    NodeAdd(CD);
    FOwner.DoNodeNew(CD);
    FOwner.DoNodeLoaded(CD);
  end;
end;

procedure TsdElement.ParseStream(R: TsdBufferReader);
var
  B: char;
begin
  // Flush the reader.. it will only flush when it has read a large chunk of data,
  // so this routine is not costly.
  R.Flush;
  // Parse name
  TableSetString(FNameID, Trim(R.ReadStringUntilBlankOrEndTag));
  FOwner.DoNodeNew(Self);
  // Parse attribute list
  B := ParseAttributeList(R);
  if B = '/' then
  begin
    // Direct tag
    B := R.NextChar;
    if B <> '>' then
      raise Exception.Create(sIllegalEndTag);
  end else
  begin
    if B <> '>' then
      raise Exception.Create(sIllegalEndTag);
    ParseElementList(R, [xeElement..xeCData, xeInstruction..xeEndTag]);
  end;
end;

procedure TsdElement.SetName(const Value: string);
begin
  TableSetString(FNameID, Value);
end;

procedure TsdElement.SetValue(const Value: string);
var
  Count: integer;
  Node: TXmlNode;
begin
  // Return the value of the first subnode after the attributes if it is CharData
  Count := AttributeCount;
  Node := FNodes[Count];
  if Node is TsdCharData then
    Node.Value := Value
  else
  begin
    Node := TsdCharData.Create(FOwner);
    Node.Value := Value;
    NodeInsert(Count, Node);
  end;
end;

{ TsdDeclaration }

function TsdDeclaration.ElementType: TsdElementType;
begin
  Result := xeDeclaration;
end;

function TsdDeclaration.GetEncoding: string;
begin
  Result := AttributeValueByName['encoding'];
end;

function TsdDeclaration.GetName: string;
begin
  Result := 'xml';
end;

function TsdDeclaration.GetVersion: string;
begin
  Result := AttributeValueByName['version'];
end;

procedure TsdDeclaration.ParseStream(R: TsdBufferReader);
var
  B: char;
begin
  // Directly parse the attribute list
  B := ParseAttributeList(R);
  if B <> '?' then
    raise Exception.Create(sIllegalEndTag);
  B := R.NextChar;
  if B <> '>' then
    raise Exception.Create(sIllegalEndTag);
end;

procedure TsdDeclaration.SetEncoding(const Value: string);
begin
  AttributeValueByName['encoding'] := Value;
end;

procedure TsdDeclaration.SetVersion(const Value: string);
begin
  AttributeValueByName['version'] := Value;
end;

{ TsdComment }

function TsdComment.ElementType: TsdElementType;
begin
  Result := xeComment;
end;

function TsdComment.GetName: string;
begin
  Result := 'Comment';
end;

procedure TsdComment.ParseStream(R: TsdBufferReader);
begin
  TableSetString(FValueID, R.ReadStringUntil('-->'));
end;

{ TsdCData }

function TsdCData.ElementType: TsdElementType;
begin
  Result := xeCData;
end;

function TsdCData.GetName: string;
begin
  Result := 'CData';
end;

function TsdCData.GetValue: string;
begin
  // The value should not be unescaped
  Result := TableGetString(FValueID);
end;

procedure TsdCData.ParseStream(R: TsdBufferReader);
begin
  TableSetString(FValueID, R.ReadStringUntil(']]>'));
end;

procedure TsdCData.SetValue(const Value: string);
begin
  // The value should not be escaped
  TableSetString(FValueID, Value);
end;

{ TsdDocType }

constructor TsdDocType.Create(AOwner: TsdFastXml);
begin
  inherited;
  FSystemLiteral := TsdQuotedText.Create(AOwner);
  FPubIDLiteral := TsdQuotedText.Create(AOwner);
end;

destructor TsdDocType.Destroy;
begin
  FreeAndNil(FSystemLiteral);
  FreeAndNil(FPubIDLiteral);
  inherited;
end;

function TsdDocType.ElementType: TsdElementType;
begin
  Result := xeDocType;
end;

procedure TsdDocType.ParseIntermediateData(R: TsdBufferReader);
// in dtd's we do not allow chardata, but pe instead. Not implemented yet
var
  B: char;
begin
  repeat
    B := R.NextCharSkipBlanks;
    // todo: PERef
    if not (B in [']', '<']) then
      R.ReadStringUntilBlankOrEndTag
    else
      break;
  until False;
end;

procedure TsdDocType.ParseStream(R: TsdBufferReader);
var
  B: char;
begin
  // Parse name
  R.SkipBlanks;
  TableSetString(FNameID, Trim(R.ReadStringUntilBlankOrEndTag));
  R.SkipBlanks;
  B := R.NextChar;
  if not (B in ['[', '>']) then
  begin
    R.MoveBack;
    // Parse external ID
    if R.CheckString('SYSTEM') then
    begin
      FExternalID := 'SYSTEM';
      FSystemLiteral.ParseStream(R);
    end else if R.CheckString('PUBLIC') then
    begin
      FExternalID := 'PUBLIC';
      FPubIDLiteral.ParseStream(R);
      FSystemLiteral.ParseStream(R);
    end else
      raise Exception.Create(sIllegalTag);
    B := R.NextCharSkipBlanks;
  end;
  if B = '[' then
  begin
    ParseElementList(R, [xeComment, xeDtdElement..xeInstruction, xeCharData]);
    B := R.NextCharSkipBlanks;
  end;
  if B <> '>' then
    raise Exception.Create(sIllegalTag);
end;

{ TsdDtdElement }

function TsdDtdElement.ElementType: TsdElementType;
begin
  Result := xeDtdElement;
end;

procedure TsdDtdElement.ParseStream(R: TsdBufferReader);
var
  B: char;
begin
  R.SkipBlanks;
  TableSetString(FNameID, Trim(R.ReadStringUntilBlankOrEndTag));
  R.SkipBlanks;
  // For now..
  B := ParseQuotedTextList(R);
  if B <> '>' then
    raise Exception.Create(sIllegalEndTag);
end;

{ TsdDtdAttList }

function TsdDtdAttList.ElementType: TsdElementType;
begin
  Result := xeDtdAttList;
end;

{ TsdDtdEntity }

function TsdDtdEntity.ElementType: TsdElementType;
begin
  Result := xeDtdElement;
end;

{ TsdDtdNotation }

function TsdDtdNotation.ElementType: TsdElementType;
begin
  Result := xeDtdNotation;
end;

{ TsdInstruction }

function TsdInstruction.ElementType: TsdElementType;
begin
  Result := xeInstruction;
end;

function TsdInstruction.GetName: string;
begin
  Result := 'PI';
end;

procedure TsdInstruction.ParseStream(R: TsdBufferReader);
begin
  TableSetString(FValueID, R.ReadStringUntil('?>'));
end;

{ TsdStyleSheet }

function TsdStyleSheet.ElementType: TsdElementType;
begin
  Result := xeStyleSheet;
end;

function TsdStyleSheet.GetName: string;
begin
  Result := 'xml-stylesheet';
end;

procedure TsdStyleSheet.ParseStream(R: TsdBufferReader);
begin
  TableSetString(FValueID, Trim(R.ReadStringUntil('?>')));
end;

{ TsdNodeList }

function TsdNodeList.ByType(AType: TsdElementType): TXmlNode;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ElementType = AType then
    begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TsdNodeList.GetItems(Index: integer): TXmlNode;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TsdFastXml }

procedure TsdFastXml.Clear;
var
  D: TsdDeclaration;
  E: TsdElement;
begin
  FStrings.Clear;
  FRootNodes.Clear;
  // Defaults
  // todo
  // Build default items in RootNodes
  // - first the declaration
  D := TsdDeclaration.Create(Self);
  D.Version := cDefaultVersionString;
  D.Encoding := cDefaultEncodingString;
  FRootNodes.Add(D);
  // - then the root node
  E := TsdElement.Create(Self);
  FRootNodes.Add(E);
end;

constructor TsdFastXml.Create(AOwner: TComponent);
begin
  inherited;
  FRootNodes := TsdNodeList.Create(True);
  FStrings := TsdStringTable.Create;
  Clear; // this sets defaults and adds default elements
end;

destructor TsdFastXml.Destroy;
begin
  FStrings.Clear;
  FreeAndNil(FRootNodes);
  FreeAndNil(FReader);
  FreeAndNil(FStrings);
  inherited;
end;

procedure TsdFastXml.DoNodeLoaded(ANode: TXmlNode);
begin
  if assigned(FOnNodeLoaded) then
    FOnNodeLoaded(Self, ANode);
end;

procedure TsdFastXml.DoNodeNew(ANode: TXmlNode);
begin
  if assigned(FOnNodeNew) then
    FOnNodeNew(Self, ANode);
end;

function TsdFastXml.GetCommentString: string;
// Get the first comment node, and return its value
var
  Node: TXmlNode;
begin
  Result := '';
  Node := FRootNodes.ByType(xeComment);
  if assigned(Node) then
    Result := Node.Value;
end;

function TsdFastXml.GetEncodingString: string;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Encoding;
end;

function TsdFastXml.GetReaderLineNumber: int64;
begin
  if assigned(FReader) then
    Result := FReader.LineNumber
  else
    Result := 0;
end;

function TsdFastXml.GetReaderPosition: int64;
begin
  if assigned(FReader) then
    Result := FReader.Position
  else
    Result := 0;
end;

function TsdFastXml.GetRoot: TsdElement;
begin
  Result := TsdElement(FRootNodes.ByType(xeElement));
end;

function TsdFastXml.GetStyleSheet: TsdStyleSheet;
begin
  Result := TsdStyleSheet(FRootNodes.ByType(xeStylesheet));
  if not assigned(Result) then
  begin
    // Add a stylesheet node as second one if none present
    Result := TsdStyleSheet.Create(Self);
    FRootNodes.Insert(1, Result);
  end;
end;

function TsdFastXml.GetVersionString: string;
begin
  Result := '';
  if FRootNodes.Count > 0 then
    if FRootNodes[0] is TsdDeclaration then
      Result := TsdDeclaration(FRootNodes[0]).Version;
end;

function TsdFastXml.IsEmpty: boolean;
var
  R: TXmlNode;
begin
  R := GetRoot;
  Result := not assigned(R) or R.IsClear;
end;

procedure TsdFastXml.LoadFromFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TsdFastXml.LoadFromStream(AStream: TStream);
begin
  FRootNodes.Clear;
  FreeAndNil(FReader);
  FReader := TsdBufferReader.Create(AStream);
  try
    FReader.ReadBOM;
    ParseStream(FReader);
    FExternalEncoding := FReader.Encoding;
  finally
    FReader.Flush(True);
    FReader.ClearData;
  end;
end;

procedure TsdFastXml.ParseStream(R: TsdBufferReader);
var
  B: char;
  ET: TsdElementType;
  NC: TsdNodeClass;
  Node: TXmlNode;
  ChData: string;
  CD: TsdCharData;
begin
  FAbortParsing := False;
  // Read next tag
  repeat
    ChData := R.ReadStringUntilChar('<', True);
    if not FPreserveWhiteSpace then
      ChData := Trim(ChData);
    if length(ChData) > 0 then
    begin
      // Add chardata node
      CD := TsdCharData.Create(Self);
      FStrings.SetString(CD.FValueID, ChData);
      FRootNodes.Add(CD);
      DoNodeNew(CD);
      DoNodeLoaded(CD);
    end;
    // At the end of the stream? Then stop
    if R.EndOfStream then
      break;
    R.MoveBack;
    B := R.NextChar;
    if B = '<' then
    begin
      // Determine tag type
      ET := R.ReadOpenTag;
      if ET = xeError then
        raise Exception.Create(sIllegalTag);
      // Determine node class
      NC := cNodeClass[ET];
      if not assigned(NC) then
        raise Exception.Create(sUnsupportedTag);
      // Create new node and add
      Node := NC.Create(Self);
      FRootNodes.Add(Node);
      if ET <> xeElement then
        DoNodeNew(Node);
      // The node will parse itself
      Node.ParseStream(R);
      DoNodeLoaded(Node);
      // After adding nodes, see if we added the declaration node
      if Node.ElementType = xeDeclaration then
      begin
        // Is this stream encoded as UTF8?
        if AnsiCompareText(TsdDeclaration(Node).Encoding, 'UTF-8') = 0 then
        begin
          // and the stream as Ascii? We assume the stream is UTF8
          if R.Encoding = se8bit then
            R.Encoding := seUTF8;
        end else
        begin
          if R.Encoding = se8bit then
            // In this case we must instruct the stream to use UTF8
            R.ConvertAscii := True;
        end;;
      end;
    end;
    // Check if application has aborted parsing
  until FAbortParsing;
end;

procedure TsdFastXml.SaveToFile(const AFileName: string);
begin
// todo
end;

function TsdFastXml.SaveToString: string;
begin
// todo
end;

procedure TsdFastXml.SetCommentString(const Value: string);
// Find first comment node and set it's value, otherwise add new comment node
// right below the xml declaration
var
  Node: TXmlNode;
begin
  Node := FRootNodes.ByType(xeComment);
  if not assigned(Node) and (length(Value) > 0) then
  begin
    Node := TsdComment.Create(Self);
    FRootNodes.Insert(1, Node);
  end;
  if assigned(Node) then
    Node.Value := Value;
end;

procedure TsdFastXml.SetEncodingString(const Value: string);
var
  Node: TXmlNode;
begin
  if Value = GetEncodingString then
    exit;
  Node := FRootNodes[0];
  if not (Node is TsdDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TsdDeclaration.Create(Self);
      FRootNodes.Insert(0, Node);
    end;
  end;
  if assigned(Node) then
    TsdDeclaration(Node).Encoding := Value;
end;

procedure TsdFastXml.SetVersionString(const Value: string);
var
  Node: TXmlNode;
begin
  if Value = GetVersionString then
    exit;
  Node := FRootNodes[0];
  if not (Node is TsdDeclaration) then
  begin
    if length(Value) > 0 then
    begin
      Node := TsdDeclaration.Create(Self);
      FRootNodes.Insert(0, Node);
    end;
  end;
  if assigned(Node) then
    TsdDeclaration(Node).Version := Value;
end;

{ TsdBufferReader }

const

  cChunkSize = 65536; // 64K blocks

function TsdBufferReader.CheckString(const S: string): boolean;
var
  i, Count: integer;
  P: PByte;
begin
  Count := length(S);
  MakeDataAvailable(Count);
  P := FCurr;
  Result := True;
  for i := 1 to Count do
  begin
    if S[i] <> char(P^) then
    begin
      Result := False;
      exit;
    end;
    inc(P);
  end;
  FCurr := P;
end;

procedure TsdBufferReader.ClearData;
begin
  FSource := nil;
  SetLength(FBuffer, 0);
  FCurr := nil;
  FLast := nil;
  FClose := 0;
  FCapacity := 0;
  SetLength(FUtf16Content, 0);
end;

constructor TsdBufferReader.Create(ASource: TStream);
begin
  inherited Create;
  FSource := ASource;
end;

procedure TsdBufferReader.Flush(Force: boolean);
var
  i, Count, Dist: integer;
  P: Pbyte;
  Lim: integer;
begin
  Dist := integer(FCurr) - integer(@FBuffer[0]);
  Lim := cChunkSize div 2;
  if Force then
    Lim := 1;
  if Dist > Lim then
  begin
    // Calcuate base line number
    P := @FBuffer[0];
    for i := 0 to Dist - 1 do
    begin
      if P^ = 13 then inc(FBaseLineNumber);
      inc(P);
    end;
    // Number of bytes to move
    Count := integer(FLast) - integer(FCurr);
    Move(FCurr^, FBuffer[0], Count);
    dec(FCurr, Dist);
    dec(FLast, Dist);
    dec(FClose, Dist);
    FBasePosition := FBasePosition + Dist;
  end;
end;

function TsdBufferReader.GetLineNumber: int64;
var
  i, Count: integer;
  P: Pbyte;
begin
  Result := FBaseLineNumber;
  Count := integer(FCurr) - integer(@FBuffer[0]);
  P := @FBuffer[0];
  for i := 0 to Count - 1 do
  begin
    if P^ = 13 then
      inc(Result);
    inc(P);
  end;
end;

function TsdBufferReader.GetPosition: int64;
begin
  Result := FBasePosition;
  if length(FBuffer) > 0 then
    inc(Result, integer(FCurr) - integer(@FBuffer[0]));
end;

function TsdBufferReader.HasDataAvailable(ACount: integer): integer;
begin
  Result := integer(FLast) - integer(FCurr);
  if Result < ACount then
  begin
    // We must make data available
    ReadChunk;
    Result := integer(FLast) - integer(FCurr);
    if Result = 0 then
      FEndOfStream := True;
  end;
end;

function TsdBufferReader.MakeDataAvailable(ACount: integer): integer;
begin
  Result := integer(FLast) - integer(FCurr);
  if Result < ACount then
  begin
    // We must make data available
    ReadChunk;
    Result := integer(FLast) - integer(FCurr);
    // Still no data available?
    if Result < ACount then
      raise Exception.Create(sPrematureEnd);
  end;
end;

procedure TsdBufferReader.MoveBack;
begin
  dec(FCurr);
end;

function TsdBufferReader.NextChar: char;
var
  Count: integer;
begin
  Count := HasDataAvailable(1);
  if Count = 0 then
  begin
    Result := #0;
    exit;
  end;
  Result := char(FCurr^);
  inc(FCurr);
end;

function TsdBufferReader.NextCharSkipBlanks: char;
var
  Count: integer;
begin
  Count := HasDataAvailable(1);
  while Count > 0 do
  begin
    Result := char(FCurr^);
    inc(FCurr);
    if not (Result in cBlankChars) then exit;
    dec(Count);
    if Count = 0 then Count := HasDataAvailable(1);
  end;
  Result := #0;
end;

procedure TsdBufferReader.ReadBOM;
var
  i, j, BytesRead: integer;
  BOM: array[0..3] of byte;
  Found: boolean;
begin
  BytesRead := FSource.Read(BOM, 4);
  if BytesRead <> 4 then
    raise Exception.Create(sInvalidStream);
  for i := 0 to cBomInfoCount - 1 do
  begin
    Found := True;
    for j := 0 to cBomInfo[i].Len - 1 do
    begin
      if BOM[j] <> cBomInfo[i].BOM[j] then
      begin
        Found := False;
        break;
      end;
    end;
    if Found then
      break;
  end;
  if Found then
  begin
    FEncoding := cBomInfo[i].Enc;
    FHasBom := cBomInfo[i].HasBOM;
  end else
  begin
    // Unknown.. default to this
    FEncoding := se8Bit;
    FHasBom := False;
  end;
  // Non-supported encodings
  if not (FEncoding in [se8bit, seUTF8, seUTF16BE, seUTF16LE]) then
    raise Exception.Create(sUnsupportedEncoding);

  // Rewind based on BOM
  if FHasBom then
    FBasePosition := cBomInfo[i].Len
  else
    FBasePosition := 0;
  FSource.Seek(FBasePosition - 4, soFromCurrent)
end;

procedure TsdBufferReader.ReadChunk;
var
  i, CurrIdx, BytesRead, ByteCount: integer;
  P: Pbyte;
  W: Pword;
begin
  if FCurr = nil then
    CurrIdx := 0
  else
    CurrIdx := integer(FCurr) - integer(@FBuffer[0]);

  case FEncoding of
  se8Bit, seUTF8:
    begin
      // Increase capacity according to chunk size
      SetCapacity(FClose + cChunkSize);
      // Read from the stream directly to our chunk
      BytesRead := FSource.Read(FBuffer[FClose], cChunkSize);
      P := @FBuffer[FClose];
      inc(FClose, BytesRead);
      if FConvertAscii then
        // in case Ascii, replace all occurances of values >= 128
        for i := 0 to BytesRead - 1 do
        begin
          if P^ >= 128 then P^ := ord('?');
          inc(P);
        end;
    end;
  seUTF16BE, seUTF16LE:
    begin
      // Increase capacity according to chunk size, we must take into account
      // that UTF16 chunks can become larger when converted to UTF8. In theory
      // this is only 3/2, but we will use a factor of 2.
      SetCapacity(FClose + cChunkSize * 2);
      // Buffer for UTF16 content
      SetLength(FUtf16Content, cChunkSize);
      // Read UTF16 content
      BytesRead := FSource.Read(FUtf16Content[0], cChunkSize);
      // If UTF16 BE (Big Endian), we must swap byte order
      if FEncoding = seUTF16BE then
      begin
        W := @FUtf16Content[0];
        for i := 0 to BytesRead div 2 - 1 do
        begin
          W^ := Swap(W^);
          inc(W);
        end;
      end;
      // Now convert from UTF16 to UTF8
      ByteCount := sdUnicodeToUtf8Mem(@FUtf16Content[0], @FBuffer[FClose], BytesRead div 2);
      inc(FClose, ByteCount);
    end;
  end;

  // Set pointers
  if FClose > 0 then
  begin
    FCurr := @FBuffer[CurrIdx];
    FLast := @FBuffer[FClose - 1]; inc(FLast);
  end else
  begin
    FCurr := nil;
    FLast := nil;
  end;
end;

function TsdBufferReader.ReadOpenTag: TsdElementType;
var
  Count: integer;
  Ch: char;
begin
  Result := xeError;
  Count := MakeDataAvailable(1);
  Ch := Char(FCurr^); inc(FCurr);
  case Ch of
  '!':
    begin
      if Count = 0 then MakeDataAvailable(1);
      Ch := Char(FCurr^); inc(FCurr);
      case Ch of
      '[': if CheckString('CDATA[') then Result := xeCData;
      'D': if CheckString('OCTYPE') then Result := xeDocType;
      'E':
        begin
          if CheckString('LEMENT') then Result := xeDtdElement;
          if CheckString('NTITY') then Result := xeDtdEntity;
        end;
      'A': if CheckString('TTLIST') then Result := xeDtdAttList;
      'N': if CheckString('OTATION') then Result := xeDtdNotation;
      '-': if CheckString('-') then Result := xeComment;
      else
        raise Exception.Create(sIllegalTag);
      end;
    end;
  '?':
    begin
      if CheckString('xml') then
      begin
        if CheckString('-stylesheet') then
          Result := xeStyleSheet
        else
          Result := xeDeclaration;
      end else
        Result := xeInstruction;
    end;
  '/': Result := xeEndTag;
  else
    Result := xeElement;
    dec(FCurr);
  end;
end;

function TsdBufferReader.ReadQuotedString(AQuote: char): string;
begin
  // It seems that the xml spec simply does not allow double quotes as in
  // Delphi, so we do not need a complicated algo to do this. We can simply
  // search for the quote again as terminator.
  Result := ReadStringUntilChar(AQuote, False);
end;

function TsdBufferReader.ReadStringUntil(const ATerm: string): string;
var
  Count, MatchCount, MatchLen: integer;
  PStart, PMatch: Pbyte;
  First: byte;
  MatchIdx, StartIdx: integer;
begin
  Count := MakeDataAvailable(1);
  MatchLen := length(ATerm);
  MatchCount := 0;
  PStart := FCurr;
  First := ord(ATerm[1]);
  repeat
    if PStart^ = First then
    begin
      // Here we do the matching
      PMatch := PStart;
      repeat
        inc(MatchCount);
        if MatchCount = MatchLen then
        begin
          // We found the terminating string
          Count := integer(PStart) - Integer(FCurr);
          SetLength(Result, Count);
          if Count > 0 then
            Move(FCurr^, Result[1], Count);
          // Adjust FCurr
          FCurr := PMatch;
          inc(FCurr);
          exit;
        end;
        // Match a character
        inc(PMatch);
        dec(Count);
        if Count <= 0 then
        begin
          MatchIdx := integer(PMatch) - integer(FCurr);
          StartIdx := integer(PStart) - integer(FCurr);
          FCurr := PMatch;
          Count := MakeDataAvailable(1);
          PMatch := FCurr; dec(FCurr, MatchIdx);
          PStart := FCurr; inc(PStart, StartIdx);
        end;
        if ord(ATerm[MatchCount + 1]) <> PMatch^ then
          break;
      until False;
      MatchCount := 0;
    end;
    inc(PStart);
    dec(Count);
    if Count <= 0 then
    begin
      StartIdx := integer(PStart) - integer(FCurr);
      FCurr := PStart;
      Count := MakeDataAvailable(1);
      PStart := FCurr; dec(FCurr, StartIdx);
    end;
  until Count = 0;
  raise Exception.Create(sPrematureEnd);
end;

function TsdBufferReader.ReadStringUntilBlankOrEndTag: string;
var
  Count: integer;
  PStart: Pbyte;
  StartIdx: integer;
begin
  Count := MakeDataAvailable(1);
  PStart := FCurr;
  repeat
    if char(PStart^) in cBlankCharsOrEndTag then
    begin
      // We found the termination
      Count := integer(PStart) - Integer(FCurr);
      SetLength(Result, Count);
      if Count > 0 then
        Move(FCurr^, Result[1], Count);
      // Adjust FCurr
      FCurr := PStart;
      exit;
    end;
    inc(PStart);
    dec(Count);
    if Count <= 0 then
    begin
      StartIdx := integer(PStart) - integer(FCurr);
      FCurr := PStart;
      Count := MakeDataAvailable(1);
      PStart := FCurr; dec(FCurr, StartIdx);
    end;
  until Count = 0;
  raise Exception.Create(sPrematureEnd);
end;

function TsdBufferReader.ReadStringUntilChar(AChar: char; AllowEOF: boolean): string;
var
  Count: integer;
  PStart: Pbyte;
  StartIdx: integer;
begin
  if AllowEOF then
  begin
    Count := HasDataAvailable(1);
    if Count = 0 then
    begin
      // End of stream
      Result := '';
      exit;
    end;
  end else
    Count := MakeDataAvailable(1);

  PStart := FCurr;
  repeat
    if char(PStart^) = AChar then
    begin
      // We found AChar
      Count := integer(PStart) - integer(FCurr);
      SetLength(Result, Count);
      if Count > 0 then
        Move(FCurr^, Result[1], Count);
      // Adjust FCurr
      FCurr := PStart;
      inc(FCurr);
      exit;
    end;
    inc(PStart);
    dec(Count);
    if Count = 0 then
    begin
      StartIdx := integer(PStart) - integer(FCurr);
      FCurr := PStart;
      if AllowEOF then
        Count := HasDataAvailable(1)
      else
        Count := MakeDataAvailable(1);
      PStart := FCurr;
      dec(FCurr, StartIdx);
    end;
  until Count = 0;
  // Arriving here: end of stream
  Count := integer(PStart) - integer(FCurr);
  SetLength(Result, Count);
  if Count > 0 then
    Move(FCurr^, Result[1], Count);
  FCurr := PStart;
end;

procedure TsdBufferReader.SetCapacity(ACapacity: integer);
begin
  if FCapacity = 0 then
    FCapacity := cChunkSize;
  while ACapacity > FCapacity do
    FCapacity := FCapacity * 2;
  SetLength(FBuffer, FCapacity);
end;

procedure TsdBufferReader.SetConvertAscii(const Value: boolean);
var
  P: Pbyte;
begin
  if FConvertAscii <> Value then
  begin
    FConvertAscii := Value;
    if FConvertAscii then
    begin
      // We must still correct the current buffer from Ascii to UTF8
      P := FCurr;
      while P <> FLast do
      begin
        if P^ >= 128 then
          P^ := ord('?');
        inc(P);
      end;
    end;
  end;
end;

procedure TsdBufferReader.SkipBlanks;
begin
  NextCharSkipBlanks;
  MoveBack;
end;

{ Utility Functions }

const

  // Count of different escape characters
  cEscapeCount = 5;

  // These are characters that must be escaped. Note that "&" is first since
  // when another would be replaced first (eg ">" by "&lt;") this could
  // cause the new "&" in "&lt;" to be replaced by "&amp;";
  cEscapes: array[0..cEscapeCount - 1] of string =
    ('&', '<', '>', '''', '"');

  // These are the strings that replace the escape strings - in the same order
  cReplaces: array[0..cEscapeCount - 1] of string =
    ('&amp;', '&lt;', '&gt;', '&apos;', '&quot;');

function sdUnicodeToUtf8Mem(Src: Pword; Dst: Pbyte; Count: integer): integer;
// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
var
  W: word;
  DStart: Pbyte;
begin
  DStart := Dst;
  while Count > 0 do
  begin
    W := Src^; inc(Src);
    if W <= $7F then
    begin
      Dst^ := byte(W);
      inc(Dst);
    end else
    begin
      if W > $7FF then
      begin
        Dst^ := byte($E0 or (W shr 12));
        inc(Dst);
        Dst^ := byte($80 or ((W shr 6) and $3F));
        inc(Dst);
        Dst^ := byte($80 or (W and $3F));
        inc(Dst);
      end else
      begin //  $7F < W <= $7FF
        Dst^ := byte($C0 or (W shr 6));
        inc(Dst);
        Dst^ := byte($80 or (W and $3F));
        inc(Dst);
      end;
    end;
    dec(Count);
  end;
  Result := integer(Dst) - integer(DStart);
end;

function sdUtf8ToUnicodeMem(Src: Pbyte; Dst: Pword; Count: integer): integer;
// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
var
  W: word;
  C: byte;
  DStart: Pword;
  SClose: Pbyte;
begin
  DStart := Dst;
  SClose := Src;
  inc(SClose, Count);
  while integer(Src) < integer(SClose) do
  begin
    // 1st byte
    W := Src^;
    inc(Src);
    if W and $80 <> 0 then
    begin
      W := W and $3F;
      if W and $20 <> 0 then
      begin
        // 2nd byte
        C := Src^;
        inc(Src);
        if C and $C0 <> $80 then
          // malformed trail byte or out of range char
          Continue;
        W := (W shl 6) or (C and $3F);
      end;
      // 2nd or 3rd byte
      C := Src^;
      inc(Src);
      if C and $C0 <> $80 then
        // malformed trail byte
        Continue;
      Dst^ := (W shl 6) or (C and $3F);
      inc(Dst);
    end else
    begin
      Dst^ := W;
      inc(Dst);
    end;
  end;
  Result := (integer(Dst) - integer(DStart)) div 2;
end;

function sdUnicodeToUtf8(const W: widestring): string;
var
  Count: integer;
begin
  Count := length(W);
  SetLength(Result, Count * 2); // just to be sure, should only need 3/2
  if Count = 0 then exit;
  Count := sdUnicodeToUtf8Mem(@W[1], @Result[1], Count);
  SetLength(Result, Count);
end;

function sdUtf8ToUnicode(const S: string): widestring;
var
  Count: Integer;
begin
  Count := length(S);
  SetLength(Result, Count);
  if Count = 0 then exit;
  Count := sdUtf8ToUnicodeMem(@S[1], @Result[1], Count);
  SetLength(Result, Count);
end;

function EscapeStringUTF8(const AValue: string): string;
// this function can use some optimization
var
  i: integer;
begin
  Result := AValue;
  for i := 0 to cEscapeCount - 1 do
    Result := StringReplace(Result, cEscapes[i], cReplaces[i], [rfReplaceAll]);
end;

function UnEscapeStringUTF8(const AValue: string): string;
// this function can use some optimization
var
  SearchStr, Reference, Replace: string;
  i, Offset, Code: Integer;
  W: word;
begin
  SearchStr := AValue;
  Result := '';
  while SearchStr <> '' do
  begin
    // find '&'
    Offset := AnsiPos('&', SearchStr);
    if Offset = 0 then
    begin
      // Nothing found
      Result := Result + SearchStr;
      Break;
    end;
    Result := Result + Copy(SearchStr, 1, Offset - 1);
    SearchStr := Copy(SearchStr, Offset, MaxInt);
    // find next ';'
    Offset := AnsiPos(';', SearchStr);
    if Offset = 0 then
    begin
      // Error: encountered a '&' but not a ';'.. we will ignore, just return
      // the unmodified value
      Result := Result + SearchStr;
      Break;
    end;
    // Reference
    Reference := copy(SearchStr, 1, Offset);
    SearchStr := Copy(SearchStr, Offset + 1, MaxInt);
    Replace := Reference;
    // See if it is a character reference
    if copy(Reference, 1, 2) = '&#' then
    begin
      Reference := copy(Reference, 3, length(Reference) - 3);
      if length(Reference) > 0 then
      begin
        if lowercase(Reference[1]) = 'x' then
          // Hex notation
          Reference[1] := '$';
        Code := StrToIntDef(Reference, -1);
        if (Code >= 0) and (Code < $FFFF) then
        begin
          W := Code;
          Replace := sdUnicodeToUtf8(WideChar(W));
        end;
      end;
    end else
    begin
      // Look up default escapes
      for i := 0 to cEscapeCount - 1 do
        if Reference = cReplaces[i] then
        begin
          // Replace
          Replace := cEscapes[i];
          Break;
        end;
    end;
    // New result
    Result := Result + Replace;
  end;
end;

end.
