unit idom2;

(*
 * Interface specifications for Dom level 2.
 *
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Initial Developers of the Original Code are:

 *   - Martijn Brinkers (m.brinkers@pobox.com)
 *   - Uwe Fechner (ufechner@csi.com)
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * for DOM specs see:
 * http://www.w3.org/TR/2000/REC-DOM-Level-2-Core-20001113/
 *
*)

interface

uses
  SysUtils,
  classes;

const

  (*
   * URI for prefix "xml"
   *)
  XML_NAMESPACE_URI   = 'http://www.w3.org/XML/1998/namespace';
  (*
   * URI for prefix "xmlns" and namespace node declarations
   *)
  XMLNS_NAMESPACE_URI = 'http://www.w3.org/2000/xmlns/';


  
  (*
   * If index or size is negative, or greater than the allowed value
  *)
  INDEX_SIZE_ERR = 1;

  (*
   * If the specified range of text does not fit into a DOMString
  *)
  DOMSTRING_SIZE_ERR = 2;

  (*
   * If any node is inserted somewhere it doesn't belong
  *)
  HIERARCHY_REQUEST_ERR = 3;

  (*
   * If a node is used in a different document than the one that created it
   * (that doesn't support it)
  *)
  WRONG_DOCUMENT_ERR = 4;

  (*
   * If an invalid or illegal character is specified, such as in a name.
   * See production 2 in the XML specification for the definition of a legal
   * character, and production 5 for the definition of a legal name character.
  *)
  INVALID_CHARACTER_ERR = 5;

  (*
   * If data is specified for a node which does not support data
  *)
  NO_DATA_ALLOWED_ERR = 6;

  (*
   * If an attempt is made to modify an object where modifications are not
   * allowed
  *)
  NO_MODIFICATION_ALLOWED_ERR = 7;

  (*
   * If an attempt is made to reference a node in a context where it does not
   * exist
  *)
  NOT_FOUND_ERR = 8;

  (*
   * If the implementation does not support the requested type of object or
   * operation
  *)
  NOT_SUPPORTED_ERR = 9;

  (*
   * If an attempt is made to add an attribute that is already in use elsewhere
  *)
  INUSE_ATTRIBUTE_ERR = 10;

  (*
   * If an attempt is made to use an object that is not, or is no longer, usable
  *)
  INVALID_STATE_ERR = 11;

  (*
   * If an invalid or illegal string is specified
  *)
  SYNTAX_ERR = 12;

  (*
   * If an attempt is made to modify the type of the underlying object
  *)
  INVALID_MODIFICATION_ERR = 13;

  (*
   * If an attempt is made to create or change an object in a way which is
   * incorrect with regard to namespaces
  *)
  NAMESPACE_ERR = 14;

  (*
   * If a parameter or an operation is not supported by the underlying object
  *)
  INVALID_ACCESS_ERR = 15;


  (* NON standard Exception codes *)

  (*
   * The DOM specs state:
   *    Note: Other numeric codes are reserved for W3C for possible future use.
   * which is problematic if new (non standard) exception codes should be added.
   * We therfore choose to have our own exception codes >= 1000 in the hope it
   * will not break future DOM specs. To minimize any inconveniance in the
   * future you should make sure not to rely on the numeric constant value but
   * always use the constant name declaration so the numeric value can be
   * changed without breaking existing code.
  *)

  PARSE_ERR = 1000;


  (*
   * The official DOM specs works with Integer values for Node Types.
   * These Integer values are provided for supporting DOM implementations
   * that work with Integer values. Conversion routines are provided to convert
   * from TNodeType <-> Integer
  *)
  (*
   * The node is an IDomElement
  *)
  ELEMENT_NODE = 1;

  (*
   * The node is an IDomAttr
  *)
  ATTRIBUTE_NODE = 2;

  (*
   * The node is a IDomText node
  *)
  TEXT_NODE = 3;

  (*
   * The node is a IDomCDATASection
  *)
  CDATA_SECTION_NODE = 4;

  (*
   * The node is an IDomEntityReference
  *)
  ENTITY_REFERENCE_NODE = 5;

  (*
   * The node is an IDomEntity
  *)
  ENTITY_NODE = 6;

  (*
   * The node is a IDomProcessingInstruction
  *)
  PROCESSING_INSTRUCTION_NODE = 7;

  (*
   * The node is a IDomComment
  *)
  COMMENT_NODE = 8;

  (*
   * The node is a IDomDocument
  *)
  DOCUMENT_NODE = 9;

  (*
   * The node is a IDomDocumentType
  *)
  DOCUMENT_TYPE_NODE = 10;

  (*
   * The node is a IDomDocumentFragment
  *)
  DOCUMENT_FRAGMENT_NODE = 11;

  (*
   * The node is a IDomNotation
  *)
  NOTATION_NODE = 12;

type

  DomString    = WideString;
  DomTimeStamp = Int64;

  DomNodeType  = Integer;
  DomExceptionType = Integer;

  EDomException = class(Exception)
    private
      fCode : DomExceptionType;
    public
      constructor create(code : DomExceptionType; const msg : DomString); overload;
      constructor createFmt(
              code       : DomExceptionType;
              const msg  : string;
              const args : array of const); overload;
      property code : DomExceptionType read fCode;
  end;

  type TAsyncEventHandler = procedure(
          sender         : TObject;
          asyncLoadState : Integer) of object;

  IDomDocumentType = interface;
  IDomDocument     = interface;
  IDomNodeList     = interface;
  IDomNamedNodeMap = interface;
  IDomElement      = interface;

  IDomImplementation  = interface
    ['{A372B60C-C953-4D93-8DAD-EBCB76A8D3F9}']

    (*
      @param Feature [in]
      @param Version [in]
    *)
    function hasFeature(
            const feature : DomString;
            const version : DomString) : Boolean;
    (*
      @param QualifiedName [in]
      @param PublicId [in]
      @param SystemId [in]
      @Raises EDomException
    *)
    function createDocumentType(
            const qualifiedName : DomString;
            const publicId      : DomString;
            const systemId      : DomString) : IDomDocumentType;

    (*
     * @param NamespaceURI [in]
     * @param QualifiedName [in]
     * @param DocType [in]
     * @Raises EDomException
    *)
    function createDocument(
            const namespaceURI  : DomString;
            const qualifiedName : DomString;
            docType             : IDomDocumentType) : IDomDocument;
  end;

  IDomNode = interface
    ['{D415EB3C-463D-4F6D-BD1F-168B8A364666}']
    {property setters/getters}

    function  get_NodeName : DomString;

    (*
     * @param Value [in]
     * @Raises EDomException
    *)
    procedure set_NodeValue(const value : DomString);

    (*
     * @Raises EDomException
    *)
    function  get_NodeValue : DomString;
    function  get_NodeType : DomNodeType;
    function  get_ParentNode : IDomNode;
    function  get_ChildNodes : IDomNodeList;
    function  get_FirstChild : IDomNode;
    function  get_LastChild : IDomNode;
    function  get_PreviousSibling : IDomNode;
    function  get_NextSibling : IDomNode;
    function  get_Attributes : IDomNamedNodeMap;
    function  get_OwnerDocument : IDomDocument;
    function  get_NamespaceURI : DomString;

    (*
     * @param Prefix [in]
     * @Raises EDomException
    *)
    procedure set_Prefix(const prefix : DomString);
    function  get_Prefix : DomString;

    function  get_LocalName : DomString;

    {methods}

    (*
     * @param NewChild [in]
     * @param RefChild [in]
     * @Raises EDomException
    *)
    function  insertBefore(const newChild, refChild : IDomNode) : IDomNode;

    (*
     * @param NewChild [in]
     * @param OldChild [in]
     * @Raises EDomException
    *)
    function  replaceChild(const newChild, oldChild : IDomNode) : IDomNode;

    (*
     * @param OldChild [in]
     * @Raises EDomException
    *)
    function  removeChild(const oldChild : IDomNode) : IDomNode;

    (*
     * @param NewChild [in]
     * @Raises EDomException
    *)
    function  appendChild(const newChild : IDomNode) : IDomNode;

    function  hasChildNodes : Boolean;

    function  hasAttributes : Boolean;

    (*
     * @param Deep [in]
    *)
    function  cloneNode(deep : Boolean) : IDomNode;

    procedure normalize;

    (*
     * @param Feature [in]
     * @param Version [in]
    *)
    function  isSupported(
            const feature : DomString;
            const version : DomString) : Boolean;

    {properties}
    property nodeName        : DomString read get_NodeName;

    (*
     * @Raises EDomException
    *)
    property nodeValue       : DomString read get_NodeValue write set_NodeValue;
    property nodeType        : DomNodeType read get_NodeType;
    property parentNode      : IDomNode read get_ParentNode;
    property childNodes      : IDomNodeList read get_ChildNodes;
    property firstChild      : IDomNode read get_FirstChild;
    property lastChild       : IDomNode read get_LastChild;
    property previousSibling : IDomNode read get_PreviousSibling;
    property nextSibling     : IDomNode read get_NextSibling;
    property attributes      : IDomNamedNodeMap read get_Attributes;
    property ownerDocument   : IDomDocument read get_OwnerDocument;
    property namespaceURI    : DomString read get_NamespaceURI;

    (*
     * @Raises EDomException
    *)
    property prefix          : DomString read get_Prefix write set_Prefix;
    property localName       : DomString read get_LocalName;
  end;


  IDomNodeList = interface
    ['{9CA29D2D-9B7B-40F2-913D-440FECE581BE}']
    {property setters/getters}
    function  get_Length : Integer;

    {methods}

    (*
     * @Param Index [in]
    *)
    function  get_Item(index : Integer) : IDomNode;

    {properties}
    property length : Integer read get_Length;
    property item[index : Integer] : IDomNode read get_Item; default;

  end;

  IDomNamedNodeMap = interface
    ['{B8879EB5-E22F-4F75-A4D3-D83DD2380D2D}']
    {property setters/getters}

    (*
     * @Param Index [in]
    *)
    function  get_Item(index : Integer) : IDomNode;
    function  get_Length : Integer;

    {methods}

    (*
     * @Param Name [in]
    *)
    function  getNamedItem(const name : DomString) : IDomNode;

    (*
     * @Param Arg [in]
     * @Raises EDomException
    *)
    function  setNamedItem(const newItem : IDomNode) : IDomNode;

    (*
     * @Param Name [in]
     * @Raises EDomException
    *)
    function  removeNamedItem(const name : DomString) : IDomNode;

    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
     * @Raises EDomException
    *)
    function  getNamedItemNS(
            const namespaceURI : DomString;
            const localName    : DomString) : IDomNode;

    (*
     * @Param Arg [in]
     * @Raises EDomException
    *)
    function  setNamedItemNS(const NewItem : IDomNode) : IDomNode;

    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
     * @Raises EDomException
    *)
    function  removeNamedItemNS(
            const namespaceURI : DomString;
            const localName    : DomString) : IDomNode;

    {properties}
    property item[index : Integer] : IDomNode read get_Item; default;
    property namedItem[const name : DomString] : IDomNode read getNamedItem;
    property length : Integer read get_Length;
  end;


  IDomCharacterData = interface(IDomNode)
    ['{2FE51653-C541-40FB-8962-9150E40211A6}']
    {property setters/getters}

    (*
     * @Param [in] Data
     * @Raises EDomException
    *)
    procedure set_Data(const data : DomString);

    (*
     * @Raises EDomException
    *)
    function  get_Data : DomString;

    function  get_Length : Integer;

    {methods}

    (*
     * @Param Offset [in]
     * @Param Count [in]
     * @Raises EDomException
    *)
    function  subStringData(offset : Integer; count : Integer) : DomString;
    (*
     * @Param Arg [in]
     * @Raises EDomException
    *)
    procedure appendData(const arg : DomString);

    (*
     * @Param Offset [in]
     * @Param Arg [in]
     * @Raises EDomException
    *)
    procedure insertData(offset : Integer; const arg : DomString);

    (*
     * @Param Offset [in]
     * @Param Count [in]
     * @Raises EDomException
    *)
    procedure deleteData(offset : Integer; count : Integer);

    (*
     * @Param Offset [in]
     * @Param Count [in]
     * @Param Arg [in]
     * @Raises EDomException
    *)
    procedure replaceData(
            offset    : Integer;
            count     : Integer;
            const arg : DomString);

    {properties}

    (*
     * @Raises EDomException
    *)
    property data : DOMString read get_Data write set_Data;

    property length : Integer read get_Length;
  end;

  IDomAttr = interface(IDomNode)
    ['{AD6B078B-C1D0-461A-AD51-45E7D72370E2}']

    {property setters/getters}

    function  get_Name : DomString;
    function  get_Specified : Boolean;

    (*
     * @Param Value [in]
     * @Raises EDomException
    *)
    procedure set_Value(const value : DomString);
    function  get_Value : DomString;
    function  get_OwnerElement : IDomElement;

    {properties}

    property name : DomString read get_Name;
    property specified : Boolean read get_Specified;
    (*
     * @Raises EDomException on write
    *)
    property value : DomString read get_Value write set_Value;
    property ownerElement : IDomElement read get_OwnerElement;
  end;

  IDomElement = interface(IDomNode)
    ['{955D5EEC-6160-4AC9-ADFB-767E6AC09511}']

    {property setters/getters}
    function  get_TagName : DomString;

    {methods}

    (**
     * @Param [in] Name
    *)
    function  getAttribute(const name : DomString) : DomString;

    (**
     * @Raises EDomException
    *)
    procedure setAttribute(const name : DomString; const value : DomString);

    (**
     * @Param [in] Name
     * @Raises EDomException
    *)
    procedure removeAttribute(const name : DomString);

    (**
     * @Param Name [in]
     * @Raises EDomException
    *)
    function  getAttributeNode(const name : DomString) : IDomAttr;

    (**
     * @Param NewAttr [in]
     * @Raises EDomException
    *)
    function setAttributeNode(const newAttr : IDomAttr) : IDomAttr;

    (**
     * @Param OldAttr [in]
     * @Raises EDomException
    *)
    function removeAttributeNode(const oldAttr : IDomAttr) : IDomAttr;

    (**
     * @Param Name [in]
    *)
    function  getElementsByTagName(const name : DomString) : IDomNodeList;

    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
    *)
    function  getAttributeNS(
            const namespaceURI : DomString;
            const localName    : DomString) : DomString;

    (*
     * @Param NamespaceURI [in]
     * @Param QualifiedName [in]
     * @Param Value [in]
     * @Raises EDomException
    *)
    procedure setAttributeNS(
            const namespaceURI  : DomString;
            const qualifiedName : DomString;
            const value         : DomString);

    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
     * @Raises EDomException
    *)
    procedure removeAttributeNS(
            const namespaceURI : DomString;
            const localName    : DomString);
    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
    *)
    function  getAttributeNodeNS(
            const namespaceURI : DomString;
            const localName    : DomString) : IDomAttr;

    (*
     * @Param NewAttr [in]
     * @Raises EDomException
    *)
    function  setAttributeNodeNS(const newAttr : IDomAttr) : IDomAttr;

    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
    *)
    function  getElementsByTagNameNS(
            const namespaceURI : DomString;
            const localName    : DomString) : IDomNodeList;

    (*
     * @Param Name [in]
    *)
    function  hasAttribute(const name : DomString) : Boolean;

    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
    *)
    function  hasAttributeNS(
            const namespaceURI : DomString;
            const localName    : DomString) : Boolean;

    {properties}
    property tagName : DomString  read get_TagName;
  end;

  IDomText = interface(IDomCharacterData)
    ['{61D2EAAC-284E-4B5B-8C34-66CD54C1AE29}']

    {methods}

    (*
     * @Param Offset [in]
     * @Raises EDomException
    *)
    function splitText(offset : Integer) : IDomText;
  end;

  IDomComment = interface(IDomCharacterData)
    ['{18F226C0-75D3-41FA-980B-580E26F2F028}']
  end;

  IDomCDataSection = interface(IDomText)
    ['{79437C77-C14C-4E4D-96E7-2F1273DC8E71}']
  end;

  IDomDocumentType = interface(IDomNode)
    ['{31C37A3A-82A0-4646-AF50-06683D36841D}']
    {property setters/getters}

    function  get_Name : DomString;
    function  get_Entities : IDomNamedNodeMap;
    function  get_Notations : IDomNamedNodeMap;
    function  get_PublicId : DomString;
    function  get_SystemId : DomString;
    function  get_InternalSubset : DomString;

    {properties}

    property name : DomString read get_Name;
    property entities : IDomNamedNodeMap read get_Entities;
    property notations : IDomNamedNodeMap read get_Notations;
    property publicId : DomString read get_PublicId;
    property systemId : DomString read get_SystemId;
    property internalSubset : DomString read get_InternalSubset;
  end;

  IDomNotation = interface(IDomNode)
    ['{C34654CA-12EC-4254-A2C2-64EAB3B63095}']

    {property setters/getters}

    function  get_PublicId : DomString;
    function  get_SystemId : DomString;

    {properties}

    property publicId : DomString read get_PublicId;
    property systemId : DomString read get_SystemId;
  end;

  IDomEntity = interface(IDomNode)
    ['{89821990-42CB-4762-A409-31CE9D7046CD}']
    {property setters/getters}

    function  get_PublicId : DomString;
    function  get_SystemId : DomString;
    function  get_NotationName : DomString;

    {properties}

    property publicId : DomString read get_PublicId;
    property systemId : DomString read get_SystemId;
    property notationName : DomString read get_NotationName;
  end;

  IDomEntityReference = interface(IDomNode)
    ['{3E331CB7-8E7D-4B72-91D2-173CEC2F0818}']
  end;

  IDomProcessingInstruction = interface(IDomNode)
    ['{D92EF1AA-963C-41AC-8DCA-E59A71C51132}']

    {property setters/getters}

    function  get_Target : DomString;

    (*
     * @Param Data [in]
     * @Raises EDomException
    *)
    procedure set_Data(const data : DomString);
    function  get_Data : DomString;

    {properties}
    property target : DomString read get_Target;
    property data : DomString read get_Data write set_Data;
  end;

  IDomDocumentFragment = interface(IDomNode)
    ['{5C0F44E9-DC84-47AF-A757-18F87A1D9830}']
  end;

  IDomDocument = interface(IDomNode)
    ['{40062327-5F8A-475D-8404-491FE7ED4DA5}']

    {property setters/getters}

    function  get_DocType : IDomDocumentType;
    function  get_DomImplementation : IDomImplementation;
    function  get_DocumentElement : IDomElement;

    {methods}

    (*
     * @Param TagName [in]
     * @Raises EDomException
    *)
    function  createElement(const tagName : DomString) : IDomElement;

    function  createDocumentFragment : IDomDocumentFragment;

    (*
     * @Param Data [in]
    *)
    function  createTextNode(const data : DomString) : IDomText;

    (*
     * @Param Data [in]
    *)
    function  createComment(const data : DomString) : IDomComment;

    (*
     * @Param Data [in]
     * @Raises EDomException
    *)
    function  createCDataSection(const data : DomString) : IDomCDataSection;

    (*
     * @Param Target [in]
     * @Param Data [in]
     * @Raises EDomException
    *)
    function  createProcessingInstruction(
            const target : DomString;
            const data   : DomString) : IDomProcessingInstruction;

    (*
     * @Param Name [in]
     * @Raises EDomException
    *)
    function  createAttribute(const name : DomString) : IDomAttr;

    (*
     * @Param Name [in]
     * @Raises EDomException
    *)
    function  createEntityReference(const name : DomString) :
            IDomEntityReference;

    (*
     * @Param TagName [in]
    *)
    function  getElementsByTagName(const tagName : DomString) : IDomNodeList; //FE

    (*
     * @Param ImportedNode [in]
     * @Param Deep [in]
     * @Raises EDomException
    *)
    function  importNode(importedNode : IDomNode; deep : Boolean) : IDomNode;

    (*
     * @Param NamespaceURI [in]
     * @Param QualifiedName [in]
     * @Raises EDomException
    *)
    function  createElementNS(
            const namespaceURI  : DomString;
            const qualifiedName : DomString) : IDomElement;

    (*
     * @Param NamespaceURI [in]
     * @Param QualifiedName [in]
     * @Raises EDomException
    *)
    function  createAttributeNS(
            const namespaceURI  : DomString;
            const qualifiedName : DomString) : IDomAttr;

    (*
     * @Param NamespaceURI [in]
     * @Param LocalName [in]
     * @Raises EDomException
    *)
    function  getElementsByTagNameNS(
            const namespaceURI : DomString;
            const localName    : DomString) : IDomNodeList; //FE

    (*
     * @Param ElementId [in]
    *)
    function  getElementById(const elementId : DomString) : IDomElement;

    {properties}

    property docType : IDomDocumentType read get_DocType;
    {implementation is a reserved word so DomImplementation is used}
    property domImplementation : IDomImplementation read get_DomImplementation;
    property documentElement : IDomElement read get_DocumentElement;
  end;



  (****************************************************************************
   *   following interfaces are not part of the DOM spec. but are needed to   *
   *   maintain vendor independence in an easy way.                           *
   ****************************************************************************)

  (*
   * Defines the interface to obtain DOM Document instances.
   *)
  IDomDocumentBuilder = interface
    ['{92724EDA-8951-4E46-8415-84221EAE0044}']
    {property setters/getters}
    (* true if DOM supports namespace *)
    function  get_IsNamespaceAware : Boolean;
    (* true if DOM is a validating parser *)
    function  get_IsValidating : Boolean;

    (* true if IDomPersist provides async support *)
    function  get_HasAsyncSupport : Boolean;

    (*
     * true if asbsolute URLs are supported, false if only relative or local
     * URLs are supported
    *)
    function get_HasAbsoluteURLSupport : Boolean;

    {methods}

    function  get_DomImplementation : IDomImplementation;
    function  newDocument : IDomDocument;

    (*
     * Parses the given XML string
     * @Param XML The xml to parse
     * @Returns The newly created document
     * @Raises DomException
     *)
    function  parse(const xml : DomString) : IDomDocument;

    (*
     * Loads and parses XML from url and returns a new document.
     *)
    function load(const url : DomString) : IDomDocument;

    property domImplementation : IDomImplementation read get_DomImplementation;
    (* true if DOM supports namespace *)
    property isNamespaceAware : Boolean read get_IsNamespaceAware;
    (* true if DOM is a validating parser *)
    property isValidating : Boolean read get_IsValidating;
    (* true if IDomPersist provides async support*)
    property hasAsyncSupport : Boolean read get_HasAsyncSupport;
    (*
     * true if asbsolute URLs are supported, false if only relative or local
     * URLs are supported
     *)
    property hasAbsoluteURLSupport : Boolean read get_HasAbsoluteURLSupport;
  end;

  (*
   * DomDocumentBuilder Factory for creating Vendor specified DocumentBuilder.
   *)
  IDomDocumentBuilderFactory = interface
    ['{27E9F2B1-98D6-49D0-AAE4-2B0D2DF128BE}']
    {property setters/getters}
    (* returns the vendorID under which this factory is registered *)
    function get_VendorID : DomString;

    {methods}
    (* creates a new IDomDocumentBuilder *)
    function newDocumentBuilder : IDomDocumentBuilder;

    (* the vendorID under which this factory is registered *)
    property vendorID : DomString read get_VendorID;
  end;


  (**
   * Interface for enumerating vendors.
   *)
  IDomVendorList = interface
    ['{2739F26E-98D6-49D0-AAE4-2B0D2DF128BE}']

    (**
     * @return  number of registered vendors
     *)
    function  get_Count: integer;

    (**
     * Get one of the registered vendors
     * @param aIndex  zero-based index of the factory to retrieve
     * @return  a document builder factory
     *)
    function  get_Item(const aIndex: integer): IDomDocumentBuilderFactory;

    property Count: integer read get_Count;
    property Item[const aIndex: integer]: IDomDocumentBuilderFactory read get_Item;
  end;

  (*
   * Exception class for Vendor Registration
   *)
  EDomVendorRegisterException = class(Exception);

  (*
   * used for registering a DomcumentBuilderFactory
   * @Param AFactory the factory that need to be registered.
   * @Raise EDomVendorRegisterException if a factory has already registered with
   * the same AVendorID.
   *)
  procedure registerDomVendorFactory(factory : IDomDocumentBuilderFactory);

  (*
   * get a DomcumentBuilderFactory based on its Vendor  ID
   * @Param AVendorID the ID that uniquely specifies the DOM implementation
   * @Raise EDomVendorRegisterException if AVendorID does not exist
   *)
  function getDocumentBuilderFactory(vendorID : DomString) : IDomDocumentBuilderFactory;

  (*
   * equivalent to get_DocumentBuilderFactory. for compatibillity with Borland
   *)
  function getDOM(const vendorDesc : string = '') : IDOMImplementation;

  (**
   * provides access to the list of all registered vendors.
   * @return  interface enabling to enumerate vendors.
   *)
  function getDomVendorList: IDomVendorList;

implementation

type

  (*
   * Register for registering different DocumentBuilderFactories. Each
   * DocumentBuilderFactory is identified by a vendorID.
   *)
  TDomVendorRegister = class(TInterfacedObject, IDomVendorList)
    private
      (* list of DocumentBuilderFactories *)
      fFactoryList : TInterfaceList;
    protected //IDomVendorList
      function  get_Count: integer;
      function  get_Item(const aIndex: integer): IDomDocumentBuilderFactory;
    public
      constructor Create;
      destructor Destroy; override;

      (*
       * add a new DocumentBuilderFactory to the list.
       * Pre-condition:
       *   - vendorID must be set
       *   - vendorID must be unique (if not EDomVendorRegisterException)
      *)
      procedure add(domDocumentBuilderFactory : IDomDocumentBuilderFactory);

      (*
       * returns the DomDocumentBuilderFactory with id vendorId
       * if vendorId is not found then result := nil
      *)
      function get_Factory(vendorID : DomString) : IDomDocumentBuilderFactory;
  end;

var
  (*
   * global TDomVendorRegister. Used to register the domDocumentBuilderFactories
  *)
  gDomVendorRegister : TDomVendorRegister;

(******************************************************************************)

{ TDomVendorRegister }

constructor TDomVendorRegister.Create;
begin
  inherited Create;
  fFactoryList := TInterfaceList.Create;
  _AddRef; // one extra lock needed
end;

destructor TDomVendorRegister.Destroy;
begin
  fFactoryList.Free;
  inherited Destroy;
end;

procedure TDomVendorRegister.add(
        domDocumentBuilderFactory : IDomDocumentBuilderFactory);
begin
  {check if a factory with same VendorID is already registered}
  if get_Factory(domDocumentBuilderFactory.vendorID) <> nil then
    Raise EDomVendorRegisterException.create('Vendor ID already present');
  fFactoryList.add(domDocumentBuilderFactory);
end;

function TDomVendorRegister.get_Count: integer;
begin
  Result := fFactoryList.Count;
end;

function TDomVendorRegister.get_Factory(vendorID : DomString) : IDomDocumentBuilderFactory;
var
  i : Integer;
begin
  for i := 0 to fFactoryList.Count - 1 do begin
    Result := fFactoryList.items[i] as IDomDocumentBuilderFactory;
    {check the name}
    if (Result.vendorID = vendorID) then exit;
  end;
  Result := nil;
end;

function TDomVendorRegister.get_Item(const aIndex: integer): IDomDocumentBuilderFactory;
begin
  Result := fFactoryList[aIndex] as IDomDocumentBuilderFactory;
end;

(******************************************************************************)
(*
 * returns the global TDomVendorRegister (create on demand)
 *)
function get_DomVendorRegisterSingleton : TDomVendorRegister;
begin
  if gDomVendorRegister = nil then
  begin
    gDomVendorRegister := TDomVendorRegister.create;
  end;
  result := gDomVendorRegister;
end;

function getDomVendorList: IDomVendorList;
begin
  Result := get_DomVendorRegisterSingleton as IDomVendorList;
end;

(******************************************************************************)

procedure registerDomVendorFactory(factory : IDomDocumentBuilderFactory);
begin
  get_DomVendorRegisterSingleton.add(factory);
end;

(******************************************************************************)

function getDocumentBuilderFactory(
        vendorID : DomString) : IDomDocumentBuilderFactory;
var
  factory : IDomDocumentBuilderFactory;
begin
  factory := get_DomVendorRegisterSingleton.get_Factory(vendorID);
  if factory = nil then
    Raise EDomVendorRegisterException.createFmt(
            'Vendor ID: %s not present', [vendorID]);

  result := factory;
end;

function getDOM(const vendorDesc : string = '') : IDOMImplementation;
begin
  result := getDocumentBuilderFactory(VendorDesc).
      newDocumentBuilder.DOMImplementation;
end;


(******************************************************************************)

constructor EDomException.create(code : DomExceptionType; const msg : DomString);
begin
  inherited create(msg);
  fCode := code;
end;

constructor EDomException.createFmt(
        code       : DomExceptionType;
        const msg  : string;
        const args : array of const);
begin
  inherited createFmt(msg, args);
  fCode := code;
end;

end.







