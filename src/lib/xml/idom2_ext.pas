unit idom2_ext;

interface

(*
 * Interface specifications for extensions to Dom level 2.
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
*)

uses idom2, classes;

type
	

  (*
   * non standard DOM extension for persistency. Asynchronous operations are
   * not always supported (check IDomDocumentBuilder.hasAsyncSupport for
   * availabillity of async operations)
  *)
  IDomPersist = interface
    ['{F644523B-3F88-49BC-9B31-976FE4B8153C}']
    {property setters/getters}
    function get_xml : DOMString;

    { Methods }

     (*
     * Indicates the current state of the XML document. 
    *)
    function  asyncLoadState : Integer;

    (*
     * loads and parses the xml document from URL source.
    *)
    function  load(source: DOMString) : Boolean;
    function  loadFromStream(const stream : TStream) : Boolean;

    (*
     * Loads and parses the given XML string
     * @Param value The xml string to parse
     * @Returns The newly created document
     * @Raises DomException
    *)
    function  loadxml(const value : DOMString) : Boolean;

    procedure save(destination: DOMString);
    procedure saveToStream(const stream : TStream);
    procedure set_OnAsyncLoad(
            const sender : TObject;
            eventHandler : TAsyncEventHandler);

    {properties}
    property xml : DomString read get_xml;
  end;

 
  { IDOMParseOptions }
 
  IDomParseOptions = interface
    ['{FA884EC2-A131-4992-904A-0D71289FB87A}']
    { Property Acessors }
    function get_async : Boolean;
    function get_preserveWhiteSpace : Boolean;
    function get_resolveExternals : Boolean;
    function get_validate : Boolean;
    procedure set_async(value : Boolean);
    procedure set_preserveWhiteSpace(value : Boolean);
    procedure set_resolveExternals(value : Boolean);
    procedure set_validate(value : Boolean);

    { Properties }
    property async : Boolean read get_async write set_async;
    property preserveWhiteSpace : Boolean
            read get_preserveWhiteSpace
            write set_preserveWhiteSpace;
    property resolveExternals : Boolean
            read get_resolveExternals
            write set_resolveExternals;
    property validate : Boolean read get_validate write set_validate;
  end;

  { IDomNodeCompare }

  // this interfaces implements the dom3 method IsSameNode
  // it is neccessary to use it with libxml2, because there can be
  // several interfaces pointing to the same node

  IDomNodeCompare = interface
    ['{ED63440C-6A94-4267-89A9-E093247F10F8}']
    function IsSameNode(node: IDomNode): boolean;
  end;

  {IDomNodeSelect}

  // this interface makes it possible to do xpath queries
  // to optain a nodelist;
  // the nodepath must be a valid xpath-expression according to
  // http://www.w3.org/TR/xpath;
  // an exception is raised, if the result type is not a node or a nodelist
  //
  // if you want to use namespace-prefixes in your xpath expression, you
  // have to register them before using them with the method registerNs
  // if you use msxml, this works with one namespace only
  // libxmldom excepts any number of registered namespaces
  // they are stored in the document, not in the node
  //
  // see also: http://www.zvon.org/xxl/XPathTutorial/General/examples.html
  // @raises: EDomException, SYNTAX_ERROR

  IDomNodeSelect = interface
    ['{A50A05D4-3E67-44CA-9872-C80CD83A47BD}']
    function selectNode(const nodePath : DomString) : IDomNode;
    function selectNodes(const nodePath : DomString) : IDomNodeList;
    procedure registerNs(const prefix : DomString; const uri : DomString);
  end;

  { IDomNodeExt }

  // this interface is similar to the interface IDomNodeEx from Borland,
  // but not the same, therefore a slightly different name is used
  // it provides methods for xslt transformation (transformNode)
  // for accessing the text-value of an element (similar to textcontent in dom3)
  // and for obtaining the string-value of a node (property xml)

  IDomNodeExt = interface(IDomNode)
    ['{1B41AE3F-6365-41FC-AFDD-26BC143F9C0F}']
    { Property Acessors }
    function get_text: DomString;
    function get_xml: DomString;
    procedure set_text(const Value: DomString);
    { Methods }
    procedure transformNode(const stylesheet: IDomNode; var output: DomString); overload;
    procedure transformNode(const stylesheet: IDomNode; var output: IDomDocument);
      overload;
    { Properties }
    property Text: DomString read get_text write set_text;
    property xml: DomString read get_xml;
  end;

  { IDomNodeListExt }

  // this interface is similar to the interface IDomNodeExt
  // and is using for serialization of nodelists

  IDomNodeListExt = interface(IDomNodeList)
    ['{1B41AE3F-6365-41FC-AFDD-26BC143F9C0F}']
    { Property Acessors }
    function get_xml: DomString;
    { Methods }
    { Properties }
    property xml: DomString read get_xml;
  end;

  {IDomOutputOptions}

  // this interface enables using the output-options, provided by libxml2
  // it will be replaced by dom3 methods in the future

  IDomOutputOptions = interface
    ['{B2ECC3F1-CC9B-4445-85C6-3D62638F7835}']
    { Property Acessors }
    function get_prettyPrint: boolean;
    function get_encoding: DomString;
    function get_parsedEncoding: DomString;
    function get_compressionLevel: integer;
    procedure set_prettyPrint(prettyPrint: boolean);
    procedure set_encoding(encoding: DomString);
    procedure set_compressionLevel(compressionLevel: integer);
    { methods }
    { Properties }
    property prettyPrint: boolean read get_prettyPrint write set_prettyPrint;
    property encoding: DomString read get_encoding write set_encoding;
    property parsedEncoding: DomString read get_parsedEncoding;
    property compressionLevel: integer read get_compressionLevel write set_compressionLevel;
  end;

  {IDomDebug}

  // this interface enables it, to get the count of currently existing documents
  // for debugging purposes

  IDomDebug = interface
  ['{D5DE14B0-C454-4E75-B6CE-4E8C07FAC9BA}']
    { Property Acessors }
    function get_doccount: integer;
    procedure set_doccount(doccount: integer);
    { Properties }
    property doccount: integer read get_doccount write set_doccount;
  end;

implementation

end.
