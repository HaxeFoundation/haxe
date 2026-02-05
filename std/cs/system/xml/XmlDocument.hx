package cs.system.xml;

/** Represents an XML document. You can use this class to load, validate, edit, add, and position XML in a document. */
@:native("System.Xml.XmlDocument")
extern class XmlDocument extends cs.system.xml.XmlNode {
	/**
	 * Gets the root  for the document.
	 * @return The  that represents the root of the XML document tree. If no root
	 * exists,  is returned.
	 */
	var DocumentElement(default, never):cs.system.xml.XmlElement;
	/**
	 * Gets the node containing the DOCTYPE declaration.
	 * @return The  containing the DocumentType (DOCTYPE declaration).
	 */
	var DocumentType(default, never):cs.system.xml.XmlDocumentType;
	/**
	 * Gets the  object for the current document.
	 * @return The  object for the current document.
	 */
	var Implementation(default, never):cs.system.xml.XmlImplementation;
	/**
	 * Gets the  associated with this implementation.
	 * @return An  enabling you to get the atomized version of a string within the
	 * document.
	 */
	var NameTable(default, never):cs.system.xml.XmlNameTable;
	/**
	 * Gets or sets a value indicating whether to preserve white space in element
	 * content.
	 * @return to preserve white space; otherwise . The default is .
	 */
	var PreserveWhitespace(default, default):Bool;
	/**
	 * Gets or sets the  object associated with this .
	 * @return An  object containing the XML Schema Definition Language (XSD) schemas
	 * associated with this ; otherwise, an empty  object.
	 */
	var Schemas(default, default):cs.system.xml.schema.XmlSchemaSet;
	/**
	 * Sets the  to use for resolving external resources.
	 * @return The  to use. In version 1.1 of the.NET Framework, the caller must be
	 * fully trusted in order to specify an .
	 */
	var XmlResolver(never, default):cs.system.xml.XmlResolver;
	@:overload(function():Void {})
	function new(nt:cs.system.xml.XmlNameTable):Void;
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself.
	 * @return The cloned  node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	@:overload(function(name:String):cs.system.xml.XmlAttribute {})
	@:overload(function(qualifiedName:String, namespaceURI:String):cs.system.xml.XmlAttribute {})
	/**
	 * Creates an  with the specified .
	 * @param name The qualified name of the attribute. If the name contains a colon,
	 * the  property reflects the part of the name preceding the first colon and the 
	 * property reflects the part of the name following the first colon. The  remains
	 * empty unless the prefix is a recognized built-in prefix such as xmlns. In this
	 * case  has a value of .
	 * @return The new .
	 */
	function CreateAttribute(prefix:String, localName:String, namespaceURI:String):cs.system.xml.XmlAttribute;
	/**
	 * Creates an  containing the specified data.
	 * @param data The content of the new .
	 * @return The new .
	 */
	function CreateCDataSection(data:String):cs.system.xml.XmlCDataSection;
	/**
	 * Creates an  containing the specified data.
	 * @param data The content of the new .
	 * @return The new .
	 */
	function CreateComment(data:String):cs.system.xml.XmlComment;
	/**
	 * Creates an .
	 * @return The new .
	 */
	function CreateDocumentFragment():cs.system.xml.XmlDocumentFragment;
	/**
	 * Returns a new  object.
	 * @param name Name of the document type.
	 * @param publicId The public identifier of the document type or . You can specify
	 * a public URI and also a system identifier to identify the location of the
	 * external DTD subset.
	 * @param systemId The system identifier of the document type or . Specifies the
	 * URL of the file location for the external DTD subset.
	 * @param internalSubset The DTD internal subset of the document type or .
	 * @return The new .
	 */
	function CreateDocumentType(name:String, publicId:String, systemId:String, internalSubset:String):cs.system.xml.XmlDocumentType;
	@:overload(function(name:String):cs.system.xml.XmlElement {})
	@:overload(function(qualifiedName:String, namespaceURI:String):cs.system.xml.XmlElement {})
	/**
	 * Creates an element with the specified name.
	 * @param name The qualified name of the element. If the name contains a colon then
	 * the  property reflects the part of the name preceding the colon and the 
	 * property reflects the part of the name after the colon. The qualified name
	 * cannot include a prefix of 'xmlns'.
	 * @return The new .
	 */
	function CreateElement(prefix:String, localName:String, namespaceURI:String):cs.system.xml.XmlElement;
	/**
	 * Creates an  with the specified name.
	 * @param name The name of the entity reference.
	 * @return The new .
	 */
	function CreateEntityReference(name:String):cs.system.xml.XmlEntityReference;
	/**
	 * Creates a new  object for navigating this document.
	 * @return An  object.
	 */
	function CreateNavigator():cs.system.xml.xpath.XPathNavigator;
	@:overload(function(nodeTypeString:String, name:String, namespaceURI:String):cs.system.xml.XmlNode {})
	@:overload(function(type:cs.system.xml.XmlNodeType, name:String, namespaceURI:String):cs.system.xml.XmlNode {})
	/**
	 * Creates an  with the specified node type, , and .
	 * @param nodeTypeString String version of the  of the new node. This parameter
	 * must be one of the values listed in the table below.
	 * @param name The qualified name of the new node. If the name contains a colon, it
	 * is parsed into  and  components.
	 * @param namespaceURI The namespace URI of the new node.
	 * @return The new .
	 */
	function CreateNode(type:cs.system.xml.XmlNodeType, prefix:String, name:String, namespaceURI:String):cs.system.xml.XmlNode;
	/**
	 * Creates an  with the specified name and data.
	 * @param target The name of the processing instruction.
	 * @param data The data for the processing instruction.
	 * @return The new .
	 */
	function CreateProcessingInstruction(target:String, data:String):cs.system.xml.XmlProcessingInstruction;
	/**
	 * Creates an  node.
	 * @param text The string must contain only the following characters &#20; &#10;
	 * &#13; and &#9;
	 * @return A new  node.
	 */
	function CreateSignificantWhitespace(text:String):cs.system.xml.XmlSignificantWhitespace;
	/**
	 * Creates an  with the specified text.
	 * @param text The text for the Text node.
	 * @return The new  node.
	 */
	function CreateTextNode(text:String):cs.system.xml.XmlText;
	/**
	 * Creates an  node.
	 * @param text The string must contain only the following characters &#20; &#10;
	 * &#13; and &#9;
	 * @return A new  node.
	 */
	function CreateWhitespace(text:String):cs.system.xml.XmlWhitespace;
	/**
	 * Creates an  node with the specified values.
	 * @param version The version must be "1.0".
	 * @param encoding The value of the encoding attribute. This is the encoding that
	 * is used when you save the  to a file or a stream; therefore, it must be set to a
	 * string supported by the  class, otherwise  fails. If this is  or String.Empty,
	 * the  method does not write an encoding attribute on the XML declaration and
	 * therefore the default encoding, UTF-8, is used. Note: If the  is saved to either
	 * a  or an , this encoding value is discarded. Instead, the encoding of the  or
	 * the  is used. This ensures that the XML written out can be read back using the
	 * correct encoding.
	 * @param standalone The value must be either "yes" or "no". If this is  or
	 * String.Empty, the  method does not write a standalone attribute on the XML
	 * declaration.
	 * @return The new  node.
	 */
	function CreateXmlDeclaration(version:String, encoding:String, standalone:String):cs.system.xml.XmlDeclaration;
	/**
	 * Gets the  with the specified ID.
	 * @param elementId The attribute ID to match.
	 * @return The  with the matching ID or  if no matching element is found.
	 */
	function GetElementById(elementId:String):cs.system.xml.XmlElement;
	@:overload(function(name:String):cs.system.xml.XmlNodeList {})
	/**
	 * Returns an  containing a list of all descendant elements that match the
	 * specified .
	 * @param name The qualified name to match. It is matched against the  property of
	 * the matching node. The special value "*" matches all tags.
	 * @return An  containing a list of all matching nodes. If no nodes match , the
	 * returned collection will be empty.
	 */
	function GetElementsByTagName(localName:String, namespaceURI:String):cs.system.xml.XmlNodeList;
	/**
	 * Imports a node from another document to the current document.
	 * @param node The node being imported.
	 * @param deep to perform a deep clone; otherwise, .
	 * @return The imported .
	 */
	function ImportNode(node:cs.system.xml.XmlNode, deep:Bool):cs.system.xml.XmlNode;
	@:overload(function(inStream:cs.system.io.Stream):Void {})
	@:overload(function(txtReader:cs.system.io.TextReader):Void {})
	@:overload(function(filename:String):Void {})
	/**
	 * Loads the XML document from the specified stream.
	 * @param inStream The stream containing the XML document to load.
	 */
	function Load(reader:cs.system.xml.XmlReader):Void;
	/**
	 * Loads the XML document from the specified string.
	 * @param xml String containing the XML document to load.
	 */
	function LoadXml(xml:String):Void;
	/**
	 * Creates an  object based on the information in the . The reader must be
	 * positioned on a node or attribute.
	 * @param reader The XML source
	 * @return The new  or  if no more nodes exist.
	 */
	function ReadNode(reader:cs.system.xml.XmlReader):cs.system.xml.XmlNode;
	@:overload(function(outStream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(filename:String):Void {})
	/**
	 * Saves the XML document to the specified stream.
	 * @param outStream The stream to which you want to save.
	 */
	function Save(w:cs.system.xml.XmlWriter):Void;
	@:overload(function(validationEventHandler:cs.system.xml.schema.ValidationEventHandler):Void {})
	/**
	 * Validates the  against the XML Schema Definition Language (XSD) schemas
	 * contained in the  property.
	 * @param validationEventHandler The  object that receives information about schema
	 * validation warnings and errors.
	 */
	function Validate(validationEventHandler:cs.system.xml.schema.ValidationEventHandler, nodeToValidate:cs.system.xml.XmlNode):Void;
	/**
	 * Saves all the children of the  node to the specified .
	 * @param xw The  to which you want to save.
	 */
	function WriteContentTo(xw:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the  node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
