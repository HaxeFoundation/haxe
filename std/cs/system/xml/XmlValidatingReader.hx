package cs.system.xml;

/** Represents a reader that provides document type definition (DTD), XML-Data Reduced (XDR) schema, and XML Schema definition language (XSD) validation. This class is obsolete. Starting with the .NET Framework 2.0, we recommend that you use the  class and the  method to create a validating XML reader. */
@:native("System.Xml.XmlValidatingReader")
extern class XmlValidatingReader extends cs.system.xml.XmlReader {
	/**
	 * Gets the encoding attribute for the document.
	 * @return The encoding value. If no encoding attribute exists, and there is not
	 * byte-order mark, this defaults to UTF-8.
	 */
	var Encoding(default, never):cs.system.text.Encoding;
	/**
	 * Gets or sets a value that specifies how the reader handles entities.
	 * @return One of the  values. If no  is specified, it defaults to
	 * EntityHandling.ExpandEntities.
	 */
	var EntityHandling(default, default):cs.system.xml.EntityHandling;
	/**
	 * Gets the current line number.
	 * @return The current line number. The starting value for this property is 1.
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the current line position.
	 * @return The current line position. The starting value for this property is 1.
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets or sets a value indicating whether to do namespace support.
	 * @return to do namespace support; otherwise, . The default is .
	 */
	var Namespaces(default, default):Bool;
	/**
	 * Gets the  used to construct this .
	 * @return The  specified in the constructor.
	 */
	var Reader(default, never):cs.system.xml.XmlReader;
	/**
	 * Gets a  to use for validation.
	 * @return The  to use for validation.
	 */
	var Schemas(default, never):cs.system.xml.schema.XmlSchemaCollection;
	/**
	 * Gets a schema type object.
	 * @return , , or  depending whether the node value is a built in XML Schema
	 * definition language (XSD) type or a user defined simpleType or complexType;  if
	 * the current node has no schema type.
	 */
	var SchemaType(default, never):Dynamic;
	/**
	 * Gets or sets a value indicating the type of validation to perform.
	 * @return One of the  values. If this property is not set, it defaults to
	 * ValidationType.Auto.
	 */
	var ValidationType(default, default):cs.system.xml.ValidationType;
	/**
	 * Sets the  used for resolving external document type definition (DTD) and schema
	 * location references. The  is also used to handle any import or include elements
	 * found in XML Schema definition language (XSD) schemas.
	 * @return The  to use. If set to , external resources are not resolved. In version
	 * 1.1 of the .NET Framework, the caller must be fully trusted to specify an .
	 */
	var XmlResolver(never, default):cs.system.xml.XmlResolver;
	@:overload(function(reader:cs.system.xml.XmlReader):Void {})
	@:overload(function(xmlFragment:cs.system.io.Stream, fragType:cs.system.xml.XmlNodeType, context:cs.system.xml.XmlParserContext):Void {})
	function new(xmlFragment:String, fragType:cs.system.xml.XmlNodeType, context:cs.system.xml.XmlParserContext):Void;
	/** Changes the  to Closed. */
	function Close():Void;
	@:overload(function(i:Int):String {})
	@:overload(function(name:String):String {})
	/**
	 * Gets the value of the attribute with the specified index.
	 * @param i The index of the attribute. The index is zero-based. (The first
	 * attribute has index 0.)
	 * @return The value of the specified attribute.
	 */
	function GetAttribute(localName:String, namespaceURI:String):String;
	/**
	 * Gets a value indicating whether the class can return line information.
	 * @return if the class can return line information; otherwise, .
	 */
	function HasLineInfo():Bool;
	/**
	 * Resolves a namespace prefix in the current element's scope.
	 * @param prefix The prefix whose namespace Uniform Resource Identifier (URI) you
	 * want to resolve. To match the default namespace, pass an empty string.
	 * @return The namespace URI to which the prefix maps or  if no matching prefix is
	 * found.
	 */
	function LookupNamespace(prefix:String):String;
	@:overload(function(i:Int):Void {})
	@:overload(function(name:String):Bool {})
	/**
	 * Moves to the attribute with the specified index.
	 * @param i The index of the attribute.
	 */
	function MoveToAttribute(localName:String, namespaceURI:String):Bool;
	/**
	 * Moves to the element that contains the current attribute node.
	 * @return if the reader is positioned on an attribute (the reader moves to the
	 * element that owns the attribute);  if the reader is not positioned on an
	 * attribute (the position of the reader does not change).
	 */
	function MoveToElement():Bool;
	/**
	 * Moves to the first attribute.
	 * @return if an attribute exists (the reader moves to the first attribute);
	 * otherwise,  (the position of the reader does not change).
	 */
	function MoveToFirstAttribute():Bool;
	/**
	 * Moves to the next attribute.
	 * @return if there is a next attribute;  if there are no more attributes.
	 */
	function MoveToNextAttribute():Bool;
	/**
	 * Reads the next node from the stream.
	 * @return if the next node was read successfully;  if there are no more nodes to
	 * read.
	 */
	function Read():Bool;
	/**
	 * Parses the attribute value into one or more , , or  nodes.
	 * @return if there are nodes to return. if the reader is not positioned on an
	 * attribute node when the initial call is made or if all the attribute values have
	 * been read. An empty attribute, such as, misc="", returns  with a single node
	 * with a value of String.Empty.
	 */
	function ReadAttributeValue():Bool;
	/**
	 * Reads the content and returns the Base64 decoded binary bytes.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadContentAsBase64(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	/**
	 * Reads the content and returns the BinHex decoded binary bytes.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadContentAsBinHex(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	/**
	 * Reads the element and decodes the Base64 content.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadElementContentAsBase64(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	/**
	 * Reads the element and decodes the BinHex content.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadElementContentAsBinHex(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	/**
	 * Reads the contents of an element or text node as a string.
	 * @return The contents of the element or text node. This can be an empty string if
	 * the reader is positioned on something other than an element or text node, or if
	 * there is no more text content to return in the current context. The text node
	 * can be either an element or an attribute text node.
	 */
	function ReadString():String;
	/**
	 * Gets the common language runtime type for the specified XML Schema definition
	 * language (XSD) type.
	 * @return The common language runtime type for the specified XML Schema type.
	 */
	function ReadTypedValue():Dynamic;
	/** Resolves the entity reference for  nodes. */
	function ResolveEntity():Void;
}
