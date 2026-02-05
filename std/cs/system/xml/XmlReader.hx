package cs.system.xml;

/** Represents a reader that provides fast, noncached, forward-only access to XML data. */
@:native("System.Xml.XmlReader")
extern class XmlReader {
	/**
	 * When overridden in a derived class, gets the number of attributes on the current
	 * node.
	 * @return The number of attributes on the current node.
	 */
	var AttributeCount(default, never):Int;
	/**
	 * When overridden in a derived class, gets the base URI of the current node.
	 * @return The base URI of the current node.
	 */
	var BaseURI(default, never):String;
	/**
	 * Gets a value indicating whether the  implements the binary content read methods.
	 * @return if the binary content read methods are implemented; otherwise .
	 */
	var CanReadBinaryContent(default, never):Bool;
	/**
	 * Gets a value indicating whether the  implements the  method.
	 * @return if the  implements the  method; otherwise .
	 */
	var CanReadValueChunk(default, never):Bool;
	/**
	 * Gets a value indicating whether this reader can parse and resolve entities.
	 * @return if the reader can parse and resolve entities; otherwise, .
	 */
	var CanResolveEntity(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the depth of the current node in the
	 * XML document.
	 * @return The depth of the current node in the XML document.
	 */
	var Depth(default, never):Int;
	/**
	 * When overridden in a derived class, gets a value indicating whether the reader
	 * is positioned at the end of the stream.
	 * @return if the reader is positioned at the end of the stream; otherwise, .
	 */
	var EOF(default, never):Bool;
	/**
	 * Gets a value indicating whether the current node has any attributes.
	 * @return if the current node has attributes; otherwise, .
	 */
	var HasAttributes(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * node can have a .
	 * @return if the node on which the reader is currently positioned can have a ;
	 * otherwise, . If , the node has a value of .
	 */
	var HasValue(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * node is an attribute that was generated from the default value defined in the
	 * DTD or schema.
	 * @return if the current node is an attribute whose value was generated from the
	 * default value defined in the DTD or schema;  if the attribute value was
	 * explicitly set.
	 */
	var IsDefault(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * node is an empty element (for example, <MyElement/>).
	 * @return if the current node is an element ( equals ) that ends with />;
	 * otherwise, .
	 */
	var IsEmptyElement(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the local name of the current node.
	 * @return The name of the current node with the prefix removed. For example,  is 
	 * for the element <bk:book>. For node types that do not have a name (like , , and
	 * so on), this property returns .
	 */
	var LocalName(default, never):String;
	/**
	 * When overridden in a derived class, gets the qualified name of the current node.
	 * @return The qualified name of the current node. For example,  is  for the
	 * element <bk:book>. The name returned is dependent on the  of the node. The
	 * following node types return the listed values. All other node types return an
	 * empty string. Node type Name The name of the attribute. The document type name.
	 * The tag name. The name of the entity referenced. The target of the processing
	 * instruction. The literal string .
	 */
	var Name(default, never):String;
	/**
	 * When overridden in a derived class, gets the namespace URI (as defined in the
	 * W3C Namespace specification) of the node on which the reader is positioned.
	 * @return The namespace URI of the current node; otherwise an empty string.
	 */
	var NamespaceURI(default, never):String;
	/**
	 * When overridden in a derived class, gets the  associated with this
	 * implementation.
	 * @return The  enabling you to get the atomized version of a string within the
	 * node.
	 */
	var NameTable(default, never):cs.system.xml.XmlNameTable;
	/**
	 * When overridden in a derived class, gets the type of the current node.
	 * @return One of the enumeration values that specify the type of the current node.
	 */
	var NodeType(default, never):cs.system.xml.XmlNodeType;
	/**
	 * When overridden in a derived class, gets the namespace prefix associated with
	 * the current node.
	 * @return The namespace prefix associated with the current node.
	 */
	var Prefix(default, never):String;
	/**
	 * When overridden in a derived class, gets the quotation mark character used to
	 * enclose the value of an attribute node.
	 * @return The quotation mark character (" or ') used to enclose the value of an
	 * attribute node.
	 */
	var QuoteChar(default, never):cs.Char16;
	/**
	 * When overridden in a derived class, gets the state of the reader.
	 * @return One of the enumeration values that specifies the state of the reader.
	 */
	var ReadState(default, never):cs.system.xml.ReadState;
	/**
	 * Gets the schema information that has been assigned to the current node as a
	 * result of schema validation.
	 * @return An  object containing the schema information for the current node.
	 * Schema information can be set on elements, attributes, or on text nodes with a
	 * non-null  (typed values). If the current node is not one of the above node
	 * types, or if the  instance does not report schema information, this property
	 * returns . If this property is called from an  or an  object, this property
	 * always returns . These  implementations do not expose schema information through
	 * the  property. If you have to get the post-schema-validation information set
	 * (PSVI) for an element, position the reader on the end tag of the element, rather
	 * than on the start tag. You get the PSVI through the  property of a reader. The
	 * validating reader that is created through  with the  property set to  has
	 * complete PSVI for an element only when the reader is positioned on the end tag
	 * of an element.
	 */
	var SchemaInfo(default, never):cs.system.xml.schema.IXmlSchemaInfo;
	/**
	 * Gets the  object used to create this  instance.
	 * @return The  object used to create this reader instance. If this reader was not
	 * created using the  method, this property returns .
	 */
	var Settings(default, never):cs.system.xml.XmlReaderSettings;
	/**
	 * When overridden in a derived class, gets the text value of the current node.
	 * @return The value returned depends on the  of the node. The following table
	 * lists node types that have a value to return. All other node types return . Node
	 * type Value The value of the attribute. The content of the CDATA section. The
	 * content of the comment. The internal subset. The entire content, excluding the
	 * target. The white space between markup in a mixed content model. The content of
	 * the text node. The white space between markup. The content of the declaration.
	 */
	var Value(default, never):String;
	/**
	 * Gets The Common Language Runtime (CLR) type for the current node.
	 * @return The CLR type that corresponds to the typed value of the node. The
	 * default is .
	 */
	var ValueType(default, never):cs.system.Type;
	/**
	 * When overridden in a derived class, gets the current  scope.
	 * @return The current  scope.
	 */
	var XmlLang(default, never):String;
	/**
	 * When overridden in a derived class, gets the current  scope.
	 * @return One of the  values. If no  scope exists, this property defaults to .
	 */
	var XmlSpace(default, never):cs.system.xml.XmlSpace;
	@:overload(function(index0:Int):String {})
	@:overload(function(index0:String):String {})
	@:native("get_Item")
	function get_Item(index0:String, index1:String):String;
	@:overload(function(input:cs.system.io.Stream):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.io.TextReader):cs.system.xml.XmlReader {})
	@:overload(function(inputUri:String):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.io.Stream, settings:cs.system.xml.XmlReaderSettings):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.io.TextReader, settings:cs.system.xml.XmlReaderSettings):cs.system.xml.XmlReader {})
	@:overload(function(inputUri:String, settings:cs.system.xml.XmlReaderSettings):cs.system.xml.XmlReader {})
	@:overload(function(reader:cs.system.xml.XmlReader, settings:cs.system.xml.XmlReaderSettings):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.io.Stream, settings:cs.system.xml.XmlReaderSettings, baseUri:String):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.io.Stream, settings:cs.system.xml.XmlReaderSettings, inputContext:cs.system.xml.XmlParserContext):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.io.TextReader, settings:cs.system.xml.XmlReaderSettings, baseUri:String):cs.system.xml.XmlReader {})
	@:overload(function(input:cs.system.io.TextReader, settings:cs.system.xml.XmlReaderSettings, inputContext:cs.system.xml.XmlParserContext):cs.system.xml.XmlReader {})
	/**
	 * Creates a new  instance using the specified stream with default settings.
	 * @param input The stream that contains the XML data. The  scans the first bytes
	 * of the stream looking for a byte order mark or other sign of encoding. When
	 * encoding is determined, the encoding is used to continue reading the stream, and
	 * processing continues parsing the input as a stream of (Unicode) characters.
	 * @return An object that is used to read the XML data in the stream.
	 */
	static function Create(inputUri:String, settings:cs.system.xml.XmlReaderSettings, inputContext:cs.system.xml.XmlParserContext):cs.system.xml.XmlReader;
	/**
	 * Returns a value indicating whether the string argument is a valid XML name.
	 * @param str The name to validate.
	 * @return if the name is valid; otherwise, .
	 */
	static function IsName(str:String):Bool;
	/**
	 * Returns a value indicating whether or not the string argument is a valid XML
	 * name token.
	 * @param str The name token to validate.
	 * @return if it is a valid name token; otherwise .
	 */
	static function IsNameToken(str:String):Bool;
	/** When overridden in a derived class, changes the  to . */
	function Close():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function(i:Int):String {})
	@:overload(function(name:String):String {})
	/**
	 * When overridden in a derived class, gets the value of the attribute with the
	 * specified index.
	 * @param i The index of the attribute. The index is zero-based. (The first
	 * attribute has index 0.)
	 * @return The value of the specified attribute. This method does not move the
	 * reader.
	 */
	function GetAttribute(name:String, namespaceURI:String):String;
	/**
	 * Asynchronously gets the value of the current node.
	 * @return The value of the current node.
	 */
	function GetValueAsync():cs.system.threading.tasks.Task_1<String>;
	@:overload(function():Bool {})
	@:overload(function(name:String):Bool {})
	/**
	 * Calls  and tests if the current content node is a start tag or empty element
	 * tag.
	 * @return if  finds a start tag or empty element tag;  if a node type other than 
	 * was found.
	 */
	function IsStartElement(localname:String, ns:String):Bool;
	/**
	 * When overridden in a derived class, resolves a namespace prefix in the current
	 * element's scope.
	 * @param prefix The prefix whose namespace URI you want to resolve. To match the
	 * default namespace, pass an empty string.
	 * @return The namespace URI to which the prefix maps or  if no matching prefix is
	 * found.
	 */
	function LookupNamespace(prefix:String):String;
	@:overload(function(i:Int):Void {})
	@:overload(function(name:String):Bool {})
	/**
	 * When overridden in a derived class, moves to the attribute with the specified
	 * index.
	 * @param i The index of the attribute.
	 */
	function MoveToAttribute(name:String, ns:String):Bool;
	/**
	 * Checks whether the current node is a content (non-white space text, , , , , or )
	 * node. If the node is not a content node, the reader skips ahead to the next
	 * content node or end of file. It skips over nodes of the following type: , , , ,
	 * or .
	 * @return The  of the current node found by the method or  if the reader has
	 * reached the end of the input stream.
	 */
	function MoveToContent():cs.system.xml.XmlNodeType;
	/**
	 * Asynchronously checks whether the current node is a content node. If the node is
	 * not a content node, the reader skips ahead to the next content node or end of
	 * file.
	 * @return The  of the current node found by the method or  if the reader has
	 * reached the end of the input stream.
	 */
	function MoveToContentAsync():cs.system.threading.tasks.Task_1<cs.system.xml.XmlNodeType>;
	/**
	 * When overridden in a derived class, moves to the element that contains the
	 * current attribute node.
	 * @return if the reader is positioned on an attribute (the reader moves to the
	 * element that owns the attribute);  if the reader is not positioned on an
	 * attribute (the position of the reader does not change).
	 */
	function MoveToElement():Bool;
	/**
	 * When overridden in a derived class, moves to the first attribute.
	 * @return if an attribute exists (the reader moves to the first attribute);
	 * otherwise,  (the position of the reader does not change).
	 */
	function MoveToFirstAttribute():Bool;
	/**
	 * When overridden in a derived class, moves to the next attribute.
	 * @return if there is a next attribute;  if there are no more attributes.
	 */
	function MoveToNextAttribute():Bool;
	/**
	 * When overridden in a derived class, reads the next node from the stream.
	 * @return if the next node was read successfully; otherwise, .
	 */
	function Read():Bool;
	/**
	 * Asynchronously reads the next node from the stream.
	 * @return if the next node was read successfully;  if there are no more nodes to
	 * read.
	 */
	function ReadAsync():cs.system.threading.tasks.Task_1<Bool>;
	/**
	 * When overridden in a derived class, parses the attribute value into one or more
	 * , , or  nodes.
	 * @return if there are nodes to return. if the reader is not positioned on an
	 * attribute node when the initial call is made or if all the attribute values have
	 * been read. An empty attribute, such as, misc="", returns  with a single node
	 * with a value of .
	 */
	function ReadAttributeValue():Bool;
	/**
	 * Reads the content as an object of the type specified.
	 * @param returnType The type of the value to be returned. Note With the release of
	 * the .NET Framework 3.5, the value of the  parameter can now be the  type.
	 * @param namespaceResolver An  object that is used to resolve any namespace
	 * prefixes related to type conversion. For example, this can be used when
	 * converting an  object to an xs:string. This value can be .
	 * @return The concatenated text content or attribute value converted to the
	 * requested type.
	 */
	function ReadContentAs(returnType:cs.system.Type, namespaceResolver:cs.system.xml.IXmlNamespaceResolver):Dynamic;
	/**
	 * Asynchronously reads the content as an object of the type specified.
	 * @param returnType The type of the value to be returned.
	 * @param namespaceResolver An  object that is used to resolve any namespace
	 * prefixes related to type conversion.
	 * @return The concatenated text content or attribute value converted to the
	 * requested type.
	 */
	function ReadContentAsAsync(returnType:cs.system.Type, namespaceResolver:cs.system.xml.IXmlNamespaceResolver):cs.system.threading.tasks.Task_1<Dynamic>;
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
	 * Asynchronously reads the content and returns the Base64 decoded binary bytes.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadContentAsBase64Async(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads the content and returns the  decoded binary bytes.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadContentAsBinHex(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	/**
	 * Asynchronously reads the content and returns the  decoded binary bytes.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadContentAsBinHexAsync(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads the text content at the current position as a .
	 * @return The text content as a  object.
	 */
	function ReadContentAsBoolean():Bool;
	/**
	 * Reads the text content at the current position as a  object.
	 * @return The text content as a  object.
	 */
	function ReadContentAsDateTime():cs.system.DateTime;
	/**
	 * Reads the text content at the current position as a  object.
	 * @return The text content as a  object.
	 */
	function ReadContentAsDateTimeOffset():cs.system.DateTimeOffset;
	/**
	 * Reads the text content at the current position as a  object.
	 * @return The text content at the current position as a  object.
	 */
	function ReadContentAsDecimal():cs.system.Decimal;
	/**
	 * Reads the text content at the current position as a double-precision
	 * floating-point number.
	 * @return The text content as a double-precision floating-point number.
	 */
	function ReadContentAsDouble():Float;
	/**
	 * Reads the text content at the current position as a single-precision floating
	 * point number.
	 * @return The text content at the current position as a single-precision floating
	 * point number.
	 */
	function ReadContentAsFloat():Single;
	/**
	 * Reads the text content at the current position as a 32-bit signed integer.
	 * @return The text content as a 32-bit signed integer.
	 */
	function ReadContentAsInt():Int;
	/**
	 * Reads the text content at the current position as a 64-bit signed integer.
	 * @return The text content as a 64-bit signed integer.
	 */
	function ReadContentAsLong():haxe.Int64;
	/**
	 * Reads the text content at the current position as an .
	 * @return The text content as the most appropriate common language runtime (CLR)
	 * object.
	 */
	function ReadContentAsObject():Dynamic;
	/**
	 * Asynchronously reads the text content at the current position as an .
	 * @return The text content as the most appropriate common language runtime (CLR)
	 * object.
	 */
	function ReadContentAsObjectAsync():cs.system.threading.tasks.Task_1<Dynamic>;
	/**
	 * Reads the text content at the current position as a  object.
	 * @return The text content as a  object.
	 */
	function ReadContentAsString():String;
	/**
	 * Asynchronously reads the text content at the current position as a  object.
	 * @return The text content as a  object.
	 */
	function ReadContentAsStringAsync():cs.system.threading.tasks.Task_1<String>;
	@:overload(function(returnType:cs.system.Type, namespaceResolver:cs.system.xml.IXmlNamespaceResolver):Dynamic {})
	/**
	 * Reads the element content as the requested type.
	 * @param returnType The type of the value to be returned. Note With the release of
	 * the .NET Framework 3.5, the value of the  parameter can now be the  type.
	 * @param namespaceResolver An  object that is used to resolve any namespace
	 * prefixes related to type conversion.
	 * @return The element content converted to the requested typed object.
	 */
	function ReadElementContentAs(returnType:cs.system.Type, namespaceResolver:cs.system.xml.IXmlNamespaceResolver, localName:String, namespaceURI:String):Dynamic;
	/**
	 * Asynchronously reads the element content as the requested type.
	 * @param returnType The type of the value to be returned.
	 * @param namespaceResolver An  object that is used to resolve any namespace
	 * prefixes related to type conversion.
	 * @return The element content converted to the requested typed object.
	 */
	function ReadElementContentAsAsync(returnType:cs.system.Type, namespaceResolver:cs.system.xml.IXmlNamespaceResolver):cs.system.threading.tasks.Task_1<Dynamic>;
	/**
	 * Reads the element and decodes the  content.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadElementContentAsBase64(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	/**
	 * Asynchronously reads the element and decodes the  content.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadElementContentAsBase64Async(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads the element and decodes the  content.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadElementContentAsBinHex(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	/**
	 * Asynchronously reads the element and decodes the  content.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadElementContentAsBinHexAsync(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	@:overload(function():Bool {})
	/**
	 * Reads the current element and returns the contents as a  object.
	 * @return The element content as a  object.
	 */
	function ReadElementContentAsBoolean(localName:String, namespaceURI:String):Bool;
	@:overload(function():cs.system.DateTime {})
	/**
	 * Reads the current element and returns the contents as a  object.
	 * @return The element content as a  object.
	 */
	function ReadElementContentAsDateTime(localName:String, namespaceURI:String):cs.system.DateTime;
	@:overload(function():cs.system.Decimal {})
	/**
	 * Reads the current element and returns the contents as a  object.
	 * @return The element content as a  object.
	 */
	function ReadElementContentAsDecimal(localName:String, namespaceURI:String):cs.system.Decimal;
	@:overload(function():Float {})
	/**
	 * Reads the current element and returns the contents as a double-precision
	 * floating-point number.
	 * @return The element content as a double-precision floating-point number.
	 */
	function ReadElementContentAsDouble(localName:String, namespaceURI:String):Float;
	@:overload(function():Single {})
	/**
	 * Reads the current element and returns the contents as single-precision
	 * floating-point number.
	 * @return The element content as a single-precision floating point number.
	 */
	function ReadElementContentAsFloat(localName:String, namespaceURI:String):Single;
	@:overload(function():Int {})
	/**
	 * Reads the current element and returns the contents as a 32-bit signed integer.
	 * @return The element content as a 32-bit signed integer.
	 */
	function ReadElementContentAsInt(localName:String, namespaceURI:String):Int;
	@:overload(function():haxe.Int64 {})
	/**
	 * Reads the current element and returns the contents as a 64-bit signed integer.
	 * @return The element content as a 64-bit signed integer.
	 */
	function ReadElementContentAsLong(localName:String, namespaceURI:String):haxe.Int64;
	@:overload(function():Dynamic {})
	/**
	 * Reads the current element and returns the contents as an .
	 * @return A boxed common language runtime (CLR) object of the most appropriate
	 * type. The  property determines the appropriate CLR type. If the content is typed
	 * as a list type, this method returns an array of boxed objects of the appropriate
	 * type.
	 */
	function ReadElementContentAsObject(localName:String, namespaceURI:String):Dynamic;
	/**
	 * Asynchronously reads the current element and returns the contents as an .
	 * @return A boxed common language runtime (CLR) object of the most appropriate
	 * type. The  property determines the appropriate CLR type. If the content is typed
	 * as a list type, this method returns an array of boxed objects of the appropriate
	 * type.
	 */
	function ReadElementContentAsObjectAsync():cs.system.threading.tasks.Task_1<Dynamic>;
	@:overload(function():String {})
	/**
	 * Reads the current element and returns the contents as a  object.
	 * @return The element content as a  object.
	 */
	function ReadElementContentAsString(localName:String, namespaceURI:String):String;
	/**
	 * Asynchronously reads the current element and returns the contents as a  object.
	 * @return The element content as a  object.
	 */
	function ReadElementContentAsStringAsync():cs.system.threading.tasks.Task_1<String>;
	@:overload(function():String {})
	@:overload(function(name:String):String {})
	/**
	 * Reads a text-only element. However, we recommend that you use the  method
	 * instead, because it provides a more straightforward way to handle this
	 * operation.
	 * @return The text contained in the element that was read. An empty string if the
	 * element is empty.
	 */
	function ReadElementString(localname:String, ns:String):String;
	/** Checks that the current content node is an end tag and advances the reader to the next node. */
	function ReadEndElement():Void;
	/**
	 * When overridden in a derived class, reads all the content, including markup, as
	 * a string.
	 * @return All the XML content, including markup, in the current node. If the
	 * current node has no children, an empty string is returned. If the current node
	 * is neither an element nor attribute, an empty string is returned.
	 */
	function ReadInnerXml():String;
	/**
	 * Asynchronously reads all the content, including markup, as a string.
	 * @return All the XML content, including markup, in the current node. If the
	 * current node has no children, an empty string is returned.
	 */
	function ReadInnerXmlAsync():cs.system.threading.tasks.Task_1<String>;
	/**
	 * When overridden in a derived class, reads the content, including markup,
	 * representing this node and all its children.
	 * @return If the reader is positioned on an element or an attribute node, this
	 * method returns all the XML content, including markup, of the current node and
	 * all its children; otherwise, it returns an empty string.
	 */
	function ReadOuterXml():String;
	/**
	 * Asynchronously reads the content, including markup, representing this node and
	 * all its children.
	 * @return If the reader is positioned on an element or an attribute node, this
	 * method returns all the XML content, including markup, of the current node and
	 * all its children; otherwise, it returns an empty string.
	 */
	function ReadOuterXmlAsync():cs.system.threading.tasks.Task_1<String>;
	@:overload(function():Void {})
	@:overload(function(name:String):Void {})
	/** Checks that the current node is an element and advances the reader to the next node. */
	function ReadStartElement(localname:String, ns:String):Void;
	/**
	 * When overridden in a derived class, reads the contents of an element or text
	 * node as a string. However, we recommend that you use the  method instead,
	 * because it provides a more straightforward way to handle this operation.
	 * @return The contents of the element or an empty string.
	 */
	function ReadString():String;
	/**
	 * Returns a new  instance that can be used to read the current node, and all its
	 * descendants.
	 * @return A new XML reader instance set to . Calling the  method positions the new
	 * reader on the node that was current before the call to the  method.
	 */
	function ReadSubtree():cs.system.xml.XmlReader;
	@:overload(function(name:String):Bool {})
	/**
	 * Advances the  to the next descendant element with the specified qualified name.
	 * @param name The qualified name of the element you wish to move to.
	 * @return if a matching descendant element is found; otherwise . If a matching
	 * child element is not found, the  is positioned on the end tag ( is ) of the
	 * element. If the  is not positioned on an element when  was called, this method
	 * returns  and the position of the  is not changed.
	 */
	function ReadToDescendant(localName:String, namespaceURI:String):Bool;
	@:overload(function(name:String):Bool {})
	/**
	 * Reads until an element with the specified qualified name is found.
	 * @param name The qualified name of the element.
	 * @return if a matching element is found; otherwise  and the  is in an end of file
	 * state.
	 */
	function ReadToFollowing(localName:String, namespaceURI:String):Bool;
	@:overload(function(name:String):Bool {})
	/**
	 * Advances the  to the next sibling element with the specified qualified name.
	 * @param name The qualified name of the sibling element you wish to move to.
	 * @return if a matching sibling element is found; otherwise . If a matching
	 * sibling element is not found, the  is positioned on the end tag ( is ) of the
	 * parent element.
	 */
	function ReadToNextSibling(localName:String, namespaceURI:String):Bool;
	/**
	 * Reads large streams of text embedded in an XML document.
	 * @param buffer The array of characters that serves as the buffer to which the
	 * text contents are written. This value cannot be .
	 * @param index The offset within the buffer where the  can start to copy the
	 * results.
	 * @param count The maximum number of characters to copy into the buffer. The
	 * actual number of characters copied is returned from this method.
	 * @return The number of characters read into the buffer. The value zero is
	 * returned when there is no more text content.
	 */
	function ReadValueChunk(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	/**
	 * Asynchronously reads large streams of text embedded in an XML document.
	 * @param buffer The array of characters that serves as the buffer to which the
	 * text contents are written. This value cannot be .
	 * @param index The offset within the buffer where the  can start to copy the
	 * results.
	 * @param count The maximum number of characters to copy into the buffer. The
	 * actual number of characters copied is returned from this method.
	 * @return The number of characters read into the buffer. The value zero is
	 * returned when there is no more text content.
	 */
	function ReadValueChunkAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task_1<Int>;
	/** When overridden in a derived class, resolves the entity reference for  nodes. */
	function ResolveEntity():Void;
	/** Skips the children of the current node. */
	function Skip():Void;
	/**
	 * Asynchronously skips the children of the current node.
	 * @return The current node.
	 */
	function SkipAsync():cs.system.threading.tasks.Task;
}
