package cs.system.xml;

/** Represents a reader that provides fast, non-cached, forward-only access to XML data. Starting with the .NET Framework 2.0, we recommend that you use the  class instead. */
@:native("System.Xml.XmlTextReader")
extern class XmlTextReader extends cs.system.xml.XmlReader {
	/**
	 * Gets or sets the  enumeration.
	 * @return The  enumeration.
	 */
	var DtdProcessing(default, default):cs.system.xml.DtdProcessing;
	/**
	 * Gets the encoding of the document.
	 * @return The encoding value. If no encoding attribute exists, and there is no
	 * byte-order mark, this defaults to UTF-8.
	 */
	var Encoding(default, never):cs.system.text.Encoding;
	/**
	 * Gets or sets a value that specifies how the reader handles entities.
	 * @return One of the  values. If no  is specified, it defaults to .
	 */
	var EntityHandling(default, default):cs.system.xml.EntityHandling;
	/**
	 * Gets the current line number.
	 * @return The current line number.
	 */
	var LineNumber(default, never):Int;
	/**
	 * Gets the current line position.
	 * @return The current line position.
	 */
	var LinePosition(default, never):Int;
	/**
	 * Gets or sets a value indicating whether to do namespace support.
	 * @return to do namespace support; otherwise, . The default is .
	 */
	var Namespaces(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether to normalize white space and attribute
	 * values.
	 * @return to normalize; otherwise, . The default is .
	 */
	var Normalization(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether to allow DTD processing. This property
	 * is obsolete. Use  instead.
	 * @return to disallow DTD processing; otherwise . The default is .
	 */
	var ProhibitDtd(default, default):Bool;
	/**
	 * Gets or sets a value that specifies how white space is handled.
	 * @return One of the  values. The default is  (returns  and  nodes).
	 */
	var WhitespaceHandling(default, default):cs.system.xml.WhitespaceHandling;
	/**
	 * Sets the  used for resolving DTD references.
	 * @return The  to use. If set to , external resources are not resolved. In version
	 * 1.1 of the .NET Framework, the caller must be fully trusted in order to specify
	 * an .
	 */
	var XmlResolver(never, default):cs.system.xml.XmlResolver;
	@:overload(function(input:cs.system.io.Stream):Void {})
	@:overload(function(input:cs.system.io.TextReader):Void {})
	@:overload(function(url:String):Void {})
	@:overload(function(input:cs.system.io.Stream, nt:cs.system.xml.XmlNameTable):Void {})
	@:overload(function(input:cs.system.io.TextReader, nt:cs.system.xml.XmlNameTable):Void {})
	@:overload(function(url:String, input:cs.system.io.Stream):Void {})
	@:overload(function(url:String, input:cs.system.io.TextReader):Void {})
	@:overload(function(url:String, nt:cs.system.xml.XmlNameTable):Void {})
	@:overload(function(xmlFragment:cs.system.io.Stream, fragType:cs.system.xml.XmlNodeType, context:cs.system.xml.XmlParserContext):Void {})
	@:overload(function(url:String, input:cs.system.io.Stream, nt:cs.system.xml.XmlNameTable):Void {})
	@:overload(function(url:String, input:cs.system.io.TextReader, nt:cs.system.xml.XmlNameTable):Void {})
	function new(xmlFragment:String, fragType:cs.system.xml.XmlNodeType, context:cs.system.xml.XmlParserContext):Void;
	/** Changes the  to . */
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
	 * Gets a collection that contains all namespaces currently in-scope.
	 * @param scope An  value that specifies the type of namespace nodes to return.
	 * @return An  object that contains all the current in-scope namespaces. If the
	 * reader is not positioned on an element, an empty dictionary (no namespaces) is
	 * returned.
	 */
	function GetNamespacesInScope(scope:cs.system.xml.XmlNamespaceScope):cs.system.collections.generic.IDictionary<String, String>;
	/**
	 * Gets the remainder of the buffered XML.
	 * @return A  containing the remainder of the buffered XML.
	 */
	function GetRemainder():cs.system.io.TextReader;
	/**
	 * Gets a value indicating whether the class can return line information.
	 * @return if the class can return line information; otherwise, .
	 */
	function HasLineInfo():Bool;
	/**
	 * Resolves a namespace prefix in the current element's scope.
	 * @param prefix The prefix whose namespace URI you want to resolve. To match the
	 * default namespace, pass an empty string. This string does not have to be
	 * atomized.
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
	 * with a value of .
	 */
	function ReadAttributeValue():Bool;
	/**
	 * Decodes Base64 and returns the decoded binary bytes.
	 * @param array The array of characters that serves as the buffer to which the text
	 * contents are written.
	 * @param offset The zero-based index into the array specifying where the method
	 * can begin to write to the buffer.
	 * @param len The number of bytes to write into the buffer.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadBase64(array:cs.NativeArray<cs.UInt8>, offset:Int, len:Int):Int;
	/**
	 * Decodes  and returns the decoded binary bytes.
	 * @param array The byte array that serves as the buffer to which the decoded
	 * binary bytes are written.
	 * @param offset The zero-based index into the array specifying where the method
	 * can begin to write to the buffer.
	 * @param len The number of bytes to write into the buffer.
	 * @return The number of bytes written to your buffer.
	 */
	function ReadBinHex(array:cs.NativeArray<cs.UInt8>, offset:Int, len:Int):Int;
	/**
	 * Reads the text contents of an element into a character buffer. This method is
	 * designed to read large streams of embedded text by calling it successively.
	 * @param buffer The array of characters that serves as the buffer to which the
	 * text contents are written.
	 * @param index The position within  where the method can begin writing text
	 * contents.
	 * @param count The number of characters to write into .
	 * @return The number of characters read. This can be  if the reader is not
	 * positioned on an element or if there is no more text content to return in the
	 * current context.
	 */
	function ReadChars(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	/**
	 * Reads the content and returns the  decoded binary bytes.
	 * @param buffer The buffer into which to copy the resulting text. This value
	 * cannot be .
	 * @param index The offset into the buffer where to start copying the result.
	 * @param count The maximum number of bytes to copy into the buffer. The actual
	 * number of bytes copied is returned from this method.
	 * @return The number of bytes written to the buffer.
	 */
	function ReadContentAsBase64(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
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
	 * Reads the contents of an element or a text node as a string.
	 * @return The contents of the element or text node. This can be an empty string if
	 * the reader is positioned on something other than an element or text node, or if
	 * there is no more text content to return in the current context. The text node
	 * can be either an element or an attribute text node.
	 */
	function ReadString():String;
	/** Resets the state of the reader to ReadState.Initial. */
	function ResetState():Void;
	/** Resolves the entity reference for  nodes. */
	function ResolveEntity():Void;
	/** Skips the children of the current node. */
	function Skip():Void;
}
