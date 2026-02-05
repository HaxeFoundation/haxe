package cs.system.xml;

/** Represents a reader that provides fast, non-cached forward only access to XML data in an . */
@:native("System.Xml.XmlNodeReader")
extern class XmlNodeReader extends cs.system.xml.XmlReader {
	function new(node:cs.system.xml.XmlNode):Void;
	/** Changes the  to . */
	function Close():Void;
	@:overload(function(attributeIndex:Int):String {})
	@:overload(function(name:String):String {})
	/**
	 * Gets the value of the attribute with the specified index.
	 * @param attributeIndex The index of the attribute. The index is zero-based. (The
	 * first attribute has index 0.)
	 * @return The value of the specified attribute.
	 */
	function GetAttribute(name:String, namespaceURI:String):String;
	/**
	 * Resolves a namespace prefix in the current element's scope.
	 * @param prefix The prefix whose namespace URI you want to resolve. To match the
	 * default namespace, pass an empty string. This string does not have to be
	 * atomized.
	 * @return The namespace URI to which the prefix maps or  if no matching prefix is
	 * found.
	 */
	function LookupNamespace(prefix:String):String;
	@:overload(function(attributeIndex:Int):Void {})
	@:overload(function(name:String):Bool {})
	/**
	 * Moves to the attribute with the specified index.
	 * @param attributeIndex The index of the attribute.
	 */
	function MoveToAttribute(name:String, namespaceURI:String):Bool;
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
	 * @return The contents of the element or text-like node (This can include CDATA,
	 * Text nodes, and so on). This can be an empty string if the reader is positioned
	 * on something other than an element or text node, or if there is no more text
	 * content to return in the current context. The text node can be either an element
	 * or an attribute text node.
	 */
	function ReadString():String;
	/** Resolves the entity reference for  nodes. */
	function ResolveEntity():Void;
	/** Skips the children of the current node. */
	function Skip():Void;
}
