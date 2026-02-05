package cs.system.xml;

/** Represents a writer that provides a fast, non-cached, forward-only way to generate streams or files that contain XML data. */
@:native("System.Xml.XmlWriter")
extern class XmlWriter {
	/**
	 * Gets the  object used to create this  instance.
	 * @return The  object used to create this writer instance. If this writer was not
	 * created using the  method, this property returns .
	 */
	var Settings(default, never):cs.system.xml.XmlWriterSettings;
	/**
	 * When overridden in a derived class, gets the state of the writer.
	 * @return One of the  values.
	 */
	var WriteState(default, never):cs.system.xml.WriteState;
	/**
	 * When overridden in a derived class, gets the current  scope.
	 * @return The current  scope.
	 */
	var XmlLang(default, never):String;
	/**
	 * When overridden in a derived class, gets an  representing the current  scope.
	 * @return An  representing the current  scope. Value Meaning This is the default
	 * if no  scope exists. The current scope is ="default". The current scope is
	 * ="preserve".
	 */
	var XmlSpace(default, never):cs.system.xml.XmlSpace;
	@:overload(function(output:cs.system.io.Stream):cs.system.xml.XmlWriter {})
	@:overload(function(output:cs.system.io.TextWriter):cs.system.xml.XmlWriter {})
	@:overload(function(outputFileName:String):cs.system.xml.XmlWriter {})
	@:overload(function(output:cs.system.text.StringBuilder):cs.system.xml.XmlWriter {})
	@:overload(function(output:cs.system.xml.XmlWriter):cs.system.xml.XmlWriter {})
	@:overload(function(output:cs.system.io.Stream, settings:cs.system.xml.XmlWriterSettings):cs.system.xml.XmlWriter {})
	@:overload(function(output:cs.system.io.TextWriter, settings:cs.system.xml.XmlWriterSettings):cs.system.xml.XmlWriter {})
	@:overload(function(outputFileName:String, settings:cs.system.xml.XmlWriterSettings):cs.system.xml.XmlWriter {})
	@:overload(function(output:cs.system.text.StringBuilder, settings:cs.system.xml.XmlWriterSettings):cs.system.xml.XmlWriter {})
	/**
	 * Creates a new  instance using the specified stream.
	 * @param output The stream to which you want to write. The  writes XML 1.0 text
	 * syntax and appends it to the specified stream.
	 * @return An  object.
	 */
	static function Create(output:cs.system.xml.XmlWriter, settings:cs.system.xml.XmlWriterSettings):cs.system.xml.XmlWriter;
	/** When overridden in a derived class, closes this stream and the underlying stream. */
	function Close():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/** When overridden in a derived class, flushes whatever is in the buffer to the underlying streams and also flushes the underlying stream. */
	function Flush():Void;
	/**
	 * Asynchronously flushes whatever is in the buffer to the underlying streams and
	 * also flushes the underlying stream.
	 * @return The task that represents the asynchronous  operation.
	 */
	function FlushAsync():cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, returns the closest prefix defined in the
	 * current namespace scope for the namespace URI.
	 * @param ns The namespace URI whose prefix you want to find.
	 * @return The matching prefix or  if no matching namespace URI is found in the
	 * current scope.
	 */
	function LookupPrefix(ns:String):String;
	/**
	 * When overridden in a derived class, writes out all the attributes found at the
	 * current position in the .
	 * @param reader The  from which to copy the attributes.
	 * @param defattr to copy the default attributes from the ; otherwise, .
	 */
	function WriteAttributes(reader:cs.system.xml.XmlReader, defattr:Bool):Void;
	/**
	 * Asynchronously writes out all the attributes found at the current position in
	 * the .
	 * @param reader The  from which to copy the attributes.
	 * @param defattr to copy the default attributes from the ; otherwise, .
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteAttributesAsync(reader:cs.system.xml.XmlReader, defattr:Bool):cs.system.threading.tasks.Task;
	@:overload(function(localName:String, value:String):Void {})
	@:overload(function(localName:String, ns:String, value:String):Void {})
	/**
	 * When overridden in a derived class, writes out the attribute with the specified
	 * local name and value.
	 * @param localName The local name of the attribute.
	 * @param value The value of the attribute.
	 */
	function WriteAttributeString(prefix:String, localName:String, ns:String, value:String):Void;
	/**
	 * Asynchronously writes out the attribute with the specified prefix, local name,
	 * namespace URI, and value.
	 * @param prefix The namespace prefix of the attribute.
	 * @param localName The local name of the attribute.
	 * @param ns The namespace URI of the attribute.
	 * @param value The value of the attribute.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteAttributeStringAsync(prefix:String, localName:String, ns:String, value:String):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, encodes the specified binary bytes as Base64
	 * and writes out the resulting text.
	 * @param buffer Byte array to encode.
	 * @param index The position in the buffer indicating the start of the bytes to
	 * write.
	 * @param count The number of bytes to write.
	 */
	function WriteBase64(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Void;
	/**
	 * Asynchronously encodes the specified binary bytes as Base64 and writes out the
	 * resulting text.
	 * @param buffer Byte array to encode.
	 * @param index The position in the buffer indicating the start of the bytes to
	 * write.
	 * @param count The number of bytes to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteBase64Async(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, encodes the specified binary bytes as  and
	 * writes out the resulting text.
	 * @param buffer Byte array to encode.
	 * @param index The position in the buffer indicating the start of the bytes to
	 * write.
	 * @param count The number of bytes to write.
	 */
	function WriteBinHex(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Void;
	/**
	 * Asynchronously encodes the specified binary bytes as  and writes out the
	 * resulting text.
	 * @param buffer Byte array to encode.
	 * @param index The position in the buffer indicating the start of the bytes to
	 * write.
	 * @param count The number of bytes to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteBinHexAsync(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes out a <![CDATA[...]]> block
	 * containing the specified text.
	 * @param text The text to place inside the CDATA block.
	 */
	function WriteCData(text:String):Void;
	/**
	 * Asynchronously writes out a <![CDATA[...]]> block containing the specified text.
	 * @param text The text to place inside the CDATA block.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteCDataAsync(text:String):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, forces the generation of a character entity
	 * for the specified Unicode character value.
	 * @param ch The Unicode character for which to generate a character entity.
	 */
	function WriteCharEntity(ch:cs.Char16):Void;
	/**
	 * Asynchronously forces the generation of a character entity for the specified
	 * Unicode character value.
	 * @param ch The Unicode character for which to generate a character entity.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteCharEntityAsync(ch:cs.Char16):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes text one buffer at a time.
	 * @param buffer Character array containing the text to write.
	 * @param index The position in the buffer indicating the start of the text to
	 * write.
	 * @param count The number of characters to write.
	 */
	function WriteChars(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void;
	/**
	 * Asynchronously writes text one buffer at a time.
	 * @param buffer Character array containing the text to write.
	 * @param index The position in the buffer indicating the start of the text to
	 * write.
	 * @param count The number of characters to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteCharsAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes out a comment <!--...--> containing
	 * the specified text.
	 * @param text Text to place inside the comment.
	 */
	function WriteComment(text:String):Void;
	/**
	 * Asynchronously writes out a comment <!--...--> containing the specified text.
	 * @param text Text to place inside the comment.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteCommentAsync(text:String):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes the DOCTYPE declaration with the
	 * specified name and optional attributes.
	 * @param name The name of the DOCTYPE. This must be non-empty.
	 * @param pubid If non-null it also writes PUBLIC "pubid" "sysid" where  and  are
	 * replaced with the value of the given arguments.
	 * @param sysid If  is  and  is non-null it writes SYSTEM "sysid" where  is
	 * replaced with the value of this argument.
	 * @param subset If non-null it writes [subset] where subset is replaced with the
	 * value of this argument.
	 */
	function WriteDocType(name:String, pubid:String, sysid:String, subset:String):Void;
	/**
	 * Asynchronously writes the DOCTYPE declaration with the specified name and
	 * optional attributes.
	 * @param name The name of the DOCTYPE. This must be non-empty.
	 * @param pubid If non-null it also writes PUBLIC "pubid" "sysid" where  and  are
	 * replaced with the value of the given arguments.
	 * @param sysid If  is  and  is non-null it writes SYSTEM "sysid" where  is
	 * replaced with the value of this argument.
	 * @param subset If non-null it writes [subset] where subset is replaced with the
	 * value of this argument.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteDocTypeAsync(name:String, pubid:String, sysid:String, subset:String):cs.system.threading.tasks.Task;
	@:overload(function(localName:String, value:String):Void {})
	@:overload(function(localName:String, ns:String, value:String):Void {})
	/**
	 * Writes an element with the specified local name and value.
	 * @param localName The local name of the element.
	 * @param value The value of the element.
	 */
	function WriteElementString(prefix:String, localName:String, ns:String, value:String):Void;
	/**
	 * Asynchronously writes an element with the specified prefix, local name,
	 * namespace URI, and value.
	 * @param prefix The prefix of the element.
	 * @param localName The local name of the element.
	 * @param ns The namespace URI of the element.
	 * @param value The value of the element.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteElementStringAsync(prefix:String, localName:String, ns:String, value:String):cs.system.threading.tasks.Task;
	/** When overridden in a derived class, closes the previous  call. */
	function WriteEndAttribute():Void;
	/** When overridden in a derived class, closes any open elements or attributes and puts the writer back in the Start state. */
	function WriteEndDocument():Void;
	/**
	 * Asynchronously closes any open elements or attributes and puts the writer back
	 * in the Start state.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteEndDocumentAsync():cs.system.threading.tasks.Task;
	/** When overridden in a derived class, closes one element and pops the corresponding namespace scope. */
	function WriteEndElement():Void;
	/**
	 * Asynchronously closes one element and pops the corresponding namespace scope.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteEndElementAsync():cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes out an entity reference as .
	 * @param name The name of the entity reference.
	 */
	function WriteEntityRef(name:String):Void;
	/**
	 * Asynchronously writes out an entity reference as .
	 * @param name The name of the entity reference.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteEntityRefAsync(name:String):cs.system.threading.tasks.Task;
	/** When overridden in a derived class, closes one element and pops the corresponding namespace scope. */
	function WriteFullEndElement():Void;
	/**
	 * Asynchronously closes one element and pops the corresponding namespace scope.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteFullEndElementAsync():cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes out the specified name, ensuring it
	 * is a valid name according to the W3C XML 1.0 recommendation
	 * (https://www.w3.org/TR/1998/REC-xml-19980210#NT-Name).
	 * @param name The name to write.
	 */
	function WriteName(name:String):Void;
	/**
	 * Asynchronously writes out the specified name, ensuring it is a valid name
	 * according to the W3C XML 1.0 recommendation
	 * (https://www.w3.org/TR/1998/REC-xml-19980210#NT-Name).
	 * @param name The name to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteNameAsync(name:String):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes out the specified name, ensuring it
	 * is a valid NmToken according to the W3C XML 1.0 recommendation
	 * (https://www.w3.org/TR/1998/REC-xml-19980210#NT-Name).
	 * @param name The name to write.
	 */
	function WriteNmToken(name:String):Void;
	/**
	 * Asynchronously writes out the specified name, ensuring it is a valid NmToken
	 * according to the W3C XML 1.0 recommendation
	 * (https://www.w3.org/TR/1998/REC-xml-19980210#NT-Name).
	 * @param name The name to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteNmTokenAsync(name:String):cs.system.threading.tasks.Task;
	@:overload(function(reader:cs.system.xml.XmlReader, defattr:Bool):Void {})
	/**
	 * When overridden in a derived class, copies everything from the reader to the
	 * writer and moves the reader to the start of the next sibling.
	 * @param reader The  to read from.
	 * @param defattr to copy the default attributes from the ; otherwise, .
	 */
	function WriteNode(navigator:cs.system.xml.xpath.XPathNavigator, defattr:Bool):Void;
	@:overload(function(reader:cs.system.xml.XmlReader, defattr:Bool):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously copies everything from the reader to the writer and moves the
	 * reader to the start of the next sibling.
	 * @param reader The  to read from.
	 * @param defattr to copy the default attributes from the ; otherwise, .
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteNodeAsync(navigator:cs.system.xml.xpath.XPathNavigator, defattr:Bool):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes out a processing instruction with a
	 * space between the name and text as follows: <?name text?>.
	 * @param name The name of the processing instruction.
	 * @param text The text to include in the processing instruction.
	 */
	function WriteProcessingInstruction(name:String, text:String):Void;
	/**
	 * Asynchronously writes out a processing instruction with a space between the name
	 * and text as follows: <?name text?>.
	 * @param name The name of the processing instruction.
	 * @param text The text to include in the processing instruction.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteProcessingInstructionAsync(name:String, text:String):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes out the namespace-qualified name.
	 * This method looks up the prefix that is in scope for the given namespace.
	 * @param localName The local name to write.
	 * @param ns The namespace URI for the name.
	 */
	function WriteQualifiedName(localName:String, ns:String):Void;
	/**
	 * Asynchronously writes out the namespace-qualified name. This method looks up the
	 * prefix that is in scope for the given namespace.
	 * @param localName The local name to write.
	 * @param ns The namespace URI for the name.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteQualifiedNameAsync(localName:String, ns:String):cs.system.threading.tasks.Task;
	@:overload(function(data:String):Void {})
	/**
	 * When overridden in a derived class, writes raw markup manually from a character
	 * buffer.
	 * @param buffer Character array containing the text to write.
	 * @param index The position within the buffer indicating the start of the text to
	 * write.
	 * @param count The number of characters to write.
	 */
	function WriteRaw(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void;
	@:overload(function(data:String):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously writes raw markup manually from a character buffer.
	 * @param buffer Character array containing the text to write.
	 * @param index The position within the buffer indicating the start of the text to
	 * write.
	 * @param count The number of characters to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteRawAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
	@:overload(function(localName:String):Void {})
	@:overload(function(localName:String, ns:String):Void {})
	/**
	 * Writes the start of an attribute with the specified local name.
	 * @param localName The local name of the attribute.
	 */
	function WriteStartAttribute(prefix:String, localName:String, ns:String):Void;
	@:overload(function():Void {})
	/** When overridden in a derived class, writes the XML declaration with the version "1.0". */
	function WriteStartDocument(standalone:Bool):Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously writes the XML declaration with the version "1.0".
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteStartDocumentAsync(standalone:Bool):cs.system.threading.tasks.Task;
	@:overload(function(localName:String):Void {})
	@:overload(function(localName:String, ns:String):Void {})
	/**
	 * When overridden in a derived class, writes out a start tag with the specified
	 * local name.
	 * @param localName The local name of the element.
	 */
	function WriteStartElement(prefix:String, localName:String, ns:String):Void;
	/**
	 * Asynchronously writes the specified start tag and associates it with the given
	 * namespace and prefix.
	 * @param prefix The namespace prefix of the element.
	 * @param localName The local name of the element.
	 * @param ns The namespace URI to associate with the element.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteStartElementAsync(prefix:String, localName:String, ns:String):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, writes the given text content.
	 * @param text The text to write.
	 */
	function WriteString(text:String):Void;
	/**
	 * Asynchronously writes the given text content.
	 * @param text The text to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteStringAsync(text:String):cs.system.threading.tasks.Task;
	/**
	 * When overridden in a derived class, generates and writes the surrogate character
	 * entity for the surrogate character pair.
	 * @param lowChar The low surrogate. This must be a value between 0xDC00 and
	 * 0xDFFF.
	 * @param highChar The high surrogate. This must be a value between 0xD800 and
	 * 0xDBFF.
	 */
	function WriteSurrogateCharEntity(lowChar:cs.Char16, highChar:cs.Char16):Void;
	/**
	 * Asynchronously generates and writes the surrogate character entity for the
	 * surrogate character pair.
	 * @param lowChar The low surrogate. This must be a value between 0xDC00 and
	 * 0xDFFF.
	 * @param highChar The high surrogate. This must be a value between 0xD800 and
	 * 0xDBFF.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteSurrogateCharEntityAsync(lowChar:cs.Char16, highChar:cs.Char16):cs.system.threading.tasks.Task;
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.system.DateTime):Void {})
	@:overload(function(value:cs.system.DateTimeOffset):Void {})
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(value:Single):Void {})
	/**
	 * Writes a  value.
	 * @param value The  value to write.
	 */
	function WriteValue(value:String):Void;
	/**
	 * When overridden in a derived class, writes out the given white space.
	 * @param ws The string of white space characters.
	 */
	function WriteWhitespace(ws:String):Void;
	/**
	 * Asynchronously writes out the given white space.
	 * @param ws The string of white space characters.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteWhitespaceAsync(ws:String):cs.system.threading.tasks.Task;
}
