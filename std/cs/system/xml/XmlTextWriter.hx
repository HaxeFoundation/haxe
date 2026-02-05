package cs.system.xml;

/** Represents a writer that provides a fast, non-cached, forward-only way of generating streams or files containing XML data that conforms to the W3C Extensible Markup Language (XML) 1.0 and the Namespaces in XML recommendations. Starting with the .NET Framework 2.0, we recommend that you use the  class instead. */
@:native("System.Xml.XmlTextWriter")
extern class XmlTextWriter extends cs.system.xml.XmlWriter {
	/**
	 * Gets the underlying stream object.
	 * @return The stream to which the  is writing or  if the  was constructed using a 
	 * that does not inherit from the  class.
	 */
	var BaseStream(default, never):cs.system.io.Stream;
	/**
	 * Indicates how the output is formatted.
	 * @return One of the  values. The default is  (no special formatting).
	 */
	var Formatting(default, default):cs.system.xml.Formatting;
	/**
	 * Gets or sets how many IndentChars to write for each level in the hierarchy when 
	 * is set to .
	 * @return Number of  for each level. The default is 2.
	 */
	var Indentation(default, default):Int;
	/**
	 * Gets or sets which character to use for indenting when  is set to .
	 * @return The character to use for indenting. The default is space. The  allows
	 * you to set this property to any character. To ensure valid XML, you must specify
	 * a valid white space character, 0x9, 0x10, 0x13 or 0x20.
	 */
	var IndentChar(default, default):cs.Char16;
	/**
	 * Gets or sets a value indicating whether to do namespace support.
	 * @return to support namespaces; otherwise, . The default is .
	 */
	var Namespaces(default, default):Bool;
	/**
	 * Gets or sets which character to use to quote attribute values.
	 * @return The character to use to quote attribute values. This must be a single
	 * quote (&#39;) or a double quote (&#34;). The default is a double quote.
	 */
	var QuoteChar(default, default):cs.Char16;
	@:overload(function(w:cs.system.io.TextWriter):Void {})
	@:overload(function(w:cs.system.io.Stream, encoding:cs.system.text.Encoding):Void {})
	function new(filename:String, encoding:cs.system.text.Encoding):Void;
	/** Closes this stream and the underlying stream. */
	function Close():Void;
	/** Flushes whatever is in the buffer to the underlying streams and also flushes the underlying stream. */
	function Flush():Void;
	/**
	 * Returns the closest prefix defined in the current namespace scope for the
	 * namespace URI.
	 * @param ns Namespace URI whose prefix you want to find.
	 * @return The matching prefix. Or  if no matching namespace URI is found in the
	 * current scope.
	 */
	function LookupPrefix(ns:String):String;
	/**
	 * Encodes the specified binary bytes as base64 and writes out the resulting text.
	 * @param buffer Byte array to encode.
	 * @param index The position within the buffer indicating the start of the bytes to
	 * write.
	 * @param count The number of bytes to write.
	 */
	function WriteBase64(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Void;
	/**
	 * Encodes the specified binary bytes as binhex and writes out the resulting text.
	 * @param buffer Byte array to encode.
	 * @param index The position in the buffer indicating the start of the bytes to
	 * write.
	 * @param count The number of bytes to write.
	 */
	function WriteBinHex(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Void;
	/**
	 * Writes out a <![CDATA[...]]> block containing the specified text.
	 * @param text Text to place inside the CDATA block.
	 */
	function WriteCData(text:String):Void;
	/**
	 * Forces the generation of a character entity for the specified Unicode character
	 * value.
	 * @param ch Unicode character for which to generate a character entity.
	 */
	function WriteCharEntity(ch:cs.Char16):Void;
	/**
	 * Writes text one buffer at a time.
	 * @param buffer Character array containing the text to write.
	 * @param index The position in the buffer indicating the start of the text to
	 * write.
	 * @param count The number of characters to write.
	 */
	function WriteChars(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void;
	/**
	 * Writes out a comment <!--...--> containing the specified text.
	 * @param text Text to place inside the comment.
	 */
	function WriteComment(text:String):Void;
	/**
	 * Writes the DOCTYPE declaration with the specified name and optional attributes.
	 * @param name The name of the DOCTYPE. This must be non-empty.
	 * @param pubid If non-null it also writes PUBLIC "pubid" "sysid" where  and  are
	 * replaced with the value of the given arguments.
	 * @param sysid If  is null and  is non-null it writes SYSTEM "sysid" where  is
	 * replaced with the value of this argument.
	 * @param subset If non-null it writes [subset] where subset is replaced with the
	 * value of this argument.
	 */
	function WriteDocType(name:String, pubid:String, sysid:String, subset:String):Void;
	/** Closes the previous  call. */
	function WriteEndAttribute():Void;
	/** Closes any open elements or attributes and puts the writer back in the Start state. */
	function WriteEndDocument():Void;
	/** Closes one element and pops the corresponding namespace scope. */
	function WriteEndElement():Void;
	/**
	 * Writes out an entity reference as .
	 * @param name Name of the entity reference.
	 */
	function WriteEntityRef(name:String):Void;
	/** Closes one element and pops the corresponding namespace scope. */
	function WriteFullEndElement():Void;
	/**
	 * Writes out the specified name, ensuring it is a valid name according to the W3C
	 * XML 1.0 recommendation.
	 * @param name Name to write.
	 */
	function WriteName(name:String):Void;
	/**
	 * Writes out the specified name, ensuring it is a valid  according to the W3C XML
	 * 1.0 recommendation.
	 * @param name Name to write.
	 */
	function WriteNmToken(name:String):Void;
	/**
	 * Writes out a processing instruction with a space between the name and text as
	 * follows: <?name text?>.
	 * @param name Name of the processing instruction.
	 * @param text Text to include in the processing instruction.
	 */
	function WriteProcessingInstruction(name:String, text:String):Void;
	/**
	 * Writes out the namespace-qualified name. This method looks up the prefix that is
	 * in scope for the given namespace.
	 * @param localName The local name to write.
	 * @param ns The namespace URI to associate with the name.
	 */
	function WriteQualifiedName(localName:String, ns:String):Void;
	@:overload(function(data:String):Void {})
	/**
	 * Writes raw markup manually from a character buffer.
	 * @param buffer Character array containing the text to write.
	 * @param index The position within the buffer indicating the start of the text to
	 * write.
	 * @param count The number of characters to write.
	 */
	function WriteRaw(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void;
	/**
	 * Writes the start of an attribute.
	 * @param prefix prefix of the attribute.
	 * @param localName of the attribute.
	 * @param ns of the attribute
	 */
	function WriteStartAttribute(prefix:String, localName:String, ns:String):Void;
	@:overload(function():Void {})
	/** Writes the XML declaration with the version "1.0". */
	function WriteStartDocument(standalone:Bool):Void;
	/**
	 * Writes the specified start tag and associates it with the given namespace and
	 * prefix.
	 * @param prefix The namespace prefix of the element.
	 * @param localName The local name of the element.
	 * @param ns The namespace URI to associate with the element. If this namespace is
	 * already in scope and has an associated prefix then the writer automatically
	 * writes that prefix also.
	 */
	function WriteStartElement(prefix:String, localName:String, ns:String):Void;
	/**
	 * Writes the given text content.
	 * @param text Text to write.
	 */
	function WriteString(text:String):Void;
	/**
	 * Generates and writes the surrogate character entity for the surrogate character
	 * pair.
	 * @param lowChar The low surrogate. This must be a value between  and .
	 * @param highChar The high surrogate. This must be a value between  and .
	 */
	function WriteSurrogateCharEntity(lowChar:cs.Char16, highChar:cs.Char16):Void;
	/**
	 * Writes out the given white space.
	 * @param ws The string of white space characters.
	 */
	function WriteWhitespace(ws:String):Void;
}
