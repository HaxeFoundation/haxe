package cs.system.xml;

/** An  class that the Windows Communication Foundation (WCF) derives from  to do serialization and deserialization. */
@:native("System.Xml.XmlDictionaryReader")
extern class XmlDictionaryReader extends cs.system.xml.XmlReader {
	/**
	 * This property always returns . Its derived classes can override to return  if
	 * they support canonicalization.
	 * @return Returns .
	 */
	var CanCanonicalize(default, never):Bool;
	/**
	 * Gets the quota values that apply to the current instance of this class.
	 * @return The  that applies to the current instance of this class.
	 */
	var Quotas(default, never):cs.system.xml.XmlDictionaryReaderQuotas;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas, session:cs.system.xml.XmlBinaryReaderSession):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas, session:cs.system.xml.XmlBinaryReaderSession, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas, session:cs.system.xml.XmlBinaryReaderSession):cs.system.xml.XmlDictionaryReader {})
	/**
	 * Creates an instance of  that can read .NET Binary XML Format.
	 * @param buffer The buffer from which to read.
	 * @param offset The starting position from which to read in .
	 * @param count The number of bytes that can be read from .
	 * @param dictionary to use.
	 * @param quotas The quotas that apply to this operation.
	 * @return An instance of .
	 */
	static function CreateBinaryReader(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas, session:cs.system.xml.XmlBinaryReaderSession, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader;
	/**
	 * Creates an instance of  from an existing .
	 * @param reader An instance of .
	 * @return An instance of .
	 */
	static function CreateDictionaryReader(reader:cs.system.xml.XmlReader):cs.system.xml.XmlDictionaryReader;
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, encodings:cs.NativeArray<cs.system.text.Encoding>, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, encodings:cs.NativeArray<cs.system.text.Encoding>, contentType:String, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encodings:cs.NativeArray<cs.system.text.Encoding>, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encodings:cs.NativeArray<cs.system.text.Encoding>, contentType:String, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, encodings:cs.NativeArray<cs.system.text.Encoding>, contentType:String, quotas:cs.system.xml.XmlDictionaryReaderQuotas, maxBufferSize:Int, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader {})
	/**
	 * Creates an instance of  that reads XML in the MTOM format.
	 * @param buffer The buffer from which to read.
	 * @param offset The starting position from which to read in .
	 * @param count The number of bytes that can be read from .
	 * @param encoding The possible character encoding of the input.
	 * @param quotas The quotas to apply to this reader.
	 * @return An instance of .
	 */
	static function CreateMtomReader(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encodings:cs.NativeArray<cs.system.text.Encoding>, contentType:String, quotas:cs.system.xml.XmlDictionaryReaderQuotas, maxBufferSize:Int, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader {})
	/**
	 * Creates an instance of .
	 * @param buffer The buffer from which to read.
	 * @param offset The starting position from which to read in .
	 * @param count The number of bytes that can be read from .
	 * @param encoding The  object that specifies the encoding properties to apply.
	 * @param quotas The  to apply.
	 * @param onClose The delegate to be called when the reader is closed.
	 * @return An instance of .
	 */
	static function CreateTextReader(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader;
	/** This method is not yet implemented. */
	function EndCanonicalization():Void;
	/**
	 * When overridden in a derived class, gets the value of an attribute.
	 * @param localName An  that represents the local name of the attribute.
	 * @param namespaceUri An  that represents the namespace of the attribute.
	 * @return The value of the attribute.
	 */
	function GetAttribute(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):String;
	/**
	 * Gets non-atomized names.
	 * @param localName The local name.
	 * @param namespaceUri The namespace for the local .
	 */
	function GetNonAtomizedNames(localName:cs.Ref<String>, namespaceUri:cs.Ref<String>):Void;
	@:overload(function(localNames:cs.NativeArray<String>, namespaceUri:String):Int {})
	/**
	 * Gets the index of the local name of the current node within an array of names.
	 * @param localNames The string array of local names to be searched.
	 * @param namespaceUri The namespace of current node.
	 * @return The index of the local name of the current node within an array of
	 * names.
	 */
	function IndexOfLocalName(localNames:cs.NativeArray<cs.system.xml.XmlDictionaryString>, namespaceUri:cs.system.xml.XmlDictionaryString):Int;
	@:overload(function(localName:String):Bool {})
	/**
	 * Checks whether the parameter, , is the local name of the current node.
	 * @param localName The local name of the current node.
	 * @return if  matches local name of the current node; otherwise .
	 */
	function IsLocalName(localName:cs.system.xml.XmlDictionaryString):Bool;
	@:overload(function(namespaceUri:String):Bool {})
	/**
	 * Checks whether the parameter, , is the namespace of the current node.
	 * @param namespaceUri The namespace of current node.
	 * @return if  matches namespace of the current node; otherwise .
	 */
	function IsNamespaceUri(namespaceUri:cs.system.xml.XmlDictionaryString):Bool;
	/**
	 * Checks whether the reader is positioned at the start of an array. This class
	 * returns , but derived classes that have the concept of arrays might return .
	 * @param type Type of the node, if a valid node; otherwise .
	 * @return if the reader is positioned at the start of an array node; otherwise .
	 */
	function IsStartArray(type:cs.Ref<cs.system.Type>):Bool;
	/**
	 * Tests whether the first tag is a start tag or empty element tag and if the local
	 * name and namespace URI match those of the current node.
	 * @param localName An  that represents the local name of the attribute.
	 * @param namespaceUri An  that represents the namespace of the attribute.
	 * @return if the first tag in the array is a start tag or empty element tag and
	 * matches  and ; otherwise .
	 */
	function IsStartElement(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Bool;
	@:overload(function():Void {})
	@:overload(function(name:String):Void {})
	@:overload(function(localName:String, namespaceUri:String):Void {})
	/** Tests whether the current content node is a start element or an empty element. */
	function MoveToStartElement(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void;
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<Bool>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.DateTime>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.Decimal>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<Float>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.Guid>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<cs.Int16>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<Int>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<haxe.Int64>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<Single>, offset:Int, count:Int):Int {})
	@:overload(function(localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.TimeSpan>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Bool>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.DateTime>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.Decimal>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Float>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.Guid>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.Int16>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Int>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<haxe.Int64>, offset:Int, count:Int):Int {})
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Single>, offset:Int, count:Int):Int {})
	/**
	 * Reads repeated occurrences of  nodes into a typed array.
	 * @param localName The local name of the element.
	 * @param namespaceUri The local name of the element.
	 * @param array The array into which the nodes are put.
	 * @param offset The starting index in the array.
	 * @param count The number of nodes to put in the array.
	 * @return The number of nodes put in the array.
	 */
	function ReadArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.TimeSpan>, offset:Int, count:Int):Int;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<Bool> {})
	/**
	 * Reads repeated occurrences of  nodes into a typed array.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return A  array of the  nodes.
	 */
	function ReadBooleanArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<Bool>;
	/**
	 * Converts a node's content to a specified type.
	 * @param type The  of the value to be returned.
	 * @param namespaceResolver An  object that is used to resolve any namespace
	 * prefixes related to type conversion. For example, this can be used when
	 * converting an  object to an xs:string. This value can be a null reference.
	 * @return The concatenated text content or attribute value converted to the
	 * requested type.
	 */
	function ReadContentAs(type:cs.system.Type, namespaceResolver:cs.system.xml.IXmlNamespaceResolver):Dynamic;
	/**
	 * Reads the content and returns the Base64 decoded binary bytes.
	 * @return A byte array that contains the Base64 decoded binary bytes.
	 */
	function ReadContentAsBase64():cs.NativeArray<cs.UInt8>;
	/**
	 * Reads the content and returns the  decoded binary bytes.
	 * @return A byte array that contains the  decoded binary bytes.
	 */
	function ReadContentAsBinHex():cs.NativeArray<cs.UInt8>;
	/**
	 * Reads the content into a  array.
	 * @param chars The array into which the characters are put.
	 * @param offset The starting index in the array.
	 * @param count The number of characters to put in the array.
	 * @return Number of characters read.
	 */
	function ReadContentAsChars(chars:cs.NativeArray<cs.Char16>, offset:Int, count:Int):Int;
	/**
	 * Converts a node's content to .
	 * @return The  representation of node's content.
	 */
	function ReadContentAsDecimal():cs.system.Decimal;
	/**
	 * Converts a node's content to .
	 * @return The  representation of node's content.
	 */
	function ReadContentAsFloat():Single;
	/**
	 * Converts a node's content to .
	 * @return The  representation of node's content.
	 */
	function ReadContentAsGuid():cs.system.Guid;
	/**
	 * Converts a node's content to a qualified name representation.
	 * @param localName The  part of the qualified name ( parameter).
	 * @param namespaceUri The  part of the qualified name ( parameter).
	 */
	function ReadContentAsQualifiedName(localName:cs.Ref<String>, namespaceUri:cs.Ref<String>):Void;
	@:overload(function():String {})
	@:overload(function(strings:cs.NativeArray<String>, index:cs.Ref<Int>):String {})
	/**
	 * Converts a node's content to a string.
	 * @return The node content in a string representation.
	 */
	function ReadContentAsString(strings:cs.NativeArray<cs.system.xml.XmlDictionaryString>, index:cs.Ref<Int>):String;
	/**
	 * Converts a node's content to .
	 * @return representation of node's content.
	 */
	function ReadContentAsTimeSpan():cs.system.TimeSpan;
	/**
	 * Converts a node's content to a unique identifier.
	 * @return The node's content represented as a unique identifier.
	 */
	function ReadContentAsUniqueId():cs.system.xml.UniqueId;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<cs.system.DateTime> {})
	/**
	 * Converts a node's content to a  array.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return The node's content represented as a  array.
	 */
	function ReadDateTimeArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<cs.system.DateTime>;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<cs.system.Decimal> {})
	/**
	 * Converts a node's content to a  array.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return The node's content represented as a  array.
	 */
	function ReadDecimalArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<cs.system.Decimal>;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<Float> {})
	/**
	 * Converts a node's content to a  array.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return The node's content represented as a  array.
	 */
	function ReadDoubleArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<Float>;
	/**
	 * Converts a node's content to a array of Base64 bytes.
	 * @return The node's content represented as an array of Base64 bytes.
	 */
	function ReadElementContentAsBase64():cs.NativeArray<cs.UInt8>;
	/**
	 * Converts a node's content to an array of  bytes.
	 * @return The node's content represented as an array of  bytes.
	 */
	function ReadElementContentAsBinHex():cs.NativeArray<cs.UInt8>;
	/**
	 * Converts an element's content to a .
	 * @return The node's content represented as a .
	 */
	function ReadElementContentAsBoolean():Bool;
	/**
	 * Converts an element's content to a .
	 * @return The node's content represented as a .
	 */
	function ReadElementContentAsDateTime():cs.system.DateTime;
	/**
	 * Converts an element's content to a .
	 * @return The node's content represented as a .
	 */
	function ReadElementContentAsDecimal():cs.system.Decimal;
	/**
	 * Converts an element's content to a .
	 * @return The node's content represented as a .
	 */
	function ReadElementContentAsDouble():Float;
	/**
	 * Converts an element's content to a floating point number ().
	 * @return The node's content represented as a floating point number ().
	 */
	function ReadElementContentAsFloat():Single;
	/**
	 * Converts an element's content to a .
	 * @return The node's content represented as a .
	 */
	function ReadElementContentAsGuid():cs.system.Guid;
	/**
	 * Converts an element's content to an integer ().
	 * @return The node's content represented as an integer ().
	 */
	function ReadElementContentAsInt():Int;
	/**
	 * Converts an element's content to a long integer ().
	 * @return The node's content represented as a long integer ().
	 */
	function ReadElementContentAsLong():haxe.Int64;
	/**
	 * Converts an element's content to a .
	 * @return The node's content represented as a .
	 */
	function ReadElementContentAsString():String;
	/**
	 * Converts an element's content to a .
	 * @return The node's content represented as a .
	 */
	function ReadElementContentAsTimeSpan():cs.system.TimeSpan;
	/**
	 * Converts an element's content to a unique identifier.
	 * @return The node's content represented as a unique identifier.
	 */
	function ReadElementContentAsUniqueId():cs.system.xml.UniqueId;
	@:overload(function():Void {})
	@:overload(function(name:String):Void {})
	@:overload(function(localName:String, namespaceUri:String):Void {})
	/** Checks whether the current node is an element and advances the reader to the next node. */
	function ReadFullStartElement(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<cs.system.Guid> {})
	/**
	 * Reads the contents of a series of nodes with the given  and  into an array of .
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return An array of .
	 */
	function ReadGuidArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<cs.system.Guid>;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<cs.Int16> {})
	/**
	 * Reads the contents of a series of nodes with the given  and  into an array of 
	 * integers ().
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return An array of  integers ().
	 */
	function ReadInt16Array(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<cs.Int16>;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<Int> {})
	/**
	 * Reads the contents of a series of nodes with the given  and  into an array of
	 * integers ().
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return An array of integers ().
	 */
	function ReadInt32Array(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<Int>;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<haxe.Int64> {})
	/**
	 * Reads the contents of a series of nodes with the given  and  into an array of 
	 * integers ().
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return An array of  integers ().
	 */
	function ReadInt64Array(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<haxe.Int64>;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<Single> {})
	/**
	 * Reads the contents of a series of nodes with the given  and  into an array of 
	 * numbers ().
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return An array of  numbers ().
	 */
	function ReadSingleArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<Single>;
	/**
	 * Checks whether the current node is an element with the given  and  and advances
	 * the reader to the next node.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 */
	function ReadStartElement(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void;
	/**
	 * Reads the contents of the current node into a string.
	 * @return A string that contains the contents of the current node.
	 */
	function ReadString():String;
	@:overload(function(localName:String, namespaceUri:String):cs.NativeArray<cs.system.TimeSpan> {})
	/**
	 * Reads the contents of a series of nodes with the given  and  into a  array.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @return A  array.
	 */
	function ReadTimeSpanArray(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):cs.NativeArray<cs.system.TimeSpan>;
	/**
	 * Not implemented.
	 * @param buffer The buffer from which to read.
	 * @param offset The starting position from which to read in .
	 * @param count The number of bytes that can be read from .
	 * @return Not implemented.
	 */
	function ReadValueAsBase64(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	/**
	 * This method is not yet implemented.
	 * @param stream The stream to read from.
	 * @param includeComments Determines whether comments are included.
	 * @param inclusivePrefixes The prefixes to be included.
	 */
	function StartCanonicalization(stream:cs.system.io.Stream, includeComments:Bool, inclusivePrefixes:cs.NativeArray<String>):Void;
	/**
	 * Not implemented in this class (it always returns ). May be overridden in derived
	 * classes.
	 * @param count Returns 0, unless overridden in a derived class.
	 * @return , unless overridden in a derived class.
	 */
	function TryGetArrayLength(count:cs.Ref<Int>):Bool;
	/**
	 * Not implemented in this class (it always returns ). May be overridden in derived
	 * classes.
	 * @param length Returns 0, unless overridden in a derived class.
	 * @return , unless overridden in a derived class.
	 */
	function TryGetBase64ContentLength(length:cs.Ref<Int>):Bool;
	/**
	 * Not implemented in this class (it always returns ). May be overridden in derived
	 * classes.
	 * @param localName Returns , unless overridden in a derived class. .
	 * @return , unless overridden in a derived class.
	 */
	function TryGetLocalNameAsDictionaryString(localName:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool;
	/**
	 * Not implemented in this class (it always returns ). May be overridden in derived
	 * classes.
	 * @param namespaceUri Returns , unless overridden in a derived class.
	 * @return , unless overridden in a derived class.
	 */
	function TryGetNamespaceUriAsDictionaryString(namespaceUri:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool;
	/**
	 * Not implemented in this class (it always returns ). May be overridden in derived
	 * classes.
	 * @param value Returns , unless overridden in a derived class.
	 * @return , unless overridden in a derived class.
	 */
	function TryGetValueAsDictionaryString(value:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool;
}
