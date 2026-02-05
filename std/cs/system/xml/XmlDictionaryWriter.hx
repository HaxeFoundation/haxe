package cs.system.xml;

/** Represents an abstract class that Windows Communication Foundation (WCF) derives from  to do serialization and deserialization. */
@:native("System.Xml.XmlDictionaryWriter")
extern class XmlDictionaryWriter extends cs.system.xml.XmlWriter {
	/**
	 * This property always returns . Its derived classes can override to return  if
	 * they support canonicalization.
	 * @return in all cases.
	 */
	var CanCanonicalize(default, never):Bool;
	@:overload(function(stream:cs.system.io.Stream):cs.system.xml.XmlDictionaryWriter {})
	@:overload(function(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary):cs.system.xml.XmlDictionaryWriter {})
	@:overload(function(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary, session:cs.system.xml.XmlBinaryWriterSession):cs.system.xml.XmlDictionaryWriter {})
	/**
	 * Creates an instance of  that writes WCF binary XML format.
	 * @param stream The stream to write to.
	 * @return An instance of .
	 */
	static function CreateBinaryWriter(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary, session:cs.system.xml.XmlBinaryWriterSession, ownsStream:Bool):cs.system.xml.XmlDictionaryWriter;
	/**
	 * Creates an instance of  from an existing .
	 * @param writer An instance of .
	 * @return An instance of .
	 */
	static function CreateDictionaryWriter(writer:cs.system.xml.XmlWriter):cs.system.xml.XmlDictionaryWriter;
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, maxSizeInBytes:Int, startInfo:String):cs.system.xml.XmlDictionaryWriter {})
	/**
	 * Creates an instance of  that writes XML in the MTOM format.
	 * @param stream The stream to write to.
	 * @param encoding The character encoding of the stream.
	 * @param maxSizeInBytes The maximum number of bytes that are buffered in the
	 * writer.
	 * @param startInfo An attribute in the ContentType SOAP header.
	 * @return An instance of .
	 */
	static function CreateMtomWriter(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, maxSizeInBytes:Int, startInfo:String, boundary:String, startUri:String, writeMessageHeaders:Bool, ownsStream:Bool):cs.system.xml.XmlDictionaryWriter;
	@:overload(function(stream:cs.system.io.Stream):cs.system.xml.XmlDictionaryWriter {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding):cs.system.xml.XmlDictionaryWriter {})
	/**
	 * Creates an instance of  that writes text XML.
	 * @param stream The stream to write to.
	 * @return An instance of .
	 */
	static function CreateTextWriter(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, ownsStream:Bool):cs.system.xml.XmlDictionaryWriter;
	/** When implemented by a derived class, it stops the canonicalization started by the matching  call. */
	function EndCanonicalization():Void;
	/**
	 * When implemented by a derived class, it starts the canonicalization.
	 * @param stream The stream to write to.
	 * @param includeComments to include comments; otherwise, .
	 * @param inclusivePrefixes The prefixes to be included.
	 */
	function StartCanonicalization(stream:cs.system.io.Stream, includeComments:Bool, inclusivePrefixes:cs.NativeArray<String>):Void;
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<Bool>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.DateTime>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.Decimal>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<Float>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.Guid>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<cs.Int16>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<Int>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<haxe.Int64>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<Single>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:String, namespaceUri:String, array:cs.NativeArray<cs.system.TimeSpan>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Bool>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.DateTime>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.Decimal>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Float>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.Guid>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.Int16>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Int>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<haxe.Int64>, offset:Int, count:Int):Void {})
	@:overload(function(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<Single>, offset:Int, count:Int):Void {})
	/**
	 * Writes nodes from a  array.
	 * @param prefix The namespace prefix.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @param array The array that contains the data.
	 * @param offset The starting index in the array.
	 * @param count The number of values to write from the array.
	 */
	function WriteArray(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, array:cs.NativeArray<cs.system.TimeSpan>, offset:Int, count:Int):Void;
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, value:String):Void {})
	/**
	 * Writes an attribute qualified name and value.
	 * @param prefix The prefix of the attribute.
	 * @param localName The local name of the attribute.
	 * @param namespaceUri The namespace URI of the attribute.
	 * @param value The attribute.
	 */
	function WriteAttributeString(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, value:String):Void;
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
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, value:String):Void {})
	/**
	 * Writes an element with a text content.
	 * @param prefix The prefix of the element.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 * @param value The element content.
	 */
	function WriteElementString(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString, value:String):Void;
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader, defattr:Bool):Void {})
	/**
	 * Writes the current XML node from an .
	 * @param reader The .
	 * @param defattr to copy the default attributes from the ; otherwise, .
	 */
	function WriteNode(reader:cs.system.xml.XmlReader, defattr:Bool):Void;
	/**
	 * Writes out the namespace-qualified name. This method looks up the prefix that is
	 * in scope for the given namespace.
	 * @param localName The local name of the qualified name.
	 * @param namespaceUri The namespace URI of the qualified name.
	 */
	function WriteQualifiedName(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void;
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void {})
	/**
	 * Writes the start of an attribute with the specified prefix, local name, and
	 * namespace URI.
	 * @param prefix The namespace prefix.
	 * @param localName The local name of the attribute.
	 * @param namespaceUri The namespace URI of the attribute.
	 */
	function WriteStartAttribute(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void;
	@:overload(function(localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void {})
	/**
	 * Writes the specified start tag and associates it with the given namespace and
	 * prefix.
	 * @param prefix The prefix of the element.
	 * @param localName The local name of the element.
	 * @param namespaceUri The namespace URI of the element.
	 */
	function WriteStartElement(prefix:String, localName:cs.system.xml.XmlDictionaryString, namespaceUri:cs.system.xml.XmlDictionaryString):Void;
	/**
	 * Writes the given text content.
	 * @param value The text to write.
	 */
	function WriteString(value:cs.system.xml.XmlDictionaryString):Void;
	@:overload(function(value:cs.system.Guid):Void {})
	@:overload(function(value:cs.system.TimeSpan):Void {})
	@:overload(function(value:cs.system.xml.IStreamProvider):Void {})
	@:overload(function(value:cs.system.xml.UniqueId):Void {})
	/**
	 * Writes a  value.
	 * @param value The  value to write.
	 */
	function WriteValue(value:cs.system.xml.XmlDictionaryString):Void;
	/**
	 * Asynchronously writes a value from an .
	 * @param value The  value to write.
	 * @return The task that represents the asynchronous  operation.
	 */
	function WriteValueAsync(value:cs.system.xml.IStreamProvider):cs.system.threading.tasks.Task;
	@:overload(function(localName:String, value:String):Void {})
	/**
	 * Writes a standard XML attribute in the current node.
	 * @param localName The local name of the attribute.
	 * @param value The value of the attribute.
	 */
	function WriteXmlAttribute(localName:cs.system.xml.XmlDictionaryString, value:cs.system.xml.XmlDictionaryString):Void;
	@:overload(function(prefix:String, namespaceUri:String):Void {})
	/**
	 * Writes a namespace declaration attribute.
	 * @param prefix The prefix that is bound to the given namespace.
	 * @param namespaceUri The namespace to which the prefix is bound.
	 */
	function WriteXmlnsAttribute(prefix:String, namespaceUri:cs.system.xml.XmlDictionaryString):Void;
}
