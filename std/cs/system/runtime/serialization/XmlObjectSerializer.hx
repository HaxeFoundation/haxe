package cs.system.runtime.serialization;

/** Provides the base class used to serialize objects as XML streams or documents. This class is abstract. */
@:native("System.Runtime.Serialization.XmlObjectSerializer")
extern class XmlObjectSerializer {
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader):Bool {})
	/**
	 * Gets a value that specifies whether the  is positioned over an XML element that
	 * can be read.
	 * @param reader An  used to read the XML stream or document.
	 * @return if the reader can read the data; otherwise, .
	 */
	function IsStartObject(reader:cs.system.xml.XmlReader):Bool;
	@:overload(function(stream:cs.system.io.Stream):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlReader):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader, verifyObjectName:Bool):Dynamic {})
	/**
	 * Reads the XML stream or document with a  and returns the deserialized object.
	 * @param stream A  used to read the XML stream or document.
	 * @return The deserialized object.
	 */
	function ReadObject(reader:cs.system.xml.XmlReader, verifyObjectName:Bool):Dynamic;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter):Void {})
	/**
	 * Writes the end of the object data as a closing XML element to the XML document
	 * or stream with an .
	 * @param writer An  used to write the XML document or stream.
	 */
	function WriteEndObject(writer:cs.system.xml.XmlWriter):Void;
	@:overload(function(stream:cs.system.io.Stream, graph:Dynamic):Void {})
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Writes the complete content (start, content, and end) of the object to the XML
	 * document or stream with the specified .
	 * @param stream A  used to write the XML document or stream.
	 * @param graph The object that contains the data to write to the stream.
	 */
	function WriteObject(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Writes only the content of the object to the XML document or stream using the
	 * specified .
	 * @param writer An  used to write the XML document or stream.
	 * @param graph The object that contains the content to write.
	 */
	function WriteObjectContent(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Writes the start of the object's data as an opening XML element using the
	 * specified .
	 * @param writer An  used to write the XML document.
	 * @param graph The object to serialize.
	 */
	function WriteStartObject(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
}
