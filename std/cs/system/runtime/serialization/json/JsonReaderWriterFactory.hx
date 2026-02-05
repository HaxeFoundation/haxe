package cs.system.runtime.serialization.json;

/** Produces instances of  that can read data encoded with JavaScript Object Notation (JSON) from a stream or buffer and map it to an XML Infoset and instances of  that can map an XML Infoset to JSON and write JSON-encoded data to a stream. */
@:native("System.Runtime.Serialization.Json.JsonReaderWriterFactory")
extern class JsonReaderWriterFactory {
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, quotas:cs.system.xml.XmlDictionaryReaderQuotas):cs.system.xml.XmlDictionaryReader {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader {})
	/**
	 * Creates an  that can map buffers encoded with JavaScript Object Notation (JSON),
	 * with a specified size and offset and character encoding, to an XML Infoset.
	 * @param buffer The input  buffer array from which to read.
	 * @param offset Starting position from which to read in .
	 * @param count Number of bytes that can be read from .
	 * @param encoding The  that specifies the character encoding used by the reader.
	 * If  is specified as the value, the reader attempts to auto-detect the encoding.
	 * @param quotas The  used to prevent Denial of Service attacks when reading
	 * untrusted data.
	 * @param onClose The  delegate to call when the reader is closed. The default
	 * value is .
	 * @return An  that can read JavaScript Object Notation (JSON).
	 */
	static function CreateJsonReader(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):cs.system.xml.XmlDictionaryReader;
	@:overload(function(stream:cs.system.io.Stream):cs.system.xml.XmlDictionaryWriter {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding):cs.system.xml.XmlDictionaryWriter {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, ownsStream:Bool):cs.system.xml.XmlDictionaryWriter {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, ownsStream:Bool, indent:Bool):cs.system.xml.XmlDictionaryWriter {})
	/**
	 * Creates an  that writes data encoded with JSON to a stream.
	 * @param stream The output  for the JSON writer.
	 * @return An  that writes data encoded with JSON to the stream based on an XML
	 * Infoset.
	 */
	static function CreateJsonWriter(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, ownsStream:Bool, indent:Bool, indentChars:String):cs.system.xml.XmlDictionaryWriter;
}
