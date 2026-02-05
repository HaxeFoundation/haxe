package cs.system.runtime.serialization.json;

/** Specifies the interface for initializing a JavaScript Object Notation (JSON) reader when reusing them to read from a particular stream or buffer. */
@:native("System.Runtime.Serialization.Json.IXmlJsonReaderInitializer")
extern interface IXmlJsonReaderInitializer {
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):Void {})
	/**
	 * Reinitializes a JavaScript Object Notation (JSON) enabled reader to a specified
	 * buffer that contains JSON-encoded data.
	 * @param buffer The input  buffer array from which to read.
	 * @param offset The starting position from which to read in .
	 * @param count The number of bytes that can be read from .
	 * @param encoding The  used by the reader.
	 * @param quotas The  to apply.
	 * @param onClose The  delegate to call when the reader is closed.
	 */
	function SetInput(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):Void;
}
