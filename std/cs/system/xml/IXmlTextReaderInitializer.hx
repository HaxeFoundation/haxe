package cs.system.xml;

/** Specifies implementation requirements for XML text readers that derive from this interface. */
@:native("System.Xml.IXmlTextReaderInitializer")
extern interface IXmlTextReaderInitializer {
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):Void {})
	/**
	 * Specifies initialization requirements for XML text readers that read a buffer.
	 * @param buffer The buffer from which to read.
	 * @param offset The starting position from which to read in .
	 * @param count The number of bytes that can be read from .
	 * @param encoding The character encoding of the stream.
	 * @param quotas The  to apply.
	 * @param onClose The delegate to be called when the reader is closed.
	 */
	function SetInput(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, encoding:cs.system.text.Encoding, quotas:cs.system.xml.XmlDictionaryReaderQuotas, onClose:cs.system.xml.OnXmlDictionaryReaderClose):Void;
}
