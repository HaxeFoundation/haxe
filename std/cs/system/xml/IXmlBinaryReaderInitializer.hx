package cs.system.xml;

/** Provides methods for reinitializing a binary reader to read a new document. */
@:native("System.Xml.IXmlBinaryReaderInitializer")
extern interface IXmlBinaryReaderInitializer {
	@:overload(function(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas, session:cs.system.xml.XmlBinaryReaderSession, onClose:cs.system.xml.OnXmlDictionaryReaderClose):Void {})
	/**
	 * Reinitializes the binary reader using the given input buffer.
	 * @param buffer The buffer from which to read.
	 * @param offset Starting position from which to read in .
	 * @param count Number of bytes that can be read from .
	 * @param dictionary to use.
	 * @param quotas to apply.
	 * @param session to use.
	 * @param onClose Delegate to call when the reader is closed.
	 */
	function SetInput(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, dictionary:cs.system.xml.IXmlDictionary, quotas:cs.system.xml.XmlDictionaryReaderQuotas, session:cs.system.xml.XmlBinaryReaderSession, onClose:cs.system.xml.OnXmlDictionaryReaderClose):Void;
}
