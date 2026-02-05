package cs.system.xml;

/** Specifies implementation requirements for XML binary writers that derive from this interface. */
@:native("System.Xml.IXmlBinaryWriterInitializer")
extern interface IXmlBinaryWriterInitializer {
	/**
	 * Specifies initialization requirements for XML binary writers that implement this
	 * method.
	 * @param stream The stream to write to.
	 * @param dictionary The  to use.
	 * @param session The  to use.
	 * @param ownsStream to indicate the stream is closed by the writer when done;
	 * otherwise, .
	 */
	function SetOutput(stream:cs.system.io.Stream, dictionary:cs.system.xml.IXmlDictionary, session:cs.system.xml.XmlBinaryWriterSession, ownsStream:Bool):Void;
}
