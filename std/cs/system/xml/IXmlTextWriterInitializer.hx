package cs.system.xml;

/** Specifies implementation requirements for XML text writers that derive from this interface. */
@:native("System.Xml.IXmlTextWriterInitializer")
extern interface IXmlTextWriterInitializer {
	/**
	 * Specifies initialization requirements for XML text writers that implement this
	 * method.
	 * @param stream The stream to write to.
	 * @param encoding The character encoding of the stream.
	 * @param ownsStream to indicate the stream is closed by the writer when done;
	 * otherwise, .
	 */
	function SetOutput(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, ownsStream:Bool):Void;
}
