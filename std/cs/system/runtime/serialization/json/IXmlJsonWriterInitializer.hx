package cs.system.runtime.serialization.json;

/** Specifies the interface for initializing a JavaScript Object Notation (JSON) writer when reusing them to write to a particular output stream. */
@:native("System.Runtime.Serialization.Json.IXmlJsonWriterInitializer")
extern interface IXmlJsonWriterInitializer {
	/**
	 * Initializes (or reinitializes) a JavaScript Object Notation (JSON) writer to a
	 * specified output stream with specified character encoding.
	 * @param stream The output  to which the writer writes.
	 * @param encoding The  that specifies the character encoding of the output stream.
	 * @param ownsStream If , the output stream is closed by the writer when done;
	 * otherwise .
	 */
	function SetOutput(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, ownsStream:Bool):Void;
}
