package cs.system.xml;

/** Represents an interface that can be implemented by classes providing streams. */
@:native("System.Xml.IStreamProvider")
extern interface IStreamProvider {
	/**
	 * Gets a stream.
	 * @return A  object.
	 */
	function GetStream():cs.system.io.Stream;
	/**
	 * Releases a stream to output.
	 * @param stream The stream being released.
	 */
	function ReleaseStream(stream:cs.system.io.Stream):Void;
}
