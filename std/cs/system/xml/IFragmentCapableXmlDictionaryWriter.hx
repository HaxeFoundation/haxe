package cs.system.xml;

/** Contains properties and methods that when implemented by a , allows processing of XML fragments. */
@:native("System.Xml.IFragmentCapableXmlDictionaryWriter")
extern interface IFragmentCapableXmlDictionaryWriter {
	/**
	 * Gets a value that indicates whether this  can process XML fragments.
	 * @return if this  can process XML fragments; otherwise, .
	 */
	var CanFragment(default, never):Bool;
	/** Ends the processing of an XML fragment. */
	function EndFragment():Void;
	/**
	 * Starts the processing of an XML fragment.
	 * @param stream The stream to write to.
	 * @param generateSelfContainedTextFragment If , any namespaces declared outside
	 * the fragment is declared again if used inside of it; if  the namespaces are not
	 * declared again.
	 */
	function StartFragment(stream:cs.system.io.Stream, generateSelfContainedTextFragment:Bool):Void;
	/**
	 * Writes an XML fragment to the underlying stream of the writer.
	 * @param buffer The buffer to write to.
	 * @param offset The starting position from which to write in .
	 * @param count The number of bytes to be written to the .
	 */
	function WriteFragment(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
}
