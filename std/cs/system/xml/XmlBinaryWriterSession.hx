package cs.system.xml;

/** Enables using a dynamic dictionary to compress common strings that appear in a message and maintain state. */
@:native("System.Xml.XmlBinaryWriterSession")
extern class XmlBinaryWriterSession {
	function new():Void;
	/** Clears out the internal collections. */
	function Reset():Void;
	/**
	 * Tries to add an  to the internal collection.
	 * @param value The  to add.
	 * @param key The key of the  that was successfully added.
	 * @return if the string could be added; otherwise, .
	 */
	function TryAdd(value:cs.system.xml.XmlDictionaryString, key:cs.Ref<Int>):Bool;
}
