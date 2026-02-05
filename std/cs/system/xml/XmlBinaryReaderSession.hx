package cs.system.xml;

/** Enables optimized strings to be managed in a dynamic way. */
@:native("System.Xml.XmlBinaryReaderSession")
extern class XmlBinaryReaderSession {
	function new():Void;
	/**
	 * Creates an  from the input parameters and adds it to an internal collection.
	 * @param id The key value.
	 * @param value The value.
	 * @return The newly created  that is added to an internal collection.
	 */
	function Add(id:Int, value:String):cs.system.xml.XmlDictionaryString;
	/** Clears the internal collection of all contents. */
	function Clear():Void;
	@:overload(function(key:Int, result:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool {})
	@:overload(function(value:String, result:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool {})
	/**
	 * Checks whether the internal collection contains an entry matching a key.
	 * @param key The key to search on.
	 * @param result When this method returns, contains a string if an entry is found;
	 * otherwise, . This parameter is passed uninitialized.
	 * @return if an entry matching the  was found; otherwise, .
	 */
	function TryLookup(value:cs.system.xml.XmlDictionaryString, result:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool;
}
