package cs.system.xml;

/** An  that defines the contract that an Xml dictionary must implement to be used by  and  implementations. */
@:native("System.Xml.IXmlDictionary")
extern interface IXmlDictionary {
	@:overload(function(key:Int, result:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool {})
	@:overload(function(value:String, result:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool {})
	/**
	 * Attempts to look up an entry in the dictionary.
	 * @param key Key to look up.
	 * @param result If  is defined, the  that is mapped to the key; otherwise .
	 * @return if key is in the dictionary; otherwise, .
	 */
	function TryLookup(value:cs.system.xml.XmlDictionaryString, result:cs.Ref<cs.system.xml.XmlDictionaryString>):Bool;
}
