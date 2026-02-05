package cs.system.xml;

/** Implements a dictionary used to optimize Windows Communication Foundation (WCF)'s XML reader/writer implementations. */
@:native("System.Xml.XmlDictionary")
extern class XmlDictionary {
	/**
	 * Gets a  empty .
	 * @return A  empty .
	 */
	static var Empty(default, never):cs.system.xml.IXmlDictionary;
	@:overload(function():Void {})
	function new(capacity:Int):Void;
	/**
	 * Adds a string to the .
	 * @param value String to add to the dictionary.
	 * @return The  that was added.
	 */
	function Add(value:String):cs.system.xml.XmlDictionaryString;
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
