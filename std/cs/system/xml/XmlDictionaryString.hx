package cs.system.xml;

/** Represents an entry stored in a . */
@:native("System.Xml.XmlDictionaryString")
extern class XmlDictionaryString {
	/**
	 * Gets an  representing the empty string.
	 * @return An  representing the empty string.
	 */
	static var Empty(default, never):cs.system.xml.XmlDictionaryString;
	/**
	 * Represents the  passed to the constructor of this instance of .
	 * @return The  for this dictionary entry.
	 */
	var Dictionary(default, never):cs.system.xml.IXmlDictionary;
	/**
	 * Gets the integer key for this instance of the class.
	 * @return The integer key for this instance of the class.
	 */
	var Key(default, never):Int;
	/**
	 * Gets the string value for this instance of the class.
	 * @return The string value for this instance of the class.
	 */
	var Value(default, never):String;
	function new(dictionary:cs.system.xml.IXmlDictionary, value:String, key:Int):Void;
	/**
	 * Displays a text representation of this object.
	 * @return The string value for this instance of the class.
	 */
	function ToString():String;
}
