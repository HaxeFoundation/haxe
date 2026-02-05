package cs.system.xml.serialization;

/** Supports mappings between .NET Framework types and XML Schema data types. */
@:native("System.Xml.Serialization.XmlMapping")
extern class XmlMapping {
	/**
	 * Gets the name of the mapped element.
	 * @return The name of the mapped element.
	 */
	var ElementName(default, never):String;
	/**
	 * Gets the namespace of the mapped element.
	 * @return The namespace of the mapped element.
	 */
	var Namespace(default, never):String;
	/**
	 * Gets the name of the XSD element of the mapping.
	 * @return The XSD element name.
	 */
	var XsdElementName(default, never):String;
	/**
	 * Sets the key used to look up the mapping.
	 * @param key The lookup key.
	 */
	function SetKey(key:String):Void;
}
