package cs.system.xml.serialization;

/** Allows the  to recognize a type when it serializes or deserializes an object. */
@:native("System.Xml.Serialization.XmlIncludeAttribute")
extern class XmlIncludeAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the type of the object to include.
	 * @return The  of the object to include.
	 */
	var Type(default, default):cs.system.Type;
	function new(type:cs.system.Type):Void;
}
