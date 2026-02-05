package cs.system.xml.serialization;

/** Controls how the  serializes an enumeration member. */
@:native("System.Xml.Serialization.XmlEnumAttribute")
extern class XmlEnumAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the value generated in an XML-document instance when the 
	 * serializes an enumeration, or the value recognized when it deserializes the
	 * enumeration member.
	 * @return The value generated in an XML-document instance when the  serializes the
	 * enumeration, or the value recognized when it is deserializes the enumeration
	 * member.
	 */
	var Name(default, default):String;
	@:overload(function():Void {})
	function new(name:String):Void;
}
