package cs.system.xml.serialization;

/** Specifies that the member can be further detected by using an enumeration. */
@:native("System.Xml.Serialization.XmlChoiceIdentifierAttribute")
extern class XmlChoiceIdentifierAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the name of the field that returns the enumeration to use when
	 * detecting types.
	 * @return The name of a field that returns an enumeration.
	 */
	var MemberName(default, default):String;
	@:overload(function():Void {})
	function new(name:String):Void;
}
