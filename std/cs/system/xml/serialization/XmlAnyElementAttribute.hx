package cs.system.xml.serialization;

/** Specifies that the member (a field that returns an array of  or  objects) contains objects that represent any XML element that has no corresponding member in the object being serialized or deserialized. */
@:native("System.Xml.Serialization.XmlAnyElementAttribute")
extern class XmlAnyElementAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the XML element name.
	 * @return The name of the XML element.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the XML namespace generated in the XML document.
	 * @return An XML namespace.
	 */
	var Namespace(default, default):String;
	/**
	 * Gets or sets the explicit order in which the elements are serialized or
	 * deserialized.
	 * @return The order of the code generation.
	 */
	var Order(default, default):Int;
	@:overload(function():Void {})
	@:overload(function(name:String):Void {})
	function new(name:String, ns:String):Void;
}
