package cs.system.xml.serialization;

/** Specifies that the public member value be serialized by the  as an encoded SOAP XML element. */
@:native("System.Xml.Serialization.SoapElementAttribute")
extern class SoapElementAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the XML Schema definition language (XSD) data type of the generated
	 * XML element.
	 * @return One of the XML Schema data types.
	 */
	var DataType(default, default):String;
	/**
	 * Gets or sets the name of the generated XML element.
	 * @return The name of the generated XML element. The default is the member
	 * identifier.
	 */
	var ElementName(default, default):String;
	/**
	 * Gets or sets a value that indicates whether the  must serialize a member that
	 * has the  attribute set to "1".
	 * @return if the  generates the  attribute; otherwise, .
	 */
	var IsNullable(default, default):Bool;
	@:overload(function():Void {})
	function new(elementName:String):Void;
}
