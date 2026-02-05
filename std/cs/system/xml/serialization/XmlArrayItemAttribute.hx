package cs.system.xml.serialization;

/** Represents an attribute that specifies the derived types that the  can place in a serialized array. */
@:native("System.Xml.Serialization.XmlArrayItemAttribute")
extern class XmlArrayItemAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the XML data type of the generated XML element.
	 * @return An XML schema definition (XSD) data type.
	 */
	var DataType(default, default):String;
	/**
	 * Gets or sets the name of the generated XML element.
	 * @return The name of the generated XML element. The default is the member
	 * identifier.
	 */
	var ElementName(default, default):String;
	/**
	 * Gets or sets a value that indicates whether the name of the generated XML
	 * element is qualified.
	 * @return One of the  values. The default is .
	 */
	var Form(default, default):cs.system.xml.schema.XmlSchemaForm;
	/**
	 * Gets or sets a value that indicates whether the  must serialize a member as an
	 * empty XML tag with the  attribute set to .
	 * @return if the  generates the  attribute; otherwise, , and no instance is
	 * generated. The default is .
	 */
	var IsNullable(default, default):Bool;
	/**
	 * Gets or sets the namespace of the generated XML element.
	 * @return The namespace of the generated XML element.
	 */
	var Namespace(default, default):String;
	/**
	 * Gets or sets the level in a hierarchy of XML elements that the  affects.
	 * @return The zero-based index of a set of indexes in an array of arrays.
	 */
	var NestingLevel(default, default):Int;
	/**
	 * Gets or sets the type allowed in an array.
	 * @return A  that is allowed in the array.
	 */
	var Type(default, default):cs.system.Type;
	@:overload(function():Void {})
	@:overload(function(elementName:String):Void {})
	@:overload(function(type:cs.system.Type):Void {})
	function new(elementName:String, type:cs.system.Type):Void;
}
