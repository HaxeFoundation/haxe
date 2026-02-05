package cs.system.xml.serialization;

/** Controls XML serialization of the attribute target as an XML root element. */
@:native("System.Xml.Serialization.XmlRootAttribute")
extern class XmlRootAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the XSD data type of the XML root element.
	 * @return An XSD (XML Schema Document) data type.
	 */
	var DataType(default, default):String;
	/**
	 * Gets or sets the name of the XML element that is generated and recognized by the
	 * class's  and  methods, respectively.
	 * @return The name of the XML root element that is generated and recognized in an
	 * XML-document instance. The default is the name of the serialized class.
	 */
	var ElementName(default, default):String;
	/**
	 * Gets or sets a value that indicates whether the  must serialize a member that is
	 * set to  into the  attribute set to .
	 * @return if the  generates the  attribute; otherwise, .
	 */
	var IsNullable(default, default):Bool;
	/**
	 * Gets or sets the namespace for the XML root element.
	 * @return The namespace for the XML element.
	 */
	var Namespace(default, default):String;
	@:overload(function():Void {})
	function new(elementName:String):Void;
}
