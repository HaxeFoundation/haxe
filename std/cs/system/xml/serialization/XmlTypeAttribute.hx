package cs.system.xml.serialization;

/** Controls the XML schema that is generated when the attribute target is serialized by the . */
@:native("System.Xml.Serialization.XmlTypeAttribute")
extern class XmlTypeAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a value that determines whether the resulting schema type is an XSD
	 * anonymous type.
	 * @return , if the resulting schema type is an XSD anonymous type; otherwise, .
	 */
	var AnonymousType(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether to include the type in XML schema
	 * documents.
	 * @return to include the type in XML schema documents; otherwise, .
	 */
	var IncludeInSchema(default, default):Bool;
	/**
	 * Gets or sets the namespace of the XML type.
	 * @return The namespace of the XML type.
	 */
	var Namespace(default, default):String;
	/**
	 * Gets or sets the name of the XML type.
	 * @return The name of the XML type.
	 */
	var TypeName(default, default):String;
	@:overload(function():Void {})
	function new(typeName:String):Void;
}
