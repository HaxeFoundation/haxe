package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class is used to include declarations and definitions from an external schema. The included declarations and definitions are then available for processing in the containing schema. */
@:native("System.Xml.Schema.XmlSchemaInclude")
extern class XmlSchemaInclude extends cs.system.xml.schema.XmlSchemaExternal {
	/**
	 * Gets or sets the  property.
	 * @return The annotation.
	 */
	var Annotation(default, default):cs.system.xml.schema.XmlSchemaAnnotation;
	function new():Void;
}
