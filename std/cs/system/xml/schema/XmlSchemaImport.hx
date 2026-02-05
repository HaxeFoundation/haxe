package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class is used to import schema components from other schemas. */
@:native("System.Xml.Schema.XmlSchemaImport")
extern class XmlSchemaImport extends cs.system.xml.schema.XmlSchemaExternal {
	/**
	 * Gets or sets the  property.
	 * @return The annotation.
	 */
	var Annotation(default, default):cs.system.xml.schema.XmlSchemaAnnotation;
	/**
	 * Gets or sets the target namespace for the imported schema as a Uniform Resource
	 * Identifier (URI) reference.
	 * @return The target namespace for the imported schema as a URI reference.
	 * Optional.
	 */
	var Namespace(default, default):String;
	function new():Void;
}
