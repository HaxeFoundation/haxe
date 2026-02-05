package cs.system.xml.schema;

/** The base class for any element that can contain annotation elements. */
@:native("System.Xml.Schema.XmlSchemaAnnotated")
extern class XmlSchemaAnnotated extends cs.system.xml.schema.XmlSchemaObject {
	/**
	 * Gets or sets the  property.
	 * @return An  representing the  property.
	 */
	var Annotation(default, default):cs.system.xml.schema.XmlSchemaAnnotation;
	/**
	 * Gets or sets the string id.
	 * @return The string id. The default is . Optional.
	 */
	var Id(default, default):String;
	/**
	 * Gets or sets the qualified attributes that do not belong to the current schema's
	 * target namespace.
	 * @return An array of qualified  objects that do not belong to the schema's target
	 * namespace.
	 */
	var UnhandledAttributes(default, default):cs.NativeArray<cs.system.xml.XmlAttribute>;
	function new():Void;
}
