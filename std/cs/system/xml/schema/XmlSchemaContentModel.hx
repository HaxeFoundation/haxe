package cs.system.xml.schema;

/** Specifies the order and structure of the child elements of a type. */
@:native("System.Xml.Schema.XmlSchemaContentModel")
extern class XmlSchemaContentModel extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets the content of the type.
	 * @return Provides the content of the type.
	 */
	var Content(default, default):cs.system.xml.schema.XmlSchemaContent;
}
