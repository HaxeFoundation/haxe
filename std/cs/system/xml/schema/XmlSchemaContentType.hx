package cs.system.xml.schema;

/** Enumerations for the content model of the complex type. This represents the content in the post-schema-validation information set (infoset). */
@:native("System.Xml.Schema.XmlSchemaContentType")
extern enum XmlSchemaContentType {
	ElementOnly;
	Empty;
	Mixed;
	TextOnly;
}
