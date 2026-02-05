package cs.system.xml.schema;

/** Enumerations for the content model of the complex type. This represents the content in the post-schema-validation information set (infoset). */
@:native("System.Xml.Schema.XmlSchemaContentType")
extern enum abstract XmlSchemaContentType(Int) {
	var ElementOnly = 2;
	var Empty = 1;
	var Mixed = 3;
	var TextOnly = 0;
}
