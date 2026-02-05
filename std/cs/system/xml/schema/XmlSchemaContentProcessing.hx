package cs.system.xml.schema;

/** Provides information about the validation mode of  and  element replacements. */
@:native("System.Xml.Schema.XmlSchemaContentProcessing")
extern enum XmlSchemaContentProcessing {
	Lax;
	None;
	Skip;
	Strict;
}
