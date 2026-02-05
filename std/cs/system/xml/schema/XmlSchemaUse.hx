package cs.system.xml.schema;

/** Indicator of how the attribute is used. */
@:native("System.Xml.Schema.XmlSchemaUse")
extern enum XmlSchemaUse {
	None;
	Optional;
	Prohibited;
	Required;
}
