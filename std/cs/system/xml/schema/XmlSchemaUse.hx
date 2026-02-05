package cs.system.xml.schema;

/** Indicator of how the attribute is used. */
@:native("System.Xml.Schema.XmlSchemaUse")
extern enum abstract XmlSchemaUse(Int) {
	var None = 0;
	var Optional = 1;
	var Prohibited = 2;
	var Required = 3;
}
