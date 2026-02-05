package cs.system.xml.schema;

/** Represents the validity of an XML item validated by the  class. */
@:native("System.Xml.Schema.XmlSchemaValidity")
extern enum abstract XmlSchemaValidity(Int) {
	var Invalid = 2;
	var NotKnown = 0;
	var Valid = 1;
}
