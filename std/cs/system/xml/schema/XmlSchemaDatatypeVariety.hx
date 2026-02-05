package cs.system.xml.schema;

/** Specifies the W3C XML schema data type variety of the type. */
@:native("System.Xml.Schema.XmlSchemaDatatypeVariety")
extern enum abstract XmlSchemaDatatypeVariety(Int) {
	var Atomic = 0;
	var List = 1;
	var Union = 2;
}
