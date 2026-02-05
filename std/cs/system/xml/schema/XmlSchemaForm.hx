package cs.system.xml.schema;

/** Indicates if attributes or elements need to be qualified with a namespace prefix. */
@:native("System.Xml.Schema.XmlSchemaForm")
extern enum abstract XmlSchemaForm(Int) {
	var None = 0;
	var Qualified = 1;
	var Unqualified = 2;
}
