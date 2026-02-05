package cs.system.xml.schema;

/** Provides information about the validation mode of  and  element replacements. */
@:native("System.Xml.Schema.XmlSchemaContentProcessing")
extern enum abstract XmlSchemaContentProcessing(Int) {
	var Lax = 2;
	var None = 0;
	var Skip = 1;
	var Strict = 3;
}
