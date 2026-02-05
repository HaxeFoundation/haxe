package cs.system.xml.schema;

/** Specifies schema validation options used by the  and  classes. */
@:native("System.Xml.Schema.XmlSchemaValidationFlags")
extern enum abstract XmlSchemaValidationFlags(Int) {
	var AllowXmlAttributes = 16;
	var None = 0;
	var ProcessIdentityConstraints = 8;
	var ProcessInlineSchema = 1;
	var ProcessSchemaLocation = 2;
	var ReportValidationWarnings = 4;
	@:op(A | B) static function or(lhs:XmlSchemaValidationFlags, rhs:XmlSchemaValidationFlags):XmlSchemaValidationFlags;
	@:op(A & B) static function and(lhs:XmlSchemaValidationFlags, rhs:XmlSchemaValidationFlags):XmlSchemaValidationFlags;
	@:op(A ^ B) static function xor(lhs:XmlSchemaValidationFlags, rhs:XmlSchemaValidationFlags):XmlSchemaValidationFlags;
	@:op(~A) static function complement(value:XmlSchemaValidationFlags):XmlSchemaValidationFlags;
}
