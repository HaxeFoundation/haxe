package cs.system.xml.schema;

/** Provides different methods for preventing derivation. */
@:native("System.Xml.Schema.XmlSchemaDerivationMethod")
extern enum abstract XmlSchemaDerivationMethod(Int) {
	var All = 255;
	var Empty = 0;
	var Extension = 2;
	var List = 8;
	var None = 256;
	var Restriction = 4;
	var Substitution = 1;
	var Union = 16;
	@:op(A | B) static function or(lhs:XmlSchemaDerivationMethod, rhs:XmlSchemaDerivationMethod):XmlSchemaDerivationMethod;
	@:op(A & B) static function and(lhs:XmlSchemaDerivationMethod, rhs:XmlSchemaDerivationMethod):XmlSchemaDerivationMethod;
	@:op(A ^ B) static function xor(lhs:XmlSchemaDerivationMethod, rhs:XmlSchemaDerivationMethod):XmlSchemaDerivationMethod;
	@:op(~A) static function complement(value:XmlSchemaDerivationMethod):XmlSchemaDerivationMethod;
}
