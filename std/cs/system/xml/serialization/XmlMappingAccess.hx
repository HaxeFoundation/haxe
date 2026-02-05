package cs.system.xml.serialization;

/** Specifies whether a mapping is read, write, or both. */
@:native("System.Xml.Serialization.XmlMappingAccess")
extern enum abstract XmlMappingAccess(Int) {
	var None = 0;
	var Read = 1;
	var Write = 2;
	@:op(A | B) static function or(lhs:XmlMappingAccess, rhs:XmlMappingAccess):XmlMappingAccess;
	@:op(A & B) static function and(lhs:XmlMappingAccess, rhs:XmlMappingAccess):XmlMappingAccess;
	@:op(A ^ B) static function xor(lhs:XmlMappingAccess, rhs:XmlMappingAccess):XmlMappingAccess;
	@:op(~A) static function complement(value:XmlMappingAccess):XmlMappingAccess;
}
