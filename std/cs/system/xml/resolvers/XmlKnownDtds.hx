package cs.system.xml.resolvers;

/** The  enumeration is used by the  and defines which well-known DTDs that the  recognizes. */
@:native("System.Xml.Resolvers.XmlKnownDtds")
extern enum abstract XmlKnownDtds(Int) {
	var All = 65535;
	var None = 0;
	var Rss091 = 2;
	var Xhtml10 = 1;
	@:op(A | B) static function or(lhs:XmlKnownDtds, rhs:XmlKnownDtds):XmlKnownDtds;
	@:op(A & B) static function and(lhs:XmlKnownDtds, rhs:XmlKnownDtds):XmlKnownDtds;
	@:op(A ^ B) static function xor(lhs:XmlKnownDtds, rhs:XmlKnownDtds):XmlKnownDtds;
	@:op(~A) static function complement(value:XmlKnownDtds):XmlKnownDtds;
}
