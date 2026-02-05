package cs.system.xml;

/** Specifies whether to remove duplicate namespace declarations in the . */
@:native("System.Xml.NamespaceHandling")
extern enum abstract NamespaceHandling(Int) {
	var Default = 0;
	var OmitDuplicates = 1;
	@:op(A | B) static function or(lhs:NamespaceHandling, rhs:NamespaceHandling):NamespaceHandling;
	@:op(A & B) static function and(lhs:NamespaceHandling, rhs:NamespaceHandling):NamespaceHandling;
	@:op(A ^ B) static function xor(lhs:NamespaceHandling, rhs:NamespaceHandling):NamespaceHandling;
	@:op(~A) static function complement(value:NamespaceHandling):NamespaceHandling;
}
