package cs.system.xml.linq;

/** Specifies load options when parsing XML. */
@:native("System.Xml.Linq.LoadOptions")
extern enum abstract LoadOptions(Int) {
	var None = 0;
	var PreserveWhitespace = 1;
	var SetBaseUri = 2;
	var SetLineInfo = 4;
	@:op(A | B) static function or(lhs:LoadOptions, rhs:LoadOptions):LoadOptions;
	@:op(A & B) static function and(lhs:LoadOptions, rhs:LoadOptions):LoadOptions;
	@:op(A ^ B) static function xor(lhs:LoadOptions, rhs:LoadOptions):LoadOptions;
	@:op(~A) static function complement(value:LoadOptions):LoadOptions;
}
