package cs.system.xml.linq;

/** Specifies serialization options. */
@:native("System.Xml.Linq.SaveOptions")
extern enum abstract SaveOptions(Int) {
	var DisableFormatting = 1;
	var None = 0;
	var OmitDuplicateNamespaces = 2;
	@:op(A | B) static function or(lhs:SaveOptions, rhs:SaveOptions):SaveOptions;
	@:op(A & B) static function and(lhs:SaveOptions, rhs:SaveOptions):SaveOptions;
	@:op(A ^ B) static function xor(lhs:SaveOptions, rhs:SaveOptions):SaveOptions;
	@:op(~A) static function complement(value:SaveOptions):SaveOptions;
}
