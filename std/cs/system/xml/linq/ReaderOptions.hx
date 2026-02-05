package cs.system.xml.linq;

/** Specifies whether to omit duplicate namespaces when loading an  with an . */
@:native("System.Xml.Linq.ReaderOptions")
extern enum abstract ReaderOptions(Int) {
	var None = 0;
	var OmitDuplicateNamespaces = 1;
	@:op(A | B) static function or(lhs:ReaderOptions, rhs:ReaderOptions):ReaderOptions;
	@:op(A & B) static function and(lhs:ReaderOptions, rhs:ReaderOptions):ReaderOptions;
	@:op(A ^ B) static function xor(lhs:ReaderOptions, rhs:ReaderOptions):ReaderOptions;
	@:op(~A) static function complement(value:ReaderOptions):ReaderOptions;
}
