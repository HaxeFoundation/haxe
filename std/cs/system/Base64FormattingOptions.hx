package cs.system;

/** Specifies whether relevant  and  methods insert line breaks in their output. */
@:native("System.Base64FormattingOptions")
extern enum abstract Base64FormattingOptions(Int) {
	var InsertLineBreaks = 1;
	var None = 0;
	@:op(A | B) static function or(lhs:Base64FormattingOptions, rhs:Base64FormattingOptions):Base64FormattingOptions;
	@:op(A & B) static function and(lhs:Base64FormattingOptions, rhs:Base64FormattingOptions):Base64FormattingOptions;
	@:op(A ^ B) static function xor(lhs:Base64FormattingOptions, rhs:Base64FormattingOptions):Base64FormattingOptions;
	@:op(~A) static function complement(value:Base64FormattingOptions):Base64FormattingOptions;
}
