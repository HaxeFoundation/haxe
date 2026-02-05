package cs.system.globalization;

/** Defines the formatting options that customize string parsing for the  and  methods. */
@:native("System.Globalization.TimeSpanStyles")
extern enum abstract TimeSpanStyles(Int) {
	var AssumeNegative = 1;
	var None = 0;
	@:op(A | B) static function or(lhs:TimeSpanStyles, rhs:TimeSpanStyles):TimeSpanStyles;
	@:op(A & B) static function and(lhs:TimeSpanStyles, rhs:TimeSpanStyles):TimeSpanStyles;
	@:op(A ^ B) static function xor(lhs:TimeSpanStyles, rhs:TimeSpanStyles):TimeSpanStyles;
	@:op(~A) static function complement(value:TimeSpanStyles):TimeSpanStyles;
}
