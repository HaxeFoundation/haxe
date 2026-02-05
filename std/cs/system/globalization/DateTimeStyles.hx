package cs.system.globalization;

/** Defines the formatting options that customize string parsing for some date and time parsing methods. */
@:native("System.Globalization.DateTimeStyles")
extern enum abstract DateTimeStyles(Int) {
	var AdjustToUniversal = 16;
	var AllowInnerWhite = 4;
	var AllowLeadingWhite = 1;
	var AllowTrailingWhite = 2;
	var AllowWhiteSpaces = 7;
	var AssumeLocal = 32;
	var AssumeUniversal = 64;
	var NoCurrentDateDefault = 8;
	var None = 0;
	var RoundtripKind = 128;
	@:op(A | B) static function or(lhs:DateTimeStyles, rhs:DateTimeStyles):DateTimeStyles;
	@:op(A & B) static function and(lhs:DateTimeStyles, rhs:DateTimeStyles):DateTimeStyles;
	@:op(A ^ B) static function xor(lhs:DateTimeStyles, rhs:DateTimeStyles):DateTimeStyles;
	@:op(~A) static function complement(value:DateTimeStyles):DateTimeStyles;
}
