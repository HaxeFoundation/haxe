package cs.system.globalization;

/** Determines the styles permitted in numeric string arguments that are passed to the  and  methods of the integral and floating-point numeric types. */
@:native("System.Globalization.NumberStyles")
extern enum abstract NumberStyles(Int) {
	var AllowCurrencySymbol = 256;
	var AllowDecimalPoint = 32;
	var AllowExponent = 128;
	var AllowHexSpecifier = 512;
	var AllowLeadingSign = 4;
	var AllowLeadingWhite = 1;
	var AllowParentheses = 16;
	var AllowThousands = 64;
	var AllowTrailingSign = 8;
	var AllowTrailingWhite = 2;
	var Any = 511;
	var Currency = 383;
	var Float = 167;
	var HexNumber = 515;
	var Integer = 7;
	var None = 0;
	var Number = 111;
	@:op(A | B) static function or(lhs:NumberStyles, rhs:NumberStyles):NumberStyles;
	@:op(A & B) static function and(lhs:NumberStyles, rhs:NumberStyles):NumberStyles;
	@:op(A ^ B) static function xor(lhs:NumberStyles, rhs:NumberStyles):NumberStyles;
	@:op(~A) static function complement(value:NumberStyles):NumberStyles;
}
