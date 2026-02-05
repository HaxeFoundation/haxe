package cs.system;

/** Specifies whether applicable  method overloads include or omit empty substrings from the return value. */
@:native("System.StringSplitOptions")
extern enum abstract StringSplitOptions(Int) {
	var None = 0;
	var RemoveEmptyEntries = 1;
	@:op(A | B) static function or(lhs:StringSplitOptions, rhs:StringSplitOptions):StringSplitOptions;
	@:op(A & B) static function and(lhs:StringSplitOptions, rhs:StringSplitOptions):StringSplitOptions;
	@:op(A ^ B) static function xor(lhs:StringSplitOptions, rhs:StringSplitOptions):StringSplitOptions;
	@:op(~A) static function complement(value:StringSplitOptions):StringSplitOptions;
}
