package cs.system.io;

/** Represents advanced options for creating a  object. */
@:native("System.IO.FileOptions")
extern enum abstract FileOptions(Int) {
	var Asynchronous = 1073741824;
	var DeleteOnClose = 67108864;
	var Encrypted = 16384;
	var None = 0;
	var RandomAccess = 268435456;
	var SequentialScan = 134217728;
	var WriteThrough = -2147483648;
	@:op(A | B) static function or(lhs:FileOptions, rhs:FileOptions):FileOptions;
	@:op(A & B) static function and(lhs:FileOptions, rhs:FileOptions):FileOptions;
	@:op(A ^ B) static function xor(lhs:FileOptions, rhs:FileOptions):FileOptions;
	@:op(~A) static function complement(value:FileOptions):FileOptions;
}
