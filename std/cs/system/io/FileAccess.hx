package cs.system.io;

/** Defines constants for read, write, or read/write access to a file. */
@:native("System.IO.FileAccess")
extern enum abstract FileAccess(Int) {
	var Read = 1;
	var ReadWrite = 3;
	var Write = 2;
	@:op(A | B) static function or(lhs:FileAccess, rhs:FileAccess):FileAccess;
	@:op(A & B) static function and(lhs:FileAccess, rhs:FileAccess):FileAccess;
	@:op(A ^ B) static function xor(lhs:FileAccess, rhs:FileAccess):FileAccess;
	@:op(~A) static function complement(value:FileAccess):FileAccess;
}
