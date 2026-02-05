package cs.system.io;

/** Contains constants for controlling the kind of access other  objects can have to the same file. */
@:native("System.IO.FileShare")
extern enum abstract FileShare(Int) {
	var Delete = 4;
	var Inheritable = 16;
	var None = 0;
	var Read = 1;
	var ReadWrite = 3;
	var Write = 2;
	@:op(A | B) static function or(lhs:FileShare, rhs:FileShare):FileShare;
	@:op(A & B) static function and(lhs:FileShare, rhs:FileShare):FileShare;
	@:op(A ^ B) static function xor(lhs:FileShare, rhs:FileShare):FileShare;
	@:op(~A) static function complement(value:FileShare):FileShare;
}
