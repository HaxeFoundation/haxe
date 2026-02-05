package cs.system.io;

/** Specifies changes to watch for in a file or folder. */
@:native("System.IO.NotifyFilters")
extern enum abstract NotifyFilters(Int) {
	var Attributes = 4;
	var CreationTime = 64;
	var DirectoryName = 2;
	var FileName = 1;
	var LastAccess = 32;
	var LastWrite = 16;
	var Security = 256;
	var Size = 8;
	@:op(A | B) static function or(lhs:NotifyFilters, rhs:NotifyFilters):NotifyFilters;
	@:op(A & B) static function and(lhs:NotifyFilters, rhs:NotifyFilters):NotifyFilters;
	@:op(A ^ B) static function xor(lhs:NotifyFilters, rhs:NotifyFilters):NotifyFilters;
	@:op(~A) static function complement(value:NotifyFilters):NotifyFilters;
}
