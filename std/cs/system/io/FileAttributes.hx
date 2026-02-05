package cs.system.io;

/** Provides attributes for files and directories. */
@:native("System.IO.FileAttributes")
extern enum abstract FileAttributes(Int) {
	var Archive = 32;
	var Compressed = 2048;
	var Device = 64;
	var Directory = 16;
	var Encrypted = 16384;
	var Hidden = 2;
	var IntegrityStream = 32768;
	var Normal = 128;
	var NoScrubData = 131072;
	var NotContentIndexed = 8192;
	var Offline = 4096;
	var ReadOnly = 1;
	var ReparsePoint = 1024;
	var SparseFile = 512;
	var System = 4;
	var Temporary = 256;
	@:op(A | B) static function or(lhs:FileAttributes, rhs:FileAttributes):FileAttributes;
	@:op(A & B) static function and(lhs:FileAttributes, rhs:FileAttributes):FileAttributes;
	@:op(A ^ B) static function xor(lhs:FileAttributes, rhs:FileAttributes):FileAttributes;
	@:op(~A) static function complement(value:FileAttributes):FileAttributes;
}
