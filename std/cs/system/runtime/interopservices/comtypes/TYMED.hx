package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  structure. */
@:native("System.Runtime.InteropServices.ComTypes.TYMED")
extern enum abstract TYMED(Int) {
	var TYMED_ENHMF = 64;
	var TYMED_FILE = 2;
	var TYMED_GDI = 16;
	var TYMED_HGLOBAL = 1;
	var TYMED_ISTORAGE = 8;
	var TYMED_ISTREAM = 4;
	var TYMED_MFPICT = 32;
	var TYMED_NULL = 0;
	@:op(A | B) static function or(lhs:TYMED, rhs:TYMED):TYMED;
	@:op(A & B) static function and(lhs:TYMED, rhs:TYMED):TYMED;
	@:op(A ^ B) static function xor(lhs:TYMED, rhs:TYMED):TYMED;
	@:op(~A) static function complement(value:TYMED):TYMED;
}
