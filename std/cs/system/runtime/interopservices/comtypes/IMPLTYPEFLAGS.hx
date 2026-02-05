package cs.system.runtime.interopservices.comtypes;

/** Defines the attributes of an implemented or inherited interface of a type. */
@:native("System.Runtime.InteropServices.ComTypes.IMPLTYPEFLAGS")
extern enum abstract IMPLTYPEFLAGS(Int) {
	var IMPLTYPEFLAG_FDEFAULT = 1;
	var IMPLTYPEFLAG_FDEFAULTVTABLE = 8;
	var IMPLTYPEFLAG_FRESTRICTED = 4;
	var IMPLTYPEFLAG_FSOURCE = 2;
	@:op(A | B) static function or(lhs:IMPLTYPEFLAGS, rhs:IMPLTYPEFLAGS):IMPLTYPEFLAGS;
	@:op(A & B) static function and(lhs:IMPLTYPEFLAGS, rhs:IMPLTYPEFLAGS):IMPLTYPEFLAGS;
	@:op(A ^ B) static function xor(lhs:IMPLTYPEFLAGS, rhs:IMPLTYPEFLAGS):IMPLTYPEFLAGS;
	@:op(~A) static function complement(value:IMPLTYPEFLAGS):IMPLTYPEFLAGS;
}
