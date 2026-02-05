package cs.system.runtime.interopservices.comtypes;

/** Describes how to transfer a structure element, parameter, or function return value between processes. */
@:native("System.Runtime.InteropServices.ComTypes.IDLFLAG")
extern enum abstract IDLFLAG(Int) {
	var IDLFLAG_FIN;
	var IDLFLAG_FLCID;
	var IDLFLAG_FOUT;
	var IDLFLAG_FRETVAL;
	var IDLFLAG_NONE;
	@:op(A | B) static function or(lhs:IDLFLAG, rhs:IDLFLAG):IDLFLAG;
	@:op(A & B) static function and(lhs:IDLFLAG, rhs:IDLFLAG):IDLFLAG;
	@:op(A ^ B) static function xor(lhs:IDLFLAG, rhs:IDLFLAG):IDLFLAG;
	@:op(~A) static function complement(value:IDLFLAG):IDLFLAG;
}
