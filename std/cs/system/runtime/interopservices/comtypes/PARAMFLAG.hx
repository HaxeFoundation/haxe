package cs.system.runtime.interopservices.comtypes;

/** Describes how to transfer a structure element, parameter, or function return value between processes. */
@:native("System.Runtime.InteropServices.ComTypes.PARAMFLAG")
extern enum abstract PARAMFLAG(Int) {
	var PARAMFLAG_FHASCUSTDATA;
	var PARAMFLAG_FHASDEFAULT;
	var PARAMFLAG_FIN;
	var PARAMFLAG_FLCID;
	var PARAMFLAG_FOPT;
	var PARAMFLAG_FOUT;
	var PARAMFLAG_FRETVAL;
	var PARAMFLAG_NONE;
	@:op(A | B) static function or(lhs:PARAMFLAG, rhs:PARAMFLAG):PARAMFLAG;
	@:op(A & B) static function and(lhs:PARAMFLAG, rhs:PARAMFLAG):PARAMFLAG;
	@:op(A ^ B) static function xor(lhs:PARAMFLAG, rhs:PARAMFLAG):PARAMFLAG;
	@:op(~A) static function complement(value:PARAMFLAG):PARAMFLAG;
}
