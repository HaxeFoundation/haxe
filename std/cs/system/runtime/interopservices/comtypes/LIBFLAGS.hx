package cs.system.runtime.interopservices.comtypes;

/** Defines flags that apply to type libraries. */
@:native("System.Runtime.InteropServices.ComTypes.LIBFLAGS")
extern enum abstract LIBFLAGS(Int) {
	var LIBFLAG_FCONTROL;
	var LIBFLAG_FHASDISKIMAGE;
	var LIBFLAG_FHIDDEN;
	var LIBFLAG_FRESTRICTED;
	@:op(A | B) static function or(lhs:LIBFLAGS, rhs:LIBFLAGS):LIBFLAGS;
	@:op(A & B) static function and(lhs:LIBFLAGS, rhs:LIBFLAGS):LIBFLAGS;
	@:op(A ^ B) static function xor(lhs:LIBFLAGS, rhs:LIBFLAGS):LIBFLAGS;
	@:op(~A) static function complement(value:LIBFLAGS):LIBFLAGS;
}
