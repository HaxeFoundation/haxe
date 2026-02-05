package cs.system.runtime.interopservices.comtypes;

/** Identifies the constants that define the properties of a variable. */
@:native("System.Runtime.InteropServices.ComTypes.VARFLAGS")
extern enum abstract VARFLAGS(Int) {
	var VARFLAG_FBINDABLE;
	var VARFLAG_FDEFAULTBIND;
	var VARFLAG_FDEFAULTCOLLELEM;
	var VARFLAG_FDISPLAYBIND;
	var VARFLAG_FHIDDEN;
	var VARFLAG_FIMMEDIATEBIND;
	var VARFLAG_FNONBROWSABLE;
	var VARFLAG_FREADONLY;
	var VARFLAG_FREPLACEABLE;
	var VARFLAG_FREQUESTEDIT;
	var VARFLAG_FRESTRICTED;
	var VARFLAG_FSOURCE;
	var VARFLAG_FUIDEFAULT;
	@:op(A | B) static function or(lhs:VARFLAGS, rhs:VARFLAGS):VARFLAGS;
	@:op(A & B) static function and(lhs:VARFLAGS, rhs:VARFLAGS):VARFLAGS;
	@:op(A ^ B) static function xor(lhs:VARFLAGS, rhs:VARFLAGS):VARFLAGS;
	@:op(~A) static function complement(value:VARFLAGS):VARFLAGS;
}
