package cs.system.runtime.interopservices.comtypes;

/** Identifies the constants that define the properties of a function. */
@:native("System.Runtime.InteropServices.ComTypes.FUNCFLAGS")
extern enum abstract FUNCFLAGS(Int) {
	var FUNCFLAG_FBINDABLE;
	var FUNCFLAG_FDEFAULTBIND;
	var FUNCFLAG_FDEFAULTCOLLELEM;
	var FUNCFLAG_FDISPLAYBIND;
	var FUNCFLAG_FHIDDEN;
	var FUNCFLAG_FIMMEDIATEBIND;
	var FUNCFLAG_FNONBROWSABLE;
	var FUNCFLAG_FREPLACEABLE;
	var FUNCFLAG_FREQUESTEDIT;
	var FUNCFLAG_FRESTRICTED;
	var FUNCFLAG_FSOURCE;
	var FUNCFLAG_FUIDEFAULT;
	var FUNCFLAG_FUSESGETLASTERROR;
	@:op(A | B) static function or(lhs:FUNCFLAGS, rhs:FUNCFLAGS):FUNCFLAGS;
	@:op(A & B) static function and(lhs:FUNCFLAGS, rhs:FUNCFLAGS):FUNCFLAGS;
	@:op(A ^ B) static function xor(lhs:FUNCFLAGS, rhs:FUNCFLAGS):FUNCFLAGS;
	@:op(~A) static function complement(value:FUNCFLAGS):FUNCFLAGS;
}
