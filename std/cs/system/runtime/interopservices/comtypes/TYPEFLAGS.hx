package cs.system.runtime.interopservices.comtypes;

/** Defines the properties and attributes of a type description. */
@:native("System.Runtime.InteropServices.ComTypes.TYPEFLAGS")
extern enum abstract TYPEFLAGS(Int) {
	var TYPEFLAG_FAGGREGATABLE;
	var TYPEFLAG_FAPPOBJECT;
	var TYPEFLAG_FCANCREATE;
	var TYPEFLAG_FCONTROL;
	var TYPEFLAG_FDISPATCHABLE;
	var TYPEFLAG_FDUAL;
	var TYPEFLAG_FHIDDEN;
	var TYPEFLAG_FLICENSED;
	var TYPEFLAG_FNONEXTENSIBLE;
	var TYPEFLAG_FOLEAUTOMATION;
	var TYPEFLAG_FPREDECLID;
	var TYPEFLAG_FPROXY;
	var TYPEFLAG_FREPLACEABLE;
	var TYPEFLAG_FRESTRICTED;
	var TYPEFLAG_FREVERSEBIND;
	@:op(A | B) static function or(lhs:TYPEFLAGS, rhs:TYPEFLAGS):TYPEFLAGS;
	@:op(A & B) static function and(lhs:TYPEFLAGS, rhs:TYPEFLAGS):TYPEFLAGS;
	@:op(A ^ B) static function xor(lhs:TYPEFLAGS, rhs:TYPEFLAGS):TYPEFLAGS;
	@:op(~A) static function complement(value:TYPEFLAGS):TYPEFLAGS;
}
