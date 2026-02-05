package cs.system.runtime.interopservices.comtypes;

/** Specifies the desired data or view aspect of the object when drawing or getting data. */
@:native("System.Runtime.InteropServices.ComTypes.DVASPECT")
extern enum abstract DVASPECT(Int) {
	var DVASPECT_CONTENT = 1;
	var DVASPECT_DOCPRINT = 8;
	var DVASPECT_ICON = 4;
	var DVASPECT_THUMBNAIL = 2;
	@:op(A | B) static function or(lhs:DVASPECT, rhs:DVASPECT):DVASPECT;
	@:op(A & B) static function and(lhs:DVASPECT, rhs:DVASPECT):DVASPECT;
	@:op(A ^ B) static function xor(lhs:DVASPECT, rhs:DVASPECT):DVASPECT;
	@:op(~A) static function complement(value:DVASPECT):DVASPECT;
}
