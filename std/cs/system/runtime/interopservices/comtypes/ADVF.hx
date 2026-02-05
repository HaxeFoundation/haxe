package cs.system.runtime.interopservices.comtypes;

/** Specifies the requested behavior when setting up an advise sink or a caching connection with an object. */
@:native("System.Runtime.InteropServices.ComTypes.ADVF")
extern enum abstract ADVF(Int) {
	var ADVF_DATAONSTOP = 64;
	var ADVF_NODATA = 1;
	var ADVF_ONLYONCE = 4;
	var ADVF_PRIMEFIRST = 2;
	var ADVFCACHE_FORCEBUILTIN = 16;
	var ADVFCACHE_NOHANDLER = 8;
	var ADVFCACHE_ONSAVE = 32;
	@:op(A | B) static function or(lhs:ADVF, rhs:ADVF):ADVF;
	@:op(A & B) static function and(lhs:ADVF, rhs:ADVF):ADVF;
	@:op(A ^ B) static function xor(lhs:ADVF, rhs:ADVF):ADVF;
	@:op(~A) static function complement(value:ADVF):ADVF;
}
