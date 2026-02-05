package cs.system.runtime.interopservices.comtypes;

/** Specifies various types of data and functions. */
@:native("System.Runtime.InteropServices.ComTypes.TYPEKIND")
extern enum abstract TYPEKIND(Int) {
	var TKIND_ALIAS = 6;
	var TKIND_COCLASS = 5;
	var TKIND_DISPATCH = 4;
	var TKIND_ENUM = 0;
	var TKIND_INTERFACE = 3;
	var TKIND_MAX = 8;
	var TKIND_MODULE = 2;
	var TKIND_RECORD = 1;
	var TKIND_UNION = 7;
}
