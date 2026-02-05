package cs.system.runtime.interopservices.comtypes;

/** Identifies the type description being bound to. */
@:native("System.Runtime.InteropServices.ComTypes.DESCKIND")
extern enum abstract DESCKIND(Int) {
	var DESCKIND_FUNCDESC = 1;
	var DESCKIND_IMPLICITAPPOBJ = 4;
	var DESCKIND_MAX = 5;
	var DESCKIND_NONE = 0;
	var DESCKIND_TYPECOMP = 3;
	var DESCKIND_VARDESC = 2;
}
