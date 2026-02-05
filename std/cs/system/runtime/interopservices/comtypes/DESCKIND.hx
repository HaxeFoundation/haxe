package cs.system.runtime.interopservices.comtypes;

/** Identifies the type description being bound to. */
@:native("System.Runtime.InteropServices.ComTypes.DESCKIND")
extern enum DESCKIND {
	DESCKIND_FUNCDESC;
	DESCKIND_IMPLICITAPPOBJ;
	DESCKIND_MAX;
	DESCKIND_NONE;
	DESCKIND_TYPECOMP;
	DESCKIND_VARDESC;
}
