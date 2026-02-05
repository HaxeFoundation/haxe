package cs.system.runtime.interopservices.comtypes;

/** Defines how to access a function. */
@:native("System.Runtime.InteropServices.ComTypes.FUNCKIND")
extern enum FUNCKIND {
	FUNC_DISPATCH;
	FUNC_NONVIRTUAL;
	FUNC_PUREVIRTUAL;
	FUNC_STATIC;
	FUNC_VIRTUAL;
}
