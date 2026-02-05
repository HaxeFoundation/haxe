package cs.system.runtime.interopservices.comtypes;

/** Defines how to access a function. */
@:native("System.Runtime.InteropServices.ComTypes.FUNCKIND")
extern enum abstract FUNCKIND(Int) {
	var FUNC_DISPATCH = 4;
	var FUNC_NONVIRTUAL = 2;
	var FUNC_PUREVIRTUAL = 1;
	var FUNC_STATIC = 3;
	var FUNC_VIRTUAL = 0;
}
