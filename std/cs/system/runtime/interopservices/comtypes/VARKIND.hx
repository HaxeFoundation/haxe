package cs.system.runtime.interopservices.comtypes;

/** Defines the kind of variable. */
@:native("System.Runtime.InteropServices.ComTypes.VARKIND")
extern enum abstract VARKIND(Int) {
	var VAR_CONST = 2;
	var VAR_DISPATCH = 3;
	var VAR_PERINSTANCE = 0;
	var VAR_STATIC = 1;
}
