package cs.system.runtime.interopservices.comtypes;

/** Defines the kind of variable. */
@:native("System.Runtime.InteropServices.ComTypes.VARKIND")
extern enum VARKIND {
	VAR_CONST;
	VAR_DISPATCH;
	VAR_PERINSTANCE;
	VAR_STATIC;
}
