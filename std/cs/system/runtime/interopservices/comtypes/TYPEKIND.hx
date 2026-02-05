package cs.system.runtime.interopservices.comtypes;

/** Specifies various types of data and functions. */
@:native("System.Runtime.InteropServices.ComTypes.TYPEKIND")
extern enum TYPEKIND {
	TKIND_ALIAS;
	TKIND_COCLASS;
	TKIND_DISPATCH;
	TKIND_ENUM;
	TKIND_INTERFACE;
	TKIND_MAX;
	TKIND_MODULE;
	TKIND_RECORD;
	TKIND_UNION;
}
