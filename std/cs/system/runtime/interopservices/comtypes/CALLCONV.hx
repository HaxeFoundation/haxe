package cs.system.runtime.interopservices.comtypes;

/** Identifies the calling convention used by a method described in a METHODDATA structure. */
@:native("System.Runtime.InteropServices.ComTypes.CALLCONV")
extern enum abstract CALLCONV(Int) {
	var CC_CDECL = 1;
	var CC_MACPASCAL = 3;
	var CC_MAX = 9;
	var CC_MPWCDECL = 7;
	var CC_MPWPASCAL = 8;
	var CC_MSCPASCAL = 2;
	var CC_PASCAL = 2;
	var CC_RESERVED = 5;
	var CC_STDCALL = 4;
	var CC_SYSCALL = 6;
}
