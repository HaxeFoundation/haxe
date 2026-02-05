package cs.system.runtime.interopservices.comtypes;

/** Identifies the calling convention used by a method described in a METHODDATA structure. */
@:native("System.Runtime.InteropServices.ComTypes.CALLCONV")
extern enum CALLCONV {
	CC_CDECL;
	CC_MACPASCAL;
	CC_MAX;
	CC_MPWCDECL;
	CC_MPWPASCAL;
	CC_MSCPASCAL;
	CC_PASCAL;
	CC_RESERVED;
	CC_STDCALL;
	CC_SYSCALL;
}
