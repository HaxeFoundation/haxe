package cs.system.runtime.interopservices.comtypes;

/** Identifies the target operating system platform. */
@:native("System.Runtime.InteropServices.ComTypes.SYSKIND")
extern enum SYSKIND {
	SYS_MAC;
	SYS_WIN16;
	SYS_WIN32;
	SYS_WIN64;
}
