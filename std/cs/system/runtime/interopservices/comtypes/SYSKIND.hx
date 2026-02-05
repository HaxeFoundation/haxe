package cs.system.runtime.interopservices.comtypes;

/** Identifies the target operating system platform. */
@:native("System.Runtime.InteropServices.ComTypes.SYSKIND")
extern enum abstract SYSKIND(Int) {
	var SYS_MAC = 2;
	var SYS_WIN16 = 0;
	var SYS_WIN32 = 1;
	var SYS_WIN64 = 3;
}
