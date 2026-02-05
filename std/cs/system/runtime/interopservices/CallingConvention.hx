package cs.system.runtime.interopservices;

/** Specifies the calling convention required to call methods implemented in unmanaged code. */
@:native("System.Runtime.InteropServices.CallingConvention")
extern enum abstract CallingConvention(Int) {
	var Cdecl = 2;
	var FastCall = 5;
	var StdCall = 3;
	var ThisCall = 4;
	var Winapi = 1;
}
