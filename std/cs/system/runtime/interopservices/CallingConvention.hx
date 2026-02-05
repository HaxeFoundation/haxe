package cs.system.runtime.interopservices;

/** Specifies the calling convention required to call methods implemented in unmanaged code. */
@:native("System.Runtime.InteropServices.CallingConvention")
extern enum CallingConvention {
	Cdecl;
	FastCall;
	StdCall;
	ThisCall;
	Winapi;
}
