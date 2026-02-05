package cs.system.runtime.interopservices;

/** Indicates the processor architecture. */
@:native("System.Runtime.InteropServices.Architecture")
extern enum abstract Architecture(Int) {
	var Arm = 2;
	var Arm64 = 3;
	var X64 = 1;
	var X86 = 0;
}
