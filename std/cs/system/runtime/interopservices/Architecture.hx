package cs.system.runtime.interopservices;

/** Indicates the processor architecture. */
@:native("System.Runtime.InteropServices.Architecture")
extern enum Architecture {
	Arm;
	Arm64;
	X64;
	X86;
}
