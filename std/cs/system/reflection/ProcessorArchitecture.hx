package cs.system.reflection;

/** Identifies the processor and bits-per-word of the platform targeted by an executable. */
@:native("System.Reflection.ProcessorArchitecture")
extern enum ProcessorArchitecture {
	Amd64;
	Arm;
	IA64;
	MSIL;
	None;
	X86;
}
