package cs.system.reflection;

/** Identifies the processor and bits-per-word of the platform targeted by an executable. */
@:native("System.Reflection.ProcessorArchitecture")
extern enum abstract ProcessorArchitecture(Int) {
	var Amd64 = 4;
	var Arm = 5;
	var IA64 = 3;
	var MSIL = 1;
	var None = 0;
	var X86 = 2;
}
