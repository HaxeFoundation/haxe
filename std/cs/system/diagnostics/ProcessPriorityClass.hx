package cs.system.diagnostics;

/** Indicates the priority that the system associates with a process. This value, together with the priority value of each thread of the process, determines each thread's base priority level. */
@:native("System.Diagnostics.ProcessPriorityClass")
extern enum abstract ProcessPriorityClass(Int) {
	var AboveNormal = 32768;
	var BelowNormal = 16384;
	var High = 128;
	var Idle = 64;
	var Normal = 32;
	var RealTime = 256;
}
