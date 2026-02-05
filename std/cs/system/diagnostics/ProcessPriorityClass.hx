package cs.system.diagnostics;

/** Indicates the priority that the system associates with a process. This value, together with the priority value of each thread of the process, determines each thread's base priority level. */
@:native("System.Diagnostics.ProcessPriorityClass")
extern enum ProcessPriorityClass {
	AboveNormal;
	BelowNormal;
	High;
	Idle;
	Normal;
	RealTime;
}
