package cs.system.diagnostics;

/** Specifies the priority level of a thread. */
@:native("System.Diagnostics.ThreadPriorityLevel")
extern enum ThreadPriorityLevel {
	AboveNormal;
	BelowNormal;
	Highest;
	Idle;
	Lowest;
	Normal;
	TimeCritical;
}
