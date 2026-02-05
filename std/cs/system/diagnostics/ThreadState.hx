package cs.system.diagnostics;

/** Specifies the current execution state of the thread. */
@:native("System.Diagnostics.ThreadState")
extern enum ThreadState {
	Initialized;
	Ready;
	Running;
	Standby;
	Terminated;
	Transition;
	Unknown;
	Wait;
}
