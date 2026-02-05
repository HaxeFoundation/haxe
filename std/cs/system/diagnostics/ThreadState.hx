package cs.system.diagnostics;

/** Specifies the current execution state of the thread. */
@:native("System.Diagnostics.ThreadState")
extern enum abstract ThreadState(Int) {
	var Initialized = 0;
	var Ready = 1;
	var Running = 2;
	var Standby = 3;
	var Terminated = 4;
	var Transition = 6;
	var Unknown = 7;
	var Wait = 5;
}
