package cs.system.diagnostics;

/** Specifies the priority level of a thread. */
@:native("System.Diagnostics.ThreadPriorityLevel")
extern enum abstract ThreadPriorityLevel(Int) {
	var AboveNormal = 1;
	var BelowNormal = -1;
	var Highest = 2;
	var Idle = -15;
	var Lowest = -2;
	var Normal = 0;
	var TimeCritical = 15;
}
