package cs.system.diagnostics;

/** Identifies the type of event that has caused the trace. */
@:native("System.Diagnostics.TraceEventType")
extern enum TraceEventType {
	Critical;
	Error;
	Information;
	Resume;
	Start;
	Stop;
	Suspend;
	Transfer;
	Verbose;
	Warning;
}
