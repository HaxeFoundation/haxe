package cs.system.diagnostics;

/** Identifies the type of event that has caused the trace. */
@:native("System.Diagnostics.TraceEventType")
extern enum abstract TraceEventType(Int) {
	var Critical = 1;
	var Error = 2;
	var Information = 8;
	var Resume = 2048;
	var Start = 256;
	var Stop = 512;
	var Suspend = 1024;
	var Transfer = 4096;
	var Verbose = 16;
	var Warning = 4;
}
