package cs.system.diagnostics.tracing;

/** Identifies the level of an event. */
@:native("System.Diagnostics.Tracing.EventLevel")
extern enum abstract EventLevel(Int) {
	var Critical = 1;
	var Error = 2;
	var Informational = 4;
	var LogAlways = 0;
	var Verbose = 5;
	var Warning = 3;
}
