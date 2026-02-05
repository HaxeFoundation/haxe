package cs.system.diagnostics.tracing;

/** Identifies the level of an event. */
@:native("System.Diagnostics.Tracing.EventLevel")
extern enum EventLevel {
	Critical;
	Error;
	Informational;
	LogAlways;
	Verbose;
	Warning;
}
