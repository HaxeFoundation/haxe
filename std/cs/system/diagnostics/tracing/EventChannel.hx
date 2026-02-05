package cs.system.diagnostics.tracing;

/** Specifies the event log channel for the event. */
@:native("System.Diagnostics.Tracing.EventChannel")
extern enum EventChannel {
	Admin;
	Analytic;
	Debug;
	None;
	Operational;
}
