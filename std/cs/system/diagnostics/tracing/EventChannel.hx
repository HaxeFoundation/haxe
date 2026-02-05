package cs.system.diagnostics.tracing;

/** Specifies the event log channel for the event. */
@:native("System.Diagnostics.Tracing.EventChannel")
extern enum abstract EventChannel(Int) {
	var Admin;
	var Analytic;
	var Debug;
	var None;
	var Operational;
}
