package cs.system.diagnostics.tracing;

/** Defines the tasks that apply to events. */
@:native("System.Diagnostics.Tracing.EventTask")
extern enum abstract EventTask(Int) {
	var None = 0;
}
