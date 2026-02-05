package cs.system.diagnostics.tracing;

/** Provides data for the  event. */
@:native("System.Diagnostics.Tracing.EventSourceCreatedEventArgs")
extern class EventSourceCreatedEventArgs extends cs.system.EventArgs {
	/**
	 * Get the event source that is attaching to the listener.
	 * @return The event source that is attaching to the listener.
	 */
	var EventSource(default, never):cs.system.diagnostics.tracing.EventSource;
	function new():Void;
}
