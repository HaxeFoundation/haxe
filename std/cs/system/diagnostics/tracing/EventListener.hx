package cs.system.diagnostics.tracing;

/** Provides methods for enabling and disabling events from event sources. */
@:native("System.Diagnostics.Tracing.EventListener")
extern class EventListener {
	/**
	 * Disables all events for the specified event source.
	 * @param eventSource The event source to disable events for.
	 */
	function DisableEvents(eventSource:cs.system.diagnostics.tracing.EventSource):Void;
	/** Releases the resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function(eventSource:cs.system.diagnostics.tracing.EventSource, level:cs.system.diagnostics.tracing.EventLevel):Void {})
	@:overload(function(eventSource:cs.system.diagnostics.tracing.EventSource, level:cs.system.diagnostics.tracing.EventLevel, matchAnyKeyword:cs.system.diagnostics.tracing.EventKeywords):Void {})
	/**
	 * Enables events for the specified event source that has the specified verbosity
	 * level or lower.
	 * @param eventSource The event source to enable events for.
	 * @param level The level of events to enable.
	 */
	function EnableEvents(eventSource:cs.system.diagnostics.tracing.EventSource, level:cs.system.diagnostics.tracing.EventLevel, matchAnyKeyword:cs.system.diagnostics.tracing.EventKeywords, arguments:cs.system.collections.generic.IDictionary<String, String>):Void;
}
