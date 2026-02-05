package cs.system.diagnostics;

/** Indicates whether a listener should trace based on the event type. */
@:native("System.Diagnostics.EventTypeFilter")
extern class EventTypeFilter extends cs.system.diagnostics.TraceFilter {
	/**
	 * Gets or sets the event type of the messages to trace.
	 * @return A bitwise combination of the  values.
	 */
	var EventType(default, default):cs.system.diagnostics.SourceLevels;
	function new(level:cs.system.diagnostics.SourceLevels):Void;
	/**
	 * Determines whether the trace listener should trace the event.
	 * @param cache A  that represents the information cache for the trace event.
	 * @param source The name of the source.
	 * @param eventType One of the  values.
	 * @param id A trace identifier number.
	 * @param formatOrMessage The format to use for writing an array of arguments, or a
	 * message to write.
	 * @param args An array of argument objects.
	 * @param data1 A trace data object.
	 * @param data An array of trace data objects.
	 * @return if the trace should be produced; otherwise, .
	 */
	function ShouldTrace(cache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, formatOrMessage:String, args:cs.NativeArray<Dynamic>, data1:Dynamic, data:cs.NativeArray<Dynamic>):Bool;
}
