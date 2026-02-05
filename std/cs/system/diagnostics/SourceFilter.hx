package cs.system.diagnostics;

/** Indicates whether a listener should trace a message based on the source of a trace. */
@:native("System.Diagnostics.SourceFilter")
extern class SourceFilter extends cs.system.diagnostics.TraceFilter {
	/**
	 * Gets or sets the name of the trace source.
	 * @return The name of the trace source.
	 */
	var Source(default, default):String;
	function new(source:String):Void;
	/**
	 * Determines whether the trace listener should trace the event.
	 * @param cache An object that represents the information cache for the trace
	 * event.
	 * @param source The name of the source.
	 * @param eventType One of the enumeration values that identifies the event type.
	 * @param id A trace identifier number.
	 * @param formatOrMessage The format to use for writing an array of arguments or a
	 * message to write.
	 * @param args An array of argument objects.
	 * @param data1 A trace data object.
	 * @param data An array of trace data objects.
	 * @return if the trace should be produced; otherwise, .
	 */
	function ShouldTrace(cache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, formatOrMessage:String, args:cs.NativeArray<Dynamic>, data1:Dynamic, data:cs.NativeArray<Dynamic>):Bool;
}
