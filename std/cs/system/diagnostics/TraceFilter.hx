package cs.system.diagnostics;

/** Provides the base class for trace filter implementations. */
@:native("System.Diagnostics.TraceFilter")
extern class TraceFilter {
	/**
	 * When overridden in a derived class, determines whether the trace listener should
	 * trace the event.
	 * @param cache The  that contains information for the trace event.
	 * @param source The name of the source.
	 * @param eventType One of the  values specifying the type of event that has caused
	 * the trace.
	 * @param id A trace identifier number.
	 * @param formatOrMessage Either the format to use for writing an array of
	 * arguments specified by the  parameter, or a message to write.
	 * @param args An array of argument objects.
	 * @param data1 A trace data object.
	 * @param data An array of trace data objects.
	 * @return to trace the specified event; otherwise, .
	 */
	function ShouldTrace(cache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, formatOrMessage:String, args:cs.NativeArray<Dynamic>, data1:Dynamic, data:cs.NativeArray<Dynamic>):Bool;
}
