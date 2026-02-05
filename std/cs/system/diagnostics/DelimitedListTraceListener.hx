package cs.system.diagnostics;

/** Directs tracing or debugging output to a text writer, such as a stream writer, or to a stream, such as a file stream. */
@:native("System.Diagnostics.DelimitedListTraceListener")
extern class DelimitedListTraceListener extends cs.system.diagnostics.TextWriterTraceListener {
	/**
	 * Gets or sets the delimiter for the delimited list.
	 * @return The delimiter for the delimited list.
	 */
	var Delimiter(default, default):String;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(stream:cs.system.io.Stream, name:String):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, name:String):Void {})
	function new(fileName:String, name:String):Void;
	@:overload(function(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, data:Dynamic):Void {})
	/**
	 * Writes trace information, a data object, and event information to the output
	 * file or stream.
	 * @param eventCache A  object that contains the current process ID, thread ID, and
	 * stack trace information.
	 * @param source A name used to identify the output, typically the name of the
	 * application that generated the trace event.
	 * @param eventType One of the  values specifying the type of event that has caused
	 * the trace.
	 * @param id A numeric identifier for the event.
	 * @param data A data object to write to the output file or stream.
	 */
	function TraceData(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, data:cs.NativeArray<Dynamic>):Void;
	@:overload(function(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, message:String):Void {})
	/**
	 * Writes trace information, a message, and event information to the output file or
	 * stream.
	 * @param eventCache A  object that contains the current process ID, thread ID, and
	 * stack trace information.
	 * @param source A name used to identify the output, typically the name of the
	 * application that generated the trace event.
	 * @param eventType One of the  values specifying the type of event that has caused
	 * the trace.
	 * @param id A numeric identifier for the event.
	 * @param message The trace message to write to the output file or stream.
	 */
	function TraceEvent(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, format:String, args:cs.NativeArray<Dynamic>):Void;
}
