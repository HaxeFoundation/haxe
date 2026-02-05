package cs.system.diagnostics;

/** Provides the  base class for the listeners who monitor trace and debug output. */
@:native("System.Diagnostics.TraceListener")
extern class TraceListener extends cs.system.MarshalByRefObject {
	/**
	 * Gets the custom trace listener attributes defined in the application
	 * configuration file.
	 * @return A  containing the custom attributes for the trace listener.
	 */
	var Attributes(default, never):cs.system.collections.specialized.StringDictionary;
	/**
	 * Gets or sets the trace filter for the trace listener.
	 * @return An object derived from the  base class.
	 */
	var Filter(default, default):cs.system.diagnostics.TraceFilter;
	/**
	 * Gets or sets the indent level.
	 * @return The indent level. The default is zero.
	 */
	var IndentLevel(default, default):Int;
	/**
	 * Gets or sets the number of spaces in an indent.
	 * @return The number of spaces in an indent. The default is four spaces.
	 */
	var IndentSize(default, default):Int;
	/**
	 * Gets a value indicating whether the trace listener is thread safe.
	 * @return if the trace listener is thread safe; otherwise, . The default is .
	 */
	var IsThreadSafe(default, never):Bool;
	/**
	 * Gets or sets a name for this .
	 * @return A name for this . The default is an empty string ("").
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets a value indicating whether to indent the output.
	 * @return if the output should be indented; otherwise, .
	 */
	var NeedIndent(default, default):Bool;
	/**
	 * Gets or sets the trace output options.
	 * @return A bitwise combination of the enumeration values. The default is .
	 */
	var TraceOutputOptions(default, default):cs.system.diagnostics.TraceOptions;
	/** When overridden in a derived class, closes the output stream so it no longer receives tracing or debugging output. */
	function Close():Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	@:overload(function(message:String):Void {})
	/**
	 * Emits an error message to the listener you create when you implement the  class.
	 * @param message A message to emit.
	 */
	function Fail(message:String, detailMessage:String):Void;
	/** When overridden in a derived class, flushes the output buffer. */
	function Flush():Void;
	@:overload(function(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, data:Dynamic):Void {})
	/**
	 * Writes trace information, a data object and event information to the listener
	 * specific output.
	 * @param eventCache A  object that contains the current process ID, thread ID, and
	 * stack trace information.
	 * @param source A name used to identify the output, typically the name of the
	 * application that generated the trace event.
	 * @param eventType One of the  values specifying the type of event that has caused
	 * the trace.
	 * @param id A numeric identifier for the event.
	 * @param data The trace data to emit.
	 */
	function TraceData(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, data:cs.NativeArray<Dynamic>):Void;
	@:overload(function(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int):Void {})
	@:overload(function(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, message:String):Void {})
	/**
	 * Writes trace and event information to the listener specific output.
	 * @param eventCache A  object that contains the current process ID, thread ID, and
	 * stack trace information.
	 * @param source A name used to identify the output, typically the name of the
	 * application that generated the trace event.
	 * @param eventType One of the  values specifying the type of event that has caused
	 * the trace.
	 * @param id A numeric identifier for the event.
	 */
	function TraceEvent(eventCache:cs.system.diagnostics.TraceEventCache, source:String, eventType:cs.system.diagnostics.TraceEventType, id:Int, format:String, args:cs.NativeArray<Dynamic>):Void;
	/**
	 * Writes trace information, a message, a related activity identity and event
	 * information to the listener specific output.
	 * @param eventCache A  object that contains the current process ID, thread ID, and
	 * stack trace information.
	 * @param source A name used to identify the output, typically the name of the
	 * application that generated the trace event.
	 * @param id A numeric identifier for the event.
	 * @param message A message to write.
	 * @param relatedActivityId A  object identifying a related activity.
	 */
	function TraceTransfer(eventCache:cs.system.diagnostics.TraceEventCache, source:String, id:Int, message:String, relatedActivityId:cs.system.Guid):Void;
	@:overload(function(o:Dynamic):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(o:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the listener you create when you
	 * implement the  class.
	 * @param o An  whose fully qualified class name you want to write.
	 */
	function Write(message:String, category:String):Void;
	@:overload(function(o:Dynamic):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(o:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the listener you create when you
	 * implement the  class, followed by a line terminator.
	 * @param o An  whose fully qualified class name you want to write.
	 */
	function WriteLine(message:String, category:String):Void;
}
