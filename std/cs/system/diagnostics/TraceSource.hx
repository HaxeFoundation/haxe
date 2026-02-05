package cs.system.diagnostics;

/** Provides a set of methods and properties that enable applications to trace the execution of code and associate trace messages with their source. */
@:native("System.Diagnostics.TraceSource")
extern class TraceSource {
	/**
	 * Gets the custom switch attributes defined in the application configuration file.
	 * @return A  containing the custom attributes for the trace switch.
	 */
	var Attributes(default, never):cs.system.collections.specialized.StringDictionary;
	/**
	 * Gets the collection of trace listeners for the trace source.
	 * @return A  that contains the active trace listeners associated with the source.
	 */
	var Listeners(default, never):cs.system.diagnostics.TraceListenerCollection;
	/**
	 * Gets the name of the trace source.
	 * @return The name of the trace source.
	 */
	var Name(default, never):String;
	/**
	 * Gets or sets the source switch value.
	 * @return A  object representing the source switch value.
	 */
	var Switch(default, default):cs.system.diagnostics.SourceSwitch;
	@:overload(function(name:String):Void {})
	function new(name:String, defaultLevel:cs.system.diagnostics.SourceLevels):Void;
	/** Closes all the trace listeners in the trace listener collection. */
	function Close():Void;
	/** Flushes all the trace listeners in the trace listener collection. */
	function Flush():Void;
	@:overload(function(eventType:cs.system.diagnostics.TraceEventType, id:Int, data:Dynamic):Void {})
	/**
	 * Writes trace data to the trace listeners in the  collection using the specified
	 * event type, event identifier, and trace data.
	 * @param eventType One of the enumeration values that specifies the event type of
	 * the trace data.
	 * @param id A numeric identifier for the event.
	 * @param data The trace data.
	 */
	function TraceData(eventType:cs.system.diagnostics.TraceEventType, id:Int, data:cs.NativeArray<Dynamic>):Void;
	@:overload(function(eventType:cs.system.diagnostics.TraceEventType, id:Int):Void {})
	@:overload(function(eventType:cs.system.diagnostics.TraceEventType, id:Int, message:String):Void {})
	/**
	 * Writes a trace event message to the trace listeners in the  collection using the
	 * specified event type and event identifier.
	 * @param eventType One of the enumeration values that specifies the event type of
	 * the trace data.
	 * @param id A numeric identifier for the event.
	 */
	function TraceEvent(eventType:cs.system.diagnostics.TraceEventType, id:Int, format:String, args:cs.NativeArray<Dynamic>):Void;
	@:overload(function(message:String):Void {})
	/**
	 * Writes an informational message to the trace listeners in the  collection using
	 * the specified message.
	 * @param message The informative message to write.
	 */
	function TraceInformation(format:String, args:cs.NativeArray<Dynamic>):Void;
	/**
	 * Writes a trace transfer message to the trace listeners in the  collection using
	 * the specified numeric identifier, message, and related activity identifier.
	 * @param id A numeric identifier for the event.
	 * @param message The trace message to write.
	 * @param relatedActivityId A structure that identifies the related activity.
	 */
	function TraceTransfer(id:Int, message:String, relatedActivityId:cs.system.Guid):Void;
}
