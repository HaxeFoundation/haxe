package cs.system.diagnostics;

/** Provides a set of methods and properties that help you trace the execution of your code. This class cannot be inherited. */
@:native("System.Diagnostics.Trace")
extern class Trace {
	/**
	 * Gets or sets whether  should be called on the  after every write.
	 * @return if  is called on the  after every write; otherwise, .
	 */
	static var AutoFlush(default, default):Bool;
	/**
	 * Gets the correlation manager for the thread for this trace.
	 * @return The  object associated with the thread for this trace.
	 */
	static var CorrelationManager(default, never):cs.system.diagnostics.CorrelationManager;
	/**
	 * Gets or sets the indent level.
	 * @return The indent level. The default is zero.
	 */
	static var IndentLevel(default, default):Int;
	/**
	 * Gets or sets the number of spaces in an indent.
	 * @return The number of spaces in an indent. The default is four.
	 */
	static var IndentSize(default, default):Int;
	/**
	 * Gets the collection of listeners that is monitoring the trace output.
	 * @return A  that represents a collection of type  monitoring the trace output.
	 */
	static var Listeners(default, never):cs.system.diagnostics.TraceListenerCollection;
	/**
	 * Gets or sets a value indicating whether the global lock should be used.
	 * @return if the global lock is to be used; otherwise, . The default is .
	 */
	static var UseGlobalLock(default, default):Bool;
	@:overload(function(condition:Bool):Void {})
	@:overload(function(condition:Bool, message:String):Void {})
	/**
	 * Checks for a condition; if the condition is , displays a message box that shows
	 * the call stack.
	 * @param condition The conditional expression to evaluate. If the condition is , a
	 * failure message is not sent and the message box is not displayed.
	 */
	static function Assert(condition:Bool, message:String, detailMessage:String):Void;
	/** Flushes the output buffer, and then closes the . */
	static function Close():Void;
	@:overload(function(message:String):Void {})
	/**
	 * Emits the specified error message.
	 * @param message A message to emit.
	 */
	static function Fail(message:String, detailMessage:String):Void;
	/** Flushes the output buffer, and causes buffered data to be written to the . */
	static function Flush():Void;
	/** Increases the current  by one. */
	static function Indent():Void;
	/** Refreshes the trace configuration data. */
	static function Refresh():Void;
	@:overload(function(message:String):Void {})
	/**
	 * Writes an error message to the trace listeners in the  collection using the
	 * specified message.
	 * @param message The informative message to write.
	 */
	static function TraceError(format:String, args:cs.NativeArray<Dynamic>):Void;
	@:overload(function(message:String):Void {})
	/**
	 * Writes an informational message to the trace listeners in the  collection using
	 * the specified message.
	 * @param message The informative message to write.
	 */
	static function TraceInformation(format:String, args:cs.NativeArray<Dynamic>):Void;
	@:overload(function(message:String):Void {})
	/**
	 * Writes a warning message to the trace listeners in the  collection using the
	 * specified message.
	 * @param message The informative message to write.
	 */
	static function TraceWarning(format:String, args:cs.NativeArray<Dynamic>):Void;
	/** Decreases the current  by one. */
	static function Unindent():Void;
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(value:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection.
	 * @param value An  whose name is sent to the .
	 */
	static function Write(message:String, category:String):Void;
	@:overload(function(condition:Bool, value:Dynamic):Void {})
	@:overload(function(condition:Bool, message:String):Void {})
	@:overload(function(condition:Bool, value:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection if a condition is .
	 * @param condition to cause a message to be written; otherwise, .
	 * @param value An  whose name is sent to the .
	 */
	static function WriteIf(condition:Bool, message:String, category:String):Void;
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(value:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection.
	 * @param value An  whose name is sent to the .
	 */
	static function WriteLine(message:String, category:String):Void;
	@:overload(function(condition:Bool, value:Dynamic):Void {})
	@:overload(function(condition:Bool, message:String):Void {})
	@:overload(function(condition:Bool, value:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection if a condition is .
	 * @param condition to cause a message to be written; otherwise, .
	 * @param value An  whose name is sent to the .
	 */
	static function WriteLineIf(condition:Bool, message:String, category:String):Void;
}
