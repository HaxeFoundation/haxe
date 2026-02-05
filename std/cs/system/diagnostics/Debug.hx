package cs.system.diagnostics;

/** Provides a set of methods and properties that help debug your code. */
@:native("System.Diagnostics.Debug")
extern class Debug {
	/**
	 * Gets or sets a value indicating whether  should be called on the  after every
	 * write.
	 * @return if  is called on the  after every write; otherwise, .
	 */
	static var AutoFlush(default, default):Bool;
	/**
	 * Gets or sets the indent level.
	 * @return The indent level. The default is 0.
	 */
	static var IndentLevel(default, default):Int;
	/**
	 * Gets or sets the number of spaces in an indent.
	 * @return The number of spaces in an indent. The default is four.
	 */
	static var IndentSize(default, default):Int;
	@:overload(function(condition:Bool):Void {})
	@:overload(function(condition:Bool, message:String):Void {})
	@:overload(function(condition:Bool, message:String, detailMessage:String):Void {})
	/**
	 * Checks for a condition; if the condition is , displays a message box that shows
	 * the call stack.
	 * @param condition The conditional expression to evaluate. If the condition is , a
	 * failure message is not sent and the message box is not displayed.
	 */
	static function Assert(condition:Bool, message:String, detailMessageFormat:String, args:cs.NativeArray<Dynamic>):Void;
	/** Flushes the output buffer and then calls the  method on each of the . */
	static function Close():Void;
	@:overload(function(message:String):Void {})
	/**
	 * Emits the specified error message.
	 * @param message A message to emit.
	 */
	static function Fail(message:String, detailMessage:String):Void;
	/** Flushes the output buffer and causes buffered data to write to the  collection. */
	static function Flush():Void;
	/** Increases the current  by one. */
	static function Indent():Void;
	@:overload(function(message:String):Void {})
	/**
	 * Writes a message followed by a line terminator to the trace listeners in the 
	 * collection.
	 * @param message The message to write.
	 */
	static function Print(format:String, args:cs.NativeArray<Dynamic>):Void;
	/** Decreases the current  by one. */
	static function Unindent():Void;
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(value:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection.
	 * @param value An object whose name is sent to the .
	 */
	static function Write(message:String, category:String):Void;
	@:overload(function(condition:Bool, value:Dynamic):Void {})
	@:overload(function(condition:Bool, message:String):Void {})
	@:overload(function(condition:Bool, value:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection if a condition is .
	 * @param condition The conditional expression to evaluate. If the condition is ,
	 * the value is written to the trace listeners in the collection.
	 * @param value An object whose name is sent to the .
	 */
	static function WriteIf(condition:Bool, message:String, category:String):Void;
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(value:Dynamic, category:String):Void {})
	@:overload(function(format:String, args:cs.NativeArray<Dynamic>):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection.
	 * @param value An object whose name is sent to the .
	 */
	static function WriteLine(message:String, category:String):Void;
	@:overload(function(condition:Bool, value:Dynamic):Void {})
	@:overload(function(condition:Bool, message:String):Void {})
	@:overload(function(condition:Bool, value:Dynamic, category:String):Void {})
	/**
	 * Writes the value of the object's  method to the trace listeners in the 
	 * collection if a condition is .
	 * @param condition The conditional expression to evaluate. If the condition is ,
	 * the value is written to the trace listeners in the collection.
	 * @param value An object whose name is sent to the .
	 */
	static function WriteLineIf(condition:Bool, message:String, category:String):Void;
}
