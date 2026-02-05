package cs.system.diagnostics;

/** Provides the default output methods and behavior for tracing. */
@:native("System.Diagnostics.DefaultTraceListener")
extern class DefaultTraceListener extends cs.system.diagnostics.TraceListener {
	/**
	 * Gets or sets a value indicating whether the application is running in
	 * user-interface mode.
	 * @return if user-interface mode is enabled; otherwise, .
	 */
	var AssertUiEnabled(default, default):Bool;
	/**
	 * Gets or sets the name of a log file to write trace or debug messages to.
	 * @return The name of a log file to write trace or debug messages to.
	 */
	var LogFileName(default, default):String;
	function new():Void;
	@:overload(function(message:String):Void {})
	/**
	 * Emits or displays a message and a stack trace for an assertion that always
	 * fails.
	 * @param message The message to emit or display.
	 */
	function Fail(message:String, detailMessage:String):Void;
	/**
	 * Writes the output to the  function and to the  method.
	 * @param message The message to write to  and .
	 */
	function Write(message:String):Void;
	/**
	 * Writes the output to the  function and to the  method, followed by a carriage
	 * return and line feed (\r\n).
	 * @param message The message to write to  and .
	 */
	function WriteLine(message:String):Void;
}
