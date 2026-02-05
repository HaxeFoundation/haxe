package cs.system.diagnostics;

/** Directs tracing or debugging output to a  or to a , such as . */
@:native("System.Diagnostics.TextWriterTraceListener")
extern class TextWriterTraceListener extends cs.system.diagnostics.TraceListener {
	/**
	 * Gets or sets the text writer that receives the tracing or debugging output.
	 * @return A  that represents the writer that receives the tracing or debugging
	 * output.
	 */
	var Writer(default, default):cs.system.io.TextWriter;
	@:overload(function():Void {})
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(writer:cs.system.io.TextWriter):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(stream:cs.system.io.Stream, name:String):Void {})
	@:overload(function(writer:cs.system.io.TextWriter, name:String):Void {})
	function new(fileName:String, name:String):Void;
	/** Closes the  so that it no longer receives tracing or debugging output. */
	function Close():Void;
	/** Flushes the output buffer for the . */
	function Flush():Void;
	/**
	 * Writes a message to this instance's .
	 * @param message A message to write.
	 */
	function Write(message:String):Void;
	/**
	 * Writes a message to this instance's  followed by a line terminator. The default
	 * line terminator is a carriage return followed by a line feed (\r\n).
	 * @param message A message to write.
	 */
	function WriteLine(message:String):Void;
}
