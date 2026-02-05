package cs.system.io;

/** Implements a  for writing information to a string. The information is stored in an underlying . */
@:native("System.IO.StringWriter")
extern class StringWriter extends cs.system.io.TextWriter {
	@:overload(function():Void {})
	@:overload(function(formatProvider:cs.system.IFormatProvider):Void {})
	@:overload(function(sb:cs.system.text.StringBuilder):Void {})
	function new(sb:cs.system.text.StringBuilder, formatProvider:cs.system.IFormatProvider):Void;
	/** Closes the current  and the underlying stream. */
	function Close():Void;
	/**
	 * Asynchronously clears all buffers for the current writer and causes any buffered
	 * data to be written to the underlying device.
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync():cs.system.threading.tasks.Task;
	/**
	 * Returns the underlying .
	 * @return The underlying .
	 */
	function GetStringBuilder():cs.system.text.StringBuilder;
	/**
	 * Returns a string containing the characters written to the current  so far.
	 * @return The string containing the characters written to the current .
	 */
	function ToString():String;
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.Char16>):Void {})
	@:overload(function(value:String):Void {})
	/**
	 * Writes a character to the string.
	 * @param value The character to write.
	 */
	function Write(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void;
	@:overload(function(value:cs.Char16):cs.system.threading.tasks.Task {})
	@:overload(function(value:String):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Writes a character to the string asynchronously.
	 * @param value The character to write to the string.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
	/**
	 * Writes the text representation a span of characters to the string, followed by a
	 * line terminator.
	 * @param buffer The span of characters to write to the string.
	 */
	function WriteLine(buffer:cs.system.ReadOnlySpan<cs.Char16>):Void;
	@:overload(function(value:cs.Char16):cs.system.threading.tasks.Task {})
	@:overload(function(value:String):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously writes a character to the string, followed by a line terminator.
	 * @param value The character to write to the string.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteLineAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
}
