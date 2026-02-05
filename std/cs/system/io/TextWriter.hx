package cs.system.io;

/** Represents a writer that can write a sequential series of characters. This class is abstract. */
@:native("System.IO.TextWriter")
extern class TextWriter extends cs.system.MarshalByRefObject {
	/** Provides a  with no backing store that can be written to, but not read from. */
	static var Null(default, never):cs.system.io.TextWriter;
	/**
	 * When overridden in a derived class, returns the character encoding in which the
	 * output is written.
	 * @return The character encoding in which the output is written.
	 */
	var Encoding(default, never):cs.system.text.Encoding;
	/**
	 * Gets an object that controls formatting.
	 * @return An  object for a specific culture, or the formatting of the current
	 * culture if no other culture is specified.
	 */
	var FormatProvider(default, never):cs.system.IFormatProvider;
	/**
	 * Gets or sets the line terminator string used by the current .
	 * @return The line terminator string for the current .
	 */
	var NewLine(default, default):String;
	/**
	 * Creates a thread-safe wrapper around the specified .
	 * @param writer The  to synchronize.
	 * @return A thread-safe wrapper.
	 */
	static function Synchronized(writer:cs.system.io.TextWriter):cs.system.io.TextWriter;
	/** Closes the current writer and releases any system resources associated with the writer. */
	function Close():Void;
	/** Releases all resources used by the  object. */
	function Dispose():Void;
	/**
	 * Asynchronously releases all resources used by the  object.
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/** Clears all buffers for the current writer and causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	/**
	 * Asynchronously clears all buffers for the current writer and causes any buffered
	 * data to be written to the underlying device.
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync():cs.system.threading.tasks.Task;
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.Char16>):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	@:overload(function(format:String, arg0:Dynamic):Void {})
	@:overload(function(format:String, arg:cs.NativeArray<Dynamic>):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void {})
	@:overload(function(format:String, arg0:Dynamic, arg1:Dynamic):Void {})
	/**
	 * Writes the text representation of a  value to the text stream.
	 * @param value The  value to write.
	 */
	function Write(format:String, arg0:Dynamic, arg1:Dynamic, arg2:Dynamic):Void;
	@:overload(function(value:cs.Char16):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):cs.system.threading.tasks.Task {})
	@:overload(function(value:String):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Writes a character to the text stream asynchronously.
	 * @param value The character to write to the text stream.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
	@:overload(function():Void {})
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Dynamic):Void {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.Char16>):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	@:overload(function(format:String, arg0:Dynamic):Void {})
	@:overload(function(format:String, arg:cs.NativeArray<Dynamic>):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void {})
	@:overload(function(format:String, arg0:Dynamic, arg1:Dynamic):Void {})
	/** Writes a line terminator to the text stream. */
	function WriteLine(format:String, arg0:Dynamic, arg1:Dynamic, arg2:Dynamic):Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	@:overload(function(value:cs.Char16):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):cs.system.threading.tasks.Task {})
	@:overload(function(value:String):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously writes a line terminator to the text stream.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteLineAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
}
