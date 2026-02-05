package cs.system.io;

/** Implements a  for writing characters to a stream in a particular encoding. */
@:native("System.IO.StreamWriter")
extern class StreamWriter extends cs.system.io.TextWriter {
	/** Provides a  with no backing store that can be written to, but not read from. */
	static var Null(default, never):cs.system.io.StreamWriter;
	/**
	 * Gets or sets a value indicating whether the  will flush its buffer to the
	 * underlying stream after every call to .
	 * @return to force  to flush its buffer; otherwise, .
	 */
	var AutoFlush(default, default):Bool;
	/**
	 * Gets the underlying stream that interfaces with a backing store.
	 * @return The stream this  is writing to.
	 */
	var BaseStream(default, never):cs.system.io.Stream;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(path:String):Void {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding):Void {})
	@:overload(function(path:String, append:Bool):Void {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, bufferSize:Int):Void {})
	@:overload(function(path:String, append:Bool, encoding:cs.system.text.Encoding):Void {})
	@:overload(function(stream:cs.system.io.Stream, encoding:cs.system.text.Encoding, bufferSize:Int, leaveOpen:Bool):Void {})
	function new(path:String, append:Bool, encoding:cs.system.text.Encoding, bufferSize:Int):Void;
	/** Closes the current  object and the underlying stream. */
	function Close():Void;
	/**
	 * Asynchronously writes any buffered data to the underlying stream and releases
	 * the unmanaged resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/** Clears all buffers for the current writer and causes any buffered data to be written to the underlying stream. */
	function Flush():Void;
	/**
	 * Clears all buffers for this stream asynchronously and causes any buffered data
	 * to be written to the underlying device.
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync():cs.system.threading.tasks.Task;
	@:overload(function(value:cs.Char16):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.Char16>):Void {})
	@:overload(function(value:String):Void {})
	/**
	 * Writes a character to the stream.
	 * @param value The character to write to the stream.
	 */
	function Write(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void;
	@:overload(function(value:cs.Char16):cs.system.threading.tasks.Task {})
	@:overload(function(value:String):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously writes a character to the stream.
	 * @param value The character to write to the stream.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.Char16>):Void {})
	/**
	 * Writes the text representation of a character span to the stream, followed by a
	 * line terminator.
	 * @param buffer The character span to write to the stream.
	 */
	function WriteLine(value:String):Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	@:overload(function(value:cs.Char16):cs.system.threading.tasks.Task {})
	@:overload(function(value:String):cs.system.threading.tasks.Task {})
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.Char16>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously writes a line terminator to the stream.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteLineAsync(buffer:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.system.threading.tasks.Task;
}
