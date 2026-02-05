package cs.system.io;

/** Writes primitive types in binary to a stream and supports writing strings in a specific encoding. */
@:native("System.IO.BinaryWriter")
extern class BinaryWriter {
	/** Specifies a  with no backing store. */
	static var Null(default, never):cs.system.io.BinaryWriter;
	/**
	 * Gets the underlying stream of the .
	 * @return The underlying stream associated with the .
	 */
	var BaseStream(default, never):cs.system.io.Stream;
	@:overload(function(output:cs.system.io.Stream):Void {})
	@:overload(function(output:cs.system.io.Stream, encoding:cs.system.text.Encoding):Void {})
	function new(output:cs.system.io.Stream, encoding:cs.system.text.Encoding, leaveOpen:Bool):Void;
	/** Closes the current  and the underlying stream. */
	function Close():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Asynchronously releases all resources used by the current instance of the 
	 * class.
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/** Clears all buffers for the current writer and causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	/**
	 * Sets the position within the current stream.
	 * @param offset A byte offset relative to .
	 * @param origin A field of  indicating the reference point from which the new
	 * position is to be obtained.
	 * @return The position with the current stream.
	 */
	function Seek(offset:Int, origin:cs.system.io.SeekOrigin):haxe.Int64;
	@:overload(function(value:Bool):Void {})
	@:overload(function(value:cs.UInt8):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(ch:cs.Char16):Void {})
	@:overload(function(chars:cs.NativeArray<cs.Char16>):Void {})
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:cs.Int16):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	@:overload(function(chars:cs.system.ReadOnlySpan<cs.Char16>):Void {})
	@:overload(function(value:cs.Int8):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(value:cs.UInt16):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Void {})
	/**
	 * Writes a one-byte  value to the current stream, with 0 representing  and 1
	 * representing .
	 * @param value The  value to write (0 or 1).
	 */
	function Write(chars:cs.NativeArray<cs.Char16>, index:Int, count:Int):Void;
}
