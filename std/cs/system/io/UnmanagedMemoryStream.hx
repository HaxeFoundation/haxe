package cs.system.io;

/** Provides access to unmanaged blocks of memory from managed code. */
@:native("System.IO.UnmanagedMemoryStream")
extern class UnmanagedMemoryStream extends cs.system.io.Stream {
	/**
	 * Gets the stream length (size) or the total amount of memory assigned to a stream
	 * (capacity).
	 * @return The size or capacity of the stream.
	 */
	var Capacity(default, never):haxe.Int64;
	/**
	 * Gets or sets a byte pointer to a stream based on the current position in the
	 * stream.
	 * @return A byte pointer.
	 */
	var PositionPointer(default, default):cs.Pointer<cs.UInt8>;
	@:overload(function(pointer:cs.Pointer<cs.UInt8>, length:haxe.Int64):Void {})
	@:overload(function(buffer:cs.system.runtime.interopservices.SafeBuffer, offset:haxe.Int64, length:haxe.Int64):Void {})
	@:overload(function(pointer:cs.Pointer<cs.UInt8>, length:haxe.Int64, capacity:haxe.Int64, access:cs.system.io.FileAccess):Void {})
	function new(buffer:cs.system.runtime.interopservices.SafeBuffer, offset:haxe.Int64, length:haxe.Int64, access:cs.system.io.FileAccess):Void;
	/** Overrides the  method so that no action is performed. */
	function Flush():Void;
	/**
	 * Overrides the  method so that the operation is cancelled if specified, but no
	 * other action is performed.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(destination:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * Reads the specified number of bytes into the specified array.
	 * @param buffer When this method returns, contains the specified byte array with
	 * the values between  and ( +  - 1) replaced by the bytes read from the current
	 * source. This parameter is passed uninitialized.
	 * @param offset The zero-based byte offset in  at which to begin storing the data
	 * read from the current stream.
	 * @param count The maximum number of bytes to read from the current stream.
	 * @return The total number of bytes read into the buffer. This can be less than
	 * the number of bytes requested if that many bytes are not currently available, or
	 * zero (0) if the end of the stream has been reached.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Asynchronously reads the specified number of bytes into the specified array.
	 * @param buffer The buffer to write the data into.
	 * @param offset The byte offset in  at which to begin writing data from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the stream has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a byte from a stream and advances the position within the stream by one
	 * byte, or returns -1 if at the end of the stream.
	 * @return The unsigned byte cast to an  object, or -1 if at the end of the stream.
	 */
	function ReadByte():Int;
	/**
	 * Sets the current position of the current stream to the given value.
	 * @param offset The point relative to origin to begin seeking from.
	 * @param loc Specifies the beginning, the end, or the current position as a
	 * reference point for origin, using a value of type .
	 * @return The new position in the stream.
	 */
	function Seek(offset:haxe.Int64, loc:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of a stream to a specified value.
	 * @param value The length of the stream.
	 */
	function SetLength(value:haxe.Int64):Void;
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Writes a block of bytes to the current stream using data from a buffer.
	 * @param buffer The byte array from which to copy bytes to the current stream.
	 * @param offset The offset in the buffer at which to begin copying bytes to the
	 * current stream.
	 * @param count The number of bytes to write to the current stream.
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
	/**
	 * Asynchronously writes a sequence of bytes to the current stream, advances the
	 * current position within this stream by the number of bytes written, and monitors
	 * cancellation requests.
	 * @param buffer The buffer to write data from.
	 * @param offset The zero-based byte offset in  from which to begin copying bytes
	 * to the stream.
	 * @param count The maximum number of bytes to write.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Writes a byte to the current position in the file stream.
	 * @param value A byte value written to the stream.
	 */
	function WriteByte(value:cs.UInt8):Void;
}
