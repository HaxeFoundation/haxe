package cs.system.io.pipes;

/** Exposes a  object around a pipe, which supports both anonymous and named pipes. */
@:native("System.IO.Pipes.PipeStream")
extern class PipeStream extends cs.system.io.Stream {
	/**
	 * Gets the size, in bytes, of the inbound buffer for a pipe.
	 * @return An integer value that represents the inbound buffer size, in bytes.
	 */
	var InBufferSize(default, never):Int;
	/**
	 * Gets a value indicating whether a  object was opened asynchronously or
	 * synchronously.
	 * @return if the  object was opened asynchronously; otherwise, .
	 */
	var IsAsync(default, never):Bool;
	/**
	 * Gets or sets a value indicating whether a  object is connected.
	 * @return if the  object is connected; otherwise, .
	 */
	var IsConnected(default, default):Bool;
	/**
	 * Gets a value indicating whether a handle to a  object is exposed.
	 * @return if a handle to the  object is exposed; otherwise, .
	 */
	var IsHandleExposed(default, never):Bool;
	/**
	 * Gets a value indicating whether there is more data in the message returned from
	 * the most recent read operation.
	 * @return if there are no more characters to read in the message; otherwise, .
	 */
	var IsMessageComplete(default, never):Bool;
	/**
	 * Gets the size, in bytes, of the outbound buffer for a pipe.
	 * @return The outbound buffer size, in bytes.
	 */
	var OutBufferSize(default, never):Int;
	/**
	 * Gets or sets the reading mode for a  object.
	 * @return One of the  values that indicates how the  object reads from the pipe.
	 */
	var ReadMode(default, default):cs.system.io.pipes.PipeTransmissionMode;
	/**
	 * Gets the safe handle for the local end of the pipe that the current  object
	 * encapsulates.
	 * @return A  object for the pipe that is encapsulated by the current  object.
	 */
	var SafePipeHandle(default, never):cs.microsoft.win32.safehandles.SafePipeHandle;
	/**
	 * Gets the pipe transmission mode supported by the current pipe.
	 * @return One of the  values that indicates the transmission mode supported by the
	 * current pipe.
	 */
	var TransmissionMode(default, never):cs.system.io.pipes.PipeTransmissionMode;
	/**
	 * Begins an asynchronous read operation.
	 * @param buffer The buffer to read data into.
	 * @param offset The byte offset in  at which to begin reading.
	 * @param count The maximum number of bytes to read.
	 * @param callback The method to call when the asynchronous read operation is
	 * completed.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous read request from other requests.
	 * @return An  object that references the asynchronous read.
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write operation.
	 * @param buffer The buffer that contains the data to write to the current stream.
	 * @param offset The zero-based byte offset in  at which to begin copying bytes to
	 * the current stream.
	 * @param count The maximum number of bytes to write.
	 * @param callback The method to call when the asynchronous write operation is
	 * completed.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous write request from other requests.
	 * @return An  object that references the asynchronous write operation.
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Ends a pending asynchronous read request.
	 * @param asyncResult The reference to the pending asynchronous request.
	 * @return The number of bytes that were read. A return value of 0 indicates the
	 * end of the stream (the pipe has been closed).
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends a pending asynchronous write request.
	 * @param asyncResult The reference to the pending asynchronous request.
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** Clears the buffer for the current stream and causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * Reads a block of bytes from a stream and writes the data to a specified buffer
	 * starting at a specified position for a specified length.
	 * @param buffer When this method returns, contains the specified byte array with
	 * the values between  and ( +  - 1) replaced by the bytes read from the current
	 * source.
	 * @param offset The byte offset in the  array at which the bytes that are read
	 * will be placed.
	 * @param count The maximum number of bytes to read.
	 * @return The total number of bytes that are read into . This might be less than
	 * the number of bytes requested if that number of bytes is not currently
	 * available, or 0 if the end of the stream is reached.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Asynchronously reads a sequence of bytes from the current stream to a byte array
	 * starting at a specified position for a specified number of bytes, advances the
	 * position within the stream by the number of bytes read, and monitors
	 * cancellation requests.
	 * @param buffer The buffer to write the data into.
	 * @param offset The byte offset in  at which to begin writing data from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous read operation. The value of its
	 * property contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the stream has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a byte from a pipe.
	 * @return The byte, cast to , or -1 indicates the end of the stream (the pipe has
	 * been closed).
	 */
	function ReadByte():Int;
	/**
	 * Sets the current position of the current stream to the specified value.
	 * @param offset The point, relative to , to begin seeking from.
	 * @param origin Specifies the beginning, the end, or the current position as a
	 * reference point for , using a value of type .
	 * @return The new position in the stream.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of the current stream to the specified value.
	 * @param value The new length of the stream.
	 */
	function SetLength(value:haxe.Int64):Void;
	/** Waits for the other end of the pipe to read all sent bytes. */
	function WaitForPipeDrain():Void;
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Writes a block of bytes to the current stream using data from a buffer.
	 * @param buffer The buffer that contains data to write to the pipe.
	 * @param offset The zero-based byte offset in  at which to begin copying bytes to
	 * the current stream.
	 * @param count The maximum number of bytes to write to the current stream.
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
	/**
	 * Asynchronously writes a specified number of bytes from a byte array starting at
	 * a specified position, advances the current position within this stream by the
	 * number of bytes written, and monitors cancellation requests.
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
	 * Writes a byte to the current stream.
	 * @param value The byte to write to the stream.
	 */
	function WriteByte(value:cs.UInt8):Void;
}
