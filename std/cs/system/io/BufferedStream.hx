package cs.system.io;

/** Adds a buffering layer to read and write operations on another stream. This class cannot be inherited. */
@:native("System.IO.BufferedStream")
extern class BufferedStream extends cs.system.io.Stream {
	/**
	 * Gets the buffer size in bytes for this buffered stream.
	 * @return An integer representing the buffer size in bytes.
	 */
	var BufferSize(default, never):Int;
	/**
	 * Gets the underlying  instance for this buffered stream.
	 * @return The underlying stream instance.
	 */
	var UnderlyingStream(default, never):cs.system.io.Stream;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	function new(stream:cs.system.io.Stream, bufferSize:Int):Void;
	/**
	 * Begins an asynchronous read operation. (Consider using  instead.)
	 * @param buffer The buffer to read the data into.
	 * @param offset The byte offset in  at which to begin writing data read from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @param callback An optional asynchronous callback, to be called when the read is
	 * complete.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous read request from other requests.
	 * @return An object that represents the asynchronous read, which could still be
	 * pending.
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write operation. (Consider using  instead.)
	 * @param buffer The buffer containing data to write to the current stream.
	 * @param offset The zero-based byte offset in  at which to begin copying bytes to
	 * the current stream.
	 * @param count The maximum number of bytes to write.
	 * @param callback The method to be called when the asynchronous write operation is
	 * completed.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous write request from other requests.
	 * @return An object that references the asynchronous write which could still be
	 * pending.
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Asynchronously releases the unmanaged resources used by the buffered stream.
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Waits for the pending asynchronous read operation to complete. (Consider using 
	 * instead.)
	 * @param asyncResult The reference to the pending asynchronous request to wait
	 * for.
	 * @return The number of bytes read from the stream, between 0 (zero) and the
	 * number of bytes you requested. Streams only return 0 only at the end of the
	 * stream, otherwise, they should block until at least 1 byte is available.
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends an asynchronous write operation and blocks until the I/O operation is
	 * complete. (Consider using  instead.)
	 * @param asyncResult The pending asynchronous request.
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** Clears all buffers for this stream and causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	/**
	 * Asynchronously clears all buffers for this stream, causes any buffered data to
	 * be written to the underlying device, and monitors cancellation requests.
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Copies bytes from the current buffered stream to an array.
	 * @param array The buffer to which bytes are to be copied.
	 * @param offset The byte offset in the buffer at which to begin reading bytes.
	 * @param count The number of bytes to be read.
	 * @return The total number of bytes read into . This can be less than the number
	 * of bytes requested if that many bytes are not currently available, or 0 if the
	 * end of the stream has been reached before any data can be read.
	 */
	function Read(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	/**
	 * Asynchronously reads a sequence of bytes from the current stream, advances the
	 * position within the stream by the number of bytes read, and monitors
	 * cancellation requests.
	 * @param buffer The buffer to write the data into.
	 * @param offset The byte offset in  at which to begin writing data from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the stream has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a byte from the underlying stream and returns the byte cast to an , or
	 * returns -1 if reading from the end of the stream.
	 * @return The byte cast to an , or -1 if reading from the end of the stream.
	 */
	function ReadByte():Int;
	/**
	 * Sets the position within the current buffered stream.
	 * @param offset A byte offset relative to .
	 * @param origin A value of type  indicating the reference point from which to
	 * obtain the new position.
	 * @return The new position within the current buffered stream.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of the buffered stream.
	 * @param value An integer indicating the desired length of the current buffered
	 * stream in bytes.
	 */
	function SetLength(value:haxe.Int64):Void;
	/**
	 * Copies bytes to the buffered stream and advances the current position within the
	 * buffered stream by the number of bytes written.
	 * @param array The byte array from which to copy  bytes to the current buffered
	 * stream.
	 * @param offset The offset in the buffer at which to begin copying bytes to the
	 * current buffered stream.
	 * @param count The number of bytes to be written to the current buffered stream.
	 */
	function Write(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	/**
	 * Asynchronously writes a sequence of bytes to the current stream, advances the
	 * current position within this stream by the number of bytes written, and monitors
	 * cancellation requests.
	 * @param buffer The buffer to write data from.
	 * @param offset The zero-based byte offset in  from which to begin copying bytes
	 * to the stream.
	 * @param count The maximum number of bytes to write.
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Writes a byte to the current position in the buffered stream.
	 * @param value A byte to write to the stream.
	 */
	function WriteByte(value:cs.UInt8):Void;
}
