package cs.system.security.cryptography;

/** Defines a stream that links data streams to cryptographic transformations. */
@:native("System.Security.Cryptography.CryptoStream")
extern class CryptoStream extends cs.system.io.Stream {
	/**
	 * Gets a value indicating whether the final buffer block has been written to the
	 * underlying stream.
	 * @return if the final block has been flushed; otherwise, .
	 */
	var HasFlushedFinalBlock(default, never):Bool;
	@:overload(function(stream:cs.system.io.Stream, transform:cs.system.security.cryptography.ICryptoTransform, mode:cs.system.security.cryptography.CryptoStreamMode):Void {})
	function new(stream:cs.system.io.Stream, transform:cs.system.security.cryptography.ICryptoTransform, mode:cs.system.security.cryptography.CryptoStreamMode, leaveOpen:Bool):Void;
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
	 * @return An  that represents the asynchronous read, which could still be pending.
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write operation. (Consider using  instead.)
	 * @param buffer The buffer to write data from.
	 * @param offset The byte offset in  from which to begin writing.
	 * @param count The maximum number of bytes to write.
	 * @param callback An optional asynchronous callback, to be called when the write
	 * is complete.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous write request from other requests.
	 * @return An  that represents the asynchronous write, which could still be
	 * pending.
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/** Releases all resources used by the . */
	function Clear():Void;
	/**
	 * Asynchronously releases the unmanaged resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Waits for the pending asynchronous read to complete. (Consider using  instead.)
	 * @param asyncResult The reference to the pending asynchronous request to finish.
	 * @return The number of bytes read from the stream, between zero (0) and the
	 * number of bytes you requested. Streams return zero (0) only at the end of the
	 * stream, otherwise, they should block until at least one byte is available.
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends an asynchronous write operation. (Consider using  instead.)
	 * @param asyncResult A reference to the outstanding asynchronous I/O request.
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** Clears all buffers for the current stream and causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	/**
	 * Clears all buffers for the current stream asynchronously, causes any buffered
	 * data to be written to the underlying device, and monitors cancellation requests.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/** Updates the underlying data source or repository with the current state of the buffer, then clears the buffer. */
	function FlushFinalBlock():Void;
	/**
	 * Reads a sequence of bytes from the current stream and advances the position
	 * within the stream by the number of bytes read.
	 * @param buffer An array of bytes. A maximum of  bytes are read from the current
	 * stream and stored in .
	 * @param offset The byte offset in  at which to begin storing the data read from
	 * the current stream.
	 * @param count The maximum number of bytes to be read from the current stream.
	 * @return The total number of bytes read into the buffer. This can be less than
	 * the number of bytes requested if that many bytes are not currently available, or
	 * zero if the end of the stream has been reached.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	/**
	 * Reads a sequence of bytes from the current stream asynchronously, advances the
	 * position within the stream by the number of bytes read, and monitors
	 * cancellation requests.
	 * @param buffer The buffer to write the data into.
	 * @param offset The byte offset in  at which to begin writing data from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous read operation. The value of the
	 * task object's  parameter contains the total number of bytes read into the
	 * buffer. The result can be less than the number of bytes requested if the number
	 * of bytes currently available is less than the requested number, or it can be 0
	 * (zero) if the end of the stream has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a byte from the stream and advances the position within the stream by one
	 * byte, or returns -1 if at the end of the stream.
	 * @return The unsigned byte cast to an , or -1 if at the end of the stream.
	 */
	function ReadByte():Int;
	/**
	 * Sets the position within the current stream.
	 * @param offset A byte offset relative to the  parameter.
	 * @param origin A  object indicating the reference point used to obtain the new
	 * position.
	 * @return This method is not supported.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of the current stream.
	 * @param value The desired length of the current stream in bytes.
	 */
	function SetLength(value:haxe.Int64):Void;
	/**
	 * Writes a sequence of bytes to the current  and advances the current position
	 * within the stream by the number of bytes written.
	 * @param buffer An array of bytes. This method copies  bytes from  to the current
	 * stream.
	 * @param offset The byte offset in  at which to begin copying bytes to the current
	 * stream.
	 * @param count The number of bytes to be written to the current stream.
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	/**
	 * Writes a sequence of bytes to the current stream asynchronously, advances the
	 * current position within the stream by the number of bytes written, and monitors
	 * cancellation requests.
	 * @param buffer The buffer to write data from.
	 * @param offset The zero-based byte offset in  from which to begin writing bytes
	 * to the stream.
	 * @param count The maximum number of bytes to write.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Writes a byte to the current position in the stream and advances the position
	 * within the stream by one byte.
	 * @param value The byte to write to the stream.
	 */
	function WriteByte(value:cs.UInt8):Void;
}
