package cs.system.io;

/** Provides a generic view of a sequence of bytes. This is an abstract class. */
@:native("System.IO.Stream")
extern class Stream extends cs.system.MarshalByRefObject {
	/** A  with no backing store. */
	static var Null(default, never):cs.system.io.Stream;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * stream supports reading.
	 * @return if the stream supports reading; otherwise, .
	 */
	var CanRead(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * stream supports seeking.
	 * @return if the stream supports seeking; otherwise, .
	 */
	var CanSeek(default, never):Bool;
	/**
	 * Gets a value that determines whether the current stream can time out.
	 * @return A value that determines whether the current stream can time out.
	 */
	var CanTimeout(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * stream supports writing.
	 * @return if the stream supports writing; otherwise, .
	 */
	var CanWrite(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the length in bytes of the stream.
	 * @return A long value representing the length of the stream in bytes.
	 */
	var Length(default, never):haxe.Int64;
	/**
	 * When overridden in a derived class, gets or sets the position within the current
	 * stream.
	 * @return The current position within the stream.
	 */
	var Position(default, default):haxe.Int64;
	/**
	 * Gets or sets a value, in milliseconds, that determines how long the stream will
	 * attempt to read before timing out.
	 * @return A value, in milliseconds, that determines how long the stream will
	 * attempt to read before timing out.
	 */
	var ReadTimeout(default, default):Int;
	/**
	 * Gets or sets a value, in milliseconds, that determines how long the stream will
	 * attempt to write before timing out.
	 * @return A value, in milliseconds, that determines how long the stream will
	 * attempt to write before timing out.
	 */
	var WriteTimeout(default, default):Int;
	/**
	 * Creates a thread-safe (synchronized) wrapper around the specified  object.
	 * @param stream The  object to synchronize.
	 * @return A thread-safe  object.
	 */
	static function Synchronized(stream:cs.system.io.Stream):cs.system.io.Stream;
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
	/** Closes the current stream and releases any resources (such as sockets and file handles) associated with the current stream. Instead of calling this method, ensure that the stream is properly disposed. */
	function Close():Void;
	@:overload(function(destination:cs.system.io.Stream):Void {})
	/**
	 * Reads the bytes from the current stream and writes them to another stream.
	 * @param destination The stream to which the contents of the current stream will
	 * be copied.
	 */
	function CopyTo(destination:cs.system.io.Stream, bufferSize:Int):Void;
	@:overload(function(destination:cs.system.io.Stream):cs.system.threading.tasks.Task {})
	@:overload(function(destination:cs.system.io.Stream, bufferSize:Int):cs.system.threading.tasks.Task {})
	@:overload(function(destination:cs.system.io.Stream, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously reads the bytes from the current stream and writes them to
	 * another stream.
	 * @param destination The stream to which the contents of the current stream will
	 * be copied.
	 * @return A task that represents the asynchronous copy operation.
	 */
	function CopyToAsync(destination:cs.system.io.Stream, bufferSize:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/** Releases all resources used by the . */
	function Dispose():Void;
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
	/** When overridden in a derived class, clears all buffers for this stream and causes any buffered data to be written to the underlying device. */
	function Flush():Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously clears all buffers for this stream and causes any buffered data
	 * to be written to the underlying device.
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * When overridden in a derived class, reads a sequence of bytes from the current
	 * stream and advances the position within the stream by the number of bytes read.
	 * @param buffer An array of bytes. When this method returns, the buffer contains
	 * the specified byte array with the values between  and ( +  - 1) replaced by the
	 * bytes read from the current source.
	 * @param offset The zero-based byte offset in  at which to begin storing the data
	 * read from the current stream.
	 * @param count The maximum number of bytes to be read from the current stream.
	 * @return The total number of bytes read into the buffer. This can be less than
	 * the number of bytes requested if that many bytes are not currently available, or
	 * zero (0) if the end of the stream has been reached.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.system.threading.tasks.Task_1<Int> {})
	/**
	 * Asynchronously reads a sequence of bytes from the current stream and advances
	 * the position within the stream by the number of bytes read.
	 * @param buffer The buffer to write the data into.
	 * @param offset The byte offset in  at which to begin writing data from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @return A task that represents the asynchronous read operation. The value of the
	 * parameter contains the total number of bytes read into the buffer. The result
	 * value can be less than the number of bytes requested if the number of bytes
	 * currently available is less than the requested number, or it can be 0 (zero) if
	 * the end of the stream has been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a byte from the stream and advances the position within the stream by one
	 * byte, or returns -1 if at the end of the stream.
	 * @return The unsigned byte cast to an , or -1 if at the end of the stream.
	 */
	function ReadByte():Int;
	/**
	 * When overridden in a derived class, sets the position within the current stream.
	 * @param offset A byte offset relative to the  parameter.
	 * @param origin A value of type  indicating the reference point used to obtain the
	 * new position.
	 * @return The new position within the current stream.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * When overridden in a derived class, sets the length of the current stream.
	 * @param value The desired length of the current stream in bytes.
	 */
	function SetLength(value:haxe.Int64):Void;
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * When overridden in a derived class, writes a sequence of bytes to the current
	 * stream and advances the current position within this stream by the number of
	 * bytes written.
	 * @param buffer An array of bytes. This method copies  bytes from  to the current
	 * stream.
	 * @param offset The zero-based byte offset in  at which to begin copying bytes to
	 * the current stream.
	 * @param count The number of bytes to be written to the current stream.
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously writes a sequence of bytes to the current stream and advances the
	 * current position within this stream by the number of bytes written.
	 * @param buffer The buffer to write data from.
	 * @param offset The zero-based byte offset in  from which to begin copying bytes
	 * to the stream.
	 * @param count The maximum number of bytes to write.
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
