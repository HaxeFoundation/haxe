package cs.system.io;

/** Provides a  for a file, supporting both synchronous and asynchronous read and write operations. */
@:native("System.IO.FileStream")
extern class FileStream extends cs.system.io.Stream {
	/**
	 * Gets the operating system file handle for the file that the current  object
	 * encapsulates.
	 * @return The operating system file handle for the file encapsulated by this 
	 * object, or -1 if the  has been closed.
	 */
	var Handle(default, never):cs.system.IntPtr;
	/**
	 * Gets a value that indicates whether the  was opened asynchronously or
	 * synchronously.
	 * @return if the  was opened asynchronously; otherwise, .
	 */
	var IsAsync(default, never):Bool;
	/**
	 * Gets the absolute path of the file opened in the .
	 * @return A string that is the absolute path of the file.
	 */
	var Name(default, never):String;
	/**
	 * Gets a  object that represents the operating system file handle for the file
	 * that the current  object encapsulates.
	 * @return An object that represents the operating system file handle for the file
	 * that the current  object encapsulates.
	 */
	var SafeFileHandle(default, never):cs.microsoft.win32.safehandles.SafeFileHandle;
	@:overload(function(handle:cs.microsoft.win32.safehandles.SafeFileHandle, access:cs.system.io.FileAccess):Void {})
	@:overload(function(handle:cs.system.IntPtr, access:cs.system.io.FileAccess):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode):Void {})
	@:overload(function(handle:cs.microsoft.win32.safehandles.SafeFileHandle, access:cs.system.io.FileAccess, bufferSize:Int):Void {})
	@:overload(function(handle:cs.system.IntPtr, access:cs.system.io.FileAccess, ownsHandle:Bool):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess):Void {})
	@:overload(function(handle:cs.microsoft.win32.safehandles.SafeFileHandle, access:cs.system.io.FileAccess, bufferSize:Int, isAsync:Bool):Void {})
	@:overload(function(handle:cs.system.IntPtr, access:cs.system.io.FileAccess, ownsHandle:Bool, bufferSize:Int):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare):Void {})
	@:overload(function(handle:cs.system.IntPtr, access:cs.system.io.FileAccess, ownsHandle:Bool, bufferSize:Int, isAsync:Bool):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare, bufferSize:Int):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare, bufferSize:Int, useAsync:Bool):Void {})
	function new(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare, bufferSize:Int, options:cs.system.io.FileOptions):Void;
	/**
	 * Begins an asynchronous read operation. Consider using  instead.
	 * @param array The buffer to read data into.
	 * @param offset The byte offset in  at which to begin reading.
	 * @param numBytes The maximum number of bytes to read.
	 * @param callback The method to be called when the asynchronous read operation is
	 * completed.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous read request from other requests.
	 * @return An object that references the asynchronous read.
	 */
	function BeginRead(array:cs.NativeArray<cs.UInt8>, offset:Int, numBytes:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write operation. Consider using  instead.
	 * @param array The buffer containing data to write to the current stream.
	 * @param offset The zero-based byte offset in  at which to begin copying bytes to
	 * the current stream.
	 * @param numBytes The maximum number of bytes to write.
	 * @param callback The method to be called when the asynchronous write operation is
	 * completed.
	 * @param state A user-provided object that distinguishes this particular
	 * asynchronous write request from other requests.
	 * @return An object that references the asynchronous write.
	 */
	function BeginWrite(array:cs.NativeArray<cs.UInt8>, offset:Int, numBytes:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	function CopyToAsync(destination:cs.system.io.Stream, bufferSize:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Asynchronously releases the unmanaged resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Waits for the pending asynchronous read operation to complete. (Consider using 
	 * instead.)
	 * @param asyncResult The reference to the pending asynchronous request to wait
	 * for.
	 * @return The number of bytes read from the stream, between 0 and the number of
	 * bytes you requested. Streams only return 0 at the end of the stream, otherwise,
	 * they should block until at least 1 byte is available.
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends an asynchronous write operation and blocks until the I/O operation is
	 * complete. (Consider using  instead.)
	 * @param asyncResult The pending asynchronous I/O request.
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	@:overload(function():Void {})
	/** Clears buffers for this stream and causes any buffered data to be written to the file. */
	function Flush(flushToDisk:Bool):Void;
	/**
	 * Asynchronously clears all buffers for this stream, causes any buffered data to
	 * be written to the underlying device, and monitors cancellation requests.
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Prevents other processes from reading from or writing to the .
	 * @param position The beginning of the range to lock. The value of this parameter
	 * must be equal to or greater than zero (0).
	 * @param length The range to be locked.
	 */
	function Lock(position:haxe.Int64, length:haxe.Int64):Void;
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * Reads a block of bytes from the stream and writes the data in a given buffer.
	 * @param array When this method returns, contains the specified byte array with
	 * the values between  and ( +  - 1) replaced by the bytes read from the current
	 * source.
	 * @param offset The byte offset in  at which the read bytes will be placed.
	 * @param count The maximum number of bytes to read.
	 * @return The total number of bytes read into the buffer. This might be less than
	 * the number of bytes requested if that number of bytes are not currently
	 * available, or zero if the end of the stream is reached.
	 */
	function Read(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Asynchronously reads a sequence of bytes from the current file stream and writes
	 * them to a byte array beginning at a specified offset, advances the position
	 * within the file stream by the number of bytes read, and monitors cancellation
	 * requests.
	 * @param buffer The buffer to write the data into.
	 * @param offset The byte offset in  at which to begin writing data from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous read operation and wraps the
	 * total number of bytes read into the buffer. The result value can be less than
	 * the number of bytes requested if the number of bytes currently available is less
	 * than the requested number, or it can be 0 (zero) if the end of the stream has
	 * been reached.
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a byte from the file and advances the read position one byte.
	 * @return The byte, cast to an , or -1 if the end of the stream has been reached.
	 */
	function ReadByte():Int;
	/**
	 * Sets the current position of this stream to the given value.
	 * @param offset The point relative to  from which to begin seeking.
	 * @param origin Specifies the beginning, the end, or the current position as a
	 * reference point for , using a value of type .
	 * @return The new position in the stream.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of this stream to the given value.
	 * @param value The new length of the stream.
	 */
	function SetLength(value:haxe.Int64):Void;
	/**
	 * Allows access by other processes to all or part of a file that was previously
	 * locked.
	 * @param position The beginning of the range to unlock.
	 * @param length The range to be unlocked.
	 */
	function Unlock(position:haxe.Int64, length:haxe.Int64):Void;
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Writes a block of bytes to the file stream.
	 * @param array The buffer containing data to write to the stream.
	 * @param offset The zero-based byte offset in  from which to begin copying bytes
	 * to the stream.
	 * @param count The maximum number of bytes to write.
	 */
	function Write(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
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
	 * Writes a byte to the current position in the file stream.
	 * @param value A byte to write to the stream.
	 */
	function WriteByte(value:cs.UInt8):Void;
}
