package cs.system.io.isolatedstorage;

/** Exposes a file within isolated storage. */
@:native("System.IO.IsolatedStorage.IsolatedStorageFileStream")
extern class IsolatedStorageFileStream extends cs.system.io.FileStream {
	@:overload(function(path:String, mode:cs.system.io.FileMode):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, isf:cs.system.io.isolatedstorage.IsolatedStorageFile):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, isf:cs.system.io.isolatedstorage.IsolatedStorageFile):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare, bufferSize:Int):Void {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare, isf:cs.system.io.isolatedstorage.IsolatedStorageFile):Void {})
	function new(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare, bufferSize:Int, isf:cs.system.io.isolatedstorage.IsolatedStorageFile):Void;
	/**
	 * Begins an asynchronous read.
	 * @param buffer The buffer to read data into.
	 * @param offset The byte offset in  at which to begin reading.
	 * @param numBytes The maximum number of bytes to read.
	 * @param userCallback The method to call when the asynchronous read operation is
	 * completed. This parameter is optional.
	 * @param stateObject The status of the asynchronous read.
	 * @return An  object that represents the asynchronous read, which is possibly
	 * still pending. This  must be passed to this stream's  method to determine how
	 * many bytes were read. This can be done either by the same code that called  or
	 * in a callback passed to .
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, numBytes:Int, userCallback:cs.system.AsyncCallback, stateObject:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write.
	 * @param buffer The buffer to write data to.
	 * @param offset The byte offset in  at which to begin writing.
	 * @param numBytes The maximum number of bytes to write.
	 * @param userCallback The method to call when the asynchronous write operation is
	 * completed. This parameter is optional.
	 * @param stateObject The status of the asynchronous write.
	 * @return An  that represents the asynchronous write, which is possibly still
	 * pending. This  must be passed to this stream's  method to ensure that the write
	 * is complete, then frees resources appropriately. This can be done either by the
	 * same code that called  or in a callback passed to .
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, numBytes:Int, userCallback:cs.system.AsyncCallback, stateObject:Dynamic):cs.system.IAsyncResult;
	/**
	 * Asynchronously releases the unmanaged resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Ends a pending asynchronous read request.
	 * @param asyncResult The pending asynchronous request.
	 * @return The number of bytes read from the stream, between zero and the number of
	 * requested bytes. Streams will only return zero at the end of the stream.
	 * Otherwise, they will block until at least one byte is available.
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends an asynchronous write.
	 * @param asyncResult The pending asynchronous I/O request to end.
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	@:overload(function():Void {})
	/** Clears buffers for this stream and causes any buffered data to be written to the file. */
	function Flush(flushToDisk:Bool):Void;
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Prevents other processes from reading from or writing to the stream.
	 * @param position The starting position of the range to lock. The value of this
	 * parameter must be equal to or greater than 0 (zero).
	 * @param length The number of bytes to lock.
	 */
	function Lock(position:haxe.Int64, length:haxe.Int64):Void;
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * Copies bytes from the current buffered  object to a byte array.
	 * @param buffer The buffer to write the data into.
	 * @param offset The offset in the buffer at which to begin writing.
	 * @param count The maximum number of bytes to read.
	 * @return The total number of bytes read into the . This can be less than the
	 * number of bytes requested if that many bytes are not currently available, or
	 * zero if the end of the stream is reached.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	/**
	 * Reads a single byte from the  object in isolated storage.
	 * @return The 8-bit unsigned integer value read from the isolated storage file.
	 */
	function ReadByte():Int;
	/**
	 * Sets the current position of this  object to the specified value.
	 * @param offset The new position of the  object.
	 * @param origin One of the  values.
	 * @return The new position in the  object.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of this  object to the specified .
	 * @param value The new length of the  object.
	 */
	function SetLength(value:haxe.Int64):Void;
	/**
	 * Allows other processes to access all or part of a file that was previously
	 * locked.
	 * @param position The starting position of the range to unlock. The value of this
	 * parameter must be equal to or greater than 0 (zero).
	 * @param length The number of bytes to unlock.
	 */
	function Unlock(position:haxe.Int64, length:haxe.Int64):Void;
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Writes a block of bytes to the isolated storage file stream object using data
	 * read from a buffer consisting of a byte array.
	 * @param buffer The byte array from which to copy bytes to the current isolated
	 * storage file stream.
	 * @param offset The byte offset in  from which to begin.
	 * @param count The maximum number of bytes to write.
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
	function WriteAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Writes a single byte to the  object.
	 * @param value The byte value to write to the isolated storage file.
	 */
	function WriteByte(value:cs.UInt8):Void;
}
