package cs.system.io.compression;

/** Provides methods and properties for compressing and decompressing streams by using the Deflate algorithm. */
@:native("System.IO.Compression.DeflateStream")
extern class DeflateStream extends cs.system.io.Stream {
	/**
	 * Gets a reference to the underlying stream.
	 * @return A stream object that represents the underlying stream.
	 */
	var BaseStream(default, never):cs.system.io.Stream;
	@:overload(function(stream:cs.system.io.Stream, compressionLevel:cs.system.io.compression.CompressionLevel):Void {})
	@:overload(function(stream:cs.system.io.Stream, mode:cs.system.io.compression.CompressionMode):Void {})
	@:overload(function(stream:cs.system.io.Stream, compressionLevel:cs.system.io.compression.CompressionLevel, leaveOpen:Bool):Void {})
	function new(stream:cs.system.io.Stream, mode:cs.system.io.compression.CompressionMode, leaveOpen:Bool):Void;
	/**
	 * Begins an asynchronous read operation. (Consider using the  method instead.)
	 * @param buffer The byte array to read the data into.
	 * @param offset The byte offset in  at which to begin reading data from the
	 * stream.
	 * @param count The maximum number of bytes to read.
	 * @param asyncCallback An optional asynchronous callback, to be called when the
	 * read operation is complete.
	 * @param asyncState A user-provided object that distinguishes this particular
	 * asynchronous read request from other requests.
	 * @return An  object that represents the asynchronous read operation, which could
	 * still be pending.
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write operation. (Consider using the  method instead.)
	 * @param array The buffer to write data from.
	 * @param offset The byte offset in  to begin writing from.
	 * @param count The maximum number of bytes to write.
	 * @param asyncCallback An optional asynchronous callback, to be called when the
	 * write operation is complete.
	 * @param asyncState A user-provided object that distinguishes this particular
	 * asynchronous write request from other requests.
	 * @return An object that represents the asynchronous write operation, which could
	 * still be pending.
	 */
	function BeginWrite(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, asyncCallback:cs.system.AsyncCallback, asyncState:Dynamic):cs.system.IAsyncResult;
	function CopyTo(destination:cs.system.io.Stream, bufferSize:Int):Void;
	function CopyToAsync(destination:cs.system.io.Stream, bufferSize:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Asynchronously releases the unmanaged resources used by the .
	 * @return A task that represents the asynchronous dispose operation.
	 */
	function DisposeAsync():cs.system.threading.tasks.ValueTask;
	/**
	 * Waits for the pending asynchronous read to complete. (Consider using the  method
	 * instead.)
	 * @param asyncResult The reference to the pending asynchronous request to finish.
	 * @return The number of bytes read from the stream, between 0 (zero) and the
	 * number of bytes you requested.  returns 0 only at the end of the stream;
	 * otherwise, it blocks until at least one byte is available.
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Ends an asynchronous write operation. (Consider using the  method instead.)
	 * @param asyncResult A reference to the outstanding asynchronous I/O request.
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** The current implementation of this method has no functionality. */
	function Flush():Void;
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * Reads a number of decompressed bytes into the specified byte array.
	 * @param array The array to store decompressed bytes.
	 * @param offset The byte offset in  at which the read bytes will be placed.
	 * @param count The maximum number of decompressed bytes to read.
	 * @return The number of bytes that were read into the byte array.
	 */
	function Read(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Asynchronously reads a sequence of bytes from the current Deflate stream, writes
	 * them to a byte array, advances the position within the Deflate stream by the
	 * number of bytes read, and monitors cancellation requests.
	 * @param array The buffer to write the data into.
	 * @param offset The byte offset in  at which to begin writing data from the
	 * Deflate stream.
	 * @param count The maximum number of bytes to read.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous read operation, which wraps the
	 * total number of bytes read into the . The result value can be less than the
	 * number of bytes requested if the number of bytes currently available is less
	 * than the requested number, or it can be 0 (zero) if the end of the Deflate
	 * stream has been reached.
	 */
	function ReadAsync(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	function ReadByte():Int;
	/**
	 * This operation is not supported and always throws a .
	 * @param offset The location in the stream.
	 * @param origin One of the  values.
	 * @return A long value.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * This operation is not supported and always throws a .
	 * @param value The length of the stream.
	 */
	function SetLength(value:haxe.Int64):Void;
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Writes compressed bytes to the underlying stream from the specified byte array.
	 * @param array The buffer that contains the data to compress.
	 * @param offset The byte offset in  from which the bytes will be read.
	 * @param count The maximum number of bytes to write.
	 */
	function Write(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
	/**
	 * Asynchronously writes compressed bytes to the underlying Deflate stream from the
	 * specified byte array.
	 * @param array The buffer that contains the data to compress.
	 * @param offset The zero-based byte offset in  from which to begin copying bytes
	 * to the Deflate stream.
	 * @param count The maximum number of bytes to write.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(array:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
