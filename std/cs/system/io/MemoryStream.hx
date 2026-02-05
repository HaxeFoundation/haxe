package cs.system.io;

/** Creates a stream whose backing store is memory. */
@:native("System.IO.MemoryStream")
extern class MemoryStream extends cs.system.io.Stream {
	/**
	 * Gets or sets the number of bytes allocated for this stream.
	 * @return The length of the usable portion of the buffer for the stream.
	 */
	var Capacity(default, default):Int;
	@:overload(function():Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(capacity:Int):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, writable:Bool):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Void {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int, writable:Bool):Void {})
	function new(buffer:cs.NativeArray<cs.UInt8>, index:Int, count:Int, writable:Bool, publiclyVisible:Bool):Void;
	/**
	 * @param buffer 
	 * @param offset 
	 * @param count 
	 * @param callback 
	 * @param state 
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * @param buffer 
	 * @param offset 
	 * @param count 
	 * @param callback 
	 * @param state 
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * @param destination 
	 * @param bufferSize 
	 */
	function CopyTo(destination:cs.system.io.Stream, bufferSize:Int):Void;
	/**
	 * Asynchronously reads all the bytes from the current stream and writes them to
	 * another stream, using a specified buffer size and cancellation token.
	 * @param destination The stream to which the contents of the current stream will
	 * be copied.
	 * @param bufferSize The size, in bytes, of the buffer. This value must be greater
	 * than zero.
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous copy operation.
	 */
	function CopyToAsync(destination:cs.system.io.Stream, bufferSize:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/** @param asyncResult  */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/** @param asyncResult  */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** Overrides the  method so that no action is performed. */
	function Flush():Void;
	/**
	 * Asynchronously clears all buffers for this stream, and monitors cancellation
	 * requests.
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous flush operation.
	 */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Returns the array of unsigned bytes from which this stream was created.
	 * @return The byte array from which this stream was created, or the underlying
	 * array if a byte array was not provided to the  constructor during construction
	 * of the current instance.
	 */
	function GetBuffer():cs.NativeArray<cs.UInt8>;
	@:overload(function(destination:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * Reads a block of bytes from the current stream and writes the data to a buffer.
	 * @param buffer When this method returns, contains the specified byte array with
	 * the values between  and ( +  - 1) replaced by the characters read from the
	 * current stream.
	 * @param offset The zero-based byte offset in  at which to begin storing data from
	 * the current stream.
	 * @param count The maximum number of bytes to read.
	 * @return The total number of bytes written into the buffer. This can be less than
	 * the number of bytes requested if that number of bytes are not currently
	 * available, or zero if the end of the stream is reached before any bytes are
	 * read.
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	@:overload(function(destination:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Asynchronously reads a sequence of bytes from the current stream, advances the
	 * position within the stream by the number of bytes read, and monitors
	 * cancellation requests.
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
	 * Reads a byte from the current stream.
	 * @return The byte cast to a , or -1 if the end of the stream has been reached.
	 */
	function ReadByte():Int;
	/**
	 * Sets the position within the current stream to the specified value.
	 * @param offset The new position within the stream. This is relative to the 
	 * parameter, and can be positive or negative.
	 * @param loc A value of type , which acts as the seek reference point.
	 * @return The new position within the stream, calculated by combining the initial
	 * reference point and the offset.
	 */
	function Seek(offset:haxe.Int64, loc:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of the current stream to the specified value.
	 * @param value The value at which to set the length.
	 */
	function SetLength(value:haxe.Int64):Void;
	/**
	 * Writes the stream contents to a byte array, regardless of the  property.
	 * @return A new byte array.
	 */
	function ToArray():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the array of unsigned bytes from which this stream was created. The
	 * return value indicates whether the conversion succeeded.
	 * @param buffer The byte array segment from which this stream was created.
	 * @return if the conversion was successful; otherwise, .
	 */
	function TryGetBuffer(buffer:cs.Ref<cs.system.ArraySegment<cs.UInt8>>):Bool;
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Writes a block of bytes to the current stream using data read from a buffer.
	 * @param buffer The buffer to write data from.
	 * @param offset The zero-based byte offset in  at which to begin copying bytes to
	 * the current stream.
	 * @param count The maximum number of bytes to write.
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(source:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
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
	 * Writes a byte to the current stream at the current position.
	 * @param value The byte to write.
	 */
	function WriteByte(value:cs.UInt8):Void;
	/**
	 * Writes the entire contents of this memory stream to another stream.
	 * @param stream The stream to write this memory stream to.
	 */
	function WriteTo(stream:cs.system.io.Stream):Void;
}
