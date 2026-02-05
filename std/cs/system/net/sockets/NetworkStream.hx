package cs.system.net.sockets;

/** Provides the underlying stream of data for network access. */
@:native("System.Net.Sockets.NetworkStream")
extern class NetworkStream extends cs.system.io.Stream {
	/**
	 * Gets a value that indicates whether data is available on the  to be read.
	 * @return if data is available on the stream to be read; otherwise, .
	 */
	var DataAvailable(default, never):Bool;
	/**
	 * Gets or sets a value that indicates whether the  can be read.
	 * @return to indicate that the  can be read; otherwise, . The default value is .
	 */
	var Readable(default, default):Bool;
	/**
	 * Gets the underlying .
	 * @return A  that represents the underlying network connection.
	 */
	var Socket(default, never):cs.system.net.sockets.Socket;
	/**
	 * Gets a value that indicates whether the  is writable.
	 * @return if data can be written to the stream; otherwise, . The default value is
	 * .
	 */
	var Writeable(default, default):Bool;
	@:overload(function(socket:cs.system.net.sockets.Socket):Void {})
	@:overload(function(socket:cs.system.net.sockets.Socket, ownsSocket:Bool):Void {})
	@:overload(function(socket:cs.system.net.sockets.Socket, access:cs.system.io.FileAccess):Void {})
	function new(socket:cs.system.net.sockets.Socket, access:cs.system.io.FileAccess, ownsSocket:Bool):Void;
	/**
	 * Begins an asynchronous read from the .
	 * @param buffer An array of type  that is the location in memory to store data
	 * read from the .
	 * @param offset The location in  to begin storing the data.
	 * @param size The number of bytes to read from the .
	 * @param callback The  delegate that is executed when  completes.
	 * @param state An object that contains any additional user-defined data.
	 * @return An  that represents the asynchronous call.
	 */
	function BeginRead(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous write to a stream.
	 * @param buffer An array of type  that contains the data to write to the .
	 * @param offset The location in  to begin sending the data.
	 * @param size The number of bytes to write to the .
	 * @param callback The  delegate that is executed when  completes.
	 * @param state An object that contains any additional user-defined data.
	 * @return An  that represents the asynchronous call.
	 */
	function BeginWrite(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Closes the  after waiting the specified time to allow data to be sent.
	 * @param timeout A 32-bit signed integer that specifies the number of milliseconds
	 * to wait to send any remaining data before closing.
	 */
	function Close(timeout:Int):Void;
	/**
	 * Handles the end of an asynchronous read.
	 * @param asyncResult An  that represents an asynchronous call.
	 * @return The number of bytes read from the .
	 */
	function EndRead(asyncResult:cs.system.IAsyncResult):Int;
	/**
	 * Handles the end of an asynchronous write.
	 * @param asyncResult The  that represents the asynchronous call.
	 */
	function EndWrite(asyncResult:cs.system.IAsyncResult):Void;
	/** Flushes data from the stream. This method is reserved for future use. */
	function Flush():Void;
	/**
	 * Flushes data from the stream as an asynchronous operation.
	 * @param cancellationToken A cancellation token used to propagate notification
	 * that this  operation should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function FlushAsync(cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	/**
	 * Reads data from the  and stores it to a byte array.
	 * @param buffer An array of type  that is the location in memory to store data
	 * read from the .
	 * @param offset The location in  to begin storing the data to.
	 * @param size The number of bytes to read from the .
	 * @return The number of bytes read from the .
	 */
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int):Int;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int> {})
	/**
	 * Reads data from the  and stores it to a specified range of a byte array as an
	 * asynchronous operation.
	 * @param buffer The buffer to write the data into.
	 * @param offset The location in  to begin storing the data to.
	 * @param size The number of bytes to read from the .
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous read operation. The value of its
	 * property contains the total number of bytes read into .
	 */
	function ReadAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<Int>;
	function ReadByte():Int;
	/**
	 * Sets the current position of the stream to the given value. This method is not
	 * currently supported and always throws a .
	 * @param offset This parameter is not used.
	 * @param origin This parameter is not used.
	 * @return The position in the stream.
	 */
	function Seek(offset:haxe.Int64, origin:cs.system.io.SeekOrigin):haxe.Int64;
	/**
	 * Sets the length of the stream. This method always throws a .
	 * @param value This parameter is not used.
	 */
	function SetLength(value:haxe.Int64):Void;
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Writes data to the  from a specified range of a byte array.
	 * @param buffer An array of type  that contains the data to write to the .
	 * @param offset The location in  from which to start writing data.
	 * @param size The number of bytes to write to the .
	 */
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int):Void;
	@:overload(function(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask {})
	/**
	 * Writes data to the  from the specified range of a byte array as an asynchronous
	 * operation.
	 * @param buffer A byte array that contains the data to write to the .
	 * @param offset The location in  from which to start writing data.
	 * @param size The number of bytes to write to the .
	 * @param cancellationToken The token to monitor for cancellation requests.
	 * @return A task that represents the asynchronous write operation.
	 */
	function WriteAsync(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	function WriteByte(value:cs.UInt8):Void;
}
