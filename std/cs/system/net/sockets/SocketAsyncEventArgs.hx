package cs.system.net.sockets;

/** Represents an asynchronous socket operation. */
@:native("System.Net.Sockets.SocketAsyncEventArgs")
extern class SocketAsyncEventArgs extends cs.system.EventArgs {
	/**
	 * Gets or sets the socket to use or the socket created for accepting a connection
	 * with an asynchronous socket method.
	 * @return The  to use or the socket created for accepting a connection with an
	 * asynchronous socket method.
	 */
	var AcceptSocket(default, default):cs.system.net.sockets.Socket;
	/**
	 * Gets the data buffer to use with an asynchronous socket method.
	 * @return A  array that represents the data buffer to use with an asynchronous
	 * socket method.
	 */
	var Buffer(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets or sets an array of data buffers to use with an asynchronous socket method.
	 * @return An  that represents an array of data buffers to use with an asynchronous
	 * socket method.
	 */
	var BufferList(default, default):cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>;
	/**
	 * Gets the number of bytes transferred in the socket operation.
	 * @return An  that contains the number of bytes transferred in the socket
	 * operation.
	 */
	var BytesTransferred(default, never):Int;
	/**
	 * Gets the exception in the case of a connection failure when a  was used.
	 * @return An  that indicates the cause of the connection error when a  was
	 * specified for the  property.
	 */
	var ConnectByNameError(default, never):cs.system.Exception;
	/**
	 * The created and connected  object after successful completion of the  method.
	 * @return The connected  object.
	 */
	var ConnectSocket(default, never):cs.system.net.sockets.Socket;
	/**
	 * Gets the maximum amount of data, in bytes, to send or receive in an asynchronous
	 * operation.
	 * @return An  that contains the maximum amount of data, in bytes, to send or
	 * receive.
	 */
	var Count(default, never):Int;
	/**
	 * Gets or sets a value that specifies if socket can be reused after a disconnect
	 * operation.
	 * @return A  that specifies if socket can be reused after a disconnect operation.
	 */
	var DisconnectReuseSocket(default, default):Bool;
	/**
	 * Gets the type of socket operation most recently performed with this context
	 * object.
	 * @return A  instance that indicates the type of socket operation most recently
	 * performed with this context object.
	 */
	var LastOperation(default, never):cs.system.net.sockets.SocketAsyncOperation;
	var MemoryBuffer(default, never):cs.system.Memory<cs.UInt8>;
	/**
	 * Gets the offset, in bytes, into the data buffer referenced by the  property.
	 * @return An  that contains the offset, in bytes, into the data buffer referenced
	 * by the  property.
	 */
	var Offset(default, never):Int;
	/**
	 * Gets the IP address and interface of a received packet.
	 * @return An  instance that contains the destination IP address and interface of a
	 * received packet.
	 */
	var ReceiveMessageFromPacketInfo(default, never):cs.system.net.sockets.IPPacketInformation;
	/**
	 * Gets or sets the remote IP endpoint for an asynchronous operation.
	 * @return An  that represents the remote IP endpoint for an asynchronous
	 * operation.
	 */
	var RemoteEndPoint(default, default):cs.system.net.EndPoint;
	/**
	 * Gets or sets an array of buffers to be sent for an asynchronous operation used
	 * by the  method.
	 * @return An array of  objects that represent an array of buffers to be sent.
	 */
	var SendPacketsElements(default, default):cs.NativeArray<cs.system.net.sockets.SendPacketsElement>;
	/**
	 * Gets or sets a bitwise combination of  values for an asynchronous operation used
	 * by the  method.
	 * @return A  that contains a bitwise combination of values that are used with an
	 * asynchronous operation.
	 */
	var SendPacketsFlags(default, default):cs.system.net.sockets.TransmitFileOptions;
	/**
	 * Gets or sets the size, in bytes, of the data block used in the send operation.
	 * @return An  that contains the size, in bytes, of the data block used in the send
	 * operation.
	 */
	var SendPacketsSendSize(default, default):Int;
	/**
	 * Gets or sets the result of the asynchronous socket operation.
	 * @return A  that represents the result of the asynchronous socket operation.
	 */
	var SocketError(default, default):cs.system.net.sockets.SocketError;
	/**
	 * Gets the results of an asynchronous socket operation or sets the behavior of an
	 * asynchronous operation.
	 * @return A  that represents the results of an asynchronous socket operation.
	 */
	var SocketFlags(default, default):cs.system.net.sockets.SocketFlags;
	/**
	 * Gets or sets a user or application object associated with this asynchronous
	 * socket operation.
	 * @return An object that represents the user or application object associated with
	 * this asynchronous socket operation.
	 */
	var UserToken(default, default):Dynamic;
	function new():Void;
	/** Releases the unmanaged resources used by the  instance and optionally disposes of the managed resources. */
	function Dispose():Void;
	@:overload(function(buffer:cs.system.Memory<cs.UInt8>):Void {})
	@:overload(function(offset:Int, count:Int):Void {})
	/**
	 * Sets the data buffer to use with an asynchronous socket method.
	 * @param buffer The data buffer to use with an asynchronous socket method.
	 * @param offset The offset, in bytes, in the data buffer where the operation
	 * starts.
	 * @param count The maximum amount of data, in bytes, to send or receive in the
	 * buffer.
	 */
	function SetBuffer(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
}
