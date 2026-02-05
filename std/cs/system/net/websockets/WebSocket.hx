package cs.system.net.websockets;

/** The WebSocket class allows applications to send and receive data after the WebSocket upgrade has completed. */
@:native("System.Net.WebSockets.WebSocket")
extern class WebSocket {
	/**
	 * Gets the default WebSocket protocol keep-alive interval.
	 * @return The default WebSocket protocol keep-alive interval. The typical value
	 * for this interval is 30 seconds (as defined by the OS or the .NET platform). It
	 * is used to initialize  value.
	 */
	static var DefaultKeepAliveInterval(default, never):cs.system.TimeSpan;
	/**
	 * Indicates the reason why the remote endpoint initiated the close handshake.
	 * @return Returns .
	 */
	var CloseStatus(default, never):Null<cs.system.net.websockets.WebSocketCloseStatus>;
	/**
	 * Allows the remote endpoint to describe the reason why the connection was closed.
	 * @return Returns .
	 */
	var CloseStatusDescription(default, never):String;
	/**
	 * Returns the current state of the WebSocket connection.
	 * @return The current state of the WebSocket connection.
	 */
	var State(default, never):cs.system.net.websockets.WebSocketState;
	/**
	 * Gets the subprotocol that was negotiated during the opening handshake.
	 * @return The subprotocol that was negotiated during the opening handshake.
	 */
	var SubProtocol(default, never):String;
	/**
	 * Create client buffers to use with this  instance.
	 * @param receiveBufferSize The size, in bytes, of the client receive buffer.
	 * @param sendBufferSize The size, in bytes, of the send buffer.
	 * @return An array with the client buffers.
	 */
	static function CreateClientBuffer(receiveBufferSize:Int, sendBufferSize:Int):cs.system.ArraySegment<cs.UInt8>;
	/**
	 * Allows callers to create a client side WebSocket class which will use the WSPC
	 * for framing purposes.
	 * @param innerStream The connection to be used for IO operations.
	 * @param subProtocol The subprotocol accepted by the client.
	 * @param receiveBufferSize The size in bytes of the client WebSocket receive
	 * buffer.
	 * @param sendBufferSize The size in bytes of the client WebSocket send buffer.
	 * @param keepAliveInterval Determines how regularly a frame is sent over the
	 * connection as a keep-alive. Applies only when the connection is idle.
	 * @param useZeroMaskingKey Indicates whether a random key or a static key (just
	 * zeros) should be used for the WebSocket masking.
	 * @param internalBuffer Will be used as the internal buffer in the WPC. The size
	 * has to be at least 2 * ReceiveBufferSize + SendBufferSize + 256 + 20 (16 on
	 * 32-bit).
	 * @return Returns .
	 */
	static function CreateClientWebSocket(innerStream:cs.system.io.Stream, subProtocol:String, receiveBufferSize:Int, sendBufferSize:Int, keepAliveInterval:cs.system.TimeSpan, useZeroMaskingKey:Bool, internalBuffer:cs.system.ArraySegment<cs.UInt8>):cs.system.net.websockets.WebSocket;
	/**
	 * Creates a new  object that operates on the specified stream, which represents a
	 * web socket connection.
	 * @param stream The stream for the connection.
	 * @param isServer to indicate it's the server-side of the connection;  if it's the
	 * client-side.
	 * @param subProtocol The agreed upon sub-protocol that was used when creating the
	 * connection.
	 * @param keepAliveInterval The keep-alive interval to use, or  to disable
	 * keep-alives.
	 * @return The new web socket.
	 */
	static function CreateFromStream(stream:cs.system.io.Stream, isServer:Bool, subProtocol:String, keepAliveInterval:cs.system.TimeSpan):cs.system.net.websockets.WebSocket;
	/**
	 * Creates a WebSocket server buffer.
	 * @param receiveBufferSize The size, in bytes, of the desired buffer.
	 * @return Returns .
	 */
	static function CreateServerBuffer(receiveBufferSize:Int):cs.system.ArraySegment<cs.UInt8>;
	/** Allows callers to register prefixes for WebSocket requests (ws and wss). */
	static function RegisterPrefixes():Void;
	/** Aborts the WebSocket connection and cancels any pending IO operations. */
	function Abort():Void;
	/**
	 * Closes the WebSocket connection as an asynchronous operation using the close
	 * handshake defined in the WebSocket protocol specification section 7.
	 * @param closeStatus Indicates the reason for closing the WebSocket connection.
	 * @param statusDescription Specifies a human readable explanation as to why the
	 * connection is closed.
	 * @param cancellationToken The token that can be used to propagate notification
	 * that operations should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function CloseAsync(closeStatus:cs.system.net.websockets.WebSocketCloseStatus, statusDescription:String, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Initiates or completes the close handshake defined in the WebSocket protocol
	 * specification section 7.
	 * @param closeStatus Indicates the reason for closing the WebSocket connection.
	 * @param statusDescription Allows applications to specify a human readable
	 * explanation as to why the connection is closed.
	 * @param cancellationToken The token that can be used to propagate notification
	 * that operations should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function CloseOutputAsync(closeStatus:cs.system.net.websockets.WebSocketCloseStatus, statusDescription:String, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/** Used to clean up unmanaged resources for ASP.NET and self-hosted implementations. */
	function Dispose():Void;
	@:overload(function(buffer:cs.system.ArraySegment<cs.UInt8>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.websockets.WebSocketReceiveResult> {})
	/**
	 * Receives data from the  connection asynchronously.
	 * @param buffer References the application buffer that is the storage location for
	 * the received data.
	 * @param cancellationToken Propagates the notification that operations should be
	 * canceled.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  array containing the received data.
	 */
	function ReceiveAsync(buffer:cs.system.Memory<cs.UInt8>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<cs.system.net.websockets.ValueWebSocketReceiveResult>;
	@:overload(function(buffer:cs.system.ArraySegment<cs.UInt8>, messageType:cs.system.net.websockets.WebSocketMessageType, endOfMessage:Bool, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Sends data over the  connection asynchronously.
	 * @param buffer The buffer to be sent over the connection.
	 * @param messageType Indicates whether the application is sending a binary or text
	 * message.
	 * @param endOfMessage Indicates whether the data in "buffer" is the last part of a
	 * message.
	 * @param cancellationToken The token that propagates the notification that
	 * operations should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function SendAsync(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, messageType:cs.system.net.websockets.WebSocketMessageType, endOfMessage:Bool, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask;
}
