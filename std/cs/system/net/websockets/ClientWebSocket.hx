package cs.system.net.websockets;

/** Provides a client for connecting to WebSocket services. */
@:native("System.Net.WebSockets.ClientWebSocket")
extern class ClientWebSocket extends cs.system.net.websockets.WebSocket {
	/**
	 * Gets the WebSocket options for the  instance.
	 * @return The WebSocket options for the  instance.
	 */
	var Options(default, never):cs.system.net.websockets.ClientWebSocketOptions;
	function new():Void;
	/** Aborts the connection and cancels any pending IO operations. */
	function Abort():Void;
	/**
	 * Close the  instance as an asynchronous operation.
	 * @param closeStatus The WebSocket close status.
	 * @param statusDescription A description of the close status.
	 * @param cancellationToken A cancellation token used to propagate notification
	 * that this  operation should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function CloseAsync(closeStatus:cs.system.net.websockets.WebSocketCloseStatus, statusDescription:String, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Close the output for the  instance as an asynchronous operation.
	 * @param closeStatus The WebSocket close status.
	 * @param statusDescription A description of the close status.
	 * @param cancellationToken A cancellation token used to propagate notification
	 * that this  operation should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function CloseOutputAsync(closeStatus:cs.system.net.websockets.WebSocketCloseStatus, statusDescription:String, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Connect to a WebSocket server as an asynchronous operation.
	 * @param uri The URI of the WebSocket server to connect to.
	 * @param cancellationToken A cancellation token used to propagate notification
	 * that the  operation should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function ConnectAsync(uri:cs.system.Uri, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/** Releases the unmanaged resources used by the  instance. */
	function Dispose():Void;
	@:overload(function(buffer:cs.system.ArraySegment<cs.UInt8>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.websockets.WebSocketReceiveResult> {})
	/**
	 * Receives data on  as an asynchronous operation.
	 * @param buffer The buffer to receive the response.
	 * @param cancellationToken A cancellation token used to propagate notification
	 * that this operation should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function ReceiveAsync(buffer:cs.system.Memory<cs.UInt8>, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<cs.system.net.websockets.ValueWebSocketReceiveResult>;
	@:overload(function(buffer:cs.system.ArraySegment<cs.UInt8>, messageType:cs.system.net.websockets.WebSocketMessageType, endOfMessage:Bool, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Sends data on  as an asynchronous operation.
	 * @param buffer The buffer containing the message to be sent.
	 * @param messageType One of the enumeration values that specifies whether the
	 * buffer is clear text or in a binary format.
	 * @param endOfMessage to indicate this is the final asynchronous send; otherwise,
	 * .
	 * @param cancellationToken A cancellation token used to propagate notification
	 * that this operation should be canceled.
	 * @return The task object representing the asynchronous operation.
	 */
	function SendAsync(buffer:cs.system.ReadOnlyMemory<cs.UInt8>, messageType:cs.system.net.websockets.WebSocketMessageType, endOfMessage:Bool, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask;
}
