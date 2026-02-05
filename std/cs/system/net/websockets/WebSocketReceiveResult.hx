package cs.system.net.websockets;

/** An instance of this class represents the result of performing a single ReceiveAsync operation on a WebSocket. */
@:native("System.Net.WebSockets.WebSocketReceiveResult")
extern class WebSocketReceiveResult {
	/**
	 * Indicates the reason why the remote endpoint initiated the close handshake.
	 * @return Returns .
	 */
	var CloseStatus(default, never):Null<cs.system.net.websockets.WebSocketCloseStatus>;
	/**
	 * Returns the optional description that describes why the close handshake has been
	 * initiated by the remote endpoint.
	 * @return Returns .
	 */
	var CloseStatusDescription(default, never):String;
	/**
	 * Indicates the number of bytes that the WebSocket received.
	 * @return Returns .
	 */
	var Count(default, never):Int;
	/**
	 * Indicates whether the message has been received completely.
	 * @return Returns .
	 */
	var EndOfMessage(default, never):Bool;
	/**
	 * Indicates whether the current message is a UTF-8 message or a binary message.
	 * @return Returns .
	 */
	var MessageType(default, never):cs.system.net.websockets.WebSocketMessageType;
	@:overload(function(count:Int, messageType:cs.system.net.websockets.WebSocketMessageType, endOfMessage:Bool):Void {})
	function new(count:Int, messageType:cs.system.net.websockets.WebSocketMessageType, endOfMessage:Bool, closeStatus:Null<cs.system.net.websockets.WebSocketCloseStatus>, closeStatusDescription:String):Void;
}
