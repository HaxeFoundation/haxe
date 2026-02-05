package cs.system.net.websockets;

/** Indicates the message type. */
@:native("System.Net.WebSockets.WebSocketMessageType")
extern enum abstract WebSocketMessageType(Int) {
	var Binary = 1;
	var Close = 2;
	var Text = 0;
}
