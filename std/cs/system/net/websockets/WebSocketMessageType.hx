package cs.system.net.websockets;

/** Indicates the message type. */
@:native("System.Net.WebSockets.WebSocketMessageType")
extern enum WebSocketMessageType {
	Binary;
	Close;
	Text;
}
