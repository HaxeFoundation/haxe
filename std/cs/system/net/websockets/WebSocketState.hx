package cs.system.net.websockets;

/** Defines the different states a WebSockets instance can be in. */
@:native("System.Net.WebSockets.WebSocketState")
extern enum WebSocketState {
	Aborted;
	Closed;
	CloseReceived;
	CloseSent;
	Connecting;
	None;
	Open;
}
