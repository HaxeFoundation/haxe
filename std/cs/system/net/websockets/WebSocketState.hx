package cs.system.net.websockets;

/** Defines the different states a WebSockets instance can be in. */
@:native("System.Net.WebSockets.WebSocketState")
extern enum abstract WebSocketState(Int) {
	var Aborted = 6;
	var Closed = 5;
	var CloseReceived = 4;
	var CloseSent = 3;
	var Connecting = 1;
	var None = 0;
	var Open = 2;
}
