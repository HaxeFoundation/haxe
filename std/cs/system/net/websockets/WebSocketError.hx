package cs.system.net.websockets;

/** Contains the list of possible WebSocket errors. */
@:native("System.Net.WebSockets.WebSocketError")
extern enum abstract WebSocketError(Int) {
	var ConnectionClosedPrematurely = 8;
	var Faulted = 2;
	var HeaderError = 7;
	var InvalidMessageType = 1;
	var InvalidState = 9;
	var NativeError = 3;
	var NotAWebSocket = 4;
	var Success = 0;
	var UnsupportedProtocol = 6;
	var UnsupportedVersion = 5;
}
