package cs.system.net.websockets;

/** Contains the list of possible WebSocket errors. */
@:native("System.Net.WebSockets.WebSocketError")
extern enum WebSocketError {
	ConnectionClosedPrematurely;
	Faulted;
	HeaderError;
	InvalidMessageType;
	InvalidState;
	NativeError;
	NotAWebSocket;
	Success;
	UnsupportedProtocol;
	UnsupportedVersion;
}
