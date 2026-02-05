package cs.system.net.websockets;

/** Represents well known WebSocket close codes as defined in section 11.7 of the WebSocket protocol spec. */
@:native("System.Net.WebSockets.WebSocketCloseStatus")
extern enum WebSocketCloseStatus {
	Empty;
	EndpointUnavailable;
	InternalServerError;
	InvalidMessageType;
	InvalidPayloadData;
	MandatoryExtension;
	MessageTooBig;
	NormalClosure;
	PolicyViolation;
	ProtocolError;
}
