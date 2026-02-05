package cs.system.net.websockets;

/** Represents well known WebSocket close codes as defined in section 11.7 of the WebSocket protocol spec. */
@:native("System.Net.WebSockets.WebSocketCloseStatus")
extern enum abstract WebSocketCloseStatus(Int) {
	var Empty = 1005;
	var EndpointUnavailable = 1001;
	var InternalServerError = 1011;
	var InvalidMessageType = 1003;
	var InvalidPayloadData = 1007;
	var MandatoryExtension = 1010;
	var MessageTooBig = 1009;
	var NormalClosure = 1000;
	var PolicyViolation = 1008;
	var ProtocolError = 1002;
}
