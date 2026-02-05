package cs.system.net.websockets;

/** Represents the result of performing a single  operation on a . */
@:native("System.Net.WebSockets.ValueWebSocketReceiveResult")
extern class ValueWebSocketReceiveResult extends cs.system.ValueType {
	var Count(default, never):Int;
	var EndOfMessage(default, never):Bool;
	var MessageType(default, never):cs.system.net.websockets.WebSocketMessageType;
	function new(count:Int, messageType:cs.system.net.websockets.WebSocketMessageType, endOfMessage:Bool):Void;
}
