package flash.net;

extern class NetConnection extends flash.events.EventDispatcher {
	var client : Dynamic;
	var connected(default,null) : Bool;
	var connectedProxyType(default,null) : String;
	var objectEncoding : UInt;
	var proxyType : String;
	var uri(default,null) : String;
	var usingTLS(default,null) : Bool;
	function new() : Void;
	function addHeader(operation : String, mustUnderstand : Bool = false, ?param : Dynamic) : Void;
	function call(command : String, responder : Responder, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function close() : Void;
	function connect(command : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	static var defaultObjectEncoding : UInt;
}
