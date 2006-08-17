package flash.net;

extern class NetConnection extends flash.events.EventDispatcher {
	function new() : Void;
	function addHeader(operation : String, ?mustUnderstand : Bool, ?param : Dynamic) : Void;
	function call(command : String, responder : flash.net.Responder, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	var client : Dynamic;
	function close() : Void;
	function connect(command : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	var connected(default,null) : Bool;
	var connectedProxyType(default,null) : String;
	var objectEncoding : UInt;
	var proxyType : String;
	var uri(default,null) : String;
	var usingTLS(default,null) : Bool;
	private function invoke(index : UInt, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	private function invokeWithArgsArray(index : UInt, args : Array<Dynamic>) : Void;
	static var defaultObjectEncoding : UInt;
	private static var kAddHeader : UInt;
	private static var kCall : UInt;
	private static var kClose : UInt;
	private static var kConnect : UInt;
	private static var kGetConnectedProxyType : UInt;
	private static var kGetUsingTLS : UInt;
}
