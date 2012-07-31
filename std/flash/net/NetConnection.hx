package flash.net;

extern class NetConnection extends flash.events.EventDispatcher {
	var client : Dynamic;
	var connected(default,null) : Bool;
	var connectedProxyType(default,null) : String;
	@:require(flash10) var farID(default,null) : String;
	@:require(flash10) var farNonce(default,null) : String;
	@:require(flash10) var maxPeerConnections : UInt;
	@:require(flash10) var nearID(default,null) : String;
	@:require(flash10) var nearNonce(default,null) : String;
	var objectEncoding : UInt;
	@:require(flash10) var protocol(default,null) : String;
	var proxyType : String;
	@:require(flash10) var unconnectedPeerStreams(default,null) : Array<Dynamic>;
	var uri(default,null) : String;
	var usingTLS(default,null) : Bool;
	function new() : Void;
	function addHeader(operation : String, mustUnderstand : Bool = false, ?param : flash.utils.Object) : Void;
	function call(command : String, responder : Responder, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function close() : Void;
	function connect(command : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	static var defaultObjectEncoding : UInt;
}
