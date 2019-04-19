package flash.net;

extern class NetConnection extends flash.events.EventDispatcher {
	var client : Dynamic;
	var connected(default,never) : Bool;
	var connectedProxyType(default,never) : String;
	@:require(flash10) var farID(default,never) : String;
	@:require(flash10) var farNonce(default,never) : String;
	@:require(flash10) var maxPeerConnections : UInt;
	@:require(flash10) var nearID(default,never) : String;
	@:require(flash10) var nearNonce(default,never) : String;
	var objectEncoding : UInt;
	@:require(flash10) var protocol(default,never) : String;
	var proxyType : String;
	@:require(flash10) var unconnectedPeerStreams(default,never) : Array<Dynamic>;
	var uri(default,never) : String;
	var usingTLS(default,never) : Bool;
	function new() : Void;
	function addHeader(operation : String, mustUnderstand : Bool = false, ?param : flash.utils.Object) : Void;
	function call(command : String, responder : Responder, restArgs : haxe.extern.Rest<Dynamic>) : Void;
	function close() : Void;
	function connect(command : String, restArgs : haxe.extern.Rest<Dynamic>) : Void;
	static var defaultObjectEncoding : UInt;
}
