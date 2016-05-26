package flash.net;

extern class LocalConnection extends flash.events.EventDispatcher {
	var client : Dynamic;
	var domain(default,never) : String;
	@:require(flash10_1) var isPerUser : Bool;
	function new() : Void;
	function allowDomain(?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function allowInsecureDomain(?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function close() : Void;
	function connect(connectionName : String) : Void;
	function send(connectionName : String, methodName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	@:require(flash10_1) static var isSupported(default,never) : Bool;
}
