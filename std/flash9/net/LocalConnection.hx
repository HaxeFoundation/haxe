package flash.net;

extern class LocalConnection extends flash.events.EventDispatcher {
	function new() : Void;
	function allowDomain( ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	function allowInsecureDomain( ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	var client : Dynamic;
	function close() : Void;
	function connect(connectionName : String) : Void;
	var domain(default,null) : String;
	function send(connectionName : String, methodName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
}
