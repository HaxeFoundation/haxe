package flash.net;

extern class LocalConnection extends flash.events.EventDispatcher {
	function new() : Void;
	function allowDomain( /* ...arguments */) : Void;
	function allowInsecureDomain( /* ...arguments */) : Void;
	var client : Dynamic;
	function close() : Void;
	function connect(connectionName : String) : Void;
	var domain(default,null) : String;
	function send(connectionName : String, methodName : String /* ...arguments */) : Void;
}
