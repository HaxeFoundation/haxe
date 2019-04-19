package flash.net;

extern class LocalConnection extends flash.events.EventDispatcher {
	var client : Dynamic;
	var domain(default,never) : String;
	@:require(flash10_1) var isPerUser : Bool;
	function new() : Void;
	function allowDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	function allowInsecureDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	function close() : Void;
	function connect(connectionName : String) : Void;
	function send(connectionName : String, methodName : String, restArgs : haxe.extern.Rest<Dynamic>) : Void;
	@:require(flash10_1) static var isSupported(default,never) : Bool;
}
