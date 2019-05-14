package flash.net;

extern class LocalConnection extends flash.events.EventDispatcher {
	@:flash.property var client(get,set) : Dynamic;
	@:flash.property var domain(get,never) : String;
	@:flash.property @:require(flash10_1) var isPerUser(get,set) : Bool;
	function new() : Void;
	function allowDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	function allowInsecureDomain(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	function close() : Void;
	function connect(connectionName : String) : Void;
	private function get_client() : Dynamic;
	private function get_domain() : String;
	private function get_isPerUser() : Bool;
	function send(connectionName : String, methodName : String, restArgs : haxe.extern.Rest<Dynamic>) : Void;
	private function set_client(value : Dynamic) : Dynamic;
	private function set_isPerUser(value : Bool) : Bool;
	@:flash.property @:require(flash10_1) static var isSupported(get,never) : Bool;
	private static function get_isSupported() : Bool;
}
