package flash.net;

extern class XMLSocket extends flash.events.EventDispatcher {
	@:flash.property var connected(get,never) : Bool;
	@:flash.property @:require(flash10) var timeout(get,set) : Int;
	function new(?host : String, port : Int = 0) : Void;
	function close() : Void;
	function connect(host : String, port : Int) : Void;
	private function get_connected() : Bool;
	private function get_timeout() : Int;
	function send(object : Dynamic) : Void;
	private function set_timeout(value : Int) : Int;
}
