package flash.net;

extern class XMLSocket extends flash.events.EventDispatcher {
	var connected(default,null) : Bool;
	function new(?host : String, port : Int = 0) : Void;
	function close() : Void;
	function connect(host : String, port : Int) : Void;
	function send(object : Dynamic) : Void;
}
