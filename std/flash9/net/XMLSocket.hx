package flash.net;

extern class XMLSocket extends flash.events.EventDispatcher {
	function new(?host : String, ?port : Int) : Void;
	function close() : Void;
	function connect(host : String, port : Int) : Void;
	var connected(default,null) : Bool;
	function send(object : Dynamic) : Void;
}
