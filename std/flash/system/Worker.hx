package flash.system;

@:final @:require(flash11_4) extern class Worker extends flash.events.EventDispatcher {
	var isPrimordial(default,null) : Bool;
	var state(default,null) : WorkerState;
	function createMessageChannel(receiver : Worker) : MessageChannel;
	function getSharedProperty(key : String) : Dynamic;
	function setSharedProperty(key : String, value : Dynamic) : Void;
	function start() : Void;
	function terminate() : Bool;
	static var current(default,null) : Worker;
	static var isSupported(default,null) : Bool;
}
