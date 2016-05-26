package flash.system;

@:final @:require(flash11_4) extern class Worker extends flash.events.EventDispatcher {
	var isPrimordial(default,never) : Bool;
	var state(default,never) : WorkerState;
	function createMessageChannel(receiver : Worker) : MessageChannel;
	function getSharedProperty(key : String) : Dynamic;
	function setSharedProperty(key : String, value : Dynamic) : Void;
	function start() : Void;
	function terminate() : Bool;
	static var current(default,never) : Worker;
	static var isSupported(default,never) : Bool;
}
