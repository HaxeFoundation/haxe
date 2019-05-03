package flash.system;

@:require(flash11_4) extern final class Worker extends flash.events.EventDispatcher {
	var isPrimordial(get,never) : Bool;
	var state(get,never) : WorkerState;
	function createMessageChannel(receiver : Worker) : MessageChannel;
	function getSharedProperty(key : String) : Dynamic;
	private function get_isPrimordial() : Bool;
	private function get_state() : WorkerState;
	function setSharedProperty(key : String, value : Dynamic) : Void;
	function start() : Void;
	function terminate() : Bool;
	static var current(get,never) : Worker;
	static var isSupported(get,never) : Bool;
	private static function get_current() : Worker;
	private static function get_isSupported() : Bool;
}
