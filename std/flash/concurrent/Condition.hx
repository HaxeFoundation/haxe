package flash.concurrent;

@:require(flash11_4) extern final class Condition {
	@:flash.property var mutex(get,never) : Mutex;
	function new(mutex : Mutex) : Void;
	private function get_mutex() : Mutex;
	function notify() : Void;
	function notifyAll() : Void;
	function wait(timeout : Float = -1) : Bool;
	@:flash.property static var isSupported(get,never) : Bool;
	private static function get_isSupported() : Bool;
}
