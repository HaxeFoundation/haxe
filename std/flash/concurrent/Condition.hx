package flash.concurrent;

@:require(flash11_4) extern final class Condition {
	var mutex(default,never) : Mutex;
	function new(mutex : Mutex) : Void;
	function notify() : Void;
	function notifyAll() : Void;
	function wait(timeout : Float = -1) : Bool;
	static var isSupported(default,never) : Bool;
}
