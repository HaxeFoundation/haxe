package flash.concurrent;

@:final @:require(flash11_4) extern class Condition {
	var mutex(default,null) : Mutex;
	function new(mutex : Mutex) : Void;
	function notify() : Void;
	function notifyAll() : Void;
	function wait(timeout : Float = -1) : Bool;
	static var isSupported(default,null) : Bool;
}
