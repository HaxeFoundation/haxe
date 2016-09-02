package flash.concurrent;

@:final @:require(flash11_4) extern class Mutex {
	function new() : Void;
	function lock() : Void;
	function tryLock() : Bool;
	function unlock() : Void;
	static var isSupported(default,never) : Bool;
}
