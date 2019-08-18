package flash.concurrent;

@:require(flash11_4) extern final class Mutex {
	function new() : Void;
	function lock() : Void;
	function tryLock() : Bool;
	function unlock() : Void;
	@:flash.property static var isSupported(get,never) : Bool;
	private static function get_isSupported() : Bool;
}
