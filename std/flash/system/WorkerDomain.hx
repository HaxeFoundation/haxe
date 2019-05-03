package flash.system;

@:require(flash11_4) extern final class WorkerDomain {
	function new() : Void;
	function createWorker(swf : flash.utils.ByteArray, giveAppPrivileges : Bool = false) : Worker;
	function listWorkers() : flash.Vector<Worker>;
	static var current(get,never) : WorkerDomain;
	static var isSupported(get,never) : Bool;
	private static function get_current() : WorkerDomain;
	private static function get_isSupported() : Bool;
}
