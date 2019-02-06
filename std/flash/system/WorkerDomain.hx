package flash.system;

@:require(flash11_4) extern final class WorkerDomain {
	function new() : Void;
	function createWorker(swf : flash.utils.ByteArray, giveAppPrivileges : Bool = false) : Worker;
	function listWorkers() : flash.Vector<Worker>;
	static var current(default,never) : WorkerDomain;
	static var isSupported(default,never) : Bool;
}
