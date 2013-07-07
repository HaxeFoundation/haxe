package flash.system;

@:final @:require(flash11_4) extern class WorkerDomain {
	function new() : Void;
	function createWorker(swf : flash.utils.ByteArray, giveAppPrivileges : Bool = false) : Worker;
	function listWorkers() : flash.Vector<Worker>;
	static var current(default,null) : WorkerDomain;
	static var isSupported(default,null) : Bool;
}
