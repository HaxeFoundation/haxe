package flash.system;

@:native("flash.system.WorkerState") extern enum abstract WorkerState(String) {
	var NEW;
	var RUNNING;
	var TERMINATED;
}
