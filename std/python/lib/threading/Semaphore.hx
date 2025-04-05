package python.lib.threading;

@:noDoc
@:pythonImport("threading", "Semaphore")
extern class Semaphore {
	function new(value:Int);
	function acquire(blocking:Bool = true, ?timeout:Float):Bool;
	function release(n:Int = 1):Void;
}