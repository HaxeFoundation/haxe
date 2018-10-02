package eval.vm;

extern class Mutex {
	function new():Void;
	function acquire():Void;
	function tryAcquire():Bool;
	function release():Void;
}