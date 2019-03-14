package eval.vm;

extern class Lock {
	function new():Void;
	function release():Void;
	function wait(?timeout:Float):Bool;
}