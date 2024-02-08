package python.lib.threading;

@:pythonImport("threading", "Condition")
extern class Condition {
	function new(?lock:haxe.extern.EitherType<Lock, RLock>):Void;
	function acquire(?blocking:Bool, ?timeout:Float):Bool;
	function release():Void;
	function wait(?timeout:Float):Bool;
	function wait_for(predicate:()->Bool, ?timeout:Float):Bool;
	function notify(n:Int = 1):Void;
	function notify_all():Void;
}
