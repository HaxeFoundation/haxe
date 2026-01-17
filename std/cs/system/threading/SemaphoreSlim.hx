package cs.system.threading;

@:native("System.Threading.SemaphoreSlim")
extern class SemaphoreSlim {
	function new(initialCount:Int, maxCount:Int):Void;
	@:overload function Wait():Void;
	@:overload function Wait(millisecondsTimeout:Int):Bool;
	@:overload function Release():Int;
	@:overload function Release(releaseCount:Int):Int;
	var CurrentCount(default, never):Int;
}
