package cs.system.threading;

@:native("System.Threading.SemaphoreSlim")
extern class SemaphoreSlim {
	function new(initialCount:Int, maxCount:Int):Void;
	function Wait():Void;
	@:overload(function(timeout:Int):Bool {})
	function Wait(timeout:cs.system.TimeSpan):Bool;
	function Release():Int;
	@:overload(function(releaseCount:Int):Int {})
	function Release():Int;
	var CurrentCount(default, never):Int;
}
