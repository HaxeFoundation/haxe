package cs.system.threading;

@:native("System.Threading.Thread")
extern class Thread {
	static var CurrentThread(default, never):Thread;

	var Name:String;
	var IsBackground:Bool;
	var ManagedThreadId(default, never):Int;
	var IsAlive(default, never):Bool;

	@:overload(function(start:ThreadStart):Void {})
	function new(start:ParameterizedThreadStart):Void;

	function Start():Void;
	@:overload(function(parameter:Dynamic):Void {})
	function Start():Void;

	function Join():Void;
	@:overload(function(millisecondsTimeout:Int):Bool {})
	function Join():Void;

	static function Sleep(millisecondsTimeout:Int):Void;
}

@:native("System.Threading.ThreadStart")
extern class ThreadStart {
	function new(callback:() -> Void):Void;
}

@:native("System.Threading.ParameterizedThreadStart")
extern class ParameterizedThreadStart {
	function new(callback:(Dynamic) -> Void):Void;
}
