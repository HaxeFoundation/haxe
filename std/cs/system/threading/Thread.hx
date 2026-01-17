package cs.system.threading;

@:native("System.Threading.Thread")
extern class Thread {
	static var CurrentThread(default, never):Thread;

	var Name:String;
	var IsBackground:Bool;
	var ManagedThreadId(default, never):Int;
	var IsAlive(default, never):Bool;

	@:overload function new(start:ThreadStart):Void;
	@:overload function new(start:ParameterizedThreadStart):Void;

	@:overload function Start():Void;
	@:overload function Start(parameter:Dynamic):Void;

	@:overload function Join():Void;
	@:overload function Join(millisecondsTimeout:Int):Bool;

	static function Sleep(millisecondsTimeout:Int):Void;
}
