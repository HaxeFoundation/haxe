package eval.uv;

extern class Timer {
	function new(timeMs:Int, cb:Void->Void);
	function close(cb:haxe.async.Callback<haxe.NoData>):Void;
	function ref():Void;
	function unref():Void;
}
