package asys;

class Timer {
	public static function delay(f:() -> Void, timeMs:Int):Timer {
		var t = new Timer(timeMs);
		t.run = function() {
			t.stop();
			f();
		};
		return t;
	}

	public static function measure<T>(f:()->T, ?pos:haxe.PosInfos):T {
		var t0 = stamp();
		var r = f();
		haxe.Log.trace((stamp() - t0) + "s", pos);
		return r;
	}

	public static function stamp():Float {
		// TODO: libuv?
		return Sys.time();
	}

	public function new(timeMs:Int) {}

	public dynamic function run():Void {}

	extern public function stop():Void;

	extern public function ref():Void;

	extern public function unref():Void;
}
