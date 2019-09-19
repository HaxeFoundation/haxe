package asys;

private typedef Native =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Timer;
	#elseif hl
	hl.uv.Timer;
	#elseif neko
	neko.uv.Timer;
	#else
	#error "timer not supported on this platform"
	#end

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

	var native:Native;

	public function new(timeMs:Int) {
		native = new Native(timeMs, () -> run());
	}

	public dynamic function run():Void {}

	public function stop():Void {
		native.close((err) -> {});
	}

	public function ref():Void {
		native.ref();
	}

	public function unref():Void {
		native.unref();
	}
}
