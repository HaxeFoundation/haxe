package unit.issues;

private abstract Bug(Float) from Float to Float {
	public inline function new(v:Float) {
		this = v;
	}

	@:to public inline function toInt():Int {
		return Math.floor(this);
	}

	@:to public function toDate():Date {
		return new Date(Std.int(this), 1, 1, 0, 0, 0);
	}

	@:from public static function fromDate(d:Date):Bug {
		return d != null ? d.getFullYear() : 0.0;
	}

	@:from public static inline function fromInt(v:Int):Bug
		return new Bug(v);
}

private abstract IDispatch(Dynamic) to Dynamic {
	public inline function new(v)
		this = v;
}

class Issue4711 extends unit.Test {
	function test() {
		var disp = new IDispatch(123);
		var t = new Bug(1);
		// must not go through fromDate just because it is the first @:from field
		t = disp;
		eq(123, Std.int(t));
	}
}
