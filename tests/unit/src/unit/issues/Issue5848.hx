package unit.issues;

private abstract Degree(Float) {
	public inline function new (f:Float) {
		this = f;
	}
	@:from // removing this line makes it compile
	public static function fromRadian (d:Radian):Degree {
		return new Degree(d.float());
	}
	public inline function float ():Float return this;
}

private abstract Radian(Float) {
	public inline function new (f:Float) {
		this = f;
	}

	@:from static inline function fromDegree (d:Degree):Radian {
		return new Radian(d.float());
	}

	public inline function float ():Float return this;
}

private class Vec2s {
	public static function foo (radian:Radian) {
		return 1;
	}
}

class Issue5848 extends unit.Test {
	function test():Void {
		ff(new Degree(90));
	}

	static public function ff(r:Radian) {
		Vec2s.foo(r);
	}
}