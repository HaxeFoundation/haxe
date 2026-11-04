package unit.issues;

private abstract AbsFloat(Float) from Float to Float {
	public inline function new(i:Float) {
		this = i;
	}

	@:to public inline function toInt() {
		return Std.int(this);
	}

	@:to public inline function toFloat() {
		return this;
	}

	@:from static public inline function fromFloat(f:Float) {
		return new AbsFloat(f);
	}

	@:from static public inline function fromInt(i:Int) {
		return new AbsFloat(i);
	}
}

class Issue9136 extends unit.Test {
	static inline var k:AbsFloat = 1;

	function test() {
		var i:Int = k;
		eq(1, i);
	}
}
