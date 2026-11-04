package unit.issues;

private class Box {
	public var v:Int;

	public inline function new(v) {
		this.v = v;
	}
}

private abstract A(Box) {
	public inline function new(v) {
		this = new Box(v);
	}

	public static inline function twice(x:A) {
		return new A(x.get() * 2);
	}

	inline function get() {
		return this.v;
	}

	public function toString() {
		return "" + this.v;
	}
}

@:multiType(T)
private abstract M<T>(Array<T>) {
	public function new();

	@:to static inline function toIntArray(t:Array<Int>):Array<Int> {
		return [1, 2];
	}
}

class Issue13096 extends Test {
	static var calls = 0;

	static function next() {
		return new A(++calls);
	}

	static function none():A {
		return null;
	}

	static function int64(v:Null<Int>):Null<haxe.Int64> {
		return v == null ? null : haxe.Int64.ofInt(v);
	}

	function test() {
		eq("1", Std.string(next()));
		eq(1, calls);
		eq("4", Std.string(A.twice(new A(2))));
		eq("null", Std.string(none()));
		eq("-1", Std.string(-haxe.Int64.ofInt(1)));
		eq("2", Std.string(int64(2)));
		eq("null", Std.string(int64(null)));
		eq(new Map<String, Int>().toString(), Std.string(new Map<String, Int>()));
		// @:multiType abstract without toString
		eq("[1,2]", Std.string(new M<Int>()));
	}
}
