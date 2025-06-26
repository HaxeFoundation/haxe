package unit.issues;

import unit.Test;

class Issue9060 extends Test {
	function test() {
		var i64 = new Int64(new Impl());
		eq("helloworld", i64.prefixDecrement());
	}
}

private class Impl {
	public inline function new() {}
}

private abstract Int64(Impl) from Impl {
	static public var MIN(get, never):Int64;

	static function get_MIN():Int64 {
		return new Int64(new Impl());
	}

	public function new(value:Impl) {
		this = value;
	}

	inline function equal(b:Int64):Bool {
		return this != null;
	}

	inline public function prefixDecrement() {
		var s = "";
		if (MIN.equal(new Int64(this))) {
			s += "hello";
		}
		s += "world";
		return s;
	}
}