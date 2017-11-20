package unit.issues;

private class C {
	public var a:Int;
	public var b:Int;

	public function new() {
		a = 1;
		b = 2;
	}

	public inline function set(inA:Int, inB:Int) {
		a = inA;
		b = inB;
	}
}

private class D {

	public var i:Int;

	public function new() {
		i = 0;
	}
}

private class E {
	public var a:Int;
	public var b:Int;

	public function new() {
		a = 1;
		b = 2;
	}

	@:extern public inline function set(inA:Int, inB:Int) {
		setA(inA);
		setB(inB);
	}

	public function setA(i:Int) a = i;
	public function setB(i:Int) b = i;
}


class Issue1827 extends Test {
	function test() {
		var t = new C();
		t.set(t.b, t.a);
		eq(2, t.a);
		eq(1, t.b);
	}

	static var i = 0;

	function test2() {
		var k = callInline(i, i++);
		eq(0, k);
		eq(1, i);

		var i = 0;
		var k = callInline(i, i++);
		eq(0, k);
		eq(1, i);

		var d = new D();
		var k = callInline(d.i, d.i++);
		eq(0, k);
		eq(1, d.i);
	}

	function test3() {
		var g = getInt();
		eq(1, call(g++, g));
		eq(1, g);
		eq(0, call(g, g++));
		eq(2, g);
		eq(1, call(g++, g++));
		eq(4, g);
		eq(-1, call2(g++, g));
		eq(5, g);
		eq(0, call2(g, g++));
		eq(6, g);
		eq(-1, call2(g++, g++));
		eq(8, g);
	}

	function test4() {
		var t = new E();
		var t2 = alias(t);
		t.set(t.b, t2.a);
		eq(2, t.a);
		eq(2, t2.a);
		eq(1, t.b);
		eq(1, t2.b);
	}

    static function alias(t:E) return t;

	static inline function call(d1, d2) {
		return d2 - d1;
	}

	static inline function call2(d1, d2) {
		return d1 - d2;
	}

	static function getInt() {
		return 0;
	}

	@:extern static inline function callInline(i1:Int, _) {
		return i1;
	}
}