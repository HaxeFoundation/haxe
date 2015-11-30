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


class Issue1827 extends Test {
	//function test() {
		//var t = new C();
		//t.set(t.b, t.a);
		//eq(2, t.a);
		//eq(1, t.b);
	//}

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

	@:extern static inline function callInline(i1, i2) {
		return i1;
	}
}