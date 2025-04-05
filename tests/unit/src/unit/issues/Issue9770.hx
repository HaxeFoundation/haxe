package unit.issues;

private abstract A1(I) {
	public var v(get, set):Int;

	public function new(self:I) {
		this = self;
	}

	function get_v():Int
		return this.v;

	function set_v(v:Int):Int
		return this.v = v;
}

private interface I {
	var v(get, set):Int;
}

private abstract A2(T) {
	public var v(get, set):Int;

	public function new(self:T) {
		this = self;
	}

	function get_v():Int
		return this.v;

	function set_v(v:Int):Int
		return this.v = v;
}

private typedef T = {
	var v(get, set):Int;
}

private class C implements I {
	@:isVar public var v(get, set):Int;

	public function new(value:Int) {
		v = value;
	}

	public function get_v():Int {
		return v;
	}

	public function set_v(value:Int):Int {
		return v = value;
	}
}

class Issue9770 extends unit.Test {
	function test() {
		var c = new C(12);
		eq(12, c.v);
		eq(13, c.v = c.v + 1);
		eq(13, c.v);
		eq(14, c.v += 1);
		eq(14, c.v);
		eq(14, c.v++);
		eq(15, c.v);
		eq(16, ++c.v);
		eq(16, c.v);

		var c:I = new C(12);
		eq(12, c.v);
		eq(13, c.v = c.v + 1);
		eq(13, c.v);
		eq(14, c.v += 1);
		eq(14, c.v);
		eq(14, c.v++);
		eq(15, c.v);
		eq(16, ++c.v);
		eq(16, c.v);

		var c:T = new C(12);
		eq(12, c.v);
		eq(13, c.v = c.v + 1);
		eq(13, c.v);
		eq(14, c.v += 1);
		eq(14, c.v);
		eq(14, c.v++);
		eq(15, c.v);
		eq(16, ++c.v);
		eq(16, c.v);

		var c = new A1(new C(12));
		eq(12, c.v);
		eq(13, c.v = c.v + 1);
		eq(13, c.v);
		eq(14, c.v += 1);
		eq(14, c.v);
		eq(14, c.v++);
		eq(15, c.v);
		eq(16, ++c.v);
		eq(16, c.v);

		var c = new A2(new C(12));
		eq(12, c.v);
		eq(13, c.v = c.v + 1);
		eq(13, c.v);
		eq(14, c.v += 1);
		eq(14, c.v);
		eq(14, c.v++);
		eq(15, c.v);
		eq(16, ++c.v);
		eq(16, c.v);
	}
}