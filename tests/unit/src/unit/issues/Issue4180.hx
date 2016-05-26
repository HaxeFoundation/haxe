package unit.issues;

private abstract A(Int) {
	public function new(v) this = v;
	@:op(A-B) function _(a:Int):A;
	public function get() return this;
}

class Issue4180 extends Test {
	@:isVar static var a(get,set):A;
	static function set_a(value:A):A return a = value;
	static function get_a():A return a;

	static var a2(default,set):A;
	static function set_a2(value:A):A return a2 = value;

	function test() {
		a = new A(0);
		a -= 5;
		eq(-5, a.get());

		a2 = new A(0);
		a2 -= 5;
		eq(-5, a2.get());
	}
}