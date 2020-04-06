package unit.issues;

class Issue8227 extends unit.Test {
	#if flash
	function test() {
		var ns = new NsCls();
		eq(ns.ns1v, 1);
		ns.ns1v = 50;
		eq(ns.ns1v, 50);

		eq(ns.ns2v, 2);
		ns.ns2v = 10;
		eq(ns.ns2v, 10);

		new Child(this);
	}
	#end
}

#if flash
@:access(unit.Test)
private class Child extends NsCls {
	public function new(test:unit.Test) {
		super();

		test.eq(ns1v, 1);
		ns1v = 50;
		test.eq(ns1v, 50);

		test.eq(ns2v, 2);
		ns2v = 10;
		test.eq(ns2v, 10);

		test.eq(ns1f(), 1);
		test.eq(call(ns1f), 1);

		test.eq(ns2f(), 2);
		test.eq(call(ns2f), 2);

		test.eq(NsCls.ns1sv, 1);
		NsCls.ns1sv = 50;
		test.eq(NsCls.ns1sv, 50);

		test.eq(NsCls.ns2sv, 2);
		NsCls.ns2sv = 10;
		test.eq(NsCls.ns2sv, 10);

		test.eq(NsCls.ns1sf(), 1);
		test.eq(call(NsCls.ns1sf), 1);

		test.eq(NsCls.ns2sf(), 2);
		test.eq(call(NsCls.ns2sf), 2);
	}

	static function call(f:()->Int):Int {
		return f();
	}
}
#end