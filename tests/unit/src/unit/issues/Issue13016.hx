package unit.issues;

private class Parent {
	public var fixed:Int;
	public var rest:Array<Int>;

	public function new(fixed:Int, ...rest:Int) {
		this.fixed = fixed;
		this.rest = rest.toArray();
	}
}

private class Child extends Parent {
	public function new(fixed:Int, rest:Array<Int>) {
		super(fixed, ...rest);
	}
}

class Issue13016 extends Test {
	static function call(a:Int, b:Int, ...r:Int) {
		return {a: a, b: b, r: r.toArray()};
	}

	function test() {
		var result = call(1, 2, ...[3, 4]);
		eq(1, result.a);
		eq(2, result.b);
		aeq([3, 4], result.r);

		// an empty spread must not disturb the fixed arguments either
		var empty = call(1, 2, ...([] : Array<Int>));
		eq(1, empty.a);
		eq(2, empty.b);
		aeq([], empty.r);

		// same expansion is used for constructor and super calls
		var child = new Child(1, [2, 3]);
		eq(1, child.fixed);
		aeq([2, 3], child.rest);
	}
}
