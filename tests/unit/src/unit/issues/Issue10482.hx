package unit.issues;

class Issue10482 extends Test {
	function test() {
		final ab = new MyAbstract(1);
		final arr = ab.foo();
		eq('plus 2', arr[0]);
		eq("call", arr[1]);
		eq("hi", arr[2]);

		eq(1, ab.arr()[0].arr()[0].value());
		eq("plus 3", ab + 3);
		eq("call", ab());
		eq("hi", ab.arr()[0].hi());

		final ab = new MyInlineAbstract(1);
		final arr = ab.foo();
		eq('plus 2', arr[0]);
		eq("call", arr[1]);
		eq("hi", arr[2]);

		eq(1, ab.arr()[0].arr()[0].value());
		eq("plus 3", ab + 3);
		eq("call", ab());
		eq("hi", ab.arr()[0].hi());
	}
}

abstract MyAbstract(Int) {
	public function new(a):Void this = a;

	public function foo():Array<String> {
		return [
			abstract + 2,
			abstract(),
			abstract.hi()
		];
	}

	public function hi() return "hi";

	public function arr() return [abstract];

	@:op(a()) function call() return "call";

	@:op(a + b) function plus(b) return 'plus $b';

	public function value():Int return this;
}

abstract MyInlineAbstract(Int) {
	public inline function new(a):Void this = a;

	public inline function foo():Array<String> {
		return [
			abstract + 2,
			abstract(),
			abstract.hi()
		];
	}

	public function hi() return "hi";

	public function arr() return [abstract];

	@:op(a()) function call() return "call";

	@:op(a + b) function plus(b) return 'plus $b';

	public function value():Int return this;
}

