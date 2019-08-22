package unit.issues;

class Issue8299 extends unit.Test {
	function test() {
		eq(-1, foo(1, 2));
	}

	static function foo<T:Int>(a:T, b:T):Int {
		return a - b;
	}
}
