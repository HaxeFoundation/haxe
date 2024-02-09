package unit.issues;

class Issue10343 extends Test {
	function test() {
		final str = "hello";
		eq(str.length, foo(str));
	}

	@:pure(false)
	public function foo<T:String>(str:T):Int {
		return str.length;
	}
}
