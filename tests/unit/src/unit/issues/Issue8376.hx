package unit.issues;

class Issue8376 extends unit.Test {
	function test() {
		eq(5, ("hello":Foo).foo(5));
	}
}

private abstract Foo(String) from String {
	public function foo(i:Int) {
		return (function() {
			return (function() return baz(i))();
		})();
	}

	function baz(i:Int) return i;
}
