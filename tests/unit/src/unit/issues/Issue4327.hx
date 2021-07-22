package unit.issues;

@:callable
abstract Example(()->String) {
	public function new() {
		this = null;
		this = fun;
	}

	function fun() {
		return "foo";
	}
}

class Issue4327 extends Test {
	function test() {
		eq("foo", new Example()());
	}
}