package unit.issues;

@:callable
abstract Example(Void->String) {
	public function new() {
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