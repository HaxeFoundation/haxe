package unit.issues;

private abstract MyAbstract<T>(Array<T>) from Array<T> {
	@:op(++A) public function pre()
		return "pre";
}

class Issue4357 extends Test {
	function test() {
		var a:MyAbstract<Int> = [123];
		eq("pre", ++a);
	}
}