package unit.issues;

private class C {
	@:astSource
	public function doSomething(s:String) {
		s += s;
	}
}

class Issue3285 extends Test {
	function test() {
		var s = "foo";
		@:useAstSource C.doSomething;
		eq("foofoo", s);
	}
}