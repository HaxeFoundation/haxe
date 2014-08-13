package unit.issues;

private abstract A(String) from String {
    @:from static function fromInt(v:Int):A return "abc";
}

private class TestClass {
    static public var a:A = 1;
	public var b:A = 1;

	public function new() { }
}

class Issue3244 extends Test {
	function test() {
		// treat as Dynamic so we can be sure the cast isn't generated here
		eq("abc", (TestClass.a : Dynamic));
		eq("abc", (new TestClass().b : Dynamic));
	}
}