package unit.issues;

@:generic class Issue6952TestClass<@:const T> {
	public function new() {}
	public function foo() {
		return T;
	}
}

class Issue6952 extends Test {

	function test() {
		var x = new Issue6952TestClass<"hello world">();
		eq("hello world", x.foo());

		var r1:EReg = new Issue6952TestClass<~/a/>().foo();
		var r2:EReg = new Issue6952TestClass<~/b/>().foo();

		eq(true, r1.match("a"));
		eq(false, r1.match("b"));
		eq(false, r2.match("a"));
		eq(true, r2.match("b"));
	}
}
