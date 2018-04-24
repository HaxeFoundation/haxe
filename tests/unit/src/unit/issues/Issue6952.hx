package unit.issues;

@:generic class Issue6952TestClass<@:const T> {
	public function new() {}
	public function foo() {
		trace(T);
	}
}

class Issue6952 extends Test {

	function test() {
		var x = new Issue6952TestClass<"hello world">();
		x.foo();
	}
}