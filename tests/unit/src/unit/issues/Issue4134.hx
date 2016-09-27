package unit.issues;

private class A {
	public function new() { }
}

private class B extends A { }

class Issue4134 extends Test {
	function test() {
		eq("first", throwSomething(new B()));
		eq("second", throwSomething(new A()));
	}

	function throwSomething(a:A) {
		try {
			throw a;
		}
		catch(b:B) {
			return "first";
		}
		catch(a:A) {
			return "second";
		}
	}
}