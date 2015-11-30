package unit.issues;

private class Ghost {
	public var value:String;
	public function new(value) {
		this.value = value;
	}
}

private class A {
	public var ghost = new Ghost("booh!");
}

private class B extends A { }

private class C extends B {
	public function new() { }
}

class Issue4557 extends Test {
	function test() {
		var c = new C();
		eq("booh!", c.ghost.value);
	}
}