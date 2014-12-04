package unit.issues;

@:generic
private class A<T> {
    public function new() {}
    private function foo() {
		var b = new B<String>();
		b.foo();
	}
}

@:generic
private class B<T> {
	public function new() {}
	@:allow(unit.issues.A)
	private function foo() {}
}

class Issue3005 extends Test {
	@:access(unit.issues.A)
	function test() {
        var a = new A<String>();
        a.foo();
	}
}