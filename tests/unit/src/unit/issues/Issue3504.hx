package unit.issues;

private class A {
	public var a:Int;
	public function new(a) this.a = a;
}

private class B extends A {
	public function new(a = false) {
		super(a ? 1 : 0);
	}
}

private class C extends A {
    public function new(a:Null<Bool> = false) {
        super(a ? 1 : 0);
    }
}

class Issue3504 extends Test {
	function test() {
		eq(new B().a, 0);
		eq(new B(false).a, 0);
		eq(new B(true).a, 1);

		eq(new C().a, 0);
		eq(new C(false).a, 0);
		eq(new C(true).a, 1);
	}
}
