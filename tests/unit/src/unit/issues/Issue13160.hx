package unit.issues;

private class A {
	public function new() {}
	public dynamic function dyn() {}
}

private class B extends A {}

private class C extends B {}

private class D {
	public function new() {}
}

private class E extends D {
	public dynamic function dyn() {}
}

class Issue13160 extends Test {
	function test() {
		final a = new A();
		a.dyn = null;
		t(a.dyn == null);

		final b = new B();
		b.dyn = null;
		t(b.dyn == null);

		final c = new C();
		c.dyn = null;
		t(c.dyn == null);

		final e = new E();
		e.dyn = null;
		t(e.dyn == null);
	}
}
