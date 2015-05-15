package unit.issues;

class Issue3516 extends Test
{
	public function test()
	{
		var b = new B();
		var c = b.uniqueFieldName(C);
		t((c is C));
		t(c != null);
	}
}

private class A<T> {
    public function new() {}
    public function uniqueFieldName(c:Class<T>):T {
        return Type.createInstance(c, []);
    }
}

private class B extends A<C> {}

private class C { @:keep public function new() {} }

class TestMain {
    static function main() {
    }
}
