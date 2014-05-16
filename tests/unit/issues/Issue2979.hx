package unit.issues;

private class A<T> {}

private abstract B<T>(T) {
    public function toA():A<T> return null;
    public function a() return toA();
}

class Issue2979 extends Test {
	function test() {
        var b:B<String> = null;
        var a:A<String>;
		unit.TestType.typedAs(a, b.a());
		unit.TestType.typedAs(a, b.toA());
	}
}