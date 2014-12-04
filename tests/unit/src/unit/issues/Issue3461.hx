package unit.issues;

private abstract E<T1>(Dynamic) from T1 to T1 {}

class Issue3461 extends Test {
	function test() {
        var a:E<Dynamic<String>> = { value: "foo" };
        var b:Dynamic<String> = a;
		a = b;
		eq("foo", b.value);
	}
}