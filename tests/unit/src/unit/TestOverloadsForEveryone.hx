package unit;

private class StaticOverloadClass {
	overload extern static public inline function test(i:Int) {
		return "Int: " + i;
	}

	overload extern static public inline function test(s:String) {
		return "String: " + s;
	}
}

private class MemberOverloadClass {
	public function new() {

	}

	overload extern public inline function test(i:Int) {
		return "Int: " + i;
	}

	overload extern public inline function test(s:String) {
		return "String: " + s;
	}
}

class TestOverloadsForEveryone extends Test {
	function test() {
		eq("Int: 12", StaticOverloadClass.test(12));
		eq("String: foo", StaticOverloadClass.test("foo"));

		var moc = new MemberOverloadClass();
		eq("Int: 12", moc.test(12));
		eq("String: foo", moc.test("foo"));
	}
}
