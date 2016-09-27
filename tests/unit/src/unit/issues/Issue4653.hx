package unit.issues;

@:genericBuild(unit.issues.misc.Issue4653Macro.getType())
private class C<T> { }

@:genericBuild(unit.issues.misc.Issue4653Macro.getStringType())
private class C2<T> { }

@:genericBuild(unit.issues.misc.Issue4666Macro.getType())
private class C3<T> { }

private class NotString {
	static public function fromCharCode(i:Int) {
		return String.fromCharCode(i);
	}
}

class Issue4653 extends Test {
	function test() {
		var s = C;
		eq("A", s.fromCharCode(65));

		var s2 = cast("foo", C2<Dynamic>);
		eq("FOO", s2.toUpperCase());

		var d = C3;
		// no idea how to test this properly
	}
}