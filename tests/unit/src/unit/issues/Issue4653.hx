package unit.issues;

@:genericBuild(unit.issues.misc.Issue4653Macro.getType())
private class C<T> { }

@:genericBuild(unit.issues.misc.Issue4666Macro.getType())
private class C2<T> { }

class Issue4653 extends Test {
	function test() {
		var s = C;
		eq("A", s.fromCharCode(65));

		var s2 = cast("foo", C<Dynamic>);
		eq("FOO", s2.toUpperCase());

		var d = C2;
		// no idea how to test this properly
	}
}