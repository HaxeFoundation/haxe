package unit.issues;

@:genericBuild(unit.issues.misc.Issue3183Macro.buildTuple())
class MyTuple<Rest> { }

class Issue3183 extends Test {
	function test() {
		var t = new MyTuple<String, Int>("foo", 12);
		unit.TestType.typedAs(t, (null : MyTuple<String, Int>));
		eq("foo", t.v0);
		eq(12, t.v1);

		var t = new MyTuple("foo", 12);
		unit.TestType.typedAs(t, (null : MyTuple<String, Int>));
		eq("foo", t.v0);
		eq(12, t.v1);

		var t:MyTuple = new MyTuple("foo", 12);
		unit.TestType.typedAs(t, (null : MyTuple<String, Int>));
		eq("foo", t.v0);
		eq(12, t.v1);
	}
}