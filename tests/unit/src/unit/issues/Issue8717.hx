package unit.issues;

class Issue8717 extends Test {
	function test() {
		var instance = Type.createInstance(unit.issues.misc.Issue8717Foo, []);
		t(Std.isOfType(instance, unit.issues.misc.Issue8717Foo));
	}
}