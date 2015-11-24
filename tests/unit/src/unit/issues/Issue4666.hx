package unit.issues;

@:genericBuild(unit.issues.misc.Issue4666Macro.getType())
private class C { }

class Issue4666 extends Test {
	public function test() {
		var c:C;
		unit.TestType.typedAs(c, (null:Dynamic));
	}
}