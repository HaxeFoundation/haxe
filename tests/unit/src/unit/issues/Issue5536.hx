package unit.issues;

@:autoBuild(unit.issues.misc.Issue5536Macro.build())
private class Builder {
	public function new() {}
}

@:generic // without @:generic all is correct
private class A<T> extends Builder {}

private class B extends A<String> {}

class Issue5536 extends Test {
	function test() {
		eq("unit.issues._Issue5536.A, unit.issues._Issue5536.B", unit.issues.misc.Issue5536Macro.getBuilt());
	}
}
