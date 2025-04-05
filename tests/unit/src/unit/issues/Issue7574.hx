package unit.issues;

@:generic
private class Foo<@:const FOO:Dynamic> {
	var foo = cast(FOO, String);

	public function new() {}

	public function test() {
		return foo;
	}
}

class Issue7574 extends unit.Test {
	function test() {
		eq("X", new Foo<"X">().test());
		eq("Y", new Foo<"Y">().test());
	}
}
