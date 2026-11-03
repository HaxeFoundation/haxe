package unit.issues;

@:keep
private class C {
	@foo.bar
	public var foo:Bool;

	@foo.bar(1)
	static public var sfoo:Bool;

	public function new() {}
}

class Issue10764 extends unit.Test {
	function test() {
		var m = haxe.rtti.Meta.getFields(C);
		t(Reflect.hasField(m.foo, "foo.bar"));
		eq(null, Reflect.field(m.foo, "foo.bar"));

		var m = haxe.rtti.Meta.getStatics(C);
		t(Reflect.hasField(m.sfoo, "foo.bar"));
		eq(1, Reflect.field(m.sfoo, "foo.bar")[0]);
	}
}
