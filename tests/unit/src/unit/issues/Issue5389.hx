package unit.issues;

@test
private class C {
}

private class C1 {
	@test function f() {}
}

class Issue5389 extends unit.Test {
	function test() {
		eq(0, Reflect.fields(haxe.rtti.Meta.getFields(C)).length);
		eq(0, Reflect.fields(haxe.rtti.Meta.getStatics(C)).length);
		eq(0, Reflect.fields(haxe.rtti.Meta.getType(C1)).length);
	}
}
