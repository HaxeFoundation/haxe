package unit.issues;

@rtti("hello")
private class SpodRtti {}

class Issue5757 extends unit.Test {
	function test() {
		var rtti = haxe.rtti.Meta.getType( SpodRtti ).rtti;
		t(rtti != null);
	}
}
