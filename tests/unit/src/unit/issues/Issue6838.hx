package unit.issues;

class Issue6838 extends unit.Test {
	function test() {
		var o = new Object();
		eq('unit.issues._Issue6838.Object', Type.getClassName(Type.getClass(o)));
	}
}

private class Object {
	public function new() {}
}