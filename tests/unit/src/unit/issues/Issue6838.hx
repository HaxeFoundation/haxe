package unit.issues;

class Issue6838 extends unit.Test {
	function test() {
		var o = new Object();
		eq(#if jvm "unit.issues.Issue6838$Object" #else 'unit.issues._Issue6838.Object' #end, Type.getClassName(Type.getClass(o)));
	}
}

private class Object {
	public function new() {}
}