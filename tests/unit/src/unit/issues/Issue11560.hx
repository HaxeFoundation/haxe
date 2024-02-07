package unit.issues;

@:keep
private class ParentClass {
	// no field variable, the issue does not reproduce when there is one
	function anyFunc() {}

	public function new() {}
}

@:keep
private class ChildClass extends ParentClass {
	var anyVar:String = null;
}

class Issue11560 extends Test {
	function test() {
		var c = new ChildClass();

		var json = haxe.Json.stringify(c, "\t");
		eq('{\n\t"anyVar": null\n}', json);
	}
}
