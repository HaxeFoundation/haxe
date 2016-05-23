package unit.issues;

private class C {
	public var field:String;

	public function new(b:Bool) {
		if (b) {
			field = "";
		}
	}
}

class Issue5053 extends unit.Test {
	function test() {
		var c = new C(true);
		eq("", c.field);
		var c2 = new C(false);
		eq(null, c2.field);
	}
}