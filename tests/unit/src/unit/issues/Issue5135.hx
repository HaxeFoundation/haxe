package unit.issues;

class Issue5135 extends unit.Test {
	var obj2:Object2;
	function test() {
		try {
			obj2 = cast(new Object1(), Object2);
			t(false);
		} catch(e:Dynamic) {
			t(true);
		}
	}
}

private class Object1 {
	public function new() {}
}

private class Object2 {
	public function new() {}
}