package unit.issues;

class Issue5557 extends unit.Test {
	function test() {
		var a = ["foo"];
		var v = a.pop();
		for (o in a)
			throw o;
		eq("foo", v);
	}
}